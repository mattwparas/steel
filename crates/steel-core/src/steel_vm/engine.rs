#![allow(unused)]

use super::{
    builtin::{BuiltInModule, FunctionSignatureMetadata},
    primitives::{register_builtin_modules, CONSTANTS},
    vm::{threads::ThreadHandle, SteelThread, Synchronizer, ThreadStateController},
};

#[cfg(feature = "dylibs")]
use super::{ffi::FFIModule, ffi::FFIWrappedModule};

#[cfg(feature = "dylibs")]
use super::dylib::DylibContainers;

use crate::{
    compiler::{
        compiler::{Compiler, SerializableCompiler},
        map::SymbolMap,
        modules::{
            intern_modules, path_to_module_name, CompiledModule, SourceModuleResolver,
            MANGLER_PREFIX, PRELUDE_WITHOUT_BASE,
        },
        program::{Executable, RawProgramWithSymbols, SerializableRawProgramWithSymbols},
    },
    containers::RegisterValue,
    core::{
        instructions::{pretty_print_dense_instructions, DenseInstruction, Instruction},
        labels::Expr,
    },
    gc::{
        shared::StandardShared,
        unsafe_erased_pointers::{
            BorrowedObject, CustomReference, OpaqueReferenceNursery, ReadOnlyBorrowedObject,
            ReferenceMarker,
        },
        Gc, Shared,
    },
    parser::{
        ast::ExprKind,
        expander::SteelMacro,
        interner::{get_interner, take_interner, InternedString},
        kernel::{fresh_kernel_image, Kernel},
        parser::{ParseError, Parser, Sources, SYNTAX_OBJECT_ID},
    },
    rerrs::{back_trace, back_trace_to_string},
    rvals::{
        AsRefMutSteelVal, AsRefSteelVal as _, FromSteelVal, IntoSteelVal, MaybeSendSyncStatic,
        Result, SteelString, SteelVal,
    },
    steel_vm::{
        builtin::{generate_function, register_context_functions},
        primitives::bootstrap_globals,
        register_fn::RegisterFn,
    },
    stop, throw,
    values::{
        closed::GlobalSlotRecycler,
        functions::{BoxedDynFunction, ByteCodeLambda},
    },
    SteelErr,
};
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use crate::values::HashMap as ImmutableHashMap;
use lasso::ThreadedRodeo;
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use rustc_hash::{FxBuildHasher, FxHashMap};
use serde::{Deserialize, Serialize};
use steel_gen::OpCode;
use steel_parser::{
    ast::List,
    parser::{SourceId, SyntaxObject},
    tokens::{IntLiteral, TokenType},
};

#[cfg(all(feature = "sync", feature = "biased", not(feature = "triomphe")))]
use steel_rc::QueueHandle;

use crate::parser::ast::IteratorExtensions;

thread_local! {
    static KERNEL_BIN_FILE: Cell<Option<&'static [u8]>> = const { Cell::new(None) };
}

// Install the binary file to be used during bootup
// pub fn install_bin_file(bin: &'static [u8]) {
//     KERNEL_BIN_FILE.with(|x| x.set(Some(bin)));
// }

#[cfg(not(feature = "sync"))]
pub trait ModuleResolver {
    fn resolve(&self, name: &str) -> Option<BuiltInModule>;
    fn names(&self) -> Vec<String>;
}

#[cfg(feature = "sync")]
pub trait ModuleResolver: MaybeSendSyncStatic {
    fn resolve(&self, name: &str) -> Option<BuiltInModule>;
    fn names(&self) -> Vec<String>;
}

#[derive(Clone, Default)]
pub struct ModuleContainer {
    modules: Arc<RwLock<HashMap<Shared<str>, BuiltInModule>>>,
    // For modules that don't exist in memory. This could be useful for a world
    // in which a builtin module exists BUT we'd like to resolve the module for
    // inference purposes.
    unresolved_modules: Arc<RwLock<Option<StandardShared<dyn ModuleResolver>>>>,
}

impl ModuleContainer {
    pub fn with_expected_capacity() -> Self {
        Self {
            modules: Arc::new(RwLock::new(HashMap::with_capacity(48))),
            unresolved_modules: Default::default(),
        }
    }

    pub fn insert(&mut self, key: Shared<str>, value: BuiltInModule) {
        self.modules.write().insert(key, value);
    }

    pub fn get_doc(&self, key: &str) -> Option<String> {
        for module in self.modules.read().values() {
            let maybe_doc = module.get_documentation(key);

            if maybe_doc.is_some() {
                return maybe_doc;
            }
        }

        None
    }

    pub fn get_metadata_by_name(&self, key: &str) -> Option<FunctionSignatureMetadata> {
        for module in self.modules.read().values() {
            let maybe_meta = module.search_by_name(key);

            if maybe_meta.is_some() {
                return maybe_meta;
            }
        }

        None
    }

    pub fn get(&self, key: &str) -> Option<BuiltInModule> {
        let mut guard = self.modules.write();
        let found = guard.get(key).cloned();

        found.or_else(|| {
            let res = self
                .unresolved_modules
                .read()
                .as_ref()
                .and_then(|x| x.resolve(key));

            if let Some(res) = res {
                // Define the module now.
                guard.insert(Shared::from(key), res.clone());

                Some(res)
            } else {
                None
            }
        })
    }

    pub(crate) fn deep_clone(&self) -> Self {
        Self {
            modules: Arc::new(RwLock::new(self.modules.read().clone())),
            unresolved_modules: Arc::new(RwLock::new(self.unresolved_modules.read().clone())),
        }
    }

    pub fn inner(&self) -> RwLockReadGuard<'_, HashMap<Shared<str>, BuiltInModule>> {
        self.modules.read()
    }

    pub(crate) fn inner_mut(
        &mut self,
    ) -> RwLockWriteGuard<'_, HashMap<Shared<str>, BuiltInModule>> {
        self.modules.write()
    }

    pub fn with_resolver<T: ModuleResolver + 'static>(&mut self, resolver: T) {
        *self.unresolved_modules.write() = Some(StandardShared::new(resolver));
    }
}

#[derive(Debug)]
pub struct EngineStatistics {
    pub rooted_count: usize,
    pub constants_count: usize,
    pub sources_size: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalCheckpoint {
    symbol_map_offset: usize,
    globals_offset: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EngineId(pub(crate) usize);

impl EngineId {
    pub fn new() -> Self {
        static ENGINE_ID: AtomicUsize = AtomicUsize::new(0);
        let id = ENGINE_ID.fetch_add(1, Ordering::Relaxed);
        Self(id)
    }

    pub fn as_usize(self) -> usize {
        self.0
    }
}

/// Handle to a steel engine. This contains a main entrypoint thread, alongside
/// the compiler and all of the state necessary to keep a VM instance alive and
/// well.
pub struct Engine {
    pub(crate) virtual_machine: SteelThread,
    // TODO: Just put this, and all the other things,
    // inside the `SteelThread` - The compiler probably
    // still... needs to be shared, but thats fine.
    modules: ModuleContainer,
    #[cfg(feature = "dylibs")]
    dylibs: DylibContainers,
    pub(crate) id: EngineId,
    // For a kernel image: what it ran to boot, which engines replay (see `BootImage`)
    boot: Option<Arc<BootRecord>>,
}

/// While a boot image is made: what an engine is made to do, in order, so
/// another can be made to do the same (see `BootStep`). A copy of a thread or
/// an engine records nothing.
#[derive(Default)]
pub(crate) struct Recording(Option<Arc<parking_lot::Mutex<Vec<BootStep>>>>);

impl Clone for Recording {
    fn clone(&self) -> Self {
        Recording(None)
    }
}

impl BootStep {
    // The step, its program reading `constants`
    pub(crate) fn with_constants(&self, constants: &crate::compiler::constants::ConstantMap) -> BootStep {
        let with = |program: &Executable| {
            let mut program = program.clone();
            program.constant_map = constants.clone();
            program
        };
        match self {
            BootStep::Run(program) => BootStep::Run(with(program)),
            BootStep::Eval(program) => BootStep::Eval(with(program)),
            other => other.clone(),
        }
    }
}

impl Recording {
    pub(crate) fn start(&mut self) {
        self.0 = Some(Default::default());
    }

    pub(crate) fn record(&self, step: impl FnOnce() -> BootStep) {
        if let Some(steps) = &self.0 {
            steps.lock().push(step());
        }
    }

    // What was recorded, and stops recording
    pub(crate) fn finish(&mut self) -> Vec<BootStep> {
        self.0
            .take()
            .map(|steps| core::mem::take(&mut *steps.lock()))
            .unwrap_or_default()
    }
}

/// The programs `eval` compiled while a recording was made, which `eval` runs
/// instead while the recording is replayed (`replay_steps`)
#[derive(Default)]
pub(crate) struct Replaying(Option<Arc<parking_lot::Mutex<(Vec<Executable>, usize)>>>);

impl Clone for Replaying {
    fn clone(&self) -> Self {
        Replaying(None)
    }
}

impl Replaying {
    // The next one, during a replay
    pub(crate) fn next(&self) -> Option<Executable> {
        let replay = self.0.as_ref()?;
        let mut replay = replay.lock();
        let (programs, next) = &mut *replay;
        let program = programs.get(*next)?.clone();
        *next += 1;
        Some(program)
    }
}

/// Runs recorded steps in `engine` again. `eval` doesn't compile: it runs what
/// it compiled when the steps were recorded, so a replay leaves the compiler as
/// it is. A registered value comes from `registered`.
pub(crate) fn replay_steps(
    engine: &mut Engine,
    steps: &[BootStep],
    registered: &dyn Fn(&str) -> Result<SteelVal>,
) -> Result<Vec<SteelVal>> {
    let evals: Vec<Executable> = steps
        .iter()
        .filter_map(|step| match step {
            BootStep::Eval(program) => Some(program.clone()),
            _ => None,
        })
        .collect();
    let replay = Arc::new(parking_lot::Mutex::new((evals, 0)));
    engine.virtual_machine.replaying = Replaying(Some(replay.clone()));

    let mut results = Vec::new();
    let mut eval_index = 0;
    let outcome = (|| {
        for step in steps {
            match step {
                BootStep::Run(program) => {
                    results = engine.virtual_machine.run_executable(program)?;
                }
                BootStep::Eval(program) => {
                    // Run here only if no `Run` before ran it through `eval`
                    let mut replay_state = replay.lock();
                    if replay_state.1 <= eval_index {
                        replay_state.1 = eval_index + 1;
                        drop(replay_state);
                        engine.virtual_machine.run_executable(program)?;
                    }
                    eval_index += 1;
                }
                BootStep::Cleared(_, slot) => {
                    engine.virtual_machine.insert_binding(*slot, SteelVal::Void);
                }
                BootStep::Registered(name, slot) => {
                    let value = registered(name)?;
                    engine.virtual_machine.insert_binding(*slot, value);
                }
            }
        }
        Ok(())
    })();
    engine.virtual_machine.replaying = Replaying(None);
    outcome.map(|()| results)
}

/// Something an engine was made to do while a boot image was made.
#[derive(Clone)]
pub(crate) enum BootStep {
    /// Ran this program
    Run(Executable),
    /// `eval` compiled and ran this program: from the host, or inside the last
    /// `Run` (a replay runs it in place of compiling, see `replay_steps`)
    Eval(Executable),
    /// The host set this global, in this slot, to void
    Cleared(String, usize),
    /// The host registered a value under this name, in this slot. The record can't
    /// make the value again: whoever replays it supplies one.
    Registered(String, usize),
}

pub(crate) mod boot_image;

/// What `Engine::new_kernel` did to make a kernel image: the engine as it was with
/// only the builtin modules registered, and the compiled programs it ran after.
pub(crate) struct BootRecord {
    builtins: Engine,
    programs: Vec<Executable>,
    // Copies of the functions the programs' compiles put in the constant map,
    // taken before anything ran them (see `untouched_closures`)
    closures: Vec<(usize, Gc<ByteCodeLambda>)>,
}

/// What engines made by `Engine::new` start from, built once per process (per
/// thread without `sync`) for each pair of sandbox settings.
///
/// Compiling the prelude is most of the time it takes to make an engine, and
/// the bytecode is the same each time. So it's compiled once, and each engine
/// takes a copy of the compiler state that leaves and runs the compiled
/// programs again, from the engine with only the builtins registered. That way
/// every engine gets its own values (boxes, parameters, mutable structs), as
/// if it had compiled the prelude itself. The macro kernel is copied
/// (`Kernel::deep_clone`). `STEEL_BOOT_IMAGE=0` compiles the prelude in every
/// engine instead.
pub(crate) struct BootImage {
    builtins: Engine,
    programs: Vec<Executable>,
    closures: Vec<(usize, Gc<ByteCodeLambda>)>,
    // An engine that ran the programs. Engines copy its compiler and kernel.
    booted: Engine,
    // What compiling the prelude left in the expander's per-thread state, for
    // engines made on other threads
    expander: Vec<crate::parser::expand_visitor::ThreadExpanderState>,
    // What the booted kernel's engine was made to do, from when it was made
    pub(crate) kernel_steps: Vec<BootStep>,
    // An image made from another (`derive_pass_image`) does these after its
    // programs, with the values of the host modules it registers
    steps: Vec<BootStep>,
    host_modules: Vec<HostModule>,
    // What the kernel's engine did after the base image's (`derive_pass_image`)
    derived_kernel_steps: Vec<BootStep>,
    // An image read as what it adds to another: `booted` is the other's engine,
    // and each engine applies this to its copy of the compiler
    delta: Option<Arc<boot_image::DerivedDelta>>,
    // The checksum of the file this image is kept in, which an image saved as
    // what it adds to this one names: one read from the cache, or stored there
    stored_as: Option<u64>,
}

/// A builtin module an image registers after the prelude, made again by whoever
/// replays it
#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum HostModule {
    PassSupport,
}

impl HostModule {
    fn module(self) -> BuiltInModule {
        match self {
            HostModule::PassSupport => crate::compiler::passes::steel_passes::pass_support_module(),
        }
    }
}

// The program the Steel passes' engine runs: its value is the registry's `passes-at`
pub(crate) const PASS_ENGINE_PROGRAM: &str = "(require \"#%private/steel/passes\")\npasses-at\n";

impl Engine {
    pub fn enter_safepoint<T, F: FnMut() -> T>(&mut self, mut thunk: F) -> T {
        let mut res = None;

        self.virtual_machine
            .enter_safepoint(|_| -> Result<SteelVal> {
                res = Some((thunk)());
                Ok(SteelVal::Void)
            });

        res.unwrap()
    }
}

/*
#[cfg(feature = "biased")]
impl Drop for Engine {
    fn drop(&mut self) {
        steel_rc::QueueHandle::run_explicit_merge();
    }
}
*/

impl Clone for Engine {
    fn clone(&self) -> Self {
        let mut virtual_machine = self.virtual_machine.clone();

        // Set up child
        virtual_machine.synchronizer = Synchronizer::new();

        let weak_ctx = Arc::downgrade(&virtual_machine.synchronizer.ctx);

        // Get a handle to the current thread?
        let handle = ThreadHandle {
            handle: Mutex::new(None),
            thread: std::thread::current(),
            thread_state_manager: virtual_machine.synchronizer.state.clone(),
            forked_thread_handle: None,
            result: Default::default(),
        }
        .into_steelval()
        .unwrap();

        // TODO: Entering safepoint should happen often
        // for the main thread?
        virtual_machine
            .synchronizer
            .register(super::vm::ThreadContext {
                ctx: weak_ctx,
                handle,
            });

        let compiler = Arc::new(RwLock::new(self.virtual_machine.compiler.write().clone()));

        // virtual_machine.compiler = Some(Arc::downgrade(&compiler));

        Self {
            virtual_machine,
            // compiler,
            modules: self.modules.clone(),
            #[cfg(feature = "dylibs")]
            dylibs: self.dylibs.clone(),
            id: EngineId::new(),
            boot: self.boot.clone(),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

// Pre-parsed ASTs along with the global state to set before we start any further processing
// #[derive(Serialize, Deserialize)]
// struct BootstrapImage {
//     interner: Arc<ThreadedRodeo<lasso::Spur, FxBuildHasher>>,
//     syntax_object_id: usize,
//     sources: Sources,
//     programs: Vec<Vec<ExprKind>>,
// }

// Pre compiled programs along with the global state to set before we start any further processing
struct StartupBootstrapImage {
    syntax_object_id: usize,
    function_id: usize,
    sources: Sources,
    pre_kernel_programs: Vec<SerializableRawProgramWithSymbols>,
    post_kernel_programs: Vec<SerializableRawProgramWithSymbols>,
    kernel: Option<KernelImage>,
    compiler: Option<SerializableCompiler>,
}

struct KernelImage {
    // Kernel macros
    compiler: SerializableCompiler,
    sources: Sources,
    kernel_source: SerializableRawProgramWithSymbols,
}

#[derive(Serialize, Deserialize)]
pub struct NonInteractiveProgramImage {
    sources: Sources,
    program: SerializableRawProgramWithSymbols,
}

impl NonInteractiveProgramImage {
    pub fn write_bytes_to_file(&self, out: &Path) {
        let mut f = std::fs::File::create(out).unwrap();
        bincode::serialize_into(&mut f, self).unwrap();
    }

    pub fn as_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        bincode::serialize_into(&mut out, self).unwrap();
        out
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        bincode::deserialize(bytes).unwrap()
    }
}

// fn steel_create_bootstrap() {
//     Engine::create_bootstrap_from_programs("src/boot/bootstrap.bin".into());
// }

pub struct LifetimeGuard<'a> {
    engine: &'a mut Engine,
    count: usize,
}

impl<'a> Drop for LifetimeGuard<'a> {
    fn drop(&mut self) {
        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::free_n(self.count);
    }
}

impl<'a> LifetimeGuard<'a> {
    pub fn with_immutable_reference<
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        mut self,
        obj: &'a T,
    ) -> Self {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            core::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_ro_object::<T, EXT>(
            obj,
        );
        self.count += 1;

        self
    }

    pub fn with_mut_reference<
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        mut self,
        obj: &'a mut T,
    ) -> Self {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            core::any::TypeId::of::<EXT>()
        );
        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_rw_object::<T, EXT>(
            obj,
        );

        self.count += 1;

        self
    }

    pub fn consume<T>(self, mut thunk: impl FnMut(&mut Engine, Vec<SteelVal>) -> T) -> T {
        let values =
            crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::drain_weak_references_to_steelvals(self.count);

        thunk(self.engine, values)
    }

    pub fn consume_once<T>(self, mut thunk: impl FnOnce(&mut Engine, Vec<SteelVal>) -> T) -> T {
        let values =
            crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::drain_weak_references_to_steelvals(self.count);

        thunk(self.engine, values)
    }
}

impl RegisterValue for Engine {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        let idx = self.virtual_machine.compiler.write().register(name);
        self.virtual_machine.recording.record(|| {
            if matches!(value, SteelVal::Void) {
                BootStep::Cleared(name.to_string(), idx)
            } else {
                BootStep::Registered(name.to_string(), idx)
            }
        });
        self.virtual_machine.insert_binding(idx, value);
        self
    }

    fn supply_context_arg(&mut self, ctx: &'static str, name: &'static str) -> &mut Self {
        if let Ok(existing) = self.extract_value(name) {
            if let SteelVal::BoxedFunction(f) = &existing {
                let func = generate_function(self, &SteelString::from(ctx), &existing, f);
                self.register_value(name, func);
            }
        }

        self
    }
}

#[steel_derive::function(name = "#%get-dylib", args(String))]
pub fn load_module_noop(target: &crate::rvals::SteelString) -> crate::rvals::Result<SteelVal> {
    stop!(Generic => "This engine has not been given the capability to load dylibs")
}

macro_rules! time {
    ($target:expr, $label:expr, $e:expr) => {{
        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let e = $e;

        #[cfg(feature = "profiling")]
        log::debug!(target: $target, "{}: {:?}", $label, now.elapsed());

        e
    }};
}

std::thread_local! {
    // Set while `Engine::new` makes the pass engine's image before any program
    static PREPARING_PASS_IMAGE: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
    // Set while this thread builds a boot image
    static BUILDING_BOOT_IMAGE: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
}

// The pass engine's image, made from the default engine's the first time a Steel
// pass runs
fn pass_boot_image() -> Result<Arc<BootImage>> {
    #[cfg(feature = "sync")]
    {
        static IMAGE: OnceCell<core::result::Result<Arc<BootImage>, String>> = OnceCell::new();
        IMAGE
            .get_or_init(|| Engine::cached_pass_image().map(Arc::new).map_err(|e| e.to_string()))
            .clone()
            .map_err(|e| SteelErr::new(crate::rerrs::ErrorKind::Generic, e))
    }
    #[cfg(not(feature = "sync"))]
    {
        std::thread_local! {
            static IMAGE: core::cell::RefCell<Option<core::result::Result<Arc<BootImage>, String>>> =
                const { core::cell::RefCell::new(None) };
        }
        if let Some(image) = IMAGE.with(|image| image.borrow().clone()) {
            return image.map_err(|e| SteelErr::new(crate::rerrs::ErrorKind::Generic, e));
        }
        let image = Engine::cached_pass_image().map(Arc::new).map_err(|e| e.to_string());
        IMAGE.with(|slot| *slot.borrow_mut() = Some(image.clone()));
        image.map_err(|e| SteelErr::new(crate::rerrs::ErrorKind::Generic, e))
    }
}

// Set by `Engine::enable_boot_image_cache`: the directory, or None for the default
// How many images this process read from the cache
#[cfg(test)]
pub(crate) static IMAGES_READ: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);

static BOOT_IMAGE_CACHE: std::sync::OnceLock<Option<PathBuf>> = std::sync::OnceLock::new();

pub(crate) fn boot_image_cache_requested() -> Option<&'static Option<PathBuf>> {
    BOOT_IMAGE_CACHE.get()
}

// One boot image for each (engine sandbox, kernel sandbox), made the first time
// an engine needs it
#[cfg(feature = "sync")]
fn boot_image(sandbox: bool, kernel_sandbox: bool) -> Arc<BootImage> {
    static IMAGES: [OnceCell<Arc<BootImage>>; 4] = [
        OnceCell::new(),
        OnceCell::new(),
        OnceCell::new(),
        OnceCell::new(),
    ];

    IMAGES[(sandbox as usize) * 2 + kernel_sandbox as usize]
        .get_or_init(|| Arc::new(Engine::cached_boot_image(sandbox, kernel_sandbox)))
        .clone()
}

#[cfg(not(feature = "sync"))]
fn boot_image(sandbox: bool, kernel_sandbox: bool) -> Arc<BootImage> {
    std::thread_local! {
        static IMAGES: core::cell::RefCell<[Option<Arc<BootImage>>; 4]> =
            const { core::cell::RefCell::new([None, None, None, None]) };
    }

    let index = (sandbox as usize) * 2 + kernel_sandbox as usize;
    if let Some(image) = IMAGES.with(|images| images.borrow()[index].clone()) {
        return image;
    }
    let image = Arc::new(Engine::cached_boot_image(sandbox, kernel_sandbox));
    IMAGES.with(|images| images.borrow_mut()[index] = Some(image.clone()));
    image
}

static STATIC_DEFAULT_PRELUDE_MACROS: OnceCell<Arc<FxHashMap<InternedString, SteelMacro>>> =
    OnceCell::new();

static STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX: OnceCell<Arc<FxHashMap<InternedString, SteelMacro>>> =
    OnceCell::new();

pub(crate) fn set_default_prelude_macros(
    prelude_macros: FxHashMap<InternedString, SteelMacro>,
    sandbox: bool,
) {
    // Set once: a loaded boot image may have set them, and a kernel image built
    // after sets the same ones
    if cfg!(feature = "sync") {
        if sandbox {
            let _ = STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX.set(Arc::new(prelude_macros));
        } else {
            let _ = STATIC_DEFAULT_PRELUDE_MACROS.set(Arc::new(prelude_macros));
        }
    } else {
        DEFAULT_PRELUDE_MACROS.with(|x| {
            let mut guard = x.borrow_mut();
            *guard = Arc::new(prelude_macros);
        })
    }
}

// The prelude macros set for this sandbox setting, for a boot image
pub(crate) fn prelude_macros_for(sandbox: bool) -> Option<Arc<FxHashMap<InternedString, SteelMacro>>> {
    if cfg!(feature = "sync") {
        if sandbox {
            STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX.get().cloned()
        } else {
            STATIC_DEFAULT_PRELUDE_MACROS.get().cloned()
        }
    } else {
        Some(DEFAULT_PRELUDE_MACROS.with(|x| x.borrow().clone()))
    }
}

pub(crate) fn default_prelude_macros() -> Arc<FxHashMap<InternedString, SteelMacro>> {
    if cfg!(feature = "sync") {
        STATIC_DEFAULT_PRELUDE_MACROS.get().cloned().unwrap_or(
            STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX
                .get()
                .cloned()
                .unwrap_or_default(),
        )
    } else {
        DEFAULT_PRELUDE_MACROS.with(|x| x.borrow().clone())
    }
}

thread_local! {
    // TODO: Replace this with a once cell?
    pub(crate) static DEFAULT_PRELUDE_MACROS: RefCell<Arc<FxHashMap<InternedString, SteelMacro>>> = RefCell::new(Arc::new(HashMap::default()));
}

impl Engine {
    pub fn engine_id(&self) -> EngineId {
        self.virtual_machine.id
    }

    #[cfg(not(feature = "sync"))]
    pub(crate) fn deep_clone(&self) -> Self {
        let mut engine = self.clone();

        engine.modules = self.modules.deep_clone();

        let mut compiler_copy = engine.virtual_machine.compiler.read().clone();
        compiler_copy.builtin_modules = engine.modules.clone();
        // Its own macros: a shallow copy of the kernel shares its transformer tables
        compiler_copy.kernel = compiler_copy.kernel.as_ref().map(Kernel::deep_clone);
        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler_copy));

        let constant_map = engine
            .virtual_machine
            .compiler
            .read()
            .constant_map
            .deep_clone();
        engine.virtual_machine.compiler.write().constant_map = constant_map;

        let heap_copy = Arc::new(parking_lot::Mutex::new(
            engine.virtual_machine.heap.lock().clone(),
        ));

        engine.virtual_machine.heap = heap_copy;
        engine
    }

    #[cfg(feature = "sync")]
    pub(crate) fn deep_clone(&self) -> Self {
        let mut engine = self.clone();
        engine.virtual_machine.global_env = engine.virtual_machine.global_env.deep_clone();

        engine.modules = self.modules.deep_clone();

        let mut compiler_copy = engine.virtual_machine.compiler.read().clone();
        compiler_copy.builtin_modules = engine.modules.clone();
        // Its own macros: a shallow copy of the kernel shares its transformer tables
        compiler_copy.kernel = compiler_copy.kernel.as_ref().map(Kernel::deep_clone);

        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler_copy));

        let constant_map = engine
            .virtual_machine
            .compiler
            .read()
            .constant_map
            .deep_clone();
        engine.virtual_machine.compiler.write().constant_map = constant_map;

        let heap_copy = Arc::new(parking_lot::Mutex::new(
            engine.virtual_machine.heap.lock().clone(),
        ));

        engine.virtual_machine.heap = heap_copy;
        engine
    }

    /// Function to access a kernel level execution environment
    /// Has access to primitives and syntax rules, but will not defer to a child
    /// kernel in the compiler
    // An engine with only the builtin modules registered, as a kernel image starts
    fn builtins_only(sandbox: bool) -> Self {
        let sources = Sources::new();
        let modules = ModuleContainer::with_expected_capacity();

        let compiler = Arc::new(RwLock::new(Compiler::default_without_kernel(
            sources,
            modules.clone(),
        )));

        let mut vm = Engine {
            virtual_machine: SteelThread::new(compiler),
            modules,
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
            id: EngineId::new(),
            boot: None,
        };

        time!(
            "engine-creation",
            "Registering builtin modules",
            register_builtin_modules(&mut vm, sandbox)
        );

        vm
    }

    pub(crate) fn new_kernel(sandbox: bool) -> Self {
        log::debug!(target:"kernel", "Instantiating a new kernel");
        #[cfg(feature = "profiling")]
        let mut total_time = crate::time::Instant::now();
        let mut vm = Self::builtins_only(sandbox);

        // Engines start from here and run the programs below again (`BootImage`)
        let builtins = vm.deep_clone();
        let mut programs = Vec::new();
        let mut closures = Vec::new();

        time!(
            "engine-creation",
            "Loading the ALL_MODULES prelude code",
            programs.push(
                vm.compile_and_run_keeping(crate::steel_vm::primitives::ALL_MODULES, &mut closures)
                    .expect("loading ALL_MODULES failed")
            )
        );

        // log::debug!(target: "kernel", "Registered modules in the kernel!: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        let mut now = crate::time::Instant::now();

        let core_libraries = [crate::stdlib::PRELUDE];

        for core in core_libraries.into_iter() {
            match vm.compile_and_run_keeping(core, &mut closures) {
                Ok(program) => programs.push(program),
                Err(e) => {
                    vm.raise_error(e);
                    panic!("Loading the standard library failed");
                }
            }
        }

        // Initialize the global macro environment with the default one. This way
        // values won't leak when top level macros are defined - and modules can clone from
        // this to begin seeding their environment.

        for prelude_macro in vm.in_scope_macros_mut().values_mut() {
            prelude_macro.mark_definition_resolved("#%prelude");
            prelude_macro.mark_prelude();
        }

        set_default_prelude_macros(vm.in_scope_macros().clone(), sandbox);

        #[cfg(feature = "profiling")]
        log::debug!(target: "kernel", "Loaded prelude in the kernel!: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Total kernel loading time: {:?}", total_time.elapsed());

        vm.boot = Some(Arc::new(BootRecord {
            builtins,
            programs,
            closures,
        }));

        vm
    }

    /// Registers a source module resolvers. This differs from `register_module_resolver` which deals
    /// specifically with builtin modules, e.g. `BuiltInModule`. This will allow you to embed
    /// source modules that can be referenced via `(require ...)` at the scheme level.
    ///
    /// This is a more flexible version of the api provided by `register_steel_module`, which allows
    /// you to directly register a module with the name, and the associated text for that module.
    pub fn register_source_module_resolver<T: SourceModuleResolver + 'static>(
        &mut self,
        resolver: T,
    ) {
        self.virtual_machine
            .compiler
            .write()
            .register_source_module_resolver(resolver);
    }

    /// Register a module resolver. This is used for creating references to modules
    /// that don't exist within the compiler. Without this, you wouldn't be able to
    /// pre-compile or analyze code that is run within another host application, without
    /// exposing some kind of compiler from that hosts runtime. The requirement then
    /// is to expose some kind of module artifact that we can then consume.
    pub fn register_module_resolver<T: ModuleResolver + 'static>(&mut self, resolver: T) {
        for name in resolver.names() {
            // Bind the resolved module so `(require-builtin <name>)` can run in this
            // engine. If it can't be resolved, reserve the binding with void so the
            // module can still be referenced during compilation.
            let value = resolver
                .resolve(&name)
                .and_then(|module| module.into_steelval().ok())
                .unwrap_or(SteelVal::Void);

            self.register_value(&format!("%-builtin-module-{}", name), value);
        }

        self.modules.with_resolver(resolver);
    }

    pub fn builtin_modules(&self) -> &ModuleContainer {
        &self.modules
    }

    #[doc(hidden)]
    pub fn disallow_dylib_loading(&mut self) -> &mut Self {
        let mut module = self.modules.inner_mut();

        // TODO: This should actually just clone the whole module, and then add this definition
        // in. That way it has its own unique module loader.
        if let Some(builtin_module) = module.get_mut("steel/meta") {
            builtin_module.register_native_fn_definition(LOAD_MODULE_NOOP_DEFINITION);
        }

        drop(module);

        self
    }

    /// Function to access a kernel level execution environment
    /// Has access to primitives and syntax rules, but will not defer to a child
    /// kernel in the compiler
    pub(crate) fn new_bootstrap_kernel(sandbox: bool) -> Self {
        // If the interner has already been initialized, it most likely means that either:
        // 1) Tests are being run
        // 2) The parser was used in a standalone fashion, somewhere, which invalidates the bootstrap
        //    process
        //
        // There are a few solutions to this - one would probably be to not use a static interner,
        // however given that its a huge chore to pass around the interner everywhere there are strings,
        // its probably inevitable we have that.
        if get_interner().is_some() {
            return Engine::new_kernel(sandbox);
        }

        intern_modules();

        if matches!(option_env!("STEEL_BOOTSTRAP"), Some("false") | None) {
            let mut vm = Engine::new_kernel(sandbox);

            // let sources = vm.sources.clone();

            // vm.register_fn("report-error!", move |error: SteelErr| {
            //     raise_error(&sources, error);
            // });

            return vm;
        }

        log::debug!(target:"kernel", "Instantiating a new kernel");

        // let sources = Sources::new();
        // let modules = ModuleContainer::default();

        // let compiler = Arc::new(RwLock::new(Compiler::default_without_kernel(
        //     sources.clone(),
        //     modules.clone(),
        // )));

        // // TODO: Pass compiler down if we want eval!
        // let mut vm = Engine {
        //     virtual_machine: SteelThread::new(sources.clone(), compiler),
        //     modules,
        //     sources,
        //     #[cfg(feature = "dylibs")]
        //     dylibs: DylibContainers::new(),
        //     id: EngineId::new(),
        // };

        // if let Some(programs) = Engine::load_from_bootstrap(&mut vm) {
        //     register_builtin_modules(&mut vm, sandbox);

        //     for program in programs {
        //         vm.compiler.write().constant_map = program.constant_map.clone();
        //         vm.virtual_machine.constant_map = program.constant_map.clone();

        //         vm.run_raw_program(program).unwrap();
        //     }

        //     log::debug!(target: "kernel", "Loaded prelude in the kernel!");

        //     let sources = vm.sources.clone();

        //     vm.register_fn("report-error!", move |error: SteelErr| {
        //         raise_error(&sources, error);
        //     });

        //     vm
        // } else {
        let mut vm = Engine::new_kernel(sandbox);

        // let sources = vm.sources.clone();

        // vm.register_fn("report-error!", move |error: SteelErr| {
        //     raise_error(&sources, error);
        // });

        vm
        // }
    }

    // fn load_from_bootstrap(vm: &mut Engine) -> Option<Vec<RawProgramWithSymbols>> {
    //     if matches!(option_env!("STEEL_BOOTSTRAP"), Some("false") | None) {
    //         return None;
    //     } else {
    //         println!("LOADING A KERNEL FROM THE BIN FILE");
    //     }

    //     let bootstrap: StartupBootstrapImage =
    //         bincode::deserialize(KERNEL_BIN_FILE.with(|x| x.get())?).unwrap();

    //     // Set the syntax object id to be AFTER the previous items have been parsed
    //     SYNTAX_OBJECT_ID.store(
    //         bootstrap.syntax_object_id,
    //         std::sync::atomic::Ordering::Relaxed,
    //     );

    //     crate::compiler::code_gen::FUNCTION_ID
    //         .store(bootstrap.function_id, std::sync::atomic::Ordering::Relaxed);

    //     vm.sources = bootstrap.sources;
    //     // vm.compiler.macro_env = bootstrap.macros;

    //     todo!();

    //     Some(
    //         bootstrap
    //             .pre_kernel_programs
    //             .into_iter()
    //             .map(SerializableRawProgramWithSymbols::into_raw_program)
    //             .collect(),
    //     )
    // }

    /// Creates a statically linked program ready to deserialize
    pub fn create_non_interactive_program_image<E: AsRef<str> + Into<Cow<'static, str>>>(
        expr: E,
        path: PathBuf,
    ) -> Result<NonInteractiveProgramImage> {
        let mut engine = Engine::new();

        engine
            .emit_raw_program(expr, path)?
            .into_serializable_program()
            .map(|program| NonInteractiveProgramImage {
                sources: engine.virtual_machine.compiler.read().sources.clone(),
                program,
            })
    }

    // Execute from a statically linked non interactive program
    pub fn execute_non_interactive_program_image(program: &'static [u8]) -> Result<()> {
        // This _has_ to match the as the creation of the program above.
        // So, engine first, then non interactive program.
        let mut engine = Engine::new();
        let program = crate::steel_vm::engine::NonInteractiveProgramImage::from_bytes(program);

        engine.virtual_machine.compiler.write().sources = program.sources;

        // TODO: The constant map needs to be brought back as well. Install it here.
        // it needs to get installed in the VM and the compiler. Lets just try that now.

        let raw_program = SerializableRawProgramWithSymbols::into_raw_program(program.program);

        engine.virtual_machine.constant_map = raw_program.constant_map.clone();
        engine.virtual_machine.compiler.write().constant_map = raw_program.constant_map.clone();

        let results = engine.run_raw_program(raw_program);

        if let Err(e) = results {
            raise_error(&engine.virtual_machine.compiler.read().sources, e);
        }

        Ok(())
    }

    // Create kernel bootstrap
    // pub fn create_kernel_bootstrap_from_programs(output_path: PathBuf) {
    //     let sources = Sources::new();

    //     let mut vm = Engine {
    //         virtual_machine: SteelThread::new(sources.clone()),
    //         compiler: Arc::new(RwLock::new(Compiler::default())),
    //         constants: None,
    //         modules: ModuleContainer::default(),
    //         sources,
    //         #[cfg(feature = "dylibs")]
    //         dylibs: DylibContainers::new(),
    //         id: EngineId::new(),
    //     };

    //     register_builtin_modules(&mut vm, false);

    //     let mut programs = Vec::new();

    //     let bootstrap_sources = [
    //         crate::steel_vm::primitives::ALL_MODULES,
    //         crate::stdlib::PRELUDE,
    //     ];

    //     for source in bootstrap_sources {
    //         let raw_program = vm.emit_raw_program_no_path(source).unwrap();
    //         programs.push(raw_program.clone());
    //         vm.run_raw_program(raw_program).unwrap();
    //     }

    //     // Grab the last value of the offset
    //     let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);
    //     let function_id =
    //         crate::compiler::code_gen::FUNCTION_ID.load(std::sync::atomic::Ordering::Relaxed);

    //     let bootstrap = StartupBootstrapImage {
    //         syntax_object_id,
    //         function_id,
    //         sources: vm.sources,
    //         pre_kernel_programs: programs
    //             .into_iter()
    //             .map(RawProgramWithSymbols::into_serializable_program)
    //             .collect::<Result<_>>()
    //             .unwrap(),
    //         // macros: vm.compiler.macro_env,
    //         post_kernel_programs: Vec::new(),
    //         kernel: None,
    //         compiler: None,
    //     };

    //     // Encode to something implementing `Write`
    //     let mut f = std::fs::File::create(output_path).unwrap();
    //     bincode::serialize_into(&mut f, &bootstrap).unwrap();
    // }

    // pub fn create_new_engine_from_bootstrap(output_path: PathBuf) {
    //     let sources = Sources::new();
    //     let mut vm = Engine {
    //         virtual_machine: SteelThread::new(sources.clone()),
    //         compiler: Arc::new(RwLock::new(Compiler::default())),
    //         constants: None,
    //         modules: ModuleContainer::default(),
    //         sources,
    //         #[cfg(feature = "dylibs")]
    //         dylibs: DylibContainers::new(),
    //         id: EngineId::new(),
    //     };

    //     register_builtin_modules(&mut vm, false);

    //     let mut pre_kernel_programs = Vec::new();

    //     let bootstrap_sources = [
    //         crate::steel_vm::primitives::ALL_MODULES,
    //         crate::stdlib::PRELUDE,
    //     ];

    //     for source in bootstrap_sources {
    //         let raw_program = vm.emit_raw_program_no_path(source).unwrap();
    //         pre_kernel_programs.push(raw_program.clone());
    //         vm.run_raw_program(raw_program).unwrap();
    //     }

    //     // This will be our new top level engine
    //     let mut top_level_engine = vm.clone();

    //     let sources = vm.sources.clone();

    //     vm.register_fn("report-error!", move |error: SteelErr| {
    //         raise_error(&sources, error);
    //     });

    //     let (kernel, kernel_program) = Kernel::bootstrap(vm);

    //     // Create kernel for the compiler for the top level vm
    //     top_level_engine.compiler.write().kernel = Some(kernel);

    //     let builtin_modules =
    //         ["(require \"#%private/steel/contract\" (for-syntax \"#%private/steel/contract\"))"];

    //     let mut post_kernel_programs = Vec::new();

    //     for source in builtin_modules {
    //         let raw_program = top_level_engine.emit_raw_program_no_path(source).unwrap();
    //         post_kernel_programs.push(raw_program.clone());
    //         top_level_engine.run_raw_program(raw_program).unwrap();
    //     }

    //     // Grab the last value of the offset
    //     let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);
    //     let function_id =
    //         crate::compiler::code_gen::FUNCTION_ID.load(std::sync::atomic::Ordering::Relaxed);

    //     let kernel_sources = top_level_engine
    //         .compiler
    //         .write()
    //         .kernel
    //         .as_ref()
    //         .unwrap()
    //         .engine
    //         .sources
    //         .clone();
    //     let bootstrap = StartupBootstrapImage {
    //         syntax_object_id,
    //         function_id,
    //         sources: top_level_engine.sources,
    //         pre_kernel_programs: pre_kernel_programs
    //             .into_iter()
    //             .map(RawProgramWithSymbols::into_serializable_program)
    //             .collect::<Result<_>>()
    //             .unwrap(),
    //         post_kernel_programs: post_kernel_programs
    //             .into_iter()
    //             .map(RawProgramWithSymbols::into_serializable_program)
    //             .collect::<Result<_>>()
    //             .unwrap(),
    //         kernel: Some(KernelImage {
    //             compiler: top_level_engine
    //                 .compiler
    //                 .write()
    //                 .kernel
    //                 .take()
    //                 .unwrap()
    //                 .engine
    //                 .compiler
    //                 .write()
    //                 .into_serializable_compiler()
    //                 .unwrap(),
    //             sources: kernel_sources,
    //             kernel_source: kernel_program.into_serializable_program().unwrap(),
    //         }),
    //         compiler: Some(
    //             top_level_engine
    //                 .compiler
    //                 .write()
    //                 .into_serializable_compiler()
    //                 .unwrap(),
    //         ),
    //     };

    //     // Encode to something implementing `Write`
    //     let mut f = std::fs::File::create(output_path).unwrap();
    //     bincode::serialize_into(&mut f, &bootstrap).unwrap();
    // }

    // pub fn top_level_load_from_bootstrap(bin: &[u8]) -> Engine {
    //     let bootstrap: StartupBootstrapImage = bincode::deserialize(bin).unwrap();

    //     let sources = Sources::new();
    //     // This is going to be the kernel
    //     let mut vm = Engine {
    //         virtual_machine: SteelThread::new(sources.clone()),
    //         compiler: Arc::new(RwLock::new(Compiler::default())),
    //         constants: None,
    //         modules: ModuleContainer::default(),
    //         sources,
    //         #[cfg(feature = "dylibs")]
    //         dylibs: DylibContainers::new(),
    //         id: EngineId::new(),
    //     };

    //     // Register the modules
    //     register_builtin_modules(&mut vm, false);

    //     // Set the syntax object id to be AFTER the previous items have been parsed
    //     SYNTAX_OBJECT_ID.store(
    //         bootstrap.syntax_object_id,
    //         std::sync::atomic::Ordering::Relaxed,
    //     );

    //     crate::compiler::code_gen::FUNCTION_ID
    //         .store(bootstrap.function_id, std::sync::atomic::Ordering::Relaxed);

    //     let bootstrap_kernel = bootstrap.kernel.unwrap();

    //     vm.sources = bootstrap_kernel.sources;
    //     vm.compiler = Arc::new(RwLock::new(bootstrap_kernel.compiler.into_compiler()));

    //     // TODO: Only need to bring around the last constant map
    //     for program in bootstrap
    //         .pre_kernel_programs
    //         .into_iter()
    //         .map(SerializableRawProgramWithSymbols::into_raw_program)
    //     {
    //         vm.compiler.write().constant_map = program.constant_map.clone();
    //         vm.virtual_machine.constant_map = program.constant_map.clone();

    //         vm.run_raw_program(program).unwrap();
    //     }

    //     log::debug!(target: "kernel", "Loaded prelude in the kernel!");

    //     let sources = vm.sources.clone();

    //     vm.register_fn("report-error!", move |error: SteelErr| {
    //         raise_error(&sources, error);
    //     });

    //     // Now we're going to set up the top level environment
    //     let mut kernel = Kernel::initialize_post_bootstrap(vm.clone());

    //     kernel
    //         .engine
    //         .run_raw_program(bootstrap_kernel.kernel_source.into_raw_program())
    //         .unwrap();

    //     vm.sources = bootstrap.sources;
    //     vm.compiler = Arc::new(RwLock::new(bootstrap.compiler.unwrap().into_compiler()));
    //     vm.compiler.write().kernel = Some(kernel);

    //     for program in bootstrap
    //         .post_kernel_programs
    //         .into_iter()
    //         .map(SerializableRawProgramWithSymbols::into_raw_program)
    //     {
    //         vm.compiler.write().constant_map = program.constant_map.clone();
    //         vm.virtual_machine.constant_map = program.constant_map.clone();

    //         vm.run_raw_program(program).unwrap();
    //     }

    //     vm
    // }

    // fn create_bootstrap() {
    //     let sources = Sources::new();

    //     let mut vm = Engine {
    //         virtual_machine: SteelThread::new(sources.clone()),
    //         compiler: Arc::new(RwLock::new(Compiler::default())),
    //         constants: None,
    //         modules: ModuleContainer::default(),
    //         sources,
    //         #[cfg(feature = "dylibs")]
    //         dylibs: DylibContainers::new(),
    //         id: EngineId::new(),
    //     };

    //     register_builtin_modules(&mut vm, false);

    //     let mut asts = Vec::new();

    //     let bootstrap_sources = [
    //         crate::steel_vm::primitives::ALL_MODULES,
    //         crate::stdlib::PRELUDE,
    //     ];

    //     for source in bootstrap_sources {
    //         let id = vm.sources.add_source(source.to_string(), None);

    //         // Could fail here
    //         let parsed: Vec<ExprKind> = Parser::new(source, Some(id))
    //             .collect::<core::result::Result<_, _>>()
    //             .unwrap();

    //         asts.push(parsed.clone());

    //         vm.run_raw_program_from_exprs(parsed).unwrap();
    //     }

    //     // Grab the last value of the offset
    //     let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);

    //     let bootstrap = BootstrapImage {
    //         interner: take_interner(),
    //         syntax_object_id,
    //         sources: vm.sources,
    //         programs: asts,
    //     };

    //     // Encode to something implementing `Write`
    //     let mut f = std::fs::File::create("src/boot/bootstrap.bin").unwrap();
    //     bincode::serialize_into(&mut f, &bootstrap).unwrap();
    // }

    /// Instantiates a raw engine instance. Includes no primitives or prelude.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_raw();
    /// assert!(vm.run("(+ 1 2 3").is_err()); // + is a free identifier
    /// ```
    pub fn new_raw() -> Self {
        #[cfg(all(feature = "sync", feature = "biased", not(feature = "triomphe")))]
        {
            steel_rc::register_thread();
        }

        let sources = Sources::new();
        let modules = ModuleContainer::default();

        let compiler = Arc::new(RwLock::new(Compiler::default_with_kernel(
            sources,
            modules.clone(),
        )));

        Engine {
            virtual_machine: SteelThread::new(compiler),
            modules,
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
            id: EngineId::new(),
            boot: None,
        }
    }

    pub fn set_prelude_string(&mut self, prelude_string: Cow<'static, str>) {
        self.virtual_machine
            .compiler
            .write()
            .module_manager
            .set_prelude_string(prelude_string);
    }

    pub fn with_default_prelude_string(&mut self) {
        self.virtual_machine
            .compiler
            .write()
            .module_manager
            .use_default_prelude_string();
    }

    /// Instantiates a raw engine instance. Includes no primitives or prelude, and does not
    /// allow for kernel level macro expansion.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_raw_no_kernel();
    /// assert!(vm.run("(+ 1 2 3").is_err()); // + is a free identifier
    /// ```
    pub fn new_raw_no_kernel() -> Self {
        #[cfg(all(feature = "sync", feature = "biased", not(feature = "triomphe")))]
        {
            steel_rc::register_thread();
        }

        let sources = Sources::new();
        let modules = ModuleContainer::default();

        let compiler = Arc::new(RwLock::new(Compiler::default_without_kernel(
            sources,
            modules.clone(),
        )));

        let mut engine = Engine {
            virtual_machine: SteelThread::new(compiler),
            modules,
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
            id: EngineId::new(),
            boot: None,
        };

        bootstrap_globals(&mut engine);

        // Set the prelude string to be completely empty,
        // so we'll have to get that working yet.
        engine.set_prelude_string(Cow::Borrowed(""));

        engine
    }

    pub fn report_engine_stats(&self) -> EngineStatistics {
        EngineStatistics {
            rooted_count: self.globals().len(),
            constants_count: self.virtual_machine.compiler.read().constant_map.len(),
            sources_size: self.virtual_machine.compiler.read().sources.size_in_bytes(),
        }
    }

    /// Registers a steel module. This is a more streamlined version of the
    /// api provided by `register_source_module_resolver`.
    pub fn register_steel_module(&mut self, module_name: String, text: String) {
        self.virtual_machine
            .compiler
            .write()
            .register_builtin(module_name, text);
    }

    /// Instantiates a new engine instance with all primitive functions enabled.
    /// This excludes the prelude and contract files.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_base();
    /// // map is found in the prelude, so this will fail
    /// assert!(vm.run(r#"(map (lambda (x) 10) (list 1 2 3 4 5))"#).is_err());
    /// ```
    #[inline]
    pub fn new_base() -> Self {
        let mut vm = Engine::new_raw();
        // Embed any primitives that we want to use
        register_builtin_modules(&mut vm, false);

        vm.compile_and_run_raw_program(crate::steel_vm::primitives::ALL_MODULES)
            .unwrap();

        vm
    }

    // pub fn cache_modules(&mut self) {
    //     self.virtual_machine
    //         .compiler
    //         .write()
    //         .module_manager
    //         .compiled_modules
    //         .cache_modules();
    // }

    /// Turn contracts on in the VM
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.virtual_machine.with_contracts(contracts);
        self
    }

    #[inline]
    pub fn new_sandboxed() -> Self {
        let mut engine = Self::booted(true, Kernel::default_sandbox());

        // Block dylib loading for sandboxed instances
        engine.disallow_dylib_loading();

        engine
    }

    /// Call the print method within the VM
    pub fn call_printing_method_in_context(&mut self, argument: SteelVal) -> Result<SteelVal> {
        let function = self.extract_value("displayln")?;
        self.call_function_with_args(function, vec![argument])
    }

    /// Internal API for calling a function directly
    pub fn call_function_with_args(
        &mut self,
        function: SteelVal,
        arguments: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        let constant_map = self.virtual_machine.compiler.read().constant_map.clone();

        self.virtual_machine
            .call_function(constant_map, function, arguments)
    }

    pub fn call_function_with_args_from_mut_slice(
        &mut self,
        function: SteelVal,
        arguments: &mut [SteelVal],
    ) -> Result<SteelVal> {
        let constant_map = self.virtual_machine.compiler.read().constant_map.clone();

        self.virtual_machine
            .call_function_from_mut_slice(constant_map, function, arguments)
    }

    /// Call a function by name directly within the target environment
    pub fn call_function_by_name_with_args(
        &mut self,
        function: &str,
        arguments: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        let constant_map = self.virtual_machine.compiler.read().constant_map.clone();
        self.extract_value(function).and_then(|function| {
            self.virtual_machine
                .call_function(constant_map, function, arguments)
        })
    }

    pub fn call_function_by_name_with_args_from_mut_slice(
        &mut self,
        function: &str,
        arguments: &mut [SteelVal],
    ) -> Result<SteelVal> {
        let constant_map = self.virtual_machine.compiler.read().constant_map.clone();

        self.extract_value(function).and_then(|function| {
            self.virtual_machine
                .call_function_from_mut_slice(constant_map, function, arguments)
        })
    }

    /// Nothing fancy, just run it
    pub fn run<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        input: E,
    ) -> Result<Vec<SteelVal>> {
        self.compile_and_run_raw_program(input)
    }

    pub fn with_immutable_reference<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a T,
    ) -> LifetimeGuard<'a> {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            core::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_ro_object::<T, EXT>(
            obj,
        );

        LifetimeGuard {
            engine: self,
            count: 1,
        }
    }

    pub fn with_mut_reference<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
    ) -> LifetimeGuard<'a> {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            core::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_rw_object::<T, EXT>(
            obj,
        );

        LifetimeGuard {
            engine: self,
            count: 1,
        }
    }

    // Tie the lifetime of this object to the scope of this execution
    pub fn run_with_reference<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
        bind_to: &'a str,
        script: &'a str,
    ) -> Result<SteelVal> {
        self.with_mut_reference(obj).consume(move |engine, args| {
            let mut args = args.into_iter();

            engine.update_value(bind_to, args.next().unwrap());

            let res = engine.compile_and_run_raw_program(Cow::Owned(script.to_owned()));

            engine.update_value(bind_to, SteelVal::Void);

            res.map(|x| x.into_iter().next().unwrap_or(SteelVal::Void))
        })
    }

    pub fn run_with_reference_from_path<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
        bind_to: &'a str,
        script: &'a str,
        path: PathBuf,
    ) -> Result<SteelVal> {
        self.with_mut_reference(obj).consume(move |engine, args| {
            let mut args = args.into_iter();

            engine.update_value(bind_to, args.next().unwrap());

            let res = engine
                .compile_and_run_raw_program_with_path(Cow::Owned(script.to_owned()), path.clone());

            engine.update_value(bind_to, SteelVal::Void);

            res.map(|x| x.into_iter().next().unwrap_or(SteelVal::Void))
        })
    }

    pub fn run_thunk_with_reference<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
        mut thunk: impl FnMut(&mut Engine, SteelVal) -> Result<SteelVal>,
    ) -> Result<SteelVal> {
        self.with_mut_reference(obj).consume(|engine, args| {
            let mut args = args.into_iter();

            thunk(engine, args.next().unwrap_or(SteelVal::Void))
        })
    }

    pub fn run_thunk_with_ro_reference<
        'a,
        'b: 'a,
        T: CustomReference + ReferenceMarker<'b, Static = EXT> + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a T,
        mut thunk: impl FnMut(&mut Engine, SteelVal) -> Result<SteelVal>,
    ) -> Result<SteelVal> {
        self.with_immutable_reference(obj).consume(|engine, args| {
            let mut args = args.into_iter();

            thunk(engine, args.next().unwrap_or(SteelVal::Void))
        })
    }

    /// Instantiates a new engine instance with all the primitive functions enabled.
    /// This is the most general engine entry point, and includes both the contract and
    /// prelude files in the root.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new();
    /// vm.run(r#"(+ 1 2 3)"#).unwrap();
    /// ```
    pub fn new() -> Self {
        Self::booted(false, Kernel::default_sandbox())
    }

    /// Like [`Engine::new`], but macros can do what programs can while they expand. Their
    /// transformers, and the modules they require with `(require (for-syntax ...))`, can use
    /// the file system, processes and the network. Otherwise those raise an error when
    /// macros call them, and macros can still read environment variables.
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_with_unsandboxed_macros();
    /// vm.run(r#"(+ 1 2 3)"#).unwrap();
    /// ```
    pub fn new_with_unsandboxed_macros() -> Self {
        Self::booted(false, false)
    }

    /// Lets the modules that macros require with `(require (for-syntax ...))` use the
    /// builtin modules registered with this engine, such as those of a host application.
    /// Sandboxed macros still can't use the network.
    pub fn share_modules_with_macros(&mut self) -> &mut Self {
        if let Some(kernel) = self.virtual_machine.compiler.write().kernel.as_mut() {
            kernel.share_host_modules = true;
        }

        self
    }

    /// Keeps boot images in `dir` (by default `$XDG_CACHE_HOME/steel`, or
    /// `~/.cache/steel`), so engines in later runs of this program start from the
    /// image instead of compiling the prelude: about 12 ms instead of about 150 for
    /// the first engine of a process. Off unless called, since it writes about 9 MB
    /// per image to that directory, a new image whenever the program is rebuilt
    /// (the eight newest are kept).
    ///
    /// Call it before making the first engine; images made before are not kept.
    /// The first call decides. `STEEL_BOOT_IMAGE_CACHE=0` turns it off and `=1` on
    /// whatever the program asks, and `STEEL_BOOT_IMAGE_DIR` picks the directory.
    ///
    /// ```no_run
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// Engine::enable_boot_image_cache(None);
    /// let mut vm = Engine::new();
    /// ```
    pub fn enable_boot_image_cache(dir: Option<PathBuf>) {
        let _ = BOOT_IMAGE_CACHE.set(dir);
    }

    // An engine with the prelude loaded: `sandbox` for the engine's builtins,
    // `kernel_sandbox` for its macros'
    fn booted(sandbox: bool, kernel_sandbox: bool) -> Self {
        #[cfg(all(feature = "sync", feature = "biased", not(feature = "triomphe")))]
        {
            steel_rc::register_thread();
        }

        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        // An engine made while a boot image is built on this thread (the Steel
        // passes' engine, with `STEEL_PASSES_ON_PRELUDE`) can't wait for it
        let building = BUILDING_BOOT_IMAGE.with(|x| x.get());
        let engine = if crate::config::config().boot_image && !building {
            let image = boot_image(sandbox, kernel_sandbox);
            Self::prepare_cached_pass_image(sandbox, kernel_sandbox);
            Self::from_boot_image(&image)
        } else {
            Self::booting(sandbox, kernel_sandbox).0
        };

        #[cfg(feature = "profiling")]
        log::info!(target: "engine-creation", "Engine Creation: {:?}", now.elapsed());

        engine
    }

    // An engine that compiles and runs the prelude itself, the program that is,
    // and untouched copies of the functions its compile put in the constant map
    fn booting(
        sandbox: bool,
        kernel_sandbox: bool,
    ) -> (Self, Executable, Vec<(usize, Gc<ByteCodeLambda>)>) {
        let mut engine = fresh_kernel_image(sandbox);
        engine.virtual_machine.compiler.write().kernel = Some(Kernel::with_sandbox(kernel_sandbox));

        let mut closures = Vec::new();
        let prelude = crate::compiler::passes::steel_passes::with_rust_passes(|| {
            engine.compile_and_run_keeping(PRELUDE_WITHOUT_BASE, &mut closures)
        });

        match prelude {
            Ok(program) => (engine, program, closures),
            Err(e) => {
                raise_error(&engine.virtual_machine.compiler.read().sources, e);
                panic!("This shouldn't happen!");
            }
        }
    }

    // A boot image from the cache, or built and kept there
    fn cached_boot_image(sandbox: bool, kernel_sandbox: bool) -> BootImage {
        let Some(cache) = boot_image::BootImageCache::new(sandbox, kernel_sandbox) else {
            return Self::checked_boot_image(sandbox, kernel_sandbox);
        };
        if let Some(image) = cache.load() {
            #[cfg(test)]
            IMAGES_READ.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
            return image;
        }
        let mut image = Self::build_boot_image(sandbox, kernel_sandbox);
        image.stored_as = cache.store(&image, sandbox, kernel_sandbox);
        image
    }

    // With the cache on, the pass engine's image is made (or read) right after
    // the default one, before any program: scopes made after the default image
    // are numbered in the order they're made, and a saved image's have to be
    // free in the process that reads it
    fn prepare_cached_pass_image(sandbox: bool, kernel_sandbox: bool) {
        std::thread_local! {
            static CHECKED: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
        }
        if (sandbox, kernel_sandbox) != (false, Kernel::default_sandbox())
            || CHECKED.with(|checked| checked.replace(true))
        {
            return;
        }
        if crate::compiler::passes::steel_passes::SteelPasses::builtin_engine_wanted()
            && boot_image::BootImageCache::new(sandbox, kernel_sandbox).is_some()
        {
            let _ = PREPARING_PASS_IMAGE.with(|preparing| {
                preparing.set(true);
                let image = pass_boot_image();
                preparing.set(false);
                image
            });
        }
    }

    // The pass engine's image from the cache, or made and kept there. Kept only
    // when made before any program (`prepare_cached_pass_image`): the scopes
    // saved with it are numbered in the order they were made
    fn cached_pass_image() -> Result<BootImage> {
        let (sandbox, kernel_sandbox) = (false, Kernel::default_sandbox());
        let base = boot_image(sandbox, kernel_sandbox);
        let cache = boot_image::BootImageCache::new(sandbox, kernel_sandbox)
            .map(|cache| cache.derived("passes"));
        if let Some(image) = cache.as_ref().and_then(|cache| cache.load_derived(&base)) {
            #[cfg(test)]
            IMAGES_READ.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
            // What compiling the passes leaves in this thread's expander state,
            // as making the image would have
            for expander in &image.expander {
                expander.install();
            }
            return Ok(image);
        }
        let image = Self::derive_pass_image(&base)?;
        if let Some(cache) = &cache {
            if PREPARING_PASS_IMAGE.with(|preparing| preparing.get()) {
                cache.store_derived(&image, &base);
            }
        } else if crate::config::config().boot_image_roundtrip {
            let bytes = boot_image::SavedDerivedImage::save(&image, &base)
                .and_then(|saved| saved.to_bytes())
                .expect("saving the pass engine's image");
            return boot_image::SavedDerivedImage::from_bytes(&bytes)
                .and_then(|saved| saved.load(&base));
        }
        Ok(image)
    }

    // A boot image, saved and read back first with `STEEL_BOOT_IMAGE_ROUNDTRIP`
    fn checked_boot_image(sandbox: bool, kernel_sandbox: bool) -> BootImage {
        let image = Self::build_boot_image(sandbox, kernel_sandbox);
        if !crate::config::config().boot_image_roundtrip {
            return image;
        }

        let start = std::time::Instant::now();
        let bytes = boot_image::SavedBootImage::save(&image, sandbox, kernel_sandbox)
            .and_then(|saved| saved.to_bytes())
            .expect("saving the boot image");
        let saved_at = start.elapsed();
        let loaded = boot_image::SavedBootImage::from_bytes(&bytes)
            .and_then(|saved| saved.load())
            .expect("reading the boot image back");
        eprintln!(
            "boot image round trip: {} bytes, saved in {:.1} ms, read and loaded in {:.1} ms",
            bytes.len(),
            saved_at.as_secs_f64() * 1000.0,
            (start.elapsed() - saved_at).as_secs_f64() * 1000.0
        );
        loaded
    }

    fn build_boot_image(sandbox: bool, kernel_sandbox: bool) -> BootImage {
        use crate::parser::expand_visitor::ThreadExpanderState;

        struct Building(bool);
        impl Drop for Building {
            fn drop(&mut self) {
                let was = self.0;
                BUILDING_BOOT_IMAGE.with(|x| x.set(was));
            }
        }
        let _building = Building(BUILDING_BOOT_IMAGE.with(|x| x.replace(true)));
        let _no_module_cache = crate::compiler::module_cache::pause();

        let foreign_before = ThreadExpanderState::foreign_expansions();
        let (booted, prelude, prelude_closures) = crate::parser::kernel::recording_kernels(|| {
            Self::booting(sandbox, kernel_sandbox)
        });
        let kernel_steps = booted
            .virtual_machine
            .compiler
            .write()
            .kernel
            .as_mut()
            .map(|kernel| kernel.engine.virtual_machine.recording.finish())
            .unwrap_or_default();
        let expander = ThreadExpanderState::capture(
            booted
                .virtual_machine
                .compiler
                .read()
                .module_manager
                .modules()
                .keys()
                .filter_map(|path| path.to_str())
                .map(InternedString::from)
                .collect::<Vec<_>>(),
            &foreign_before,
        );
        let record = booted
            .boot
            .clone()
            .expect("a kernel image keeps what it ran to boot");

        let mut programs = record.programs.clone();
        programs.push(prelude);
        let mut closures = record.closures.clone();
        closures.extend(prelude_closures);

        BootImage {
            builtins: record.builtins.deep_clone(),
            programs,
            closures,
            booted,
            expander: vec![expander],
            kernel_steps,
            steps: Vec::new(),
            host_modules: Vec::new(),
            derived_kernel_steps: Vec::new(),
            delta: None,
            stored_as: None,
        }
    }

    /// An engine for the Steel compiler passes, and the registry's `passes-at`,
    /// from an image made once: the default engine's, then the pass support
    /// module and the passes. None when there is no boot image to start from.
    pub(crate) fn pass_engine() -> Option<Result<(Engine, SteelVal)>> {
        if !crate::config::config().boot_image || BUILDING_BOOT_IMAGE.with(|x| x.get()) {
            return None;
        }
        let image = match pass_boot_image() {
            Ok(image) => image,
            Err(e) => return Some(Err(e)),
        };
        let (engine, results) = Self::replay_boot_image(&image);
        Some(
            results
                .last()
                .cloned()
                .map(|passes_at| (engine, passes_at))
                .ok_or_else(|| SteelErr::new(crate::rerrs::ErrorKind::Generic, "the pass registry didn't load".into())),
        )
    }

    // The default engine's image, then the steps that make it the pass engine
    fn derive_pass_image(base: &BootImage) -> Result<BootImage> {
        use crate::parser::expand_visitor::ThreadExpanderState;

        // Compiling the passes doesn't run them
        let _in_pass_engine = crate::compiler::passes::steel_passes::InPassEngine::enter();
        let _no_module_cache = crate::compiler::module_cache::pause();
        let foreign_before = ThreadExpanderState::foreign_expansions();
        let (mut engine, _) = Self::replay_boot_image(base);
        // Compiling the passes runs phase 1 code in the kernel's engine
        if let Some(kernel) = engine.virtual_machine.compiler.write().kernel.as_mut() {
            kernel.engine.virtual_machine.recording.start();
        }

        let module = HostModule::PassSupport.module();
        {
            use crate::gc::ShareableMut;
            if !module.module.read().context_functions.is_empty() {
                stop!(Generic => "the pass support module can't be replayed");
            }
        }
        let name = module.unreadable_name();
        engine.register_module(module);
        let slot = engine
            .virtual_machine
            .compiler
            .read()
            .symbol_map
            .get(&InternedString::from(name.as_str()))?;

        let mut closures = base.closures.clone();
        let program = engine.compile_and_run_keeping(PASS_ENGINE_PROGRAM, &mut closures)?;

        let derived_kernel_steps = engine
            .virtual_machine
            .compiler
            .write()
            .kernel
            .as_mut()
            .map(|kernel| kernel.engine.virtual_machine.recording.finish())
            .unwrap_or_default();

        let expander = ThreadExpanderState::capture(
            engine
                .virtual_machine
                .compiler
                .read()
                .module_manager
                .modules()
                .keys()
                .filter_map(|path| path.to_str())
                .map(InternedString::from)
                .collect::<Vec<_>>(),
            &foreign_before,
        );
        let mut expanders = base.expander.clone();
        expanders.push(expander);

        let mut steps = base.steps.clone();
        steps.push(BootStep::Registered(name, slot));
        steps.push(BootStep::Run(program));
        let mut host_modules = base.host_modules.clone();
        host_modules.push(HostModule::PassSupport);

        Ok(BootImage {
            builtins: base.builtins.deep_clone(),
            programs: base.programs.clone(),
            closures,
            booted: engine,
            expander: expanders,
            kernel_steps: base.kernel_steps.clone(),
            steps,
            host_modules,
            derived_kernel_steps,
            delta: None,
            stored_as: None,
        })
    }

    // An engine from the boot image: its own copy of the booted compiler and
    // kernel, and its own values from running the prelude's programs again
    fn from_boot_image(image: &BootImage) -> Self {
        Self::replay_boot_image(image).0
    }

    // `from_boot_image`, with what the last program returned
    fn replay_boot_image(image: &BootImage) -> (Self, Vec<SteelVal>) {
        let _no_module_cache = crate::compiler::module_cache::pause();
        // The thread that made the image has compiled the prelude; this one may not have
        for expander in &image.expander {
            expander.install();
        }

        let mut engine = image.builtins.deep_clone();

        let mut compiler = image.booted.virtual_machine.compiler.read().clone();
        compiler.builtin_modules = engine.modules.clone();
        // Installed once the programs have run. Without a kernel the jit leaves
        // functions alone, as it does while a kernel image boots: the compiler
        // already counts the globals of every program as closed, which lets the jit
        // bake in their values, and a slot a later program defines doesn't hold it
        // yet. Functions are compiled when they're called after.
        let kernel = match &image.delta {
            Some(delta) => {
                let mut copy = compiler;
                let kernel = delta.apply(&mut copy).expect("applying the pass engine's image");
                compiler = copy;
                Some(kernel)
            }
            None => compiler.kernel.take().map(|kernel| kernel.deep_clone()),
        };
        compiler.constant_map = compiler.constant_map.deep_clone();
        // Functions in the constant map are objects the jit compiles against one
        // engine's globals, so each engine gets its own
        for (index, closure) in &image.closures {
            compiler
                .constant_map
                .replace(*index, SteelVal::Closure(Gc::new(closure.fresh_copy())));
        }
        let constants = compiler.constant_map.clone();
        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler));

        #[cfg(feature = "biased")]
        QueueHandle::register_thread();

        let mut results = Vec::new();
        let mut run = |engine: &mut Engine, program: &Executable| {
            // Constants the programs refer to are in the engine's copy of the map
            let mut program = program.clone();
            program.constant_map = constants.clone();

            match engine.virtual_machine.run_executable(&program) {
                Ok(values) => results = values,
                Err(e) => {
                    raise_error(&engine.virtual_machine.compiler.read().sources, e);
                    panic!("Running the prelude again failed");
                }
            }
        };
        for program in &image.programs {
            run(&mut engine, program);
        }
        drop(run);
        if !image.steps.is_empty() {
            let steps: Vec<BootStep> = image
                .steps
                .iter()
                .map(|step| step.with_constants(&constants))
                .collect();
            let modules = engine.modules.clone();
            let registered = |name: &str| -> Result<SteelVal> {
                let module = image
                    .host_modules
                    .iter()
                    .map(|host| host.module())
                    .find(|module| module.unreadable_name() == name)
                    .expect("an image registers only its host modules");
                modules.clone().insert(module.name(), module.clone());
                module.into_steelval()
            };
            match replay_steps(&mut engine, &steps, &registered) {
                Ok(values) => results = values,
                Err(e) => {
                    raise_error(&engine.virtual_machine.compiler.read().sources, e);
                    panic!("Running the prelude again failed");
                }
            }
        }

        let mut compiler = engine.virtual_machine.compiler.write();
        compiler.builtin_modules = engine.modules.clone();
        compiler.kernel = kernel;
        drop(compiler);

        (engine, results)
    }

    // Compiles and runs a program like `compile_and_run_raw_program`, keeping it,
    // and adds to `closures` copies of the functions the compile put in the
    // constant map, taken before running the program can touch them
    fn compile_and_run_keeping(
        &mut self,
        source: &'static str,
        closures: &mut Vec<(usize, Gc<ByteCodeLambda>)>,
    ) -> Result<Executable> {
        #[cfg(feature = "biased")]
        QueueHandle::register_thread();

        let before = self.virtual_machine.compiler.read().constant_map.len();
        let program = self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_executable(source, None)
        })?;

        let constants = self.virtual_machine.compiler.read().constant_map.clone();
        for index in before..constants.len() {
            if let SteelVal::Closure(closure) = constants.get(index) {
                closures.push((index, Gc::new(closure.fresh_copy())));
            }
        }

        let executable = self.raw_program_to_executable(program)?;
        self.gc_shadowed_roots();
        self.virtual_machine.run_executable(&executable)?;

        Ok(executable)
    }

    /// Adds a directory for the engine to resolve paths from.
    ///
    /// By default, the engine will search $STEEL_HOME/cogs for modules,
    /// but any additional path added this way will increase the module
    /// resolution search space.
    pub fn add_search_directory(&mut self, dir: PathBuf) {
        self.virtual_machine
            .compiler
            .write()
            .add_search_directory(dir)
    }

    pub fn with_interrupted(&mut self, interrupted: Arc<AtomicBool>) {
        self.virtual_machine.with_interrupted(interrupted);
    }

    /// Sets the mode of a compiler pass written in Steel, `*` for every pass
    /// (see compiler/passes/steel_passes.rs)
    pub(crate) fn set_compiler_pass_mode(
        &mut self,
        name: &str,
        mode: crate::compiler::passes::steel_passes::PassMode,
    ) {
        self.virtual_machine
            .compiler
            .write()
            .steel_passes
            .set_mode(name, mode);
    }

    /// Whether a Steel pass in `compare` mode that differs from its Rust version
    /// fails compilation
    pub(crate) fn set_compiler_pass_compare_fails(&mut self, fails: bool) {
        self.virtual_machine
            .compiler
            .write()
            .steel_passes
            .set_compare_fails(fails);
    }

    /// A module of Steel compiler passes to load, as `name`, into the engine
    /// passes run in
    pub(crate) fn add_compiler_pass_module(&mut self, name: &str, source: &str) {
        self.virtual_machine
            .compiler
            .write()
            .steel_passes
            .add_module_source(name.to_owned(), source.to_owned());
    }

    pub fn get_thread_state_controller(&self) -> ThreadStateController {
        self.virtual_machine.synchronizer.state.clone()
    }

    /// Consumes the current `Engine` and emits a new `Engine` with the prelude added
    /// to the environment. The prelude won't work unless the primitives are also enabled.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_base().with_prelude().unwrap();
    /// vm.run("(+ 1 2 3)").unwrap();
    /// ```
    pub fn with_prelude(mut self) -> Result<Self> {
        let core_libraries = &[crate::stdlib::PRELUDE];

        for core in core_libraries {
            self.compile_and_run_raw_program(*core)?;
        }

        Ok(self)
    }

    /// Registers the prelude to the environment of the given Engine.
    /// The prelude won't work unless the primitives are also enabled.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new_base();
    /// vm.register_prelude().unwrap();
    /// vm.run("(+ 1 2 3)").unwrap();
    /// ```
    pub fn register_prelude(&mut self) -> Result<&mut Self> {
        let core_libraries = &[crate::stdlib::PRELUDE];

        for core in core_libraries {
            self.compile_and_run_raw_program(*core)?;
        }

        Ok(self)
    }

    /// Generates a file for the language server's `STEEL_LSP_HOME` describing
    /// the modules this engine has on top of a stock engine. Each function gets
    /// the signature it declares (see [`crate::steel_vm::host_signature`]),
    /// which for `register_fn` functions comes from their Rust types. The
    /// language server trusts these, so regenerate the file whenever the
    /// registered functions change.
    pub fn emit_lsp_definitions(&self) -> String {
        let stock: std::collections::HashSet<String> = Engine::new()
            .builtin_modules()
            .inner()
            .keys()
            .map(|name| name.to_string())
            .collect();
        let modules = self.builtin_modules().inner();
        let mut names: Vec<_> = modules
            .keys()
            .filter(|name| !stock.contains(name.as_ref()))
            .cloned()
            .collect();
        names.sort();

        let quote = |s: &str| format!("{:?}", s);
        let mut out = String::from(
            ";; Generated by Engine::emit_lsp_definitions. The signatures come from\n\
             ;; the functions' Rust types.\n",
        );
        for (index, name) in names.iter().enumerate() {
            let module = &modules[name];
            let binding = format!("module-{index}");
            out.push_str(&format!(
                "\n(define {binding} (#%module {}))\n",
                quote(name)
            ));
            let mut functions = module.names();
            functions.sort();
            for function in functions {
                let value = crate::steel_vm::host_signature::declared_in(module, &function)
                    .map(|signature| crate::steel_vm::host_signature::render(&signature))
                    .unwrap_or_else(|| "void".to_string());
                out.push_str(&format!(
                    "(#%module-add {binding} {} {value})\n",
                    quote(&function)
                ));
            }
        }
        out
    }

    pub(crate) fn has_module(&self, name: &str) -> bool {
        self.modules.get(name).is_some()
    }

    // Registers the given module into the virtual machine
    pub fn register_module(&mut self, module: BuiltInModule) -> &mut Self {
        {
            use crate::gc::ShareableMut;
            let mut guard = module.module.write();
            if !guard.context_functions.is_empty() {
                let funcs = register_context_functions(self, &mut guard.context_functions);
                // Overwrite the existing values, and then insert the module.
                for (key, func) in funcs {
                    guard.register_value(&key, func);
                }
            }
        }

        // Add the module to the map
        self.modules.insert(module.name(), module.clone());
        // Register the actual module itself as a value to make the virtual machine capable of reading from it
        self.register_value(
            module.unreadable_name().as_str(),
            module.into_steelval().unwrap(),
        );

        self
    }

    #[cfg(feature = "dylibs")]
    pub fn register_external_module(
        &mut self,
        module: abi_stable::std_types::RBox<FFIModule>,
    ) -> Result<&mut Self> {
        let external_module = FFIWrappedModule::new(module, None)?.build();

        self.modules
            .insert(external_module.name(), external_module.clone());

        self.register_value(
            external_module.unreadable_name().as_str(),
            external_module.into_steelval().unwrap(),
        );

        Ok(self)
    }

    pub fn emit_raw_program_no_path<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr: E,
    ) -> Result<RawProgramWithSymbols> {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_executable(expr, None)
        })
    }

    pub fn emit_raw_program<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr: E,
        path: PathBuf,
    ) -> Result<RawProgramWithSymbols> {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_executable(expr, Some(path))
        })
    }

    #[doc(hidden)]
    pub fn debug_build_strings(&mut self, program: RawProgramWithSymbols) -> Result<Vec<String>> {
        program.debug_generate_instructions(&mut self.virtual_machine.compiler.write().symbol_map)
    }

    pub fn debug_print_build(
        &mut self,
        name: String,
        program: RawProgramWithSymbols,
    ) -> Result<()> {
        program.debug_build(name, &mut self.virtual_machine.compiler.write().symbol_map)
    }

    pub fn globals(&self) -> MappedRwLockReadGuard<'_, Vec<InternedString>> {
        RwLockReadGuard::map(self.virtual_machine.compiler.read(), |x| {
            x.symbol_map.values()
        })
    }

    // TODO: Remove duplicates!
    pub fn readable_globals(&self, after_offset: usize) -> Vec<InternedString> {
        let mut seen = HashSet::new();

        self.virtual_machine
            .compiler
            .read()
            .symbol_map
            .values()
            .iter()
            .skip(after_offset)
            .filter(|x| {
                let resolved = x.resolve();
                !resolved.starts_with("#")
                    && !resolved.starts_with("%")
                    && !resolved.starts_with("mangler#%")
                    && !resolved.starts_with(MANGLER_PREFIX)
                    && !resolved.starts_with("__module")
                    && !resolved.ends_with("__doc__")
            })
            .filter_map(|x| {
                if seen.contains(x) {
                    None
                } else {
                    seen.insert(x);
                    Some(x)
                }
            })
            .copied()
            .collect()
    }

    // Generate dynamically linked files, containing all of the necessary information
    // This means - compiling all macros as well.
    fn load_raw_program(&mut self, mut program: RawProgramWithSymbols) {
        fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
            match &t.ty {
                TokenType::BooleanLiteral(b) => Ok((*b).into()),
                TokenType::Number(n) => n.resolve().into_steelval(),
                TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.clone().into())),
                TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
                // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
                TokenType::Keyword(k) => Ok(SteelVal::SymbolV((*k).into())),
                what => {
                    // println!("getting here in the eval_atom - code_gen");
                    stop!(UnexpectedToken => what; t.span)
                }
            }
        }

        for expr in &mut program.instructions {
            // Reform the program to conform to the current state of _this_ engine.
            for instruction in expr.iter_mut() {
                todo!()

                // match instruction {
                //     Instruction {
                //         op_code: OpCode::PUSHCONST,
                //         contents: Some(Expr::Atom(constant_value)),
                //         ..
                //     } => {
                //         let value =
                //             eval_atom(&constant_value).expect("This must be a constant value");

                //         instruction.payload_size = self.compiler.constant_map.add_or_get(value);
                //     }

                //     Instruction {
                //         op_code: OpCode::PUSHCONST,
                //         contents: Some(Expr::List(expression)),
                //         ..
                //     } => {
                //         let value = SteelVal::try_from(expression.clone())
                //             .expect("This conversion must work");

                //         instruction.payload_size = self.compiler.constant_map.add_or_get(value);
                //     }

                //     _ => {
                //         todo!()
                //     }
                // }
            }
        }
    }

    pub fn expand_to_file<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        exprs: E,
        path: PathBuf,
    ) {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .fully_expand_to_file(exprs, Some(path))
                .unwrap()
        });
    }

    pub fn load_from_expanded_file(&mut self, path: &str) {
        let program = self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .load_from_file(path)
                .unwrap()
        });

        self.run_raw_program(program).unwrap();
    }

    /// Compiles and runs an already expanded program, as produced by
    /// `emit_expanded_ast_without_optimizations`. Used by tests that rewrite
    /// that AST, such as the type inference soundness check.
    #[cfg(test)]
    pub(crate) fn compile_and_run_expanded(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<Vec<SteelVal>> {
        let program = self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_raw_program(exprs, path)
        })?;

        self.run_raw_program(program)
    }

    // TODO -> clean up this API a lot
    pub fn compile_and_run_raw_program_with_path<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        exprs: E,
        path: PathBuf,
    ) -> Result<Vec<SteelVal>> {
        let program = self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_executable(exprs, Some(path))
        })?;

        self.run_raw_program(program)
    }

    pub(crate) fn run_raw_program_from_exprs(
        &mut self,
        exprs: Vec<ExprKind>,
    ) -> Result<Vec<SteelVal>> {
        let program = self
            .virtual_machine
            .compiler
            .write()
            .compile_executable_from_expressions(exprs)?;
        self.run_raw_program(program)
    }

    pub fn compile_and_run_raw_program<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        exprs: E,
    ) -> Result<Vec<SteelVal>> {
        #[cfg(feature = "biased")]
        QueueHandle::register_thread();

        let program = self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .compile_executable(exprs, None)
        })?;

        self.run_raw_program(program)
    }

    pub fn raw_program_to_executable(
        &mut self,
        program: RawProgramWithSymbols,
    ) -> Result<Executable> {
        let symbol_map_offset = self.virtual_machine.compiler.read().symbol_map.len();
        // Only this build's replacements are rolled back
        self.virtual_machine.compiler.write().symbol_map.clear_replaced();

        let result = program.build(
            "TestProgram".to_string(),
            &mut self.virtual_machine.compiler.write().symbol_map,
        );

        // Revisit if we need to do this at all?
        if result.is_err() {
            let mut guard = self.virtual_machine.compiler.write();

            // Reset the compiled modules as well.
            guard.symbol_map.roll_back(symbol_map_offset);
            guard.module_manager.rollback_metadata();
        } else {
            self.virtual_machine.compiler.write().symbol_map.clear_replaced();
        }

        result
    }

    // TODO: Add doc for this
    #[doc(hidden)]
    pub fn environment_offset(&self) -> GlobalCheckpoint {
        GlobalCheckpoint {
            symbol_map_offset: self.virtual_machine.compiler.read().symbol_map.len(),
            globals_offset: self.virtual_machine.global_env.len(),
        }
    }

    // TODO: Add doc for this
    #[doc(hidden)]
    pub fn rollback_to_checkpoint(&mut self, checkpoint: GlobalCheckpoint) -> Result<()> {
        self.virtual_machine
            .compiler
            .write()
            .symbol_map
            .roll_back(checkpoint.symbol_map_offset);

        // // TODO: Does this even need to happen?
        // #[cfg(feature = "sync")]
        // {
        //     self.virtual_machine
        //         .global_env
        //         .bindings_vec
        //         .write()
        //         // .unwrap()
        //         .truncate(checkpoint.globals_offset);
        // }

        #[cfg(not(feature = "sync"))]
        {
            self.virtual_machine
                .global_env
                .bindings_vec
                .truncate(checkpoint.globals_offset);
        }

        Ok(())
    }

    pub fn run_raw_program(&mut self, program: RawProgramWithSymbols) -> Result<Vec<SteelVal>> {
        // TODO: Add hook here to check what values are now unreachable!
        let executable = self.raw_program_to_executable(program)?;

        self.virtual_machine
            .recording
            .record(|| BootStep::Run(executable.clone()));

        self.gc_shadowed_roots();

        self.virtual_machine.run_executable(&executable)
    }

    fn gc_shadowed_roots(&mut self) {
        // Unfortunately, we have to invoke a whole GC algorithm here
        // for shadowed rooted values
        let guard = self.virtual_machine.compiler.write();
        if guard.symbol_map.free_list.should_collect() {
            drop(guard);

            let mut heap_lock = self
                .virtual_machine
                .enter_safepoint(|thread| thread.heap.lock_arc());

            #[cfg(feature = "sync")]
            self.virtual_machine
                .with_locked_env_holding_heap(|ctx, env| {
                    #[cfg(feature = "sync")]
                    {
                        GlobalSlotRecycler::free_shadowed_rooted_values(
                            // &mut self.virtual_machine.global_env.bindings_vec.write(),
                            env.0.as_mut_slice(),
                            // .unwrap(),
                            &mut ctx.compiler.write().symbol_map,
                            &mut heap_lock,
                        );
                    }

                    #[cfg(not(feature = "sync"))]
                    {
                        GlobalSlotRecycler::free_shadowed_rooted_values(
                            &mut ctx.global_env.bindings_vec,
                            &mut ctx.compiler.write().symbol_map,
                            &mut heap_lock,
                        );
                    }

                    // Drop the pure functions which have been lifted.
                    ctx.function_interner
                        .pure_function_interner
                        .retain(|key, value| crate::gc::Gc::strong_count(value) > 1);

                    // TODO:
                    // It is also necessary to keep track of the values to drop
                    // for closures - this can be done by tracking the closure id
                    // throughout the generated bytecode. Same way we keep track of
                    // the referenced globals; we track the referenced closures.
                    // FIXME: Add that code here

                    ctx.compiler
                        .write()
                        .symbol_map
                        .free_list
                        .increment_generation();
                });

            #[cfg(not(feature = "sync"))]
            self.virtual_machine.with_locked_env(|ctx| {
                #[cfg(feature = "sync")]
                {
                    GlobalSlotRecycler::free_shadowed_rooted_values(
                        &mut ctx.virtual_machine.global_env.bindings_vec.write(),
                        // &mut env.0.as_mut_slice(),
                        // .unwrap(),
                        &mut ctx.compiler.write().symbol_map,
                        &mut heap_lock,
                    );
                }

                #[cfg(not(feature = "sync"))]
                {
                    GlobalSlotRecycler::free_shadowed_rooted_values(
                        &mut ctx.global_env.bindings_vec,
                        &mut ctx.compiler.write().symbol_map,
                        &mut heap_lock,
                    );
                }

                // Drop the pure functions which have been lifted.
                ctx.function_interner
                    .pure_function_interner
                    .retain(|key, value| crate::gc::Gc::strong_count(value) > 1);

                // TODO:
                // It is also necessary to keep track of the values to drop
                // for closures - this can be done by tracking the closure id
                // throughout the generated bytecode. Same way we keep track of
                // the referenced globals; we track the referenced closures.
                // FIXME: Add that code here

                ctx.compiler
                    .write()
                    .symbol_map
                    .free_list
                    .increment_generation();
            });
        }
    }

    pub fn run_executable(&mut self, executable: &Executable) -> Result<Vec<SteelVal>> {
        self.virtual_machine.run_executable(executable)
    }

    /// Directly emit the expanded ast
    pub fn emit_expanded_ast(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .emit_expanded_ast(expr, path)
        })
    }

    fn with_sources_guard<T>(&self, func: impl FnOnce() -> T) -> T {
        let res = func();

        self.virtual_machine
            .compiler
            .write()
            .gc_sources(self.virtual_machine.function_interner.spans.values());

        res
    }

    pub fn emit_expanded_ast_without_optimizations(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .emit_expanded_ast_without_optimizations(expr, path)
        })
    }

    /// Emit the unexpanded AST
    pub fn emit_ast_to_string(expr: &str) -> Result<String> {
        let parsed = Self::emit_ast(expr)?;
        Ok(parsed.into_iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }

    pub fn emit_ast(expr: &str) -> Result<Vec<ExprKind>> {
        let parsed: core::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr, SourceId::none()).collect();
        Ok(parsed?)
    }

    /// Emit the fully expanded AST as a pretty printed string
    pub fn emit_fully_expanded_ast_to_string(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<String> {
        self.with_sources_guard(|| {
            Ok(self
                .virtual_machine
                .compiler
                .write()
                .emit_expanded_ast(expr, path)?
                .into_iter()
                .map(|x| x.to_pretty(60))
                .join("\n\n"))
        })
    }

    /// Emits the fully expanded AST directly.
    pub fn emit_fully_expanded_ast(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.with_sources_guard(|| {
            self.virtual_machine
                .compiler
                .write()
                .emit_expanded_ast(expr, path)
        })
    }

    /// Registers an external value of any type as long as it implements [`FromSteelVal`](crate::rvals::FromSteelVal) and
    /// [`IntoSteelVal`](crate::rvals::IntoSteelVal). This method does the coercion to embed the type into the `Engine`'s
    /// environment with the name `name`. This function can fail only if the conversion from `T` to [`SteelVal`](crate::rvals::SteelVal) fails.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new();
    /// let external_value = "hello-world".to_string();
    /// vm.register_external_value("hello-world", external_value).unwrap();
    /// vm.run("hello-world").unwrap(); // Will return the string
    /// ```
    pub fn register_external_value<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        name: &str,
        value: T,
    ) -> Result<&mut Self> {
        let converted = value.into_steelval()?;
        Ok(self.register_value(name, converted))
    }

    /// Registers a [`SteelVal`](crate::rvals::SteelVal) under the name `name` in the `Engine`'s internal environment.
    ///
    /// # Examples
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// use steel::rvals::SteelVal;
    ///
    /// let mut vm = Engine::new();
    /// let external_value = SteelVal::StringV("hello-world".to_string().into());
    /// vm.register_value("hello-world", external_value);
    /// vm.run("hello-world").unwrap(); // Will return the string
    /// ```
    pub fn register_value(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.register_value_inner(name, value)
    }

    /// Registers a variadic native function under `name`. The function receives the
    /// raw `&[SteelVal]` arguments and returns a `Result<SteelVal>`. `arity`, when
    /// `Some`, declares the expected fixed arity; callers must still check argument
    /// counts inside `func` if they need a runtime error.
    pub fn register_native_fn(
        &mut self,
        name: &str,
        arity: Option<u32>,
        func: impl Fn(&[SteelVal]) -> Result<SteelVal> + Send + Sync + 'static,
    ) -> &mut Self {
        let name_arc = Arc::new(name.to_string());
        self.register_value(
            name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                Arc::new(func),
                Some(name_arc),
                arity,
            ))),
        )
    }

    pub(crate) fn undefine_global(&mut self, name: &str) {
        let idx = self
            .virtual_machine
            .compiler
            .write()
            .symbol_map
            .define(&name.into());
        self.virtual_machine
            .insert_binding(idx, crate::env::undefined_global());
    }

    pub fn update_value(&mut self, name: &str, value: SteelVal) -> Option<&mut Self> {
        let idx = self.virtual_machine.compiler.read().get_idx(name)?;

        let _ = self
            .virtual_machine
            .enter_safepoint(|thread| thread.heap.lock_arc());

        #[cfg(feature = "sync")]
        {
            self.virtual_machine
                .with_locked_env(|_, env| env.set_idx(idx, value));
        }

        #[cfg(not(feature = "sync"))]
        {
            self.virtual_machine
                .with_locked_env(|this| this.global_env.repl_set_idx(idx, value))
                .ok()?;
        }

        Some(self)
    }

    /// Registers multiple values at once
    pub fn register_values(
        &mut self,
        values: impl Iterator<Item = (String, SteelVal)>,
    ) -> &mut Self {
        for (name, value) in values {
            self.register_value(name.as_str(), value);
        }
        self
    }

    /// Registers a predicate for a given type. When embedding external values, it is convenient
    /// to be able to have a predicate to test if the given value is the specified type.
    /// In order to be registered, a type must implement [`FromSteelVal`](crate::rvals::FromSteelVal)
    /// and [`IntoSteelVal`](crate::rvals::IntoSteelVal)
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// # use steel::steel_vm::register_fn::RegisterFn;
    /// # use steel::rvals::FromSteelVal;
    /// # use steel::rvals::IntoSteelVal;
    /// # use steel::rvals::Result;
    /// # use steel::rvals::SteelVal;
    ///
    /// #[derive(Clone, Debug)]
    /// struct ExternalStruct {
    ///     value: isize,
    /// }
    ///
    /// impl ExternalStruct {
    ///     pub fn new(value: isize) -> Self {
    ///         Self { value }
    ///     }
    /// }
    ///
    /// impl FromSteelVal for ExternalStruct {
    ///     fn from_steelval(val: &SteelVal) -> Result<Self> {
    ///         let value: isize = match val {
    ///             SteelVal::IntV(v) => *v,
    ///             _ => unimplemented!(),
    ///         };
    ///         Ok(Self { value })
    ///     }
    /// }
    ///
    /// impl IntoSteelVal for ExternalStruct {
    ///     fn into_steelval(self) -> Result<SteelVal> {
    ///         Ok(SteelVal::IntV(self.value))
    ///     }
    /// }
    ///
    /// let mut vm = Engine::new();
    ///
    /// // Register the type with the VM.
    /// vm.register_type::<ExternalStruct>("ExternalStruct?");
    ///
    /// // Register a constructor as a function with the same name as the struct.
    /// vm.register_fn("ExternalStruct", ExternalStruct::new);
    ///
    /// vm.compile_and_run_raw_program(r#"(define new-external-struct (ExternalStruct 42))"#)
    ///     .unwrap();
    ///
    /// let extracted = vm.extract::<ExternalStruct>("new-external-struct").unwrap();
    /// assert_eq!(extracted.value, 42);
    /// ```
    pub fn register_type<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected 1 argument, got {}", predicate_name, args.len()));
            }

            assert!(args.len() == 1);

            Ok(SteelVal::BoolV(T::from_steelval(&args[0]).is_ok()))
        };

        self.register_value(
            predicate_name,
            SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(predicate_name),
                Some(1),
            ))),
        )
    }

    /// Extracts a value with the given identifier `name` from the internal environment.
    /// If a script calculated some series of bound values, then it can be extracted this way.
    /// This will return the [`SteelVal`](crate::rvals::SteelVal), not the underlying data.
    /// To unwrap the value, use the [`extract`](crate::steel_vm::engine::Engine::extract) method and pass the type parameter.
    ///
    /// The function will return an error if the `name` is not currently bound in the `Engine`'s internal environment.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// use steel::rvals::SteelVal;
    /// let mut vm = Engine::new();
    /// vm.run("(define a 10)").unwrap();
    /// assert_eq!(vm.extract_value("a").unwrap(), SteelVal::IntV(10));
    /// ```
    pub fn extract_value(&self, name: &str) -> Result<SteelVal> {
        let idx = self.virtual_machine.compiler.read().get_idx(name).ok_or_else(throw!(
            Generic => format!("free identifier: {name} - identifier given cannot be found in the global environment")
        ))?;

        self.virtual_machine.extract_value(idx)
            .ok_or_else(throw!(
                Generic => format!("free identifier: {name} - identifier given cannot be found in the global environment")
            ))
    }

    /// Extracts a value with the given identifier `name` from the internal environment, and attempts to coerce it to the
    /// given type. This will return an error if the `name` is not currently bound in the `Engine`'s internal environment, or
    /// if the type passed in does not match the value (and thus the coercion using [`FromSteelVal`](crate::rvals::FromSteelVal) fails)
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// let mut vm = Engine::new();
    /// vm.run("(define a 10)").unwrap();
    /// assert_eq!(vm.extract::<usize>("a").unwrap(), 10);
    /// ```
    pub fn extract<T: FromSteelVal>(&self, name: &str) -> Result<T> {
        T::from_steelval(&self.extract_value(name)?)
    }

    /// Raise the error within the stack trace
    pub fn raise_error(&self, error: SteelErr) {
        raise_error(&self.virtual_machine.compiler.read().sources, error)
    }

    /// Emit an error string reporing, the back trace.
    pub fn raise_error_to_string(&self, error: SteelErr) -> Option<String> {
        raise_error_to_string(&self.virtual_machine.compiler.read().sources, error)
    }

    pub fn add_module(&mut self, path: String) -> Result<()> {
        self.virtual_machine
            .compiler
            .write()
            .compile_module(path.into(), self.modules.clone())
    }

    pub fn modules(&self) -> MappedRwLockReadGuard<'_, crate::HashMap<PathBuf, CompiledModule>> {
        RwLockReadGuard::map(self.virtual_machine.compiler.read(), |x| x.modules())
    }

    pub fn module_metadata(&self) -> crate::HashMap<PathBuf, crate::time::SystemTime> {
        self.virtual_machine
            .compiler
            .read()
            .module_manager
            .file_metadata
            .clone()
    }

    pub fn global_exists(&self, ident: &str) -> bool {
        let spur = if let Some(spur) = InternedString::try_get(ident) {
            spur
        } else {
            return false;
        };

        self.virtual_machine
            .compiler
            .read()
            .symbol_map
            .get(&spur)
            .is_ok()
    }

    pub fn symbol_map(&self) -> MappedRwLockReadGuard<'_, SymbolMap> {
        RwLockReadGuard::map(self.virtual_machine.compiler.read(), |x| &x.symbol_map)
    }

    pub fn in_scope_macros(
        &self,
    ) -> MappedRwLockReadGuard<'_, FxHashMap<InternedString, SteelMacro>> {
        RwLockReadGuard::map(self.virtual_machine.compiler.read(), |x| &x.macro_env)
    }

    pub fn in_scope_macros_mut(
        &mut self,
    ) -> MappedRwLockWriteGuard<'_, FxHashMap<InternedString, SteelMacro>> {
        RwLockWriteGuard::map(self.virtual_machine.compiler.write(), |x| &mut x.macro_env)
    }

    // TODO: Re-implement the module path expansion
    pub fn get_module(&self, path: PathBuf) -> Result<SteelVal> {
        let module_path = path_to_module_name(path);
        self.extract_value(&module_path)
    }

    pub fn get_source_id(&self, path: &Path) -> Option<SourceId> {
        self.virtual_machine
            .compiler
            .read()
            .sources
            .get_source_id(path)
    }

    pub fn get_path_for_source_id(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.virtual_machine
            .compiler
            .read()
            .sources
            .get_path(source_id)
    }

    pub fn get_source(&self, source_id: &SourceId) -> Option<Arc<Cow<'static, str>>> {
        self.virtual_machine
            .compiler
            .read()
            .sources
            .sources
            .get(*source_id)
            .cloned()
    }

    #[doc(hidden)]
    pub fn get_doc_for_identifier(&self, ident: &str) -> Option<String> {
        let value = self.extract_value(ident).ok()?;

        match self.virtual_machine.compiler.read().get_doc(value)? {
            crate::compiler::compiler::StringOrSteelString::String(s) => Some(s),
            crate::compiler::compiler::StringOrSteelString::SteelString(steel_string) => {
                Some(steel_string.as_str().to_string())
            }
        }
    }
}

// #[cfg(test)]
// mod on_progress_tests {
//     use super::*;
//     use core::cell::Cell;
//     use alloc::rc::Rc;

//     // TODO: At the moment the on progress business is turned off

//     // #[test]
//     // fn count_every_thousand() {
//     //     let mut vm = Engine::new();

//     //     let external_count = Rc::new(Cell::new(0));
//     //     let embedded_count = Rc::clone(&external_count);

//     //     vm.on_progress(move |count| {
//     //         // parameter is 'usize' - number of instructions performed up to this point
//     //         if count % 1000 == 0 {
//     //             // print out a progress log every 1000 operations
//     //             println!("Number of instructions up to this point: {}", count);
//     //             embedded_count.set(embedded_count.get() + 1);

//     //             // Returning false here would quit the evaluation of the function
//     //             return true;
//     //         }
//     //         true
//     //     });

//     //     // This should end with "Number of instructions up to this point: 4000"
//     //     vm.run(
//     //         r#"
//     //         (define (loop x)
//     //             (if (equal? x 1000)
//     //                 x
//     //                 (loop (+ x 1))))
//     //         (displayln (loop 0))
//     //     "#,
//     //     )
//     //     .unwrap();

//     //     assert_eq!(external_count.get(), 4);
//     // }
// }

fn raise_error(sources: &Sources, error: SteelErr) {
    if let Some(span) = error.span() {
        let source_id = span.source_id();

        if let Some(source_id) = source_id {
            let sources = &sources.sources;

            let file_name = sources.get_path(&source_id);

            if let Some(file_content) = sources.get(source_id) {
                // Build stack trace if we have it:
                if let Some(trace) = error.stack_trace() {
                    // TODO: Flatten recursive calls into the same stack trace
                    // and present the count
                    for dehydrated_context in trace.trace().iter().take(20) {
                        // Report a call stack with whatever we actually have,
                        if let Some(span) = dehydrated_context.span() {
                            let id = span.source_id();
                            if let Some(id) = id {
                                if let Some(source) = sources.get(id) {
                                    let trace_line_file_name = sources.get_path(&id);

                                    let resolved_file_name = trace_line_file_name
                                        .as_ref()
                                        .and_then(|x| x.to_str())
                                        .unwrap_or_default();

                                    back_trace(resolved_file_name, source, *span);
                                }
                            }
                        }
                    }
                }

                let resolved_file_name = file_name.unwrap_or_default();

                error.emit_result(resolved_file_name.to_str().unwrap(), file_content);
                return;
            }
        }
    }

    println!("Unable to locate source and span information for this error: {error}");
}

// If we are to construct an error object, emit that
pub(crate) fn raise_error_to_string(sources: &Sources, error: SteelErr) -> Option<String> {
    if let Some(span) = error.span() {
        let source_id = span.source_id();
        if let Some(source_id) = source_id {
            let sources = &sources.sources;

            let file_name = sources.get_path(&source_id);

            if let Some(file_content) = sources.get(source_id) {
                let mut back_traces = Vec::with_capacity(20);

                // Build stack trace if we have it:
                if let Some(trace) = error.stack_trace() {
                    // TODO: Flatten recursive calls into the same stack trace
                    // and present the count
                    for dehydrated_context in trace.trace().iter().take(20) {
                        // Report a call stack with whatever we actually have,
                        if let Some(span) = dehydrated_context.span() {
                            // Missing the span, its not particularly worth reporting?
                            if span.start == 0 && span.end == 0 {
                                continue;
                            }

                            let id = span.source_id();

                            if let Some(id) = id {
                                if let Some(source) = sources.get(id) {
                                    let trace_line_file_name = sources.get_path(&id);

                                    let resolved_file_name = trace_line_file_name
                                        .as_ref()
                                        .and_then(|x| x.to_str())
                                        .unwrap_or_default();

                                    let bt =
                                        back_trace_to_string(resolved_file_name, source, *span);
                                    back_traces.push(bt);
                                }
                            }
                        }
                    }
                }

                let resolved_file_name = file_name.unwrap_or_default();

                let final_error =
                    error.emit_result_to_string(resolved_file_name.to_str().unwrap(), file_content);

                back_traces.push(final_error);

                return Some(back_traces.join("\n"));
            }
        }
    }

    // println!("Unable to locate source and span information for this error: {error}");

    None
}

pub struct EngineBuilder {
    engine: Engine,
}

impl EngineBuilder {
    pub fn raw() -> Self {
        Self {
            engine: Engine::new_raw(),
        }
    }
}

#[cfg(test)]
mod engine_api_tests {
    use crate::custom_reference;

    use super::*;

    struct ReferenceStruct {
        value: usize,
    }

    impl ReferenceStruct {
        pub fn get_value(&mut self) -> usize {
            self.value
        }

        pub fn get_value_immutable(&self) -> usize {
            self.value
        }
    }

    impl CustomReference for ReferenceStruct {}
    custom_reference!(ReferenceStruct);

    #[test]
    fn test_references_in_engine() {
        let mut engine = Engine::new();
        let mut external_object = ReferenceStruct { value: 10 };

        engine.register_value("*external*", SteelVal::Void);
        engine.register_fn("external-get-value", ReferenceStruct::get_value);

        {
            let res = engine
                .run_with_reference::<ReferenceStruct, ReferenceStruct>(
                    &mut external_object,
                    "*external*",
                    "(external-get-value *external*)",
                )
                .unwrap();

            assert_eq!(res, SteelVal::IntV(10));
        }
    }

    #[test]
    fn test_references_in_engine_get_removed_after_lifetime() {
        let mut engine = Engine::new();
        let mut external_object = ReferenceStruct { value: 10 };

        engine.register_value("*external*", SteelVal::Void);
        engine.register_fn("external-get-value", ReferenceStruct::get_value);

        let res = engine
            .run_with_reference::<ReferenceStruct, ReferenceStruct>(
                &mut external_object,
                "*external*",
                "(external-get-value *external*)",
            )
            .unwrap();

        assert_eq!(res, SteelVal::IntV(10));

        // Afterwards, the value should be gone
        assert_eq!(engine.extract_value("*external*").unwrap(), SteelVal::Void);
    }

    #[test]
    fn test_immutable_references_in_engine_get_removed_after_lifetime() {
        let mut engine = Engine::new();
        let external_object = ReferenceStruct { value: 10 };

        engine.register_fn("external-get-value", ReferenceStruct::get_value);

        engine.register_fn(
            "external-get-value-imm",
            ReferenceStruct::get_value_immutable,
        );

        let res = engine
            .run_thunk_with_ro_reference::<ReferenceStruct, ReferenceStruct>(
                &external_object,
                |mut engine, value| {
                    engine.register_value("*external*", value);
                    engine
                        .compile_and_run_raw_program("(external-get-value-imm *external*)")
                        .map(|x| x.into_iter().next().unwrap())
                },
            )
            .unwrap();

        assert_eq!(res, SteelVal::IntV(10));

        // This absolutely has to fail, otherwise we're in trouble.
        assert!(engine
            .compile_and_run_raw_program("(external-get-value-imm *external*)")
            .is_err());
    }

    #[test]
    fn kept_closure_keeps_the_globals_of_its_shadowed_callees() {
        let mut engine = Engine::new();
        engine
            .run("(define sink 0) (define kept (mutable-vector #f))")
            .unwrap();
        // The padding stops inlining. `outer` then calls `inner` through its global slot.
        let pad = "(set! sink (+ sink 1)) ".repeat(40);
        let defs = |n: usize| {
            format!(
                "(define (inner x) (begin {pad} (+ x {n})))
                 (define (outer x) (begin {pad} (inner x)))"
            )
        };
        engine.run(defs(0)).unwrap();
        engine
            .run("(vector-set! kept 0 (lambda (x) (outer x)))")
            .unwrap();
        for n in 1..=300 {
            engine.run(defs(n)).unwrap();
            // A redefinition rebinds the global, so the kept closure calls the newest `inner`
            let result = engine.run("((vector-ref kept 0) 1)").unwrap();
            assert_eq!(
                result,
                vec![SteelVal::IntV(1 + n as isize)],
                "after {n} redefinitions"
            );
        }
    }
}

#[cfg(test)]
mod engine_sandbox_tests {
    use super::*;

    #[test]
    fn sandbox() {
        let mut engine = Engine::new_sandboxed();

        assert!(engine
            .compile_and_run_raw_program(r#"(create-directory! "foo-bar")"#)
            .is_err());
    }

    #[test]
    fn opening_a_file_in_sandboxed_engine_fails() {
        let mut engine = Engine::new_sandboxed();

        static PROGRAMS: &[&str] = &[
            r#"(open-input-file "foo-bar.txt")"#,
            r#"(open-output-file "foo-bar.txt")"#,
            r#"(open-output-file "foo-bar.txt" #:exists 'append)"#,
            r#"(open-input-output-file "foo-bar.txt")"#,
            r#"(open-input-output-file "foo-bar.txt" #:exists 'update)"#,
        ];

        for program in PROGRAMS {
            let error = engine.compile_and_run_raw_program(*program).unwrap_err();

            assert!(error.to_string().contains("sandboxed engine"));
        }
    }
}

#[cfg(test)]
mod derive_macro_tests {
    use super::*;

    #[derive(steel_derive::_Steel, PartialEq, Debug)]
    #[steel(getters)]
    struct FooBar {
        baz: usize,
    }

    #[test]
    fn test_struct_registered() {
        let mut module = BuiltInModule::new("foo");
        FooBar::register_type(&mut module);
    }

    #[derive(steel_derive::_Steel, PartialEq, Debug)]
    #[steel(equality, getters, constructors)]
    enum TestEnumVariants {
        Foo,
        Bar,
        Baz(usize, usize),
        Bazinga {
            foo: usize,
            bananas: usize,
        },
        #[steel(ignore)]
        Ignored(SteelString),
    }

    fn test() {
        let mut module = BuiltInModule::new("foo");
        TestEnumVariants::register_enum_variants(&mut module);
    }

    enum Foo {
        Bananas(usize, usize, usize, usize),
        Bananas2(usize, usize, usize, usize),
    }

    #[test]
    fn test_primitives_are_registered() {
        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("foo");

        TestEnumVariants::register_enum_variants(&mut module);
        engine.register_module(module);

        engine
            .run(
                r#"
(require-builtin foo)
(assert! (TestEnumVariants-Foo? (TestEnumVariants-Foo)))
(assert! (TestEnumVariants-Bar? (TestEnumVariants-Bar)))
(assert! (TestEnumVariants-Baz? (TestEnumVariants-Baz 10 20)))
(assert! (TestEnumVariants-Bazinga? (TestEnumVariants-Bazinga 100 200)))

(assert! (equal? (TestEnumVariants-Bazinga-foo (TestEnumVariants-Bazinga 100 200)) 100))
(assert! (equal? (TestEnumVariants-Bazinga-bananas (TestEnumVariants-Bazinga 100 200)) 200))
(assert! (equal? (TestEnumVariants-Baz-0 (TestEnumVariants-Baz 100 200)) 100))
(assert! (equal? (TestEnumVariants-Baz-1 (TestEnumVariants-Baz 100 200)) 200))

(assert! (not (equal? 100 (TestEnumVariants-Foo))))

            "#,
            )
            .unwrap();

        assert!(engine
            .run(r#"(TestEnumVariants-Ignored "Hello world")"#)
            .is_err())
    }
}

#[test]
fn test_steel_quote_macro() {
    use thin_vec::thin_vec;
    let foobarbaz = ExprKind::atom("foo");
    let foobarbaz_list = ExprKind::List(List::new(thin_vec![ExprKind::atom("foo")]));

    let expanded = steel_derive::internal_steel_quote! {
        (define bananas #foobarbaz)
        (define x (begin @foobarbaz_list ...))
    }
    .unwrap();

    println!("{}", expanded);
}

#[test]
fn test_raw_engine() {
    let mut engine = Engine::new_raw_no_kernel();
    engine.register_steel_module(
        "in-memory".to_owned(),
        "(provide foo) (define (foo) 10)".to_owned(),
    );

    engine.run(r#"(require "in-memory")"#).unwrap();
    let res = engine.run("(foo)").unwrap();

    assert_eq!(res[0], SteelVal::IntV(10));
}

#[test]
fn test_required_macro_expands_inside_let_body() {
    let mut engine = Engine::new();
    engine.register_steel_module(
        "twice-macro".to_owned(),
        "(provide twice) (define-syntax twice (syntax-rules () [(_ x) (* 2 x)]))".to_owned(),
    );

    engine.run(r#"(require "twice-macro")"#).unwrap();

    let res = engine
        .run(
            r#"
            (define (sum-doubled n)
              (let loop ([i 0] [acc 0])
                (if (= i n)
                    acc
                    (loop (+ i 1) (+ acc (twice i))))))
            (sum-doubled 4)
            "#,
        )
        .unwrap();
    assert_eq!(res.last().unwrap(), &SteelVal::IntV(12));

    let res = engine.run("(let ([i 3]) i (twice i))").unwrap();
    assert_eq!(res.last().unwrap(), &SteelVal::IntV(6));

    let res = engine
        .run("(let loop ([twice (lambda (x) 100)]) (twice 1))")
        .unwrap();
    assert_eq!(res.last().unwrap(), &SteelVal::IntV(100));
}

#[test]
fn test_ctx_func() {
    let mut engine = Engine::new();

    let mut module = BuiltInModule::new("test/module-ctx-func");

    module.register_fn("foo", |implicit: SteelVal| {
        println!("Called with implicit: {}", implicit);
    });

    engine.register_value("global-context", SteelVal::StringV("Hello world!".into()));

    module.supply_context_arg("global-context", "foo");

    engine.register_module(module);

    engine
        .run("(require-builtin test/module-ctx-func)")
        .unwrap();
    engine.run("(foo)").unwrap();
    engine.update_value("global-context", SteelVal::IntV(10));
    engine.run("(foo)").unwrap();
}

#[test]
fn test_ctx_func_registration() {
    let mut engine = Engine::new();

    let mut module = BuiltInModule::new("test/module-ctx-func-registration");
    engine.register_value("global-context", SteelVal::StringV("Hello world!".into()));

    module.register_fn_with_ctx("global-context", "foo", |implicit: SteelVal| {
        println!("Called with implicit: {}", implicit);
    });

    engine.register_module(module);

    engine
        .run("(require-builtin test/module-ctx-func-registration)")
        .unwrap();
    engine.run("(foo)").unwrap();
    engine.update_value("global-context", SteelVal::IntV(10));
    engine.run("(foo)").unwrap();
}

#[test]
fn test_ctx_func_registration_multiple() {
    let mut engine = Engine::new();

    let mut module = BuiltInModule::new("test/module-ctx-func-registration-multiple");
    engine.register_value("global-context", SteelVal::StringV("Hello world!".into()));

    module.register_fn_with_ctx("global-context", "foo", |implicit: SteelVal| {
        println!("Called with implicit: {}", implicit);
    });
    module.register_fn_with_ctx(
        "global-context",
        "bar",
        |implicit: SteelVal, arg: SteelVal| {
            println!("Bar Called with implicit: {}", implicit);
            println!("Bar Called with arg: {}", arg);
        },
    );

    engine.register_module(module);

    engine
        .run("(require-builtin test/module-ctx-func-registration-multiple)")
        .unwrap();
    engine.run("(bar 10)").unwrap();
    engine.update_value("global-context", SteelVal::IntV(10));
    engine.run("(bar 100)").unwrap();
}

#[cfg(test)]
mod resolver_tests {
    use super::*;

    use std::collections::HashMap;

    use crate::steel_vm::{
        builtin::BuiltInModule,
        engine::{Engine, ModuleResolver},
        register_fn::RegisterFn,
    };

    #[derive(Clone)]
    struct ModuleContainer(HashMap<String, BuiltInModule>);

    impl ModuleResolver for ModuleContainer {
        fn resolve(&self, name: &str) -> Option<BuiltInModule> {
            self.0.get(name).cloned()
        }

        fn names(&self) -> Vec<String> {
            self.0.keys().cloned().collect()
        }
    }

    impl Default for ModuleContainer {
        fn default() -> Self {
            let foo_mod = foo_module();

            Self(HashMap::from_iter([(foo_mod.name().to_string(), foo_mod)]))
        }
    }

    fn bar() -> String {
        String::from("baz")
    }

    fn foo_module() -> BuiltInModule {
        let mut module = BuiltInModule::new("foo");
        module.register_fn("bar", bar);
        module
    }

    fn engine_with_resolver(resolver: impl ModuleResolver + 'static) -> Engine {
        let mut engine = Engine::new();
        engine.register_module_resolver(resolver);
        engine
    }

    fn engine_with_modules(modules: impl Iterator<Item = BuiltInModule>) -> Engine {
        let mut engine = Engine::new();
        for module in modules {
            engine.register_module(module);
        }
        engine
    }

    #[test]
    fn with_resolver() {
        let modules = ModuleContainer::default();
        let mut engine = engine_with_resolver(modules);
        assert!(engine.run("(require-builtin foo)").is_ok());
    }

    #[test]
    fn with_modules() {
        let modules = ModuleContainer::default();
        let mut engine = engine_with_modules(modules.0.values().cloned());
        assert!(engine.run("(require-builtin foo)").is_ok());
    }

    #[test]
    fn modules_are_not_shared_between_engines() {
        let mut with_module = engine_with_modules(std::iter::once(foo_module()));
        assert!(with_module.run("(require-builtin foo)").is_ok());

        let mut without_module = Engine::new();
        let err = without_module
            .run("(require-builtin foo)")
            .expect_err("foo leaked into an engine it was never registered with");

        assert!(
            err.to_string().contains("module not found"),
            "expected foo to be unresolvable, got: {}",
            err
        );
    }

    #[test]
    fn interrupt_reaches_a_loop_that_calls_nothing() {
        let mut engine = Engine::new();
        let controller = engine.get_thread_state_controller();
        engine.run("(define (spin n) (spin (+ n 1)))").unwrap();

        let interrupter = std::thread::spawn({
            let controller = controller.clone();
            move || {
                std::thread::sleep(std::time::Duration::from_millis(300));
                controller.interrupt();
            }
        });
        let err = engine
            .run("(spin 0)")
            .expect_err("the loop only ends by being interrupted");
        assert!(err.to_string().contains("Interrupted"), "got: {}", err);
        interrupter.join().unwrap();

        // Still interrupted, so the next run is stopped as it starts.
        assert!(engine.run("(spin 0)").is_err());

        controller.resume();
        assert!(engine.run("(+ 1 2)").is_ok());
    }

    #[test]
    fn interrupts_come_and_go() {
        let mut engine = Engine::new();
        let controller = engine.get_thread_state_controller();
        engine
            .run(
                "(define (spin n) (spin (+ n 1)))
                 (define (ping n) (pong (+ n 1)))
                 (define (pong n) (ping (+ n 1)))
                 (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
                 (define (recurse) (fib 20) (recurse))",
            )
            .unwrap();

        for program in ["(spin 0)", "(ping 0)", "(recurse)", "(spin 0)", "(ping 0)"] {
            let interrupter = std::thread::spawn({
                let controller = controller.clone();
                move || {
                    std::thread::sleep(std::time::Duration::from_millis(50));
                    controller.interrupt();
                }
            });
            let err = engine
                .run(program)
                .expect_err("the loop only ends by being interrupted");
            assert!(err.to_string().contains("Interrupted"), "got: {}", err);
            interrupter.join().unwrap();

            controller.resume();
            assert!(engine.run("(fib 15)").is_ok());
        }
    }

    #[test]
    fn engines_on_their_own_threads_collect_side_by_side() {
        let program = "
            (struct node (next value) #:mutable)
            (define cell (box 0))
            (define numbers (mutable-vector 0 0 0 0))
            (define (build n previous)
              (if (= n 0)
                  previous
                  (build (- n 1) (node previous (list n n n)))))
            (define (work rounds)
              (when (> rounds 0)
                (set-box! cell (build 200 #f))
                (vector-set! numbers (modulo rounds 4) (box rounds))
                (when (= 0 (modulo rounds 50))
                  (#%gc-collect))
                (work (- rounds 1))))
            (work 1500)
            (node-value (unbox cell))";

        let workers: Vec<_> = (0..4)
            .map(|_| {
                std::thread::spawn(move || {
                    let mut engine = Engine::new();
                    let results = engine.run(program).unwrap();
                    results.last().map(|value| value.to_string())
                })
            })
            .collect();

        for worker in workers {
            assert_eq!(worker.join().unwrap().as_deref(), Some("(1 1 1)"));
        }
    }

    #[test]
    fn resolvers_are_not_shared_between_engines() {
        let mut with_resolver = engine_with_resolver(ModuleContainer::default());
        assert!(with_resolver.run("(require-builtin foo)").is_ok());

        let mut without_resolver = Engine::new();
        let err = without_resolver
            .run("(require-builtin foo)")
            .expect_err("the resolver leaked into an engine it was never registered with");

        assert!(
            err.to_string().contains("module not found"),
            "expected `foo` to be unresolvable, got: {}",
            err
        );
    }
}

#[cfg(test)]
mod kernel_replay_check {
    use super::*;

    // How a value compares, loosely: functions by id, others by kind and printed form
    fn describe(value: &SteelVal) -> String {
        match value {
            SteelVal::Closure(c) => format!("closure {}", c.id),
            SteelVal::BoxedFunction(f) => format!("host fn {:?}", f.name()),
            SteelVal::FuncV(_) | SteelVal::MutFunc(_) | SteelVal::BuiltIn(_) => "native".into(),
            SteelVal::HeapAllocated(b) => format!("box {}", describe(&b.get())),
            SteelVal::Custom(_) => format!("custom {}", value).chars().take(60).collect(),
            other => {
                let mut parts: Vec<String> = format!("{other}")
                    .split('\n')
                    .map(|x| x.trim().to_string())
                    .collect();
                // A hash table's entries print in no particular order
                parts.sort();
                parts.join(" ")
            }
        }
    }

    // A boot image saved to disk can't keep the macro kernel's engine, whose
    // globals include the closures of the prelude's procedural macros. It keeps
    // what the engine was made to do (`BootStep`) and does that again. This checks
    // that doing so, then copying the plain globals calls changed (counters), gives
    // the engine the prelude left: every global the same, apart from slots the
    // collector freed in the original (they hold nothing code refers to).
    #[test]
    fn replaying_the_kernel_engine_gives_its_globals() {
        // Built here: the cached image may have been read back, and keep no record
        let image = Engine::build_boot_image(false, Kernel::default_sandbox());
        let booted = image.booted.virtual_machine.compiler.read();
        let kernel = booted.kernel.as_ref().unwrap();

        let record = fresh_kernel_image(Kernel::default_sandbox())
            .boot
            .clone()
            .unwrap();

        let mut engine = record.builtins.deep_clone();
        let mut compiler = kernel.engine.virtual_machine.compiler.read().clone();
        compiler.builtin_modules = engine.modules.clone();
        compiler.constant_map = compiler.constant_map.deep_clone();
        let constants = compiler.constant_map.clone();
        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler));

        let run = |engine: &mut Engine, program: &Executable| {
            let mut program = program.clone();
            program.constant_map = constants.clone();
            engine.virtual_machine.run_executable(&program).unwrap();
        };
        for program in &record.programs {
            run(&mut engine, program);
        }
        // The host functions, made in another engine and put in the slots they had
        let mut scratch = record.builtins.deep_clone();
        crate::parser::kernel::register_fresh_transformer_functions(&mut scratch);

        let mut counts = (0, 0, 0, 0);
        for step in &image.kernel_steps {
            match step {
                BootStep::Run(_) => counts.0 += 1,
                BootStep::Cleared(..) => counts.1 += 1,
                BootStep::Registered(..) => counts.2 += 1,
                BootStep::Eval(_) => counts.3 += 1,
            }
        }
        let steps: Vec<BootStep> = image
            .kernel_steps
            .iter()
            .map(|step| step.with_constants(&constants))
            .collect();
        let before = engine.virtual_machine.compiler.read().symbol_map.len();
        replay_steps(&mut engine, &steps, &|name| scratch.extract_value(name)).unwrap();
        // Nothing is compiled again
        assert_eq!(engine.virtual_machine.compiler.read().symbol_map.len(), before);
        // The transformer functions are the only values the host registers
        assert_eq!(counts.2, 5);
        assert!(counts.0 + counts.3 > 0);

        // Calls during expansion change plain globals (counters): copy those
        let original = kernel.engine.virtual_machine.global_env.roots().to_vec();
        let mut fixed = 0;
        for (i, value) in original.iter().enumerate() {
            let plain = matches!(
                value,
                SteelVal::IntV(_)
                    | SteelVal::BoolV(_)
                    | SteelVal::NumV(_)
                    | SteelVal::CharV(_)
                    | SteelVal::StringV(_)
                    | SteelVal::SymbolV(_)
            );
            let same = engine.virtual_machine.global_env.roots().get(i) == Some(value);
            if plain && !same {
                fixed += 1;
                engine.virtual_machine.insert_binding(i, value.clone());
            }
        }
        let _ = fixed;

        let replayed = engine.virtual_machine.global_env.roots();
        let kernel_names = kernel.engine.virtual_machine.compiler.read().symbol_map.values().clone();
        let mut differ = Vec::new();
        for i in 0..original.len().max(replayed.len()) {
            let booted_value = original.get(i);
            if booted_value.is_some_and(crate::env::is_undefined_global) {
                continue;
            }
            let a = booted_value.map(describe).unwrap_or_else(|| "<none>".into());
            let b = replayed.get(i).map(describe).unwrap_or_else(|| "<none>".into());
            if a != b {
                differ.push(format!(
                    "slot {i} {:?}: booted {a:.200} / replayed {b:.200}",
                    kernel_names.get(i).map(|x| x.resolve())
                ));
            }
        }
        assert!(differ.is_empty(), "{}", differ.join("\n"));
    }
}
