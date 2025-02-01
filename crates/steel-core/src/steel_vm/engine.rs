#![allow(unused)]

use super::{
    builtin::{BuiltInModule, FunctionSignatureMetadata},
    primitives::{register_builtin_modules, CONSTANTS},
    vm::{SteelThread, Synchronizer, ThreadStateController},
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
            intern_modules, path_to_module_name, CompiledModule, MANGLER_PREFIX,
            PRELUDE_WITHOUT_BASE,
        },
        program::{
            number_literal_to_steel, Executable, RawProgramWithSymbols,
            SerializableRawProgramWithSymbols,
        },
    },
    containers::RegisterValue,
    core::{
        instructions::{pretty_print_dense_instructions, DenseInstruction, Instruction},
        labels::Expr,
    },
    gc::{
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
    steel_vm::register_fn::RegisterFn,
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
    path::PathBuf,
    rc::Rc,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use crate::values::HashMap as ImmutableHashMap;
use fxhash::{FxBuildHasher, FxHashMap};
use lasso::ThreadedRodeo;
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use serde::{Deserialize, Serialize};
use steel_gen::OpCode;
use steel_parser::{
    parser::{SourceId, SyntaxObject},
    tokens::{IntLiteral, TokenType},
};

use crate::parser::ast::IteratorExtensions;

thread_local! {
    static KERNEL_BIN_FILE: Cell<Option<&'static [u8]>> = Cell::new(None);
}

// Install the binary file to be used during bootup
// pub fn install_bin_file(bin: &'static [u8]) {
//     KERNEL_BIN_FILE.with(|x| x.set(Some(bin)));
// }

#[cfg(not(feature = "sync"))]
pub trait ModuleResolver {
    fn resolve(&self, name: &str) -> Option<BuiltInModule>;
}

#[cfg(feature = "sync")]
pub trait ModuleResolver: MaybeSendSyncStatic {
    fn resolve(&self, name: &str) -> Option<BuiltInModule>;
}

#[derive(Clone, Default)]
pub struct ModuleContainer {
    modules: Arc<RwLock<HashMap<Shared<str>, BuiltInModule>>>,
    // For modules that don't exist in memory. This could be useful for a world
    // in which a builtin module exists BUT we'd like to resolve the module for
    // inference purposes.
    unresolved_modules: Arc<RwLock<Option<Shared<dyn ModuleResolver>>>>,
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
        self.modules.read().get(key).cloned().or_else(|| {
            self.unresolved_modules
                .read()
                .as_ref()
                .and_then(|x| x.resolve(key))
        })
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
        *self.unresolved_modules.write() = Some(Shared::new(resolver));
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
    // pub(crate) compiler: Arc<RwLock<Compiler>>,
    modules: ModuleContainer,
    sources: Sources,
    #[cfg(feature = "dylibs")]
    dylibs: DylibContainers,
    pub(crate) id: EngineId,
}

impl Engine {
    pub fn enter_safepoint<T, F: FnMut() -> T>(&mut self, mut thunk: F) -> T {
        let mut res = None;

        self.virtual_machine.enter_safepoint(|_| {
            res = Some((thunk)());
            Ok(SteelVal::Void)
        });

        res.unwrap()
    }
}

impl Clone for Engine {
    fn clone(&self) -> Self {
        let mut virtual_machine = self.virtual_machine.clone();

        virtual_machine.synchronizer = Synchronizer::new();

        let compiler = Arc::new(RwLock::new(self.virtual_machine.compiler.write().clone()));

        // virtual_machine.compiler = Some(Arc::downgrade(&compiler));

        Self {
            virtual_machine,
            // compiler,
            modules: self.modules.clone(),
            sources: self.sources.clone(),
            #[cfg(feature = "dylibs")]
            dylibs: self.dylibs.clone(),
            id: EngineId::new(),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

// Pre-parsed ASTs along with the global state to set before we start any further processing
#[derive(Serialize, Deserialize)]
struct BootstrapImage {
    interner: Arc<ThreadedRodeo<lasso::Spur, FxBuildHasher>>,
    syntax_object_id: usize,
    sources: Sources,
    programs: Vec<Vec<ExprKind>>,
}

// Pre compiled programs along with the global state to set before we start any further processing
#[derive(Serialize, Deserialize)]
struct StartupBootstrapImage {
    syntax_object_id: usize,
    function_id: usize,
    sources: Sources,
    pre_kernel_programs: Vec<SerializableRawProgramWithSymbols>,
    post_kernel_programs: Vec<SerializableRawProgramWithSymbols>,
    kernel: Option<KernelImage>,
    compiler: Option<SerializableCompiler>,
}

#[derive(Serialize, Deserialize)]
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
    pub fn write_bytes_to_file(&self, out: &PathBuf) {
        let mut f = std::fs::File::create(out).unwrap();
        bincode::serialize_into(&mut f, self).unwrap();
    }

    pub fn as_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        bincode::serialize_into(&mut out, self).unwrap();
        out
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        bincode::deserialize(&bytes).unwrap()
    }
}

// fn steel_create_bootstrap() {
//     Engine::create_bootstrap_from_programs("src/boot/bootstrap.bin".into());
// }

pub struct LifetimeGuard<'a> {
    engine: &'a mut Engine,
}

impl<'a> Drop for LifetimeGuard<'a> {
    fn drop(&mut self) {
        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::free_all();
    }
}

impl<'a> LifetimeGuard<'a> {
    pub fn with_immutable_reference<
        'b: 'a,
        T: CustomReference + 'b,
        EXT: CustomReference + 'static,
    >(
        self,
        obj: &'a T,
    ) -> Self
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            std::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_ro_object::<T, EXT>(
            obj,
        );

        self
    }

    pub fn with_mut_reference<'b: 'a, T: CustomReference + 'b, EXT: CustomReference + 'static>(
        self,
        obj: &'a mut T,
    ) -> Self
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            std::any::TypeId::of::<EXT>()
        );
        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_rw_object::<T, EXT>(
            obj,
        );

        self
    }

    pub fn consume<T>(self, mut thunk: impl FnMut(&mut Engine, Vec<SteelVal>) -> T) -> T {
        let values =
            crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::drain_weak_references_to_steelvals();

        thunk(self.engine, values)
    }

    pub fn consume_once<T>(self, mut thunk: impl FnOnce(&mut Engine, Vec<SteelVal>) -> T) -> T {
        let values =
            crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::drain_weak_references_to_steelvals();

        thunk(self.engine, values)
    }
}

impl RegisterValue for Engine {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        let idx = self.virtual_machine.compiler.write().register(name);
        self.virtual_machine.insert_binding(idx, value);
        self
    }
}

#[steel_derive::function(name = "#%get-dylib")]
pub fn load_module_noop(target: &crate::rvals::SteelString) -> crate::rvals::Result<SteelVal> {
    stop!(Generic => "This engine has not been given the capability to load dylibs")
}

macro_rules! time {
    ($target:expr, $label:expr, $e:expr) => {{
        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let e = $e;

        #[cfg(feature = "profiling")]
        log::debug!(target: $target, "{}: {:?}", $label, now.elapsed());

        e
    }};
}

static STATIC_DEFAULT_PRELUDE_MACROS: OnceCell<FxHashMap<InternedString, SteelMacro>> =
    OnceCell::new();

static STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX: OnceCell<FxHashMap<InternedString, SteelMacro>> =
    OnceCell::new();

pub(crate) fn set_default_prelude_macros(
    prelude_macros: FxHashMap<InternedString, SteelMacro>,
    sandbox: bool,
) {
    if cfg!(feature = "sync") {
        if sandbox {
            STATIC_DEFAULT_PRELUDE_MACROS_SANDBOX
                .set(prelude_macros)
                .unwrap();
        } else {
            STATIC_DEFAULT_PRELUDE_MACROS.set(prelude_macros).unwrap();
        }
    } else {
        DEFAULT_PRELUDE_MACROS.with(|x| {
            let mut guard = x.borrow_mut();
            *guard = prelude_macros;
        })
    }
}

pub(crate) fn default_prelude_macros() -> FxHashMap<InternedString, SteelMacro> {
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
    pub(crate) static DEFAULT_PRELUDE_MACROS: RefCell<FxHashMap<InternedString, SteelMacro>> = RefCell::new(HashMap::default());
}

impl Engine {
    pub fn engine_id(&self) -> EngineId {
        self.virtual_machine.id
    }

    #[cfg(not(feature = "sync"))]
    pub(crate) fn deep_clone(&self) -> Self {
        let mut engine = self.clone();

        let compiler_copy = engine.virtual_machine.compiler.read().clone();
        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler_copy));

        let constant_map = engine
            .virtual_machine
            .compiler
            .read()
            .constant_map
            .deep_clone();
        engine.virtual_machine.compiler.write().constant_map = constant_map;

        let heap_copy = Arc::new(Mutex::new(
            engine.virtual_machine.heap.lock().unwrap().clone(),
        ));

        engine.virtual_machine.heap = heap_copy;
        engine
    }

    #[cfg(feature = "sync")]
    pub(crate) fn deep_clone(&self) -> Self {
        let mut engine = self.clone();
        engine.virtual_machine.global_env = engine.virtual_machine.global_env.deep_clone();

        let mut compiler_copy = engine.virtual_machine.compiler.read().clone();

        engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler_copy));

        let constant_map = engine
            .virtual_machine
            .compiler
            .read()
            .constant_map
            .deep_clone();
        engine.virtual_machine.compiler.write().constant_map = constant_map;

        let heap_copy = Arc::new(Mutex::new(
            engine.virtual_machine.heap.lock().unwrap().clone(),
        ));

        engine.virtual_machine.heap = heap_copy;
        engine
    }

    /// Function to access a kernel level execution environment
    /// Has access to primitives and syntax rules, but will not defer to a child
    /// kernel in the compiler
    pub(crate) fn new_kernel(sandbox: bool) -> Self {
        log::debug!(target:"kernel", "Instantiating a new kernel");
        #[cfg(feature = "profiling")]
        let mut total_time = std::time::Instant::now();
        #[cfg(feature = "profiling")]
        let mut now = std::time::Instant::now();
        let sources = Sources::new();
        let modules = ModuleContainer::with_expected_capacity();

        let compiler = Arc::new(RwLock::new(Compiler::default_without_kernel(
            sources.clone(),
            modules.clone(),
        )));

        let mut vm = Engine {
            virtual_machine: SteelThread::new(sources.clone(), compiler),
            modules,
            sources,
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
            id: EngineId::new(),
        };

        time!(
            "engine-creation",
            "Registering builtin modules",
            register_builtin_modules(&mut vm, sandbox)
        );

        time!(
            "engine-creation",
            "Loading the ALL_MODULES prelude code",
            vm.compile_and_run_raw_program(crate::steel_vm::primitives::ALL_MODULES)
                .expect("loading ALL_MODULES failed")
        );

        // log::debug!(target: "kernel", "Registered modules in the kernel!: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        let mut now = std::time::Instant::now();

        let core_libraries = [crate::stdlib::PRELUDE];

        for core in core_libraries.into_iter() {
            if let Err(e) = vm.compile_and_run_raw_program(core) {
                vm.raise_error(e);
                panic!("Loading the standard library failed");
            }
        }

        // Initialize the global macro environment with the default one. This way
        // values won't leak when top level macros are defined - and modules can clone from
        // this to begin seeding their environment.

        set_default_prelude_macros(vm.in_scope_macros().clone(), sandbox);

        #[cfg(feature = "profiling")]
        log::debug!(target: "kernel", "Loaded prelude in the kernel!: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Total kernel loading time: {:?}", total_time.elapsed());

        vm
    }

    /// Register a module resolver. This is used for creating references to modules
    /// that don't exist within the compiler. Without this, you wouldn't be able to
    /// pre-compile or analyze code that is run within another host application, without
    /// exposing some kind of compiler from that hosts runtime. The requirement then
    /// is to expose some kind of module artifact that we can then consume.
    pub fn register_module_resolver<T: ModuleResolver + 'static>(&mut self, resolver: T) {
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

            let sources = vm.sources.clone();

            vm.register_fn("report-error!", move |error: SteelErr| {
                raise_error(&sources, error);
            });

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

        let sources = vm.sources.clone();

        vm.register_fn("report-error!", move |error: SteelErr| {
            raise_error(&sources, error);
        });

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
                sources: engine.sources.clone(),
                program,
            })
    }

    // Execute from a statically linked non interactive program
    pub fn execute_non_interactive_program_image(program: &'static [u8]) -> Result<()> {
        // This _has_ to match the as the creation of the program above.
        // So, engine first, then non interactive program.
        let mut engine = Engine::new();
        let program = crate::steel_vm::engine::NonInteractiveProgramImage::from_bytes(program);

        engine.sources = program.sources;

        // TODO: The constant map needs to be brought back as well. Install it here.
        // it needs to get installed in the VM and the compiler. Lets just try that now.

        let raw_program = SerializableRawProgramWithSymbols::into_raw_program(program.program);

        engine.virtual_machine.constant_map = raw_program.constant_map.clone();
        engine.virtual_machine.compiler.write().constant_map = raw_program.constant_map.clone();

        let results = engine.run_raw_program(raw_program);

        if let Err(e) = results {
            raise_error(&engine.sources, e);
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
    //             .collect::<std::result::Result<_, _>>()
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
        let sources = Sources::new();
        let modules = ModuleContainer::default();

        let compiler = Arc::new(RwLock::new(Compiler::default_with_kernel(
            sources.clone(),
            modules.clone(),
        )));

        Engine {
            virtual_machine: SteelThread::new(sources.clone(), compiler),
            modules,
            sources,
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
            id: EngineId::new(),
        }
    }

    pub fn report_engine_stats(&self) -> EngineStatistics {
        EngineStatistics {
            rooted_count: self.globals().len(),
            constants_count: self.virtual_machine.compiler.read().constant_map.len(),
            sources_size: self.sources.size_in_bytes(),
        }
    }

    /// Registers a steel module
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

    /// Turn contracts on in the VM
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.virtual_machine.with_contracts(contracts);
        self
    }

    #[inline]
    pub fn new_sandboxed() -> Self {
        let mut engine = fresh_kernel_image(true);

        engine.virtual_machine.compiler.write().kernel = Some(Kernel::new());

        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        if let Err(e) = engine.run(PRELUDE_WITHOUT_BASE) {
            raise_error(&engine.sources, e);
            panic!("This shouldn't happen!");
        }

        #[cfg(feature = "profiling")]
        log::info!(target: "engine-creation", "Engine Creation: {:?}", now.elapsed());

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
        T: CustomReference + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a T,
    ) -> LifetimeGuard<'a>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            std::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_ro_object::<T, EXT>(
            obj,
        );

        LifetimeGuard { engine: self }
    }

    pub fn with_mut_reference<'a, 'b: 'a, T: CustomReference + 'b, EXT: CustomReference + 'static>(
        &'a mut self,
        obj: &'a mut T,
    ) -> LifetimeGuard<'a>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        assert_eq!(
            crate::gc::unsafe_erased_pointers::type_id::<T>(),
            std::any::TypeId::of::<EXT>()
        );

        crate::gc::unsafe_erased_pointers::OpaqueReferenceNursery::allocate_rw_object::<T, EXT>(
            obj,
        );

        LifetimeGuard { engine: self }
    }

    // Tie the lifetime of this object to the scope of this execution
    pub fn run_with_reference<'a, 'b: 'a, T: CustomReference + 'b, EXT: CustomReference + 'static>(
        &'a mut self,
        obj: &'a mut T,
        bind_to: &'a str,
        script: &'a str,
    ) -> Result<SteelVal>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
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
        T: CustomReference + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
        bind_to: &'a str,
        script: &'a str,
        path: PathBuf,
    ) -> Result<SteelVal>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
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
        T: CustomReference + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a mut T,
        mut thunk: impl FnMut(&mut Engine, SteelVal) -> Result<SteelVal>,
    ) -> Result<SteelVal>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        self.with_mut_reference(obj).consume(|engine, args| {
            let mut args = args.into_iter();

            thunk(engine, args.into_iter().next().unwrap_or(SteelVal::Void))
        })
    }

    pub fn run_thunk_with_ro_reference<
        'a,
        'b: 'a,
        T: CustomReference + 'b,
        EXT: CustomReference + 'static,
    >(
        &'a mut self,
        obj: &'a T,
        mut thunk: impl FnMut(&mut Engine, SteelVal) -> Result<SteelVal>,
    ) -> Result<SteelVal>
    where
        T: ReferenceMarker<'b, Static = EXT>,
    {
        self.with_immutable_reference(obj).consume(|engine, args| {
            let mut args = args.into_iter();

            thunk(engine, args.into_iter().next().unwrap_or(SteelVal::Void))
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
        let mut engine = fresh_kernel_image(false);

        {
            engine.virtual_machine.compiler.write().kernel = Some(Kernel::new());
        }

        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        if let Err(e) = engine.run(PRELUDE_WITHOUT_BASE) {
            raise_error(&engine.sources, e);
            panic!("This shouldn't happen!");
        }

        #[cfg(feature = "profiling")]
        log::info!(target: "engine-creation", "Engine Creation: {:?}", now.elapsed());

        engine
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

    // Registers the given module into the virtual machine
    pub fn register_module(&mut self, module: BuiltInModule) -> &mut Self {
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
        let external_module = FFIWrappedModule::new(module)?.build();

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
        self.virtual_machine
            .compiler
            .write()
            .compile_executable(expr, None)
    }

    pub fn emit_raw_program<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr: E,
        path: PathBuf,
    ) -> Result<RawProgramWithSymbols> {
        self.virtual_machine
            .compiler
            .write()
            .compile_executable(expr, Some(path))
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
                TokenType::Number(n) => number_literal_to_steel(n),
                TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.clone().into())),
                TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
                // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
                TokenType::Keyword(k) => Ok(SteelVal::SymbolV(k.clone().into())),
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
        self.virtual_machine
            .compiler
            .write()
            .fully_expand_to_file(exprs, Some(path))
            .unwrap();
    }

    pub fn load_from_expanded_file(&mut self, path: &str) {
        let program = self
            .virtual_machine
            .compiler
            .write()
            .load_from_file(path)
            .unwrap();

        self.run_raw_program(program).unwrap();
    }

    // TODO -> clean up this API a lot
    pub fn compile_and_run_raw_program_with_path<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        exprs: E,
        path: PathBuf,
    ) -> Result<Vec<SteelVal>> {
        let program = self
            .virtual_machine
            .compiler
            .write()
            .compile_executable(exprs, Some(path))?;

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
        let program = self
            .virtual_machine
            .compiler
            .write()
            .compile_executable(exprs, None)?;

        self.run_raw_program(program)
    }

    pub fn raw_program_to_executable(
        &mut self,
        program: RawProgramWithSymbols,
    ) -> Result<Executable> {
        let symbol_map_offset = self.virtual_machine.compiler.read().symbol_map.len();

        let result = program.build(
            "TestProgram".to_string(),
            &mut self.virtual_machine.compiler.write().symbol_map,
        );

        // Revisit if we need to do this at all?
        if result.is_err() {
            self.virtual_machine
                .compiler
                .write()
                .symbol_map
                .roll_back(symbol_map_offset);
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

        #[cfg(feature = "sync")]
        {
            self.virtual_machine
                .global_env
                .bindings_vec
                .write()
                .unwrap()
                .truncate(checkpoint.globals_offset);
        }

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

        // Unfortunately, we have to invoke a whole GC algorithm here
        // for shadowed rooted values
        if self
            .virtual_machine
            .compiler
            .write()
            .symbol_map
            .free_list
            .should_collect()
        {
            #[cfg(feature = "sync")]
            {
                GlobalSlotRecycler::free_shadowed_rooted_values(
                    &mut self
                        .virtual_machine
                        .global_env
                        .bindings_vec
                        .write()
                        .unwrap(),
                    &mut self.virtual_machine.compiler.write().symbol_map,
                    &mut self.virtual_machine.heap.lock().unwrap(),
                );
            }

            #[cfg(not(feature = "sync"))]
            {
                GlobalSlotRecycler::free_shadowed_rooted_values(
                    &mut self.virtual_machine.global_env.bindings_vec,
                    &mut self.virtual_machine.compiler.write().symbol_map,
                    &mut self.virtual_machine.heap.lock().unwrap(),
                );
            }

            // Drop the pure functions which have been lifted.
            self.virtual_machine
                .function_interner
                .pure_function_interner
                .retain(|key, value| crate::gc::Gc::strong_count(value) > 1);

            // TODO:
            // It is also necessary to keep track of the values to drop
            // for closures - this can be done by tracking the closure id
            // throughout the generated bytecode. Same way we keep track of
            // the referenced globals; we track the referenced closures.
            // FIXME: Add that code here

            self.virtual_machine
                .compiler
                .write()
                .symbol_map
                .free_list
                .increment_generation();
        }

        self.virtual_machine.run_executable(&executable)
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
        self.virtual_machine
            .compiler
            .write()
            .emit_expanded_ast(expr, path)
    }

    pub fn emit_expanded_ast_without_optimizations(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.virtual_machine
            .compiler
            .write()
            .emit_expanded_ast_without_optimizations(expr, path)
    }

    /// Emit the unexpanded AST
    pub fn emit_ast_to_string(expr: &str) -> Result<String> {
        let parsed = Self::emit_ast(expr)?;
        Ok(parsed.into_iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }

    pub fn emit_ast(expr: &str) -> Result<Vec<ExprKind>> {
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr, SourceId::none()).collect();
        Ok(parsed?)
    }

    /// Emit the fully expanded AST as a pretty printed string
    pub fn emit_fully_expanded_ast_to_string(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<String> {
        Ok(self
            .virtual_machine
            .compiler
            .write()
            .emit_expanded_ast(expr, path)?
            .into_iter()
            .map(|x| x.to_pretty(60))
            .join("\n\n"))
    }

    /// Emits the fully expanded AST directly.
    pub fn emit_fully_expanded_ast(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.virtual_machine
            .compiler
            .write()
            .emit_expanded_ast(expr, path)
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

    pub fn update_value(&mut self, name: &str, value: SteelVal) -> Option<&mut Self> {
        let idx = self.virtual_machine.compiler.read().get_idx(name)?;
        self.virtual_machine.global_env.repl_set_idx(idx, value);
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
    /// use steel::steel_vm::register_fn::RegisterFn;
    /// fn foo() -> usize {
    ///    10
    /// }
    ///
    /// let mut vm = Engine::new();
    /// vm.register_fn("foo", foo);
    ///
    /// vm.run(r#"(foo)"#).unwrap(); // Returns vec![10]
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
        raise_error(&self.sources, error)
    }

    /// Emit an error string reporing, the back trace.
    pub fn raise_error_to_string(&self, error: SteelErr) -> Option<String> {
        raise_error_to_string(&self.sources, error)
    }

    /// Execute a program given as the `expr`, and computes a `Vec<SteelVal>` corresponding to the output of each expression given.
    /// This method contains no path information used for error reporting, and simply runs the expression as is. Modules will be
    /// imported with the root directory as wherever the executable was started.
    /// Any parsing, compilation, or runtime error will be reflected here, ideally with span information as well. The error will not
    /// be reported automatically.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate steel;
    /// # use steel::steel_vm::engine::Engine;
    /// use steel::rvals::SteelVal;
    /// let mut vm = Engine::new();
    /// let output = vm.run("(+ 1 2) (* 5 5) (- 10 5)").unwrap();
    /// assert_eq!(output, vec![SteelVal::IntV(3), SteelVal::IntV(25), SteelVal::IntV(5)]);
    /// ```
    // pub fn run(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, None, constants)?;
    //     self.virtual_machine.execute_program(program)
    // }

    /// Execute a program, however do not run any callbacks as registered with `on_progress`.
    // pub fn run_without_callbacks(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, None, constants)?;
    //     self.virtual_machine
    //         .execute_program::<DoNotUseCallback, ApplyContract>(program)
    // }

    pub fn add_module(&mut self, path: String) -> Result<()> {
        self.virtual_machine.compiler.write().compile_module(
            path.into(),
            &mut self.sources,
            self.modules.clone(),
        )
    }

    pub fn modules(&self) -> MappedRwLockReadGuard<'_, FxHashMap<PathBuf, CompiledModule>> {
        RwLockReadGuard::map(self.virtual_machine.compiler.read(), |x| x.modules())
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

    pub fn get_source_id(&self, path: &PathBuf) -> Option<SourceId> {
        self.sources.get_source_id(path)
    }

    pub fn get_path_for_source_id(&self, source_id: &SourceId) -> Option<PathBuf> {
        self.sources.get_path(source_id)
    }

    pub fn get_source(&self, source_id: &SourceId) -> Option<Cow<'static, str>> {
        self.sources
            .sources
            .lock()
            .unwrap()
            .get(*source_id)
            .cloned()
    }
}

// #[cfg(test)]
// mod on_progress_tests {
//     use super::*;
//     use std::cell::Cell;
//     use std::rc::Rc;

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
            let sources = sources.sources.lock().unwrap();

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

                                    back_trace(&resolved_file_name, &source, *span);
                                }
                            }
                        }
                    }
                }

                let resolved_file_name = file_name.unwrap_or_default();

                error.emit_result(resolved_file_name.to_str().unwrap(), &file_content);
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
            let sources = sources.sources.lock().unwrap();

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
                                        back_trace_to_string(&resolved_file_name, &source, *span);
                                    back_traces.push(bt);
                                }
                            }
                        }
                    }
                }

                let resolved_file_name = file_name.unwrap_or_default();

                let final_error = error
                    .emit_result_to_string(resolved_file_name.to_str().unwrap(), &file_content);

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
}

#[cfg(test)]
mod derive_macro_tests {
    use super::*;

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
