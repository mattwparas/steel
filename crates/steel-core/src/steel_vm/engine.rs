#![allow(unused)]

use super::{
    builtin::BuiltInModule,
    primitives::{register_builtin_modules, register_builtin_modules_without_io, CONSTANTS},
    vm::SteelThread,
};

#[cfg(feature = "dylibs")]
use super::{ffi::FFIModule, ffi::FFIWrappedModule};

#[cfg(feature = "dylibs")]
use super::dylib::DylibContainers;

use crate::{
    compiler::{
        compiler::{Compiler, SerializableCompiler},
        modules::{CompiledModule, PRELUDE_WITHOUT_BASE},
        program::{Executable, RawProgramWithSymbols, SerializableRawProgramWithSymbols},
    },
    containers::RegisterValue,
    core::{instructions::Instruction, labels::Expr},
    gc::unsafe_erased_pointers::{
        BorrowedObject, CustomReference, OpaqueReferenceNursery, ReadOnlyBorrowedObject,
        ReferenceMarker,
    },
    parser::{
        ast::ExprKind,
        expander::SteelMacro,
        interner::{get_interner, take_interner, InternedString},
        parser::SYNTAX_OBJECT_ID,
    },
    parser::{
        kernel::{fresh_kernel_image, Kernel},
        parser::{ParseError, Parser, Sources},
    },
    rerrs::{back_trace, back_trace_to_string},
    rvals::{
        cycles::{install_printer, print_in_engine, PRINT_IN_ENGINE_DEFINITION},
        FromSteelVal, IntoSteelVal, Result, SteelVal,
    },
    steel_vm::register_fn::RegisterFn,
    stop, throw,
    values::functions::BoxedDynFunction,
    SteelErr,
};
use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};

use im_rc::HashMap as ImmutableHashMap;
use lasso::ThreadedRodeo;
use serde::{Deserialize, Serialize};
use steel_gen::OpCode;
use steel_parser::{
    parser::SyntaxObject,
    tokens::{MaybeBigInt, TokenType},
};

use crate::parser::ast::IteratorExtensions;

thread_local! {
    static KERNEL_BIN_FILE: Cell<Option<&'static [u8]>> = Cell::new(None);
}

// Install the binary file to be used during bootup
// pub fn install_bin_file(bin: &'static [u8]) {
//     KERNEL_BIN_FILE.with(|x| x.set(Some(bin)));
// }

#[derive(Clone, Default)]
pub struct ModuleContainer {
    modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
    // Modules that... might eventually be a dynamic library
    maybe_module: HashSet<String>,
}

impl ModuleContainer {
    pub fn insert(&mut self, key: Rc<str>, value: BuiltInModule) {
        self.modules.insert(key, value);
    }

    pub fn get(&mut self, key: &str) -> Option<BuiltInModule> {
        self.modules.get(key).cloned()
    }

    pub fn inner(&self) -> &ImmutableHashMap<Rc<str>, BuiltInModule> {
        &self.modules
    }

    pub(crate) fn inner_mut(&mut self) -> &mut ImmutableHashMap<Rc<str>, BuiltInModule> {
        &mut self.modules
    }
}

#[derive(Debug)]
pub struct EngineStatistics {
    pub rooted_count: usize,
    pub constants_count: usize,
    pub sources_size: usize,
}

#[derive(Clone)]
pub struct Engine {
    virtual_machine: SteelThread,
    compiler: Compiler,
    constants: Option<ImmutableHashMap<InternedString, SteelVal>>,
    modules: ModuleContainer,
    sources: Sources,
    #[cfg(feature = "dylibs")]
    dylibs: DylibContainers,
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

// Pre-parsed ASTs along with the global state to set before we start any further processing
#[derive(Serialize, Deserialize)]
struct BootstrapImage {
    interner: Arc<ThreadedRodeo>,
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
        let idx = self.compiler.register(name);
        self.virtual_machine.insert_binding(idx, value);
        self
    }
}

#[steel_derive::function(name = "#%get-dylib")]
fn load_module_noop(target: &crate::rvals::SteelString) -> crate::rvals::Result<SteelVal> {
    stop!(Generic => "This engine has not been given the capability to load dylibs")
}

macro_rules! time {
    ($target:expr, $label:expr, $e:expr) => {{
        let now = std::time::Instant::now();

        let e = $e;

        log::debug!(target: $target, "{}: {:?}", $label, now.elapsed());

        e
    }};
}

impl Engine {
    /// Function to access a kernel level execution environment
    /// Has access to primitives and syntax rules, but will not defer to a child
    /// kernel in the compiler
    pub(crate) fn new_kernel() -> Self {
        log::debug!(target:"kernel", "Instantiating a new kernel");
        let mut now = std::time::Instant::now();

        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        time!(
            "engine-creation",
            "Registering builtin modules",
            register_builtin_modules(&mut vm)
        );

        time!(
            "engine-creation",
            "Loading the all modules prelude code",
            vm.compile_and_run_raw_program(crate::steel_vm::primitives::ALL_MODULES)
                .unwrap()
        );

        log::debug!(target: "kernel", "Registered modules in the kernel!: {:?}", now.elapsed());

        let mut now = std::time::Instant::now();

        let core_libraries = [crate::stdlib::PRELUDE];

        for core in core_libraries.into_iter() {
            vm.compile_and_run_raw_program(core).unwrap();
        }

        log::debug!(target: "kernel", "Loaded prelude in the kernel!: {:?}", now.elapsed());

        vm
    }

    pub fn builtin_modules(&self) -> &ModuleContainer {
        &self.modules
    }

    #[doc(hidden)]
    pub fn disallow_dylib_loading(&mut self) -> &mut Self {
        // This isn't amazing
        let module = self.modules.inner_mut();

        if let Some(builtin_module) = module.get_mut("steel/meta") {
            builtin_module.register_native_fn_definition(LOAD_MODULE_NOOP_DEFINITION);
        }

        self
    }

    /// Function to access a kernel level execution environment
    /// Has access to primitives and syntax rules, but will not defer to a child
    /// kernel in the compiler
    pub(crate) fn new_bootstrap_kernel() -> Self {
        // if !install_drop_handler() {
        //     panic!("Unable to install the drop handler!");
        // }

        // If the interner has already been initialized, it most likely means that either:
        // 1) Tests are being run
        // 2) The parser was used in a standalone fashion, somewhere, which invalidates the bootstrap
        //    process
        //
        // There are a few solutions to this - one would probably be to not use a static interner,
        // however given that its a huge chore to pass around the interner everywhere there are strings,
        // its probably inevitable we have that.
        if get_interner().is_some() {
            return Engine::new_kernel();
        }

        if matches!(option_env!("STEEL_BOOTSTRAP"), Some("false") | None) {
            let mut vm = Engine::new_kernel();

            let sources = vm.sources.clone();

            vm.register_fn("report-error!", move |error: SteelErr| {
                raise_error(&sources, error);
            });

            return vm;
        }

        log::debug!(target:"kernel", "Instantiating a new kernel");

        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        if let Some(programs) = Engine::load_from_bootstrap(&mut vm) {
            register_builtin_modules(&mut vm);

            for program in programs {
                vm.compiler.constant_map = program.constant_map.clone();
                vm.virtual_machine.constant_map = program.constant_map.clone();

                vm.run_raw_program(program).unwrap();
            }

            log::debug!(target: "kernel", "Loaded prelude in the kernel!");

            let sources = vm.sources.clone();

            vm.register_fn("report-error!", move |error: SteelErr| {
                raise_error(&sources, error);
            });

            vm
        } else {
            let mut vm = Engine::new_kernel();

            let sources = vm.sources.clone();

            vm.register_fn("report-error!", move |error: SteelErr| {
                raise_error(&sources, error);
            });

            vm
        }
    }

    // fn load_from_bootstrap(vm: &mut Engine) -> Option<Vec<Vec<ExprKind>>> {
    //     let bootstrap: BootstrapImage =
    //         bincode::deserialize(include_bytes!("../boot/bootstrap.bin")).unwrap();

    //     // Set the syntax object id to be AFTER the previous items have been parsed
    //     SYNTAX_OBJECT_ID.store(
    //         bootstrap.syntax_object_id,
    //         std::sync::atomic::Ordering::Relaxed,
    //     );

    //     // Set up the interner to have this latest state
    //     if crate::parser::interner::initialize_with(bootstrap.interner).is_err() {
    //         return None;
    //     }

    //     vm.sources = bootstrap.sources;

    //     Some(bootstrap.programs)
    // }

    fn load_from_bootstrap(vm: &mut Engine) -> Option<Vec<RawProgramWithSymbols>> {
        if matches!(option_env!("STEEL_BOOTSTRAP"), Some("false") | None) {
            return None;
        } else {
            println!("LOADING A KERNEL FROM THE BIN FILE");
        }

        let bootstrap: StartupBootstrapImage =
            bincode::deserialize(KERNEL_BIN_FILE.with(|x| x.get())?).unwrap();

        // Set the syntax object id to be AFTER the previous items have been parsed
        SYNTAX_OBJECT_ID.store(
            bootstrap.syntax_object_id,
            std::sync::atomic::Ordering::Relaxed,
        );

        crate::compiler::code_gen::FUNCTION_ID
            .store(bootstrap.function_id, std::sync::atomic::Ordering::Relaxed);

        vm.sources = bootstrap.sources;
        // vm.compiler.macro_env = bootstrap.macros;

        todo!();

        Some(
            bootstrap
                .pre_kernel_programs
                .into_iter()
                .map(SerializableRawProgramWithSymbols::into_raw_program)
                .collect(),
        )
    }

    /// Creates a statically linked program ready to deserialize
    pub fn create_non_interactive_program_image(
        expr: &str,
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
    pub fn execute_non_interactive_program_image(
        program: NonInteractiveProgramImage,
    ) -> Result<()> {
        // This _has_ to match the as the creation of the program above
        let mut engine = Engine::new();
        engine.sources = program.sources;
        let raw_program = SerializableRawProgramWithSymbols::into_raw_program(program.program);
        let results = engine.run_raw_program(raw_program);

        if let Err(e) = results {
            raise_error(&engine.sources, e);
        }

        Ok(())
    }

    // Create kernel bootstrap
    pub fn create_kernel_bootstrap_from_programs(output_path: PathBuf) {
        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        register_builtin_modules(&mut vm);

        let mut programs = Vec::new();

        let bootstrap_sources = [
            crate::steel_vm::primitives::ALL_MODULES,
            crate::stdlib::PRELUDE,
        ];

        for source in bootstrap_sources {
            let raw_program = vm.emit_raw_program_no_path(source).unwrap();
            programs.push(raw_program.clone());
            vm.run_raw_program(raw_program).unwrap();
        }

        // Grab the last value of the offset
        let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);
        let function_id =
            crate::compiler::code_gen::FUNCTION_ID.load(std::sync::atomic::Ordering::Relaxed);

        let bootstrap = StartupBootstrapImage {
            syntax_object_id,
            function_id,
            sources: vm.sources,
            pre_kernel_programs: programs
                .into_iter()
                .map(RawProgramWithSymbols::into_serializable_program)
                .collect::<Result<_>>()
                .unwrap(),
            // macros: vm.compiler.macro_env,
            post_kernel_programs: Vec::new(),
            kernel: None,
            compiler: None,
        };

        // Encode to something implementing `Write`
        let mut f = std::fs::File::create(output_path).unwrap();
        bincode::serialize_into(&mut f, &bootstrap).unwrap();
    }

    pub fn create_new_engine_from_bootstrap(output_path: PathBuf) {
        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        register_builtin_modules(&mut vm);

        let mut pre_kernel_programs = Vec::new();

        let bootstrap_sources = [
            crate::steel_vm::primitives::ALL_MODULES,
            crate::stdlib::PRELUDE,
        ];

        for source in bootstrap_sources {
            let raw_program = vm.emit_raw_program_no_path(source).unwrap();
            pre_kernel_programs.push(raw_program.clone());
            vm.run_raw_program(raw_program).unwrap();
        }

        // This will be our new top level engine
        let mut top_level_engine = vm.clone();

        let sources = vm.sources.clone();

        vm.register_fn("report-error!", move |error: SteelErr| {
            raise_error(&sources, error);
        });

        let (kernel, kernel_program) = Kernel::bootstrap(vm);

        // Create kernel for the compiler for the top level vm
        top_level_engine.compiler.kernel = Some(kernel);

        let builtin_modules =
            ["(require \"#%private/steel/contract\" (for-syntax \"#%private/steel/contract\"))"];

        let mut post_kernel_programs = Vec::new();

        for source in builtin_modules {
            let raw_program = top_level_engine.emit_raw_program_no_path(source).unwrap();
            post_kernel_programs.push(raw_program.clone());
            top_level_engine.run_raw_program(raw_program).unwrap();
        }

        // Grab the last value of the offset
        let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);
        let function_id =
            crate::compiler::code_gen::FUNCTION_ID.load(std::sync::atomic::Ordering::Relaxed);

        let kernel_sources = top_level_engine
            .compiler
            .kernel
            .as_ref()
            .unwrap()
            .engine
            .sources
            .clone();
        let bootstrap = StartupBootstrapImage {
            syntax_object_id,
            function_id,
            sources: top_level_engine.sources,
            pre_kernel_programs: pre_kernel_programs
                .into_iter()
                .map(RawProgramWithSymbols::into_serializable_program)
                .collect::<Result<_>>()
                .unwrap(),
            post_kernel_programs: post_kernel_programs
                .into_iter()
                .map(RawProgramWithSymbols::into_serializable_program)
                .collect::<Result<_>>()
                .unwrap(),
            kernel: Some(KernelImage {
                compiler: top_level_engine
                    .compiler
                    .kernel
                    .take()
                    .unwrap()
                    .engine
                    .compiler
                    .into_serializable_compiler()
                    .unwrap(),
                sources: kernel_sources,
                kernel_source: kernel_program.into_serializable_program().unwrap(),
            }),
            compiler: Some(
                top_level_engine
                    .compiler
                    .into_serializable_compiler()
                    .unwrap(),
            ),
        };

        // Encode to something implementing `Write`
        let mut f = std::fs::File::create(output_path).unwrap();
        bincode::serialize_into(&mut f, &bootstrap).unwrap();
    }

    pub fn top_level_load_from_bootstrap(bin: &[u8]) -> Engine {
        let bootstrap: StartupBootstrapImage = bincode::deserialize(bin).unwrap();

        // This is going to be the kernel
        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        // Register the modules
        register_builtin_modules(&mut vm);

        // Set the syntax object id to be AFTER the previous items have been parsed
        SYNTAX_OBJECT_ID.store(
            bootstrap.syntax_object_id,
            std::sync::atomic::Ordering::Relaxed,
        );

        crate::compiler::code_gen::FUNCTION_ID
            .store(bootstrap.function_id, std::sync::atomic::Ordering::Relaxed);

        let bootstrap_kernel = bootstrap.kernel.unwrap();

        vm.sources = bootstrap_kernel.sources;
        vm.compiler = bootstrap_kernel.compiler.into_compiler();

        // TODO: Only need to bring around the last constant map
        for program in bootstrap
            .pre_kernel_programs
            .into_iter()
            .map(SerializableRawProgramWithSymbols::into_raw_program)
        {
            vm.compiler.constant_map = program.constant_map.clone();
            vm.virtual_machine.constant_map = program.constant_map.clone();

            vm.run_raw_program(program).unwrap();
        }

        log::debug!(target: "kernel", "Loaded prelude in the kernel!");

        let sources = vm.sources.clone();

        vm.register_fn("report-error!", move |error: SteelErr| {
            raise_error(&sources, error);
        });

        // Now we're going to set up the top level environment
        let mut kernel = Kernel::initialize_post_bootstrap(vm.clone());

        kernel
            .engine
            .run_raw_program(bootstrap_kernel.kernel_source.into_raw_program())
            .unwrap();

        vm.sources = bootstrap.sources;
        vm.compiler = bootstrap.compiler.unwrap().into_compiler();
        vm.compiler.kernel = Some(kernel);

        for program in bootstrap
            .post_kernel_programs
            .into_iter()
            .map(SerializableRawProgramWithSymbols::into_raw_program)
        {
            vm.compiler.constant_map = program.constant_map.clone();
            vm.virtual_machine.constant_map = program.constant_map.clone();

            vm.run_raw_program(program).unwrap();
        }

        vm
    }

    fn create_bootstrap() {
        let mut vm = Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        };

        register_builtin_modules(&mut vm);

        let mut asts = Vec::new();

        let bootstrap_sources = [
            crate::steel_vm::primitives::ALL_MODULES,
            crate::stdlib::PRELUDE,
        ];

        for source in bootstrap_sources {
            let id = vm.sources.add_source(source.to_string(), None);

            // Could fail here
            let parsed: Vec<ExprKind> = Parser::new(source, Some(id))
                .collect::<std::result::Result<_, _>>()
                .unwrap();

            asts.push(parsed.clone());

            vm.run_raw_program_from_exprs(parsed).unwrap();
        }

        // Grab the last value of the offset
        let syntax_object_id = SYNTAX_OBJECT_ID.load(std::sync::atomic::Ordering::Relaxed);

        let bootstrap = BootstrapImage {
            interner: take_interner(),
            syntax_object_id,
            sources: vm.sources,
            programs: asts,
        };

        // Encode to something implementing `Write`
        let mut f = std::fs::File::create("src/boot/bootstrap.bin").unwrap();
        bincode::serialize_into(&mut f, &bootstrap).unwrap();
    }

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
        Engine {
            virtual_machine: SteelThread::new(),
            compiler: Compiler::default_with_kernel(),
            constants: None,
            modules: ModuleContainer::default(),
            sources: Sources::new(),
            #[cfg(feature = "dylibs")]
            dylibs: DylibContainers::new(),
        }
    }

    pub fn report_engine_stats(&self) -> EngineStatistics {
        EngineStatistics {
            rooted_count: self.globals().len(),
            constants_count: self.compiler.constant_map.len(),
            sources_size: self.sources.size_in_bytes(),
        }
    }

    /// Registers a steel module
    pub fn register_steel_module(&mut self, module_name: String, text: String) {
        self.compiler.register_builtin(module_name, text);
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

        register_builtin_modules(&mut vm);

        vm.compile_and_run_raw_program(crate::steel_vm::primitives::ALL_MODULES)
            .unwrap();

        // vm.dylibs.load_modules(&mut vm);

        vm
    }

    /// Turn contracts on in the VM
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.virtual_machine.with_contracts(contracts);
        self
    }

    #[inline]
    pub fn new_sandboxed() -> Self {
        let mut vm = Engine::new_raw();

        register_builtin_modules_without_io(&mut vm);

        vm.compile_and_run_raw_program(crate::steel_vm::primitives::SANDBOXED_MODULES)
            .unwrap();

        let core_libraries = [crate::stdlib::PRELUDE];

        for core in core_libraries.into_iter() {
            vm.compile_and_run_raw_program(core).unwrap();
        }

        vm
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
        self.virtual_machine
            .call_function(self.compiler.constant_map.clone(), function, arguments)
    }

    /// Call a function by name directly within the target environment
    pub fn call_function_by_name_with_args(
        &mut self,
        function: &str,
        arguments: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        self.extract_value(function).and_then(|function| {
            self.virtual_machine.call_function(
                self.compiler.constant_map.clone(),
                function,
                arguments,
            )
        })
    }

    /// Nothing fancy, just run it
    pub fn run(&mut self, input: &str) -> Result<Vec<SteelVal>> {
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
        self.with_mut_reference(obj).consume(|engine, args| {
            let mut args = args.into_iter();

            engine.register_value(bind_to, args.next().unwrap());

            let res = engine.compile_and_run_raw_program(script);

            engine.register_value(bind_to, SteelVal::Void);

            res.map(|x| x.into_iter().next().unwrap())
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

            engine.register_value(bind_to, args.next().unwrap());

            let res = engine.compile_and_run_raw_program_with_path(script, path.clone());

            engine.register_value(bind_to, SteelVal::Void);

            res.map(|x| x.into_iter().next().unwrap())
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

            thunk(engine, args.into_iter().next().unwrap())
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

            thunk(engine, args.into_iter().next().unwrap())
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
        let mut engine = fresh_kernel_image();

        engine.compiler.kernel = Some(Kernel::new());

        let now = std::time::Instant::now();

        if let Err(e) = engine.run(PRELUDE_WITHOUT_BASE) {
            raise_error(&engine.sources, e);
            panic!("This shouldn't happen!");
        }

        log::info!(target: "engine-creation", "Engine Creation: {:?}", now.elapsed());

        engine
    }

    pub(crate) fn new_printer() -> Self {
        let mut engine = fresh_kernel_image();

        engine.compiler.kernel = Some(Kernel::new());

        engine
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
            self.compile_and_run_raw_program(core)?;
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
            self.compile_and_run_raw_program(core)?;
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

    // /// Emits a program with path information embedded for error messaging.
    // pub fn emit_program_with_path(&mut self, expr: &str, path: PathBuf) -> Result<Program> {
    //     let constants = self.constants();
    //     self.compiler.compile_program(expr, Some(path), constants)
    // }

    /// Emits a program for a given `expr` directly without providing any error messaging for the path.
    // pub fn emit_program(&mut self, expr: &str) -> Result<Program> {
    //     let constants = self.constants();
    //     self.compiler.compile_program(expr, None, constants)
    // }

    pub fn emit_raw_program_no_path(&mut self, expr: &str) -> Result<RawProgramWithSymbols> {
        let constants = self.constants();
        self.compiler.compile_executable(
            expr,
            None,
            constants,
            self.modules.clone(),
            &mut self.sources,
        )
    }

    pub fn emit_raw_program(&mut self, expr: &str, path: PathBuf) -> Result<RawProgramWithSymbols> {
        let constants = self.constants();
        self.compiler.compile_executable(
            expr,
            Some(path),
            constants,
            self.modules.clone(),
            &mut self.sources,
        )
    }

    pub fn debug_print_build(
        &mut self,
        name: String,
        program: RawProgramWithSymbols,
    ) -> Result<()> {
        program.debug_build(name, &mut self.compiler.symbol_map)
    }

    pub fn globals(&self) -> &Vec<InternedString> {
        self.compiler.symbol_map.values()
    }

    // pub fn get_exported_module_functions(&self, path: PathBuf) -> impl Iterator<Item = InternedString> {

    // }

    // Attempts to disassemble the given expression into a series of bytecode dumps
    // pub fn disassemble(&mut self, expr: &str) -> Result<String> {
    //     let constants = self.constants();
    //     self.compiler
    //         .emit_debug_instructions(expr, constants)
    //         .map(|x| {
    //             x.into_iter()
    //                 .map(|i| crate::core::instructions::disassemble(&i))
    //                 .join("\n\n")
    //         })
    // }

    // pub fn execute_without_callbacks(
    //     &mut self,
    //     bytecode: Rc<[DenseInstruction]>,
    //     constant_map: &ConstantMap,
    // ) -> Result<SteelVal> {
    //     self.virtual_machine
    //         .execute::<DoNotUseCallback>(bytecode, constant_map, &[])
    // }

    /// Execute bytecode with a constant map directly.
    // pub fn execute(
    //     &mut self,
    //     bytecode: Rc<[DenseInstruction]>,
    //     constant_map: ConstantMap,
    // ) -> Result<SteelVal> {
    //     self.virtual_machine
    //         .execute(bytecode, constant_map, Rc::from([]))
    // }

    /// Emit the bytecode directly, with a path provided.
    // pub fn emit_instructions_with_path(
    //     &mut self,
    //     exprs: &str,
    //     path: PathBuf,
    // ) -> Result<Vec<Vec<DenseInstruction>>> {
    //     let constants = self.constants();
    //     self.compiler
    //         .emit_instructions(exprs, Some(path), constants)
    // }

    // /// Emit instructions directly, without a path for error messaging.
    // pub fn emit_instructions(&mut self, exprs: &str) -> Result<Vec<Vec<DenseInstruction>>> {
    //     let constants = self.constants();
    //     self.compiler.emit_instructions(exprs, None, constants)
    // }

    /// Execute a program directly, returns a vector of `SteelVal`s corresponding to each expr in the `Program`.
    // pub fn execute_program(&mut self, program: Program) -> Result<Vec<SteelVal>> {
    //     self.virtual_machine
    //         .execute_program::<UseCallback, ApplyContract>(program)
    // }

    // Generate dynamically linked files, containing all of the necessary information
    // This means - compiling all macros as well.
    fn load_raw_program(&mut self, mut program: RawProgramWithSymbols) {
        fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
            match &t.ty {
                TokenType::BooleanLiteral(b) => Ok((*b).into()),
                TokenType::NumberLiteral(n) => Ok(SteelVal::NumV(*n)),
                TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.into())),
                TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
                TokenType::IntegerLiteral(steel_parser::tokens::MaybeBigInt::Small(n)) => {
                    Ok(SteelVal::IntV(*n))
                }
                TokenType::IntegerLiteral(MaybeBigInt::Big(b)) => b.clone().into_steelval(),
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
            for i in 0..expr.len() {
                let instruction = &mut expr[i];

                match instruction {
                    Instruction {
                        op_code: OpCode::PUSHCONST,
                        contents: Some(Expr::Atom(constant_value)),
                        ..
                    } => {
                        let value =
                            eval_atom(&constant_value).expect("This must be a constant value");

                        instruction.payload_size = self.compiler.constant_map.add_or_get(value);
                    }

                    Instruction {
                        op_code: OpCode::PUSHCONST,
                        contents: Some(Expr::List(expression)),
                        ..
                    } => {
                        let value = SteelVal::try_from(expression.clone())
                            .expect("This conversion must work");

                        instruction.payload_size = self.compiler.constant_map.add_or_get(value);
                    }

                    _ => {
                        todo!()
                    }
                }
            }
        }
    }

    // TODO -> clean up this API a lot
    pub fn compile_and_run_raw_program_with_path(
        &mut self,
        exprs: &str,
        path: PathBuf,
    ) -> Result<Vec<SteelVal>> {
        let constants = self.constants();
        let program = self.compiler.compile_executable(
            exprs,
            Some(path),
            constants,
            self.modules.clone(),
            &mut self.sources,
        )?;

        // program.profile_instructions();

        self.run_raw_program(program)
    }

    pub(crate) fn run_raw_program_from_exprs(
        &mut self,
        exprs: Vec<ExprKind>,
    ) -> Result<Vec<SteelVal>> {
        let constants = self.constants();
        let program = self.compiler.compile_executable_from_expressions(
            exprs,
            self.modules.clone(),
            constants,
            &mut self.sources,
        )?;
        self.run_raw_program(program)
    }

    pub fn compile_and_run_raw_program(&mut self, exprs: &str) -> Result<Vec<SteelVal>> {
        let constants = self.constants();
        let program = self.compiler.compile_executable(
            exprs,
            None,
            constants,
            self.modules.clone(),
            &mut self.sources,
        )?;

        self.run_raw_program(program)
    }

    pub fn raw_program_to_executable(
        &mut self,
        program: RawProgramWithSymbols,
    ) -> Result<Executable> {
        let symbol_map_offset = self.compiler.symbol_map.len();

        let result = program.build("TestProgram".to_string(), &mut self.compiler.symbol_map);

        // Revisit if we need to do this at all?
        if result.is_err() {
            self.compiler.symbol_map.roll_back(symbol_map_offset);
        }

        result
    }

    pub fn run_raw_program(&mut self, program: RawProgramWithSymbols) -> Result<Vec<SteelVal>> {
        let executable = self.raw_program_to_executable(program)?;
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
        let constants = self.constants();
        self.compiler.emit_expanded_ast(
            expr,
            constants,
            path,
            &mut self.sources,
            self.modules.clone(),
        )
    }

    /// Emit the unexpanded AST
    pub fn emit_ast_to_string(expr: &str) -> Result<String> {
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr, None).collect();
        let parsed = parsed?;
        Ok(parsed.into_iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }

    /// Emit the fully expanded AST as a pretty printed string
    pub fn emit_fully_expanded_ast_to_string(
        &mut self,
        expr: &str,
        path: Option<PathBuf>,
    ) -> Result<String> {
        let constants = self.constants();
        Ok(self
            .compiler
            .emit_expanded_ast(
                expr,
                constants,
                path,
                &mut self.sources,
                self.modules.clone(),
            )?
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
        let constants = self.constants();
        self.compiler.emit_expanded_ast(
            expr,
            constants,
            path,
            &mut self.sources,
            self.modules.clone(),
        )
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

        // let idx = self.compiler.register(name);
        // self.virtual_machine.insert_binding(idx, value);
        // self
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
            SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(predicate_name),
                Some(1),
            ))),
        )
    }

    // /// Registers a callback function. If registered, this callback will be called on every instruction
    // /// Allows for the introspection of the currently running process. The callback here takes as an argument the current instruction number.
    // ///
    // /// # Examples
    // ///
    // /// ```
    // /// # extern crate steel;
    // /// # use steel::steel_vm::engine::Engine;
    // /// let mut vm = Engine::new();
    // /// vm.on_progress(|count| {
    // ///     // parameter is 'usize' - number of instructions performed up to this point
    // ///     if count % 1000 == 0 {
    // ///         // print out a progress log every 1000 operations
    // ///         println!("Number of instructions up to this point: {}", count);
    // ///         // Returning false here would quit the evaluation of the function
    // ///         return true;
    // ///     }
    // ///     true
    // /// });
    // /// // This should end with "Number of instructions up to this point: 12000"
    // /// vm.run(
    // ///     r#"
    // ///     (define (loop x)
    // ///         (if (equal? x 1000)
    // ///             x
    // ///             (loop (+ x 1))))
    // ///     (loop 0)
    // /// "#,
    // /// )
    // /// .unwrap();
    // /// ```
    // pub fn on_progress<FN: Fn(usize) -> bool + 'static>(&mut self, _callback: FN) -> &mut Self {
    //     // self.virtual_machine.on_progress(callback);
    //     self
    // }

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
        let idx = self.compiler.get_idx(name).ok_or_else(throw!(
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

    // TODO: Come back to this
    /*
    // / Execute a program (as per [`run`](crate::steel_vm::engine::Engine::run)), however do not enforce any contracts. Any contracts that are added are not
    // / enforced.
    // /
    // / # Examples
    // /
    // / ```
    // / # extern crate steel;
    // / # use steel::steel_vm::engine::Engine;
    // / use steel::rvals::SteelVal;
    // / let mut vm = Engine::new();
    // / let output = vm.run_without_contracts(r#"
    // /        (define/contract (foo x)
    // /           (->/c integer? any/c)
    // /           "hello world")
    // /
    // /        (foo "bad-input")
    // / "#).unwrap();
    // / ```
    // pub fn run_without_contracts(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, None, constants)?;
    //     self.virtual_machine.execute_program::<UseCallback>(program)
    // }
     */

    /// Execute a program without invoking any callbacks, or enforcing any contract checking
    // pub fn run_without_callbacks_or_contracts(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, None, constants)?;
    //     self.virtual_machine
    //         .execute_program::<DoNotUseCallback, DoNotApplyContracts>(program)
    // }

    /// Similar to [`run`](crate::steel_vm::engine::Engine::run), however it includes path information
    /// for error reporting purposes.
    // pub fn run_with_path(&mut self, expr: &str, path: PathBuf) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, Some(path), constants)?;
    //     self.virtual_machine.execute_program(program)
    // }

    // pub fn compile_and_run_raw_program(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     let constants = self.constants();
    //     let program = self.compiler.compile_program(expr, None, constants)?;
    //     self.virtual_machine.execute_program(program)
    // }

    // pub fn compile_and_run_raw_program(&mut self, expr: &str) -> Result<Vec<SteelVal>> {
    //     self.compile_and_run_raw_program(expr)
    // }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    // pub fn parse_and_execute_from_path<P: AsRef<Path>>(
    //     &mut self,
    //     path: P,
    // ) -> Result<Vec<SteelVal>> {
    //     let mut file = std::fs::File::open(path)?;
    //     let mut exprs = String::new();
    //     file.read_to_string(&mut exprs)?;
    //     self.compile_and_run_raw_program(exprs.as_str(), )
    // }

    // pub fn parse_and_execute_from_path<P: AsRef<Path>>(
    //     &mut self,
    //     path: P,
    // ) -> Result<Vec<SteelVal>> {
    //     let path_buf = PathBuf::from(path.as_ref());
    //     let mut file = std::fs::File::open(path)?;
    //     let mut exprs = String::new();
    //     file.read_to_string(&mut exprs)?;
    //     self.run_with_path(exprs.as_str(), path_buf)
    // }

    // TODO this does not take into account the issues with
    // people registering new functions that shadow the original one
    fn constants(&mut self) -> ImmutableHashMap<InternedString, SteelVal> {
        // TODO: The constants need to be invalidated, if any of them are redefined within
        // the scope of execution.

        if let Some(hm) = &mut self.constants {
            if !hm.is_empty() {
                for constant in CONSTANTS {
                    let value = self
                        .compiler
                        .get_idx(constant)
                        .ok_or_else(throw!(
                            Generic => format!("unreachable")
                        ))
                        .and_then(|idx| {
                            self.virtual_machine.extract_value(idx).ok_or_else(throw!(
                                Generic => "unreachable"
                            ))
                        });

                    if let Ok(v) = value {
                        hm.insert((*constant).into(), v);
                    }
                }
            }
        }

        let mut hm = ImmutableHashMap::new();
        for constant in CONSTANTS {
            if let Ok(v) = self.extract_value(constant) {
                hm.insert((*constant).into(), v);
            }
        }
        self.constants = Some(hm.clone());

        hm
    }

    pub fn add_module(&mut self, path: String) -> Result<()> {
        self.compiler
            .compile_module(path.into(), &mut self.sources, self.modules.clone())
    }

    pub fn modules(&self) -> &HashMap<PathBuf, CompiledModule> {
        self.compiler.modules()
    }

    pub fn global_exists(&self, ident: &str) -> bool {
        let spur = if let Some(spur) = InternedString::try_get(ident) {
            spur
        } else {
            return false;
        };

        self.compiler.symbol_map.get(&spur).is_ok()
    }

    pub fn in_scope_macros(&self) -> &HashMap<InternedString, SteelMacro> {
        &self.compiler.macro_env
    }

    pub fn get_module(&self, path: PathBuf) -> Result<SteelVal> {
        let module_path =
            "__module-mangler".to_string() + path.as_os_str().to_str().unwrap() + "__%#__";

        self.extract_value(&module_path)
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
        if let Some(source_id) = span.source_id() {
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
                            if let Some(id) = span.source_id() {
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

                let resolved_file_name = file_name.cloned().unwrap_or_default();

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
        if let Some(source_id) = span.source_id() {
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

                            if let Some(id) = span.source_id() {
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

                let resolved_file_name = file_name.cloned().unwrap_or_default();

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
