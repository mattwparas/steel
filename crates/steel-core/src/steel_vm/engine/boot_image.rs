//! A boot image as bytes, so a process can start from one made by another
//! instead of compiling the prelude.
//!
//! What it holds is what `BootImage` holds, in a form that doesn't depend on
//! the process that made it:
//!
//! - Engines aren't saved. Each is made again: the builtin modules registered
//!   in a new engine, their values moved to the slots they had (registration
//!   order isn't the same in every process), then the programs it ran, run
//!   again. The macro kernel's engine also does again what the host did to it
//!   (`BootStep`), and gets back the plain globals that calls during
//!   expansion changed (counters).
//! - Compilers are saved as their state (`SavedCompiler`), the kernel as
//!   everything but its engine (`SavedKernel`).
//! - Constants are saved by value, functions as their bytecode, under the same
//!   ids, in the same slots: the programs refer to them by index.
//!
//! See STEEL_BOOT_IMAGE_PLAN.md.

use super::*;
use crate::compiler::compiler::SavedCompiler;
use crate::compiler::constants::ConstantMap;
use crate::compiler::map::SymbolMap;
use crate::core::instructions::DenseInstruction;
use crate::parser::expand_visitor::ThreadExpanderState;
use crate::parser::kernel::SavedKernel;
use crate::steel_vm::vm::InstructionPointer;
use steel_parser::span::Span;

/// Values written with the value serializer, as they are
/// (`SerializationContext::verbatim`): a function keeps its id and its body,
/// which refers to the constants and globals of the engine it was saved from
#[derive(Serialize, Deserialize)]
struct SavedValues {
    nodes: Vec<crate::rvals::SerializableSteelVal>,
    values: Vec<crate::rvals::SerializedId>,
}

impl SavedValues {
    // `compiler` and `globals` are the engine's the values are from
    fn save(values: Vec<SteelVal>, compiler: &Compiler, globals: &[SteelVal]) -> Result<Self> {
        use crate::rvals::{into_serializable_value, SerializableSteelVal, SerializationContext};
        let mut ctx = SerializationContext::new(
            &compiler.builtin_modules,
            globals,
            &compiler.symbol_map,
            &compiler.constant_map,
            &compiler.module_manager,
        )
        .verbatim();
        let values = values
            .into_iter()
            .map(|value| into_serializable_value(value, &mut ctx))
            .collect::<Result<_>>()?;
        if !ctx.reachable_structs.is_empty() {
            stop!(Generic => "a value with a struct type can't be saved in a boot image");
        }
        let nodes = core::mem::take(&mut ctx.nodes);
        // Read back with another engine's heap, which needn't outlive them
        if nodes.iter().any(|node| {
            matches!(
                node,
                SerializableSteelVal::HeapAllocated(_)
                    | SerializableSteelVal::HeapAllocatedVector(_)
                    | SerializableSteelVal::MutableStruct(_)
                    | SerializableSteelVal::Port(_)
            )
        }) {
            stop!(Generic => "a mutable value can't be saved in a boot image");
        }
        Ok(SavedValues { nodes, values })
    }

    // Made in `engine`
    fn restore(&self, engine: &mut Engine) -> Result<Vec<SteelVal>> {
        use crate::rvals::{from_serializable_value, HeapSerializer};
        let mut built_functions = FxHashMap::default();
        let (names, sent) = (Default::default(), Default::default());
        let heap = engine.virtual_machine.lock_heap_holding(&[]);
        let mut ctx = HeapSerializer::new(
            &self.nodes,
            heap,
            &mut built_functions,
            &mut engine.virtual_machine,
            crate::rvals::process_origin(),
            &names,
            &sent,
        )
        .verbatim();
        self.values
            .iter()
            .map(|id| from_serializable_value(&mut ctx, *id))
            .collect()
    }
}

// The constant map, with untouched copies of the functions in it (see
// `untouched_closures`)
fn save_constants(
    compiler: &Compiler,
    globals: &[SteelVal],
    untouched: &[(usize, Gc<ByteCodeLambda>)],
) -> Result<SavedValues> {
    let untouched: FxHashMap<usize, &Gc<ByteCodeLambda>> =
        untouched.iter().map(|(index, closure)| (*index, closure)).collect();
    let map = &compiler.constant_map;
    let values = (0..map.len())
        .map(|index| match untouched.get(&index) {
            Some(closure) => SteelVal::Closure((*closure).clone()),
            None => map.get(index),
        })
        .collect();
    SavedValues::save(values, compiler, globals)
}

/// A constant map that starts with another's constants: the ones after those
#[derive(Serialize, Deserialize)]
struct ConstantsAfter {
    from: usize,
    values: SavedValues,
}

impl ConstantsAfter {
    fn save(
        compiler: &Compiler,
        globals: &[SteelVal],
        untouched: &[(usize, Gc<ByteCodeLambda>)],
        base: &ConstantMap,
    ) -> Result<Self> {
        let map = &compiler.constant_map;
        let same = |a: &SteelVal, b: &SteelVal| match (a, b) {
            (SteelVal::Closure(a), SteelVal::Closure(b)) => a.id == b.id,
            (a, b) => a == b,
        };
        let from = (0..base.len().min(map.len()))
            .take_while(|&index| same(&base.get(index), &map.get(index)))
            .count();
        let untouched: FxHashMap<usize, &Gc<ByteCodeLambda>> =
            untouched.iter().map(|(index, closure)| (*index, closure)).collect();
        let values = (from..map.len())
            .map(|index| match untouched.get(&index) {
                Some(closure) => SteelVal::Closure((*closure).clone()),
                None => map.get(index),
            })
            .collect();
        Ok(ConstantsAfter {
            from,
            values: SavedValues::save(values, compiler, globals)?,
        })
    }

    fn restore(&self, base: &ConstantMap, engine: &mut Engine) -> Result<ConstantMap> {
        if self.from > base.len() {
            stop!(Generic => "a boot image's constants start with more than its base has");
        }
        let mut values: Vec<SteelVal> = (0..self.from).map(|index| base.get(index)).collect();
        values.extend(self.values.restore(engine)?);
        Ok(ConstantMap::from_vec(values))
    }
}

fn restore_constants(saved: &SavedValues, engine: &mut Engine) -> Result<ConstantMap> {
    Ok(ConstantMap::from_vec(saved.restore(engine)?))
}

/// Globals an image keeps by value after replaying: ones calls can change
/// without a program running (gensym counters)
#[derive(Serialize, Deserialize)]
struct PlainGlobals {
    slots: Vec<usize>,
    values: SavedValues,
}

impl PlainGlobals {
    fn save(engine: &Engine) -> Result<Self> {
        let roots = engine.virtual_machine.global_env.roots();
        let (slots, values): (Vec<usize>, Vec<SteelVal>) = roots
            .iter()
            .enumerate()
            .filter(|(_, value)| {
                matches!(
                    value,
                    SteelVal::IntV(_)
                        | SteelVal::BoolV(_)
                        | SteelVal::NumV(_)
                        | SteelVal::CharV(_)
                        | SteelVal::StringV(_)
                        | SteelVal::SymbolV(_)
                )
            })
            .map(|(slot, value)| (slot, value.clone()))
            .unzip();
        let compiler = engine.virtual_machine.compiler.read();
        Ok(PlainGlobals {
            slots,
            values: SavedValues::save(values, &compiler, roots)?,
        })
    }

    fn values(&self, engine: &mut Engine) -> Result<Vec<(usize, SteelVal)>> {
        Ok(self.slots.iter().copied().zip(self.values.restore(engine)?).collect())
    }

    fn restore(&self, engine: &mut Engine) -> Result<()> {
        let values = self.values.restore(engine)?;
        for (slot, value) in self.slots.iter().zip(values) {
            engine.virtual_machine.insert_binding(*slot, value);
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
struct SavedExecutable {
    name: String,
    version: String,
    instructions: Vec<Vec<DenseInstruction>>,
    spans: Vec<Vec<Span>>,
}

impl SavedExecutable {
    fn save(program: &Executable) -> Self {
        SavedExecutable {
            name: program.name.to_string(),
            version: program.version.to_string(),
            instructions: program
                .instructions
                .iter()
                .map(|x| x.iter().copied().collect())
                .collect(),
            spans: program.spans.iter().map(|x| x.to_vec()).collect(),
        }
    }

    // Under `constants`: the programs and the compiler share the map
    fn restore(&self, constants: &ConstantMap) -> Executable {
        Executable {
            name: Shared::new(self.name.clone()),
            version: Shared::new(self.version.clone()),
            time_stamp: None,
            instructions: self
                .instructions
                .iter()
                .map(|x| InstructionPointer::from(x.clone()))
                .collect(),
            constant_map: constants.clone(),
            spans: self.spans.iter().map(|x| Shared::from(x.clone())).collect(),
        }
    }
}

impl SavedBootStep {
    fn restore(&self, constants: &ConstantMap) -> BootStep {
        match self {
            SavedBootStep::Run(program) => BootStep::Run(program.restore(constants)),
            SavedBootStep::Eval(program) => BootStep::Eval(program.restore(constants)),
            SavedBootStep::Cleared(slot) => BootStep::Cleared(String::new(), *slot),
            SavedBootStep::Registered(name, slot) => BootStep::Registered(name.clone(), *slot),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum SavedBootStep {
    Run(SavedExecutable),
    Eval(SavedExecutable),
    Cleared(usize),
    Registered(String, usize),
}

/// How to make one engine again
#[derive(Serialize, Deserialize)]
struct SavedEngine {
    sandbox: bool,
    // The slots the builtin modules' values had
    builtins: SymbolMap,
    programs: Vec<SavedBootStep>,
    // Globals with plain values, put back after the programs run (None for
    // the main engine)
    plain_globals: Option<PlainGlobals>,
    // Globals the collector had cleared, cleared again after
    undefined: Vec<usize>,
    compiler: SavedCompiler,
    constants: SavedValues,
}

/// What making the image changed outside any engine: counters names and ids
/// come from, which a process reading the image continues past, and tables the
/// compiler reads. A process reading it has to give modules the same scopes.
#[derive(Serialize, Deserialize)]
struct SavedProcess {
    scope_counter: u32,
    module_scopes: Vec<(String, steel_parser::scopes::Scope)>,
    source_ids: u32,
    syntax_object_ids: usize,
    function_ids: usize,
    // `analysis`'s binder names, `set!` values and hoisted cars, template
    // prefixes, phase 1 instances, local macro names
    name_counters: [usize; 6],
    struct_info: Vec<((InternedString, InternedString), (Vec<InternedString>, bool))>,
    procedural: Vec<InternedString>,
    prelude_macros: Vec<(bool, FxHashMap<InternedString, SteelMacro>)>,
    prelude_aliases: bool,
    // Module prefix paths, in the order of their numbers
    module_prefixes: Vec<String>,
}

fn name_counters() -> [&'static core::sync::atomic::AtomicUsize; 6] {
    use crate::compiler::passes::analysis;
    [
        &analysis::FRESHEN_COUNTER,
        &analysis::SET_BOX_VALUE_COUNTER,
        &analysis::CAR_HOIST_COUNTER,
        &crate::compiler::modules::TEMPLATE_PREFIXES,
        &crate::compiler::modules::PHASE1_INSTANCES,
        &crate::parser::expand_visitor::LOCAL_MACRO_NAMES,
    ]
}

impl SavedProcess {
    fn save() -> Self {
        use core::sync::atomic::Ordering::Relaxed;
        // Copied before the counters are read: copying syntax gives it new ids
        let (struct_info, procedural) = crate::parser::expand_visitor::recorded_globally();
        let prelude_macros = [false, true]
            .into_iter()
            .filter_map(|sandbox| Some((sandbox, (*prelude_macros_for(sandbox)?).clone())))
            .collect();
        crate::compiler::module_cache::note_image_counters(
            steel_parser::scopes::scope_counter(),
            steel_parser::parser::SYNTAX_OBJECT_ID.load(Relaxed) as u32,
            steel_parser::parser::source_id_counter(),
        );
        SavedProcess {
            scope_counter: steel_parser::scopes::scope_counter(),
            module_scopes: steel_parser::scopes::module_scopes(),
            source_ids: steel_parser::parser::source_id_counter(),
            syntax_object_ids: steel_parser::parser::SYNTAX_OBJECT_ID.load(Relaxed),
            function_ids: crate::compiler::code_gen::FUNCTION_ID.load(Relaxed),
            name_counters: name_counters().map(|counter| counter.load(Relaxed)),
            struct_info,
            procedural,
            prelude_macros,
            prelude_aliases: crate::parser::expand_visitor::prelude_aliases(),
            module_prefixes: crate::compiler::modules::module_prefixes(),
        }
    }

    // Before anything else is read from the image
    fn restore(self) -> Result<()> {
        use core::sync::atomic::Ordering::Relaxed;
        if !steel_parser::scopes::restore_module_scopes(&self.module_scopes) {
            stop!(Generic => "a module has another scope here than in the boot image");
        }
        if !crate::compiler::modules::restore_module_prefixes(&self.module_prefixes) {
            stop!(Generic => "a module has another prefix number here than in the boot image");
        }
        steel_parser::scopes::raise_scope_counter(self.scope_counter);
        steel_parser::parser::raise_source_id_counter(self.source_ids);
        steel_parser::parser::SYNTAX_OBJECT_ID.fetch_max(self.syntax_object_ids, Relaxed);
        crate::compiler::code_gen::FUNCTION_ID.fetch_max(self.function_ids, Relaxed);
        for (counter, saved) in name_counters().into_iter().zip(self.name_counters) {
            counter.fetch_max(saved, Relaxed);
        }
        crate::parser::expand_visitor::restore_recorded_globally(self.struct_info, self.procedural);
        for (sandbox, macros) in self.prelude_macros {
            set_default_prelude_macros(macros, sandbox);
        }
        crate::parser::expand_visitor::restore_prelude_aliases(self.prelude_aliases);
        crate::compiler::module_cache::note_image_counters(
            self.scope_counter,
            self.syntax_object_ids as u32,
            self.source_ids,
        );
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct SavedBootImage {
    main: SavedEngine,
    kernel: SavedKernel,
    kernel_engine: SavedEngine,
    expander: ThreadExpanderState,
    process: SavedProcess,
}

impl SavedBootImage {
    /// The image `boot_image(sandbox, kernel_sandbox)` gives, saved
    pub(crate) fn save(image: &BootImage, sandbox: bool, kernel_sandbox: bool) -> Result<Self> {
        if !image.steps.is_empty() {
            stop!(Generic => "an image made from another is saved with `SavedDerivedImage`");
        }
        let compiler = image.booted.virtual_machine.compiler.read();
        let Some(kernel) = compiler.kernel.as_ref() else {
            stop!(Generic => "a boot image's compiler has a kernel");
        };
        // Builtins are put back in their slots by name. A generated name (a
        // context function's) can belong to another function in another process
        let generated = |engine: &Engine| {
            engine
                .virtual_machine
                .compiler
                .read()
                .symbol_map
                .values()
                .iter()
                .any(|name| name.resolve().starts_with("###ctx-func"))
        };
        if generated(&image.builtins) {
            stop!(Generic => "a builtin module with context functions can't be saved in a boot image");
        }
        let kernel_record = fresh_kernel_image(kernel_sandbox)
            .boot
            .clone()
            .expect("a kernel image keeps what it ran to boot");

        let kernel_builtins = kernel_record
            .builtins
            .virtual_machine
            .compiler
            .read()
            .symbol_map
            .clone();
        let kernel_compiler = kernel.engine.virtual_machine.compiler.read();
        let kernel_programs = kernel_record
            .programs
            .iter()
            .map(|program| SavedBootStep::Run(SavedExecutable::save(program)))
            .chain(image.kernel_steps.iter().map(|step| match step {
                BootStep::Run(program) => SavedBootStep::Run(SavedExecutable::save(program)),
                BootStep::Eval(program) => SavedBootStep::Eval(SavedExecutable::save(program)),
                BootStep::Cleared(_, slot) => SavedBootStep::Cleared(*slot),
                BootStep::Registered(name, slot) => SavedBootStep::Registered(name.clone(), *slot),
            }))
            .collect();

        let saved = SavedBootImage {
            main: SavedEngine {
                sandbox,
                builtins: image.builtins.virtual_machine.compiler.read().symbol_map.clone(),
                programs: image
                    .programs
                    .iter()
                    .map(|program| SavedBootStep::Run(SavedExecutable::save(program)))
                    .collect(),
                plain_globals: None,
                undefined: Vec::new(),
                compiler: compiler.save(),
                constants: save_constants(
                    &compiler,
                    image.booted.virtual_machine.global_env.roots(),
                    &image.closures,
                )?,
            },
            kernel: kernel.save(),
            kernel_engine: SavedEngine {
                sandbox: kernel_sandbox,
                builtins: kernel_builtins,
                programs: kernel_programs,
                plain_globals: Some(PlainGlobals::save(&kernel.engine)?),
                undefined: undefined_slots(&kernel.engine),
                compiler: kernel_compiler.save(),
                constants: save_constants(
                    &kernel_compiler,
                    kernel.engine.virtual_machine.global_env.roots(),
                    &[],
                )?,
            },
            expander: image.expander[0].clone(),
            process: SavedProcess::save(),
        };
        Ok(saved)
    }

    pub(crate) fn to_bytes(&self) -> Result<Vec<u8>> {
        to_tabled_bytes(self)
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self> {
        from_tabled_bytes(bytes)
    }

    /// The boot image this was saved from, made again
    pub(crate) fn load(self) -> Result<BootImage> {
        let SavedBootImage {
            main,
            kernel,
            kernel_engine,
            expander,
            process,
        } = self;
        process.restore()?;

        let mut builtins = builtins_engine(main.sandbox, &main.builtins);
        let constants = restore_constants(&main.constants, &mut builtins)?;
        let mut compiler =
            Compiler::restore(main.compiler, constants.clone(), builtins.modules.clone());
        compiler.sources.register_paths_globally();
        check_module_prefixes(&compiler)?;

        let mut failed = None;
        let kernel = Kernel::restore(kernel, |register| {
            match make_kernel_engine(kernel_engine, register) {
                Ok(engine) => engine,
                Err(e) => {
                    failed = Some(e);
                    Engine::new_raw()
                }
            }
        });
        if let Some(e) = failed {
            return Err(e);
        }
        compiler.kernel = Some(kernel);

        let closures = (0..constants.len())
            .filter_map(|index| match constants.get(index) {
                SteelVal::Closure(closure) => Some((index, Gc::new(closure.fresh_copy()))),
                _ => None,
            })
            .collect();

        let programs = main
            .programs
            .iter()
            .map(|step| match step {
                SavedBootStep::Run(program) => Ok(program.restore(&constants)),
                _ => stop!(Generic => "a boot image's main engine only runs programs"),
            })
            .collect::<Result<Vec<_>>>()?;

        let mut booted = builtins.deep_clone();
        booted.virtual_machine.compiler = Arc::new(RwLock::new(compiler));

        Ok(BootImage {
            builtins,
            programs,
            closures,
            booted,
            expander: vec![expander],
            kernel_steps: Vec::new(),
            steps: Vec::new(),
            host_modules: Vec::new(),
            derived_kernel_steps: Vec::new(),
            delta: None,
            stored_as: None,
        })
    }
}

// An image's bytes: the tables of the strings and scope sets it uses (each
// written once), their length first, then the image, which refers to them by
// number (`steel_parser::tables`)
fn to_tabled_bytes<T: Serialize>(value: &T) -> Result<Vec<u8>> {
    let error = |e: bincode::Error| {
        crate::SteelErr::new(crate::rerrs::ErrorKind::Generic, format!("saving a boot image: {e}"))
    };
    let (body, tables) = steel_parser::tables::writing_tables(|| bincode::serialize(value));
    let body = body.map_err(error)?;
    let tables = bincode::serialize(&tables).map_err(error)?;
    let mut bytes = Vec::with_capacity(8 + tables.len() + body.len());
    bytes.extend_from_slice(&(tables.len() as u64).to_le_bytes());
    bytes.extend_from_slice(&tables);
    bytes.extend_from_slice(&body);
    Ok(bytes)
}

fn from_tabled_bytes<T: serde::de::DeserializeOwned>(bytes: &[u8]) -> Result<T> {
    let error = |e: bincode::Error| {
        crate::SteelErr::new(crate::rerrs::ErrorKind::Generic, format!("reading a boot image: {e}"))
    };
    let malformed =
        || crate::SteelErr::new(crate::rerrs::ErrorKind::Generic, "a malformed boot image".into());
    let (length, rest) = bytes.split_at_checked(8).ok_or_else(malformed)?;
    let length = u64::from_le_bytes(length.try_into().map_err(|_| malformed())?) as usize;
    let (tables, body) = rest.split_at_checked(length).ok_or_else(malformed)?;
    let tables: steel_parser::tables::Tables = bincode::deserialize(tables).map_err(error)?;
    steel_parser::tables::reading_tables(&tables, || bincode::deserialize(body)).map_err(error)
}

/// An image made from the default engine's (the pass engine's), saved as what
/// it adds: read back only over the image it was made from, which the cache key
/// ties it to
#[derive(Serialize, Deserialize)]
pub(crate) struct SavedDerivedImage {
    // The checksum of the base image's file: only that image can be the base
    // (`BootImage::stored_as`)
    base: u64,
    host_modules: Vec<HostModule>,
    steps: Vec<SavedBootStep>,
    // What compiling the passes changed in the base image's compiler, and the
    // constants it added
    compiler: crate::compiler::compiler::SavedCompilerDelta,
    constants: ConstantsAfter,
    // The macro kernel's tables, over the base image's kernel engine, whose
    // globals this one only changed the plain values of (`derive_pass_image`
    // checks)
    kernel: SavedKernel,
    // The kernel engine's compiler and constants, and what it did after the
    // base image's (phase 1 code of the passes' modules)
    kernel_compiler: crate::compiler::compiler::SavedCompilerDelta,
    kernel_constants: ConstantsAfter,
    kernel_steps: Vec<SavedBootStep>,
    kernel_plain_globals: PlainGlobals,
    kernel_undefined: Vec<usize>,
    expander: ThreadExpanderState,
    process: SavedProcess,
}

impl SavedDerivedImage {
    /// `image`, made from `base`, saved as what it adds to it
    pub(crate) fn save(image: &BootImage, base: &BootImage) -> Result<Self> {
        if image.delta.is_some() {
            stop!(Generic => "an image read as a delta is saved by the one it was read from");
        }
        // An image built here and not kept has no file; it is only read back here
        // (`STEEL_BOOT_IMAGE_ROUNDTRIP`)
        let base_file = base.stored_as.unwrap_or(0);
        let base_compiler = base.booted.virtual_machine.compiler.read();
        let Some(base_kernel) = base_compiler.kernel.as_ref() else {
            stop!(Generic => "a boot image's compiler has a kernel");
        };
        let base_kernel_compiler = base_kernel.engine.virtual_machine.compiler.read();
        let compiler = image.booted.virtual_machine.compiler.read();
        let Some(kernel) = compiler.kernel.as_ref() else {
            stop!(Generic => "a boot image's compiler has a kernel");
        };
        let (Some(expander), true) = (image.expander.last(), image.expander.len() == 2) else {
            stop!(Generic => "only an image made from the default one is saved this way");
        };
        let steps = image
            .steps
            .iter()
            .map(|step| match step {
                BootStep::Run(program) => SavedBootStep::Run(SavedExecutable::save(program)),
                BootStep::Eval(program) => SavedBootStep::Eval(SavedExecutable::save(program)),
                BootStep::Cleared(_, slot) => SavedBootStep::Cleared(*slot),
                BootStep::Registered(name, slot) => SavedBootStep::Registered(name.clone(), *slot),
            })
            .collect();
        let kernel_compiler = kernel.engine.virtual_machine.compiler.read();
        Ok(SavedDerivedImage {
            base: base_file,
            host_modules: image.host_modules.clone(),
            steps,
            compiler: compiler.save().delta_from(&base_compiler.save()),
            constants: ConstantsAfter::save(
                &compiler,
                image.booted.virtual_machine.global_env.roots(),
                &image.closures,
                &base_compiler.constant_map,
            )?,
            kernel: kernel.save(),
            kernel_compiler: kernel_compiler.save().delta_from(&base_kernel_compiler.save()),
            kernel_constants: ConstantsAfter::save(
                &kernel_compiler,
                kernel.engine.virtual_machine.global_env.roots(),
                &[],
                &base_kernel_compiler.constant_map,
            )?,
            kernel_steps: image
                .derived_kernel_steps
                .iter()
                .map(|step| match step {
                    BootStep::Run(program) => SavedBootStep::Run(SavedExecutable::save(program)),
                    BootStep::Eval(program) => SavedBootStep::Eval(SavedExecutable::save(program)),
                    BootStep::Cleared(_, slot) => SavedBootStep::Cleared(*slot),
                    BootStep::Registered(name, slot) => {
                        SavedBootStep::Registered(name.clone(), *slot)
                    }
                })
                .collect(),
            kernel_undefined: undefined_slots(&kernel.engine),
            kernel_plain_globals: PlainGlobals::save(&kernel.engine)?,
            expander: expander.clone(),
            // Made before any program (`cached_pass_image`), so everything
            // here is the images'
            process: SavedProcess::save(),
        })
    }

    pub(crate) fn to_bytes(&self) -> Result<Vec<u8>> {
        to_tabled_bytes(self)
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self> {
        from_tabled_bytes(bytes)
    }

    /// The image this was saved from, made again over `base`: each engine made
    /// from it applies what it adds to its copy of `base`'s compiler
    pub(crate) fn load(self, base: &BootImage) -> Result<BootImage> {
        let SavedDerivedImage {
            base: base_file,
            host_modules,
            steps,
            compiler,
            constants,
            kernel,
            kernel_compiler,
            kernel_constants,
            kernel_steps,
            kernel_plain_globals,
            kernel_undefined,
            expander,
            process,
        } = self;
        if !base.steps.is_empty() {
            stop!(Generic => "a derived image is made from the default one");
        }
        // Images built by other processes may lay out their kernels otherwise
        if base.stored_as.unwrap_or(0) != base_file {
            stop!(Generic => "the image is made from another base image than this one");
        }
        process.restore()?;

        for (path, prefix) in compiler.modules() {
            if prefix != crate::compiler::modules::path_to_prefix_name(path.clone()) {
                stop!(Generic => "a module has another prefix here than in the boot image: {}", path.display());
            }
        }

        let base_compiler = base.booted.virtual_machine.compiler.read();
        let Some(base_kernel) = base_compiler.kernel.as_ref() else {
            stop!(Generic => "a boot image's compiler has a kernel");
        };
        // Constants hold nothing mutable (`SavedValues::save`), so any engine's
        // heap can make them
        let mut scratch = base.builtins.deep_clone();
        let constants = constants.restore(&base_compiler.constant_map, &mut scratch)?;
        let kernel_constants = kernel_constants.restore(
            &base_kernel.engine.virtual_machine.compiler.read().constant_map,
            &mut scratch,
        )?;
        drop(base_compiler);
        let kernel_steps = kernel_steps
            .iter()
            .map(|step| step.restore(&kernel_constants))
            .collect();
        let kernel_plain_globals = kernel_plain_globals.values(&mut scratch)?;

        let closures = (0..constants.len())
            .filter_map(|index| match constants.get(index) {
                SteelVal::Closure(closure) => Some((index, Gc::new(closure.fresh_copy()))),
                _ => None,
            })
            .collect();
        let steps = steps.iter().map(|step| step.restore(&constants)).collect();
        let mut expanders = base.expander.clone();
        expanders.push(expander);

        Ok(BootImage {
            builtins: base.builtins.deep_clone(),
            programs: base.programs.clone(),
            closures,
            booted: base.booted.clone(),
            expander: expanders,
            kernel_steps: base.kernel_steps.clone(),
            steps,
            host_modules,
            derived_kernel_steps: Vec::new(),
            stored_as: None,
            delta: Some(Arc::new(DerivedDelta {
                compiler,
                constants,
                kernel,
                kernel_compiler,
                kernel_constants,
                kernel_steps,
                kernel_plain_globals,
                kernel_undefined,
            })),
        })
    }
}

/// A derived image read from disk, as what it adds to its base image
pub(crate) struct DerivedDelta {
    compiler: crate::compiler::compiler::SavedCompilerDelta,
    constants: ConstantMap,
    kernel: SavedKernel,
    kernel_compiler: crate::compiler::compiler::SavedCompilerDelta,
    kernel_constants: ConstantMap,
    kernel_steps: Vec<BootStep>,
    kernel_plain_globals: Vec<(usize, SteelVal)>,
    kernel_undefined: Vec<usize>,
}

impl DerivedDelta {
    /// Makes `compiler`, a copy of the base image's compiler, the derived one,
    /// and gives the kernel for it: the base's kernel engine copied, with what
    /// the derived image changed in it
    pub(crate) fn apply(&self, compiler: &mut Compiler) -> Result<Kernel> {
        compiler.apply_delta(self.compiler.clone());
        compiler.constant_map = self.constants.clone();
        compiler.sources.register_paths_globally();

        let Some(base_kernel) = compiler.kernel.take() else {
            stop!(Generic => "a boot image's compiler has a kernel");
        };
        let mut failed = None;
        let mut kernel = Kernel::restore_over(self.kernel.clone(), &base_kernel.engine, |engine| {
            {
                let mut kernel_compiler = engine.virtual_machine.compiler.write();
                kernel_compiler.apply_delta(self.kernel_compiler.clone());
                kernel_compiler.constant_map = self.kernel_constants.clone();
                kernel_compiler.sources.register_paths_globally();
            }
            let scratch = engine.clone();
            if let Err(e) =
                replay_steps(engine, &self.kernel_steps, &|name| scratch.extract_value(name))
            {
                failed = Some(e);
            }
        });
        if let Some(e) = failed {
            return Err(e);
        }
        for (slot, value) in &self.kernel_plain_globals {
            kernel.engine.virtual_machine.insert_binding(*slot, value.clone());
        }
        for slot in &self.kernel_undefined {
            kernel
                .engine
                .virtual_machine
                .insert_binding(*slot, crate::env::undefined_global());
        }
        // That is the derived image's kernel. An engine gets its own copy of
        // that, as one from an image made in this process does
        Ok(kernel.deep_clone())
    }
}

/// Boot images kept in a directory, one file per key. The key covers what an
/// image depends on: this executable (its bytecode format, the builtins and the
/// layouts of what's saved), the prelude's sources, the sandbox settings, and
/// the `STEEL_*` settings, some of which change how code compiles. A file whose
/// key or contents don't match is not used, and is replaced.
pub(crate) struct BootImageCache {
    path: std::path::PathBuf,
    key: u64,
}

const MAGIC: &[u8; 8] = b"STLBOOTA";

// How many images the directory keeps: each build of an executable has its own
// (the default engine's and the pass engine's)
const KEPT_IMAGES: usize = 16;

// Checks the contents weren't changed: a changed string or number can still
// decode
fn checksum(bytes: &[u8]) -> u64 {
    use std::hash::Hasher;
    let mut hasher = rustc_hash::FxHasher::default();
    for chunk in bytes.chunks(8) {
        let mut word = [0u8; 8];
        word[..chunk.len()].copy_from_slice(chunk);
        hasher.write_u64(u64::from_le_bytes(word));
    }
    hasher.write_usize(bytes.len());
    hasher.finish()
}

/// Where boot images (and the module cache, in `modules`) are kept: the
/// setting's directory, else the one the program asked for, else the user's
/// cache directory
pub(crate) fn cache_dir() -> Option<std::path::PathBuf> {
    let config = crate::config::config();
    match (&config.boot_image_dir, super::boot_image_cache_requested()) {
        (Some(dir), _) => Some(std::path::PathBuf::from(dir)),
        (None, Some(Some(dir))) => Some(dir.clone()),
        (None, _) => Some(
            std::env::var_os("XDG_CACHE_HOME")
                .map(std::path::PathBuf::from)
                .or_else(|| std::env::var_os("HOME").map(|home| std::path::PathBuf::from(home).join(".cache")))?
                .join("steel"),
        ),
    }
}

impl BootImageCache {
    /// The cache for these settings, or None when caching is off or there is
    /// nowhere to keep it
    pub(crate) fn new(sandbox: bool, kernel_sandbox: bool) -> Option<Self> {
        let config = crate::config::config();
        // Off unless the program asks (`Engine::enable_boot_image_cache`); the
        // setting overrides either way
        let requested = super::boot_image_cache_requested();
        if !config.boot_image_cache.unwrap_or(requested.is_some()) {
            return None;
        }

        let dir = cache_dir()?;

        let key = Self::key(sandbox, kernel_sandbox)?;
        Some(BootImageCache {
            path: dir.join(format!("boot-{key:016x}.img")),
            key,
        })
    }

    #[cfg(test)]
    fn at(path: std::path::PathBuf, key: u64) -> Self {
        BootImageCache { path, key }
    }

    fn key(sandbox: bool, kernel_sandbox: bool) -> Option<u64> {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        env!("CARGO_PKG_VERSION").hash(&mut hasher);
        let exe = std::fs::metadata(std::env::current_exe().ok()?).ok()?;
        exe.len().hash(&mut hasher);
        exe.modified().ok()?.hash(&mut hasher);

        (sandbox, kernel_sandbox).hash(&mut hasher);
        crate::steel_vm::primitives::ALL_MODULES.hash(&mut hasher);
        crate::stdlib::PRELUDE.hash(&mut hasher);
        crate::stdlib::KERNEL.hash(&mut hasher);
        crate::compiler::modules::PRELUDE_WITHOUT_BASE.hash(&mut hasher);
        crate::compiler::modules::BUILT_INS.hash(&mut hasher);

        let mut settings: Vec<(String, String)> = std::env::vars()
            .filter(|(name, _)| name.starts_with("STEEL_"))
            .collect();
        settings.sort();
        settings.hash(&mut hasher);

        Some(hasher.finish())
    }

    /// The cache for an image made from this one's, named by `tag`
    pub(crate) fn derived(&self, tag: &str) -> Self {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        (self.key, tag).hash(&mut hasher);
        let key = hasher.finish();
        BootImageCache {
            path: self.path.with_file_name(format!("boot-{key:016x}.img")),
            key,
        }
    }

    // The saved image in the file, if it is there and matches, and its checksum
    fn read(&self) -> Option<(Vec<u8>, u64)> {
        let bytes = std::fs::read(&self.path).ok()?;
        let rest = bytes.strip_prefix(MAGIC.as_slice())?;
        let (key, rest) = rest.split_at_checked(8)?;
        let (sum, image) = rest.split_at_checked(8)?;
        let sum = u64::from_le_bytes(sum.try_into().ok()?);
        if u64::from_le_bytes(key.try_into().ok()?) != self.key || sum != checksum(image) {
            return None;
        }
        Some((image.to_vec(), sum))
    }

    /// The image in the cache, if it is there, matches and loads
    pub(crate) fn load(&self) -> Option<BootImage> {
        let (bytes, sum) = self.read()?;
        let mut image = SavedBootImage::from_bytes(&bytes).ok()?.load().ok()?;
        image.stored_as = Some(sum);
        Some(image)
    }

    /// An image made from `base`, if it is there, matches and loads
    pub(crate) fn load_derived(&self, base: &BootImage) -> Option<BootImage> {
        SavedDerivedImage::from_bytes(&self.read()?.0).ok()?.load(base).ok()
    }

    /// Keeps a built image. A failure to write leaves the cache as it was
    /// Keeps a built image, and gives the checksum it's kept under
    pub(crate) fn store(&self, image: &BootImage, sandbox: bool, kernel_sandbox: bool) -> Option<u64> {
        let bytes =
            SavedBootImage::save(image, sandbox, kernel_sandbox).and_then(|saved| saved.to_bytes()).ok()?;
        self.write(&bytes);
        Some(checksum(&bytes))
    }

    /// Keeps an image made from another
    pub(crate) fn store_derived(&self, image: &BootImage, base: &BootImage) {
        if let Ok(bytes) = SavedDerivedImage::save(image, base).and_then(|saved| saved.to_bytes()) {
            self.write(&bytes);
        }
    }

    fn write(&self, bytes: &[u8]) {
        let Some(dir) = self.path.parent() else {
            return;
        };
        if std::fs::create_dir_all(dir).is_err() {
            return;
        }

        // Written whole, then renamed, so a reader never sees part of one
        let temporary = dir.join(format!(".boot-{:016x}.{}.tmp", self.key, std::process::id()));
        let mut contents = Vec::with_capacity(MAGIC.len() + 16 + bytes.len());
        contents.extend_from_slice(MAGIC);
        contents.extend_from_slice(&self.key.to_le_bytes());
        contents.extend_from_slice(&checksum(bytes).to_le_bytes());
        contents.extend_from_slice(bytes);
        if std::fs::write(&temporary, contents).is_ok()
            && std::fs::rename(&temporary, &self.path).is_err()
        {
            let _ = std::fs::remove_file(&temporary);
        }

        Self::prune(dir);
    }

    // Leaves the most recently written images
    fn prune(dir: &std::path::Path) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        let mut images: Vec<(std::time::SystemTime, std::path::PathBuf)> = entries
            .filter_map(|entry| entry.ok())
            .filter(|entry| {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                name.starts_with("boot-") && name.ends_with(".img")
            })
            .filter_map(|entry| Some((entry.metadata().ok()?.modified().ok()?, entry.path())))
            .collect();
        images.sort_by(|a, b| b.0.cmp(&a.0));
        for (_, path) in images.into_iter().skip(KEPT_IMAGES) {
            let _ = std::fs::remove_file(path);
        }
    }
}

// An engine with the builtin modules registered, their values in the slots
// `layout` gives them
fn builtins_engine(sandbox: bool, layout: &SymbolMap) -> Engine {
    let mut fresh = Engine::builtins_only(sandbox);
    let values: Vec<SteelVal> = layout
        .values()
        .iter()
        .map(|name| fresh.extract_value(name.resolve()).unwrap_or(SteelVal::Void))
        .collect();

    fresh.virtual_machine.compiler.write().symbol_map = layout.clone();
    for (slot, value) in values.into_iter().enumerate() {
        fresh.virtual_machine.insert_binding(slot, value);
    }
    fresh
}

// A module's prefix is numbered in the order modules first need one, per
// process (`MODULE_PREFIXES`); the image's numbers are restored before anything
// else is read (`restore_module_prefixes`). A saved module keeps the one it had,
// which code here computing it from the path again (struct info) has to agree
// with: otherwise the image is refused.
fn check_module_prefixes(compiler: &Compiler) -> Result<()> {
    for (path, module) in compiler.module_manager.modules() {
        if module.prefix() != crate::compiler::modules::path_to_prefix_name(path.clone()) {
            stop!(Generic => "a module has another prefix here than in the boot image: {}", path.display());
        }
    }
    Ok(())
}

// The slots holding the undefined value, which the collector leaves in globals
// it frees
fn undefined_slots(engine: &Engine) -> Vec<usize> {
    engine
        .virtual_machine
        .global_env
        .roots()
        .iter()
        .enumerate()
        .filter(|(_, value)| crate::env::is_undefined_global(value))
        .map(|(slot, _)| slot)
        .collect()
}

// The macro kernel's engine, made again
fn make_kernel_engine(saved: SavedEngine, register: &dyn Fn(&mut Engine)) -> Result<Engine> {
    let builtins = builtins_engine(saved.sandbox, &saved.builtins);

    // The transformer functions, made in another engine and put in the slots they had
    let mut scratch = builtins.deep_clone();
    register(&mut scratch);

    let mut engine = builtins;
    let constants = restore_constants(&saved.constants, &mut engine)?;
    let compiler = Compiler::restore(saved.compiler, constants.clone(), engine.modules.clone());
    compiler.sources.register_paths_globally();
    engine.virtual_machine.compiler = Arc::new(RwLock::new(compiler));

    let steps: Vec<BootStep> = saved
        .programs
        .iter()
        .map(|step| step.restore(&constants))
        .collect();
    replay_steps(&mut engine, &steps, &|name| scratch.extract_value(name))?;

    if let Some(plain) = &saved.plain_globals {
        plain.restore(&mut engine)?;
    }
    for slot in &saved.undefined {
        engine
            .virtual_machine
            .insert_binding(*slot, crate::env::undefined_global());
    }

    Ok(engine)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn last(engine: &mut Engine, program: &str) -> SteelVal {
        engine
            .compile_and_run_raw_program(program.to_string())
            .unwrap_or_else(|e| panic!("{program}: {e}"))
            .pop()
            .unwrap_or(SteelVal::Void)
    }

    // Functions written verbatim come back with their ids and bodies, and run
    // in the engine they were written from
    #[test]
    fn saved_values_keep_functions_as_they_are() {
        let mut engine = Engine::new();
        let program = "(define (f x) (+ x 1))
                       (define g (let ([y (list 1 2)]) (lambda (x) (cons x y))))
                       (list f g (list 1 2) #(a \"b\" #\\c) 1/3 (expt 2 100))";
        let SteelVal::ListV(values) = last(&mut engine, program) else {
            panic!("a list");
        };
        let values: Vec<SteelVal> = values.iter().cloned().collect();

        let compiler = engine.virtual_machine.compiler.read().clone();
        let saved =
            SavedValues::save(values.clone(), &compiler, engine.virtual_machine.global_env.roots())
                .unwrap();
        let bytes = bincode::serialize(&saved).unwrap();
        let saved: SavedValues = bincode::deserialize(&bytes).unwrap();
        let restored = saved.restore(&mut engine).unwrap();

        for (before, after) in values.iter().zip(&restored) {
            match (before, after) {
                (SteelVal::Closure(a), SteelVal::Closure(b)) => {
                    assert_eq!(a.id, b.id);
                    assert_eq!(a.captures().len(), b.captures().len());
                    let code = |l: &ByteCodeLambda| {
                        let mut body: Vec<_> =
                            l.body_exp().iter().map(|i| (i.op_code, i.payload_size)).collect();
                        if let (Some(header), Some(first)) = (l.header, body.first_mut()) {
                            first.0 = header;
                        }
                        body
                    };
                    assert_eq!(code(a), code(b));
                }
                (a, b) => assert_eq!(a, b),
            }
        }
        let call = |engine: &mut Engine, f: &SteelVal| {
            engine
                .call_function_with_args(f.clone(), vec![SteelVal::IntV(41)])
                .unwrap()
                .to_string()
        };
        assert_eq!(call(&mut engine, &restored[0]), "42");
        assert_eq!(call(&mut engine, &restored[1]), "(41 1 2)");

        // Mutable values aren't saved
        let boxed = last(&mut engine, "(box 1)");
        assert!(SavedValues::save(vec![boxed], &compiler, engine.virtual_machine.global_env.roots()).is_err());
    }

    // A program that doesn't ask keeps no boot images (this crate's tests don't)
    #[test]
    fn the_cache_is_off_unless_asked_for() {
        if std::env::var_os("STEEL_BOOT_IMAGE_CACHE").is_some() {
            return;
        }
        assert!(BootImageCache::new(false, Kernel::default_sandbox()).is_none());
    }

    #[test]
    fn the_cache_keeps_images_and_refuses_changed_ones() {
        let kernel_sandbox = Kernel::default_sandbox();
        let dir = std::env::temp_dir().join(format!("steel-boot-cache-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let path = dir.join("boot-0000000000000001.img");
        let cache = BootImageCache::at(path.clone(), 1);

        let image = Engine::build_boot_image(false, kernel_sandbox);
        cache.store(&image, false, kernel_sandbox);
        let loaded = cache.load().expect("a stored image loads");
        let mut engine = Engine::from_boot_image(&loaded);
        assert_eq!(last(&mut engine, "(apply + (map add1 (list 1 2)))"), SteelVal::IntV(5));

        // Another key is another image
        assert!(BootImageCache::at(path.clone(), 2).load().is_none());

        // One changed byte in the contents is noticed
        let mut bytes = std::fs::read(&path).unwrap();
        let middle = bytes.len() / 2;
        bytes[middle] ^= 1;
        std::fs::write(&path, &bytes).unwrap();
        assert!(cache.load().is_none());

        // Only the most recent images are kept
        for key in 10..(10 + KEPT_IMAGES as u64 + 3) {
            std::fs::write(dir.join(format!("boot-{key:016x}.img")), b"old").unwrap();
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
        cache.store(&image, false, kernel_sandbox);
        let kept = std::fs::read_dir(&dir).unwrap().count();
        assert_eq!(kept, KEPT_IMAGES);
        assert!(cache.load().is_some(), "the image just stored is kept");

        let _ = std::fs::remove_dir_all(&dir);
    }

    // An engine from a boot image saved to bytes and read back does what one
    // from the image itself does
    #[test]
    fn a_saved_boot_image_makes_working_engines() {
        let kernel_sandbox = Kernel::default_sandbox();
        // Built here: the cached image may itself have been read back
        // (`STEEL_BOOT_IMAGE_ROUNDTRIP`), and one read back keeps no record to save
        let image = Engine::build_boot_image(false, kernel_sandbox);
        let bytes = SavedBootImage::save(&image, false, kernel_sandbox)
            .unwrap()
            .to_bytes()
            .unwrap();
        eprintln!("boot image: {} bytes", bytes.len());

        let loaded = SavedBootImage::from_bytes(&bytes).unwrap().load().unwrap();

        for _ in 0..2 {
            let mut engine = Engine::from_boot_image(&loaded);
            let checks: &[(&str, &str)] = &[
                ("(+ 1 2)", "3"),
                ("(map add1 (list 1 2 3))", "(2 3 4)"),
                ("(match (list 1 2 3) [(list a b c) (+ a b c)])", "6"),
                ("(define/contract (f x) (->/c int? int?) (+ x 1)) (f 41)", "42"),
                (
                    "(define-syntax (twice stx) (syntax-case stx () [(_ e) #'(begin e e)]))
                     (define n 0) (twice (set! n (+ n 1))) n",
                    "2",
                ),
                ("(struct point (x y)) (point-y (point 1 2))", "2"),
                ("(sort (list 3 1 2) <)", "(1 2 3)"),
                (
                    "(let ([p (open-output-string)]) (display \"hi\" p) (get-output-string p))",
                    "\"hi\"",
                ),
                ("(with-handler (lambda (e) 'caught) (car 1))", "caught"),
            ];
            for (program, expected) in checks {
                let value = last(&mut engine, program);
                assert_eq!(format!("{value:?}").trim_start_matches("'"), *expected, "{program}");
            }
        }
    }

    // The pass engine's image, as derived and as saved and read back, gives
    // engines with the built-in passes registered, each with its own globals
    #[test]
    fn the_pass_engine_image_registers_the_passes() {
        let kernel_sandbox = Kernel::default_sandbox();
        let base = Engine::build_boot_image(false, kernel_sandbox);
        let derived = Engine::derive_pass_image(&base).unwrap();
        let bytes = SavedDerivedImage::save(&derived, &base).unwrap().to_bytes().unwrap();
        eprintln!("pass engine image: {} bytes", bytes.len());
        let loaded = SavedDerivedImage::from_bytes(&bytes).unwrap().load(&base).unwrap();

        // Only over the image it was made from: another process's may lay its
        // kernel out otherwise
        let mut other = Engine::build_boot_image(false, kernel_sandbox);
        other.stored_as = Some(1);
        assert!(SavedDerivedImage::from_bytes(&bytes).unwrap().load(&other).is_err());

        for image in [&derived, &loaded] {
            let (mut a, results) = Engine::replay_boot_image(image);
            let passes_at = results.last().cloned().expect("the program's value");
            for (point, name) in [("anonymous-lets", "anonymous-lets"), ("lower-rest", "lower-rest")] {
                let found = a
                    .call_function_with_args(passes_at.clone(), vec![SteelVal::SymbolV(point.into())])
                    .unwrap();
                assert!(format!("{found}").contains(name), "{point}: {found}");
            }
            assert_eq!(last(&mut a, "(map add1 (list 1 2))").to_string(), "(2 3)");

            let (mut b, _) = Engine::replay_boot_image(image);
            last(&mut a, "(define only-in-a 1)");
            assert!(b.compile_and_run_raw_program("only-in-a".to_string()).is_err());
        }
    }

    // What a process looks like after making engines from the boot images, as
    // text: two processes that started from the same images, one that built
    // them and one that read them, give the same
    mod fingerprint {
        use super::*;

        // Programs compiled and run in a new engine: their bytecode and values
        // depend on the names, ids and macros left by the stdlib
        const PROGRAMS: &[&str] = &[
            "(define (f x) (+ x 1)) (f 41)",
            "(define-syntax swap! (syntax-rules () [(_ a b) (let ([t a]) (set! a b) (set! b t))]))
             (define p 1) (define q 2) (swap! p q) (list p q)",
            "(define-syntax (twice stx) (syntax-case stx () [(_ e) #'(begin e e)]))
             (define n 0) (twice (set! n (+ n 1))) n",
            "(match (list 1 (list 2 3)) [(list a (list b c)) (+ a b c)])",
            "(struct point (x y) #:transparent) (point-x (point 3 4))",
            "(struct cell (value) #:mutable) (define c (cell 1)) (set-cell-value! c 5) (cell-value c)",
            "(define/contract (g x) (->/c int? int?) (* x 2)) (g 21)",
            "(define (h #:key [k 1]) k) (h #:key 7)",
            "(let loop ([i 0] [acc '()]) (if (= i 5) (reverse acc) (loop (+ i 1) (cons i acc))))",
            "(define xs (list 1 2 3)) `(a ,@xs ,(car xs))",
            "(define ht (hash 'a 1 'b 2)) (hash-ref ht 'b)",
            "(call/cc (lambda (k) (+ 1 (k 10))))",
            "(with-handler (lambda (e) (error-object-message e)) (car 1))",
            "(define out (open-output-string)) (write (list 1 \"two\" #\\3) out) (get-output-string out)",
            "(define v (make-vector 3 0)) (vector-set! v 1 'x) v",
            "(eval '(+ 1 2))",
            "(require-builtin steel/strings) (string-upcase \"abc\")",
            "(define-values (a b) (values 1 2)) (+ a b)",
            "(define (fold-sum xs) (foldl + 0 xs)) (fold-sum (range 0 10))",
            "(assert! (equal? (map (lambda (x) (* x x)) '(1 2 3)) '(1 4 9)))",
            "(define cl (case-lambda [(x) (list 'one x)] [(x y) (list 'two x y)] [(x . rest) (list 'many x rest)]))
             (list (cl 1) (cl 1 2) (cl 1 2 3))",
            // Requires written by macro templates, plain and with a prefix
            "(require \"boot-check/macros\") (import-helper a 4) (import-prefixed b 5) (list a b)",
        ];

        // Modules the programs require
        const MODULES: &[(&str, &str)] = &[
            ("boot-check/helper", "(provide helper) (define (helper x) (* x 10))"),
            (
                "boot-check/macros",
                "(provide import-helper import-prefixed)
                 (define-syntax import-helper
                   (syntax-rules ()
                     [(_ name x) (begin (require \"boot-check/helper\") (define name (helper x)))]))
                 (define-syntax import-prefixed
                   (syntax-rules ()
                     [(_ name x) (begin (require (prefix-in h: \"boot-check/helper\"))
                                        (define name (h:helper x)))]))",
            ),
        ];

        // Sequences in the saved forms come from hash tables, so they're
        // compared as sorted
        fn canonical(value: serde_json::Value) -> serde_json::Value {
            use serde_json::Value;
            match value {
                Value::Array(items) => {
                    let mut items: Vec<Value> = items.into_iter().map(canonical).collect();
                    items.sort_by_cached_key(|item| item.to_string());
                    Value::Array(items)
                }
                Value::Object(map) => {
                    Value::Object(map.into_iter().map(|(k, v)| (k, canonical(v))).collect())
                }
                other => other,
            }
        }

        fn json(out: &mut String, section: &str, value: &impl Serialize) {
            let value = serde_json::to_value(value)
                .unwrap_or_else(|e| panic!("{section}: {e}"));
            out.push_str(&format!(
                "== {section}\n{}\n",
                serde_json::to_string_pretty(&canonical(value)).unwrap()
            ));
        }

        fn code(instructions: &[DenseInstruction]) -> String {
            instructions
                .iter()
                .map(|i| format!("{:?} {}", i.op_code, i.payload_size))
                .collect::<Vec<_>>()
                .join("; ")
        }

        fn describe(value: &SteelVal) -> String {
            match value {
                SteelVal::Closure(c) => {
                    format!("closure {} arity {}: {}", c.id, c.arity(), code(&c.body_exp()))
                }
                SteelVal::IntV(_)
                | SteelVal::NumV(_)
                | SteelVal::BoolV(_)
                | SteelVal::CharV(_)
                | SteelVal::StringV(_)
                | SteelVal::SymbolV(_)
                | SteelVal::Void => format!("{value}"),
                SteelVal::ListV(l) => format!(
                    "list [{}]",
                    l.iter().map(describe).collect::<Vec<_>>().join(", ")
                ),
                SteelVal::FuncV(_) | SteelVal::Custom(_) | SteelVal::BoxedFunction(_) => {
                    let text = format!("{value}");
                    text.chars().take(80).collect()
                }
                other => format!("{:?}", core::mem::discriminant(other)),
            }
        }

        fn globals(out: &mut String, section: &str, engine: &Engine) {
            out.push_str(&format!("== {section}\n"));
            let names = engine.virtual_machine.compiler.read().symbol_map.values().clone();
            let roots = engine.virtual_machine.global_env.roots();
            for (slot, name) in names.iter().enumerate() {
                // A struct type's key is its index in the process's table of
                // struct types, which building the image adds to first
                let value = if name.resolve().contains("struct-key") {
                    "a struct type key".into()
                } else {
                    roots.get(slot).map(describe).unwrap_or_else(|| "-".into())
                };
                out.push_str(&format!("{slot} {}: {value}\n", name.resolve()));
            }
        }

        fn compiler(out: &mut String, section: &str, engine: &Engine) {
            let compiler = engine.virtual_machine.compiler.read();
            // What the saved forms leave out, read without serde
            out.push_str(&format!("== {section} unsaved\n"));
            for (key, value) in compiler.unsaved_state() {
                out.push_str(&format!("{key}: {value}\n"));
            }
            json(out, &format!("{section} compiler"), &compiler.save());
            out.push_str(&format!("== {section} constants\n"));
            for index in 0..compiler.constant_map.len() {
                out.push_str(&format!("{index}: {}\n", describe(&compiler.constant_map.get(index))));
            }
            if let Some(kernel) = compiler.kernel.as_ref() {
                json(out, &format!("{section} kernel"), &kernel.save());
                json(
                    out,
                    &format!("{section} kernel compiler"),
                    &kernel.engine.virtual_machine.compiler.read().save(),
                );
                globals(out, &format!("{section} kernel globals"), &kernel.engine);
            }
        }

        // Syntax object ids, and names made from them, depend on how many syntax
        // objects the process made before (copying syntax gives it new ids, and
        // reading an image copies some): masked. A struct type's key likewise.
        fn masked(text: String) -> String {
            const MARKERS: [&str; 5] = [
                "\"syntax_object_id\": ",
                "\"syntax_object_ids\": ",
                "closure-lifting-",
                "##packed#",
                "##__lifted_pure_function",
            ];
            let bytes = text.as_bytes();
            let mut out = String::with_capacity(text.len());
            let mut start = 0;
            let mut i = 0;
            while i < bytes.len() {
                let Some(marker) = MARKERS.iter().find(|m| bytes[i..].starts_with(m.as_bytes()))
                else {
                    i += 1;
                    continue;
                };
                let end = i + marker.len();
                let digits = bytes[end..].iter().take_while(|b| b.is_ascii_digit()).count();
                out.push_str(&text[start..end]);
                if digits > 0 {
                    out.push('N');
                }
                start = end + digits;
                i = start;
            }
            out.push_str(&text[start..]);
            out
        }

        pub(super) fn take() -> String {
            masked(take_raw())
        }

        fn take_raw() -> String {
            let mut out = String::new();
            let mut engine = Engine::new();

            let process = SavedProcess::save();
            json(&mut out, "process", &process);
            let modules: Vec<InternedString> = engine
                .virtual_machine
                .compiler
                .read()
                .module_manager
                .modules()
                .keys()
                .filter_map(|path| path.to_str())
                .map(InternedString::from)
                .collect();
            json(&mut out, "expander", &ThreadExpanderState::capture(modules, &Default::default()));
            out.push_str("== thread state\n");
            for (key, value) in crate::parser::expand_visitor::thread_state_for_tests()
                .into_iter()
                .chain(crate::parser::spec_requests::thread_state_for_tests())
                .chain(crate::compiler::passes::pass_queries::thread_state_for_tests())
                .chain(crate::compiler::modules::thread_state_for_tests())
                .chain(crate::parser::kernel::thread_state_for_tests())
            {
                out.push_str(&format!("{key}: {value}\n"));
            }
            globals(&mut out, "engine globals", &engine);
            compiler(&mut out, "engine", &engine);

            if let Some(loaded) = Engine::pass_engine() {
                let (pass_engine, passes_at) = loaded.unwrap();
                out.push_str(&format!("== passes-at\n{}\n", describe(&passes_at)));
                globals(&mut out, "pass engine globals", &pass_engine);
                compiler(&mut out, "pass engine", &pass_engine);
            }

            for (name, source) in MODULES {
                engine.register_steel_module(name.to_string(), source.to_string());
            }
            for program in PROGRAMS {
                out.push_str(&format!("== program {program}\n"));
                // Run first: compiling a require without running it leaves the
                // module marked loaded but never run
                let values = engine
                    .compile_and_run_raw_program(program.to_string())
                    .unwrap_or_else(|e| panic!("{program}: {e}"));
                let raw = engine
                    .emit_raw_program_no_path(program.to_string())
                    .unwrap_or_else(|e| panic!("{program}: {e}"));
                for instructions in &raw.instructions {
                    out.push_str(&crate::core::instructions::disassemble(instructions));
                    out.push('\n');
                }
                out.push_str(&format!(
                    "=> {}\n",
                    values.iter().map(describe).collect::<Vec<_>>().join(", ")
                ));
            }
            globals(&mut out, "engine globals after the programs", &engine);
            out.push_str("== engine unsaved after the programs\n");
            for (key, value) in engine.virtual_machine.compiler.read().unsaved_state() {
                out.push_str(&format!("{key}: {value}\n"));
            }
            json(&mut out, "process after the programs", &SavedProcess::save());
            out
        }
    }

    // A process that reads the boot images from the cache is the process that
    // built them: same counters and tables, same engines, same compiled code.
    // Catches state an image doesn't save or restore. Two child processes of
    // this test share an empty cache: the first builds the images, the second
    // reads them.
    #[test]
    fn a_loaded_boot_image_is_the_built_one() {
        const TEST: &str = "steel_vm::engine::boot_image::tests::a_loaded_boot_image_is_the_built_one";
        if let Some(out) = std::env::var_os("BOOT_CHECK_OUT") {
            let text = fingerprint::take();
            let read = crate::steel_vm::engine::IMAGES_READ.load(core::sync::atomic::Ordering::Relaxed);
            std::fs::write(&out, format!("read {read}\n{text}")).unwrap();
            return;
        }

        let dir = std::env::temp_dir().join(format!("steel-boot-check-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let run = |role: &str| -> (usize, String) {
            let out = dir.join(format!("{role}.txt"));
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args(["--exact", TEST, "--nocapture", "--test-threads=1"])
                .env("BOOT_CHECK_OUT", &out)
                // Compiled where it's called: a function the jit compiled has
                // its first instruction replaced, so the background jit getting
                // to it first in one process would show as a difference
                .env("STEEL_JIT_BACKGROUND", "0")
                .env("STEEL_BOOT_IMAGE_CACHE", "1")
                .env("STEEL_BOOT_IMAGE_DIR", dir.join("cache"))
                .status()
                .unwrap();
            assert!(status.success(), "the {role} process failed");
            let text = std::fs::read_to_string(&out).unwrap();
            let (read, rest) = text.split_once('\n').unwrap();
            (read.trim_start_matches("read ").parse().unwrap(), rest.to_string())
        };

        let (built_read, built) = run("built");
        // The image files, not the other caches kept beside them
        let images = std::fs::read_dir(dir.join("cache"))
            .unwrap()
            .filter(|e| e.as_ref().is_ok_and(|e| e.path().extension().is_some_and(|x| x == "img")))
            .count();
        let (loaded_read, loaded) = run("loaded");
        assert_eq!(built_read, 0, "the first process builds the images");
        assert!(images >= 1);
        assert_eq!(loaded_read, images, "the second process reads every image");

        if built != loaded {
            // The first section that differs, and its first differing line
            let sections = |text: &str| -> Vec<(String, Vec<String>)> {
                let mut sections: Vec<(String, Vec<String>)> = Vec::new();
                for line in text.lines() {
                    match line.strip_prefix("== ") {
                        Some(name) => sections.push((name.to_string(), Vec::new())),
                        None => match sections.last_mut() {
                            Some((_, lines)) => lines.push(line.to_string()),
                            None => sections.push((String::new(), vec![line.to_string()])),
                        },
                    }
                }
                sections
            };
            let (built_sections, loaded_sections) = (sections(&built), sections(&loaded));
            let mut report = Vec::new();
            for (b, l) in built_sections.iter().zip(&loaded_sections) {
                if b == l {
                    continue;
                }
                let first = b.1.iter().zip(&l.1).position(|(x, y)| x != y);
                report.push(match first {
                    Some(at) => format!(
                        "{} (line {at}):\n  built:  {}\n  loaded: {}",
                        b.0, b.1[at], l.1[at]
                    ),
                    None => format!("{} / {}: {} vs {} lines", b.0, l.0, b.1.len(), l.1.len()),
                });
            }
            panic!(
                "a loaded boot image differs from the built one, fingerprints in {}:\n{}",
                dir.display(),
                report.join("\n")
            );
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}
