//! Running compiler passes written in Steel (STEEL_PASSES_IN_STEEL_PLAN.md).
//!
//! Passes register themselves with `#%private/steel/passes` in an engine of
//! their own (not the kernel: it can't run procedural macros, and passes want
//! `match-syntax`), and run at the hook points in
//! `Compiler::lower_expressions_impl`. Each has a mode:
//!
//! - `off`: doesn't run;
//! - `steel`: the Steel pass's result is compiled;
//! - `compare`: the Steel pass and its Rust version both run on the same
//!   input, the Rust result is compiled, and any difference is reported
//!   (an error with `STEEL_PASS_COMPARE_FAIL`);
//! - `rust`: only the Rust version runs.
//!
//! `STEEL_PASSES=name=mode,...` (`*` for every pass) overrides the mode a
//! pass registered with; `STEEL_PASS_MODULES=file,...` loads more passes.
//! Built-in passes default to `steel`, so compilers with a kernel load the pass
//! engine unless `STEEL_PASSES=*=rust` (or `*=off`). Compilers on a thread
//! share one pass engine, unless a test gave one its own pass modules.
//!
//! Passes ported from Rust (`anonymous-lets`, `empty-lets`) run at sites: the
//! place in the compiler where the Rust version runs, which `run_site` then
//! runs, replaces or compares with by the pass's mode.
//!
//! A Steel pass that raises, or returns something that isn't a program, is
//! reported once and its input is kept. Passes that fail to load are reported
//! and switched off.

use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::ast::ExprKind;

use crate::compiler::passes::syntax_bridge::{shape_difference, SyntaxBridge};
use crate::gc::Gc;
use crate::parser::kernel::Kernel;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::steel_vm::engine::Engine;

type LoadedEngine = std::sync::Arc<std::sync::Mutex<(Engine, SteelVal)>>;

std::thread_local! {
    // Set while the pass engine is made or used: code it compiles doesn't run passes
    static IN_PASS_ENGINE: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };

    // Set while an engine loads its prelude, which is compiled with the Rust passes
    static RUST_PASSES_ONLY: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };

    // The pass engine compilers on this thread share when they load only the
    // built-in passes and STEEL_PASS_MODULES, or why it didn't load. Never
    // dropped: an engine dropped while the thread exits can reach thread locals
    // already gone.
    static SHARED_ENGINE: core::mem::ManuallyDrop<core::cell::RefCell<Option<core::result::Result<LoadedEngine, String>>>> =
        const { core::mem::ManuallyDrop::new(core::cell::RefCell::new(None)) };
}

pub(crate) struct InPassEngine(bool);

impl InPassEngine {
    pub(crate) fn enter() -> Self {
        InPassEngine(IN_PASS_ENGINE.with(|flag| flag.replace(true)))
    }
}

impl Drop for InPassEngine {
    fn drop(&mut self) {
        let was = self.0;
        IN_PASS_ENGINE.with(|flag| flag.set(was));
    }
}

/// Runs `f` with compilation using only the Rust passes, unless
/// `STEEL_PASSES_ON_PRELUDE` is set. For the prelude: the Rust passes give the
/// same bytecode, and the Steel ones (and loading their engine) take most of the
/// time an engine takes to start.
pub(crate) fn with_rust_passes<T>(f: impl FnOnce() -> T) -> T {
    if crate::config::config().passes_on_prelude {
        return f();
    }

    struct Restore(bool);

    impl Drop for Restore {
        fn drop(&mut self) {
            let was = self.0;
            RUST_PASSES_ONLY.with(|flag| flag.set(was));
        }
    }

    let _restore = Restore(RUST_PASSES_ONLY.with(|flag| flag.replace(true)));
    f()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PassMode {
    Off,
    Steel,
    Compare,
    Rust,
}

impl PassMode {
    fn parse(name: &str) -> Option<Self> {
        Some(match name {
            "off" => PassMode::Off,
            "steel" => PassMode::Steel,
            "compare" => PassMode::Compare,
            "rust" => PassMode::Rust,
            _ => return None,
        })
    }
}

type RustPass = fn(Vec<ExprKind>) -> Result<Vec<ExprKind>>;

// The Rust version of each pass that has one, for `compare` and `rust`
fn rust_version(name: &str) -> Option<RustPass> {
    match name {
        "identity" => Some(Ok),
        _ => None,
    }
}

#[derive(Clone)]
struct Pass {
    name: String,
    default: PassMode,
    procedure: SteelVal,
}

#[derive(Clone, Default)]
pub(crate) struct SteelPasses {
    modes: FxHashMap<String, PassMode>,
    // The mode for every pass `modes` doesn't name (`*=...`)
    all: Option<PassMode>,
    // Modules of passes to load: file paths, or names in `sources`
    modules: Vec<String>,
    // Modules given as source, registered with the pass engine under their name
    sources: Vec<(String, String)>,
    compare_fails: bool,
    // The engine passes run in, with `passes-at`, once loaded
    engine: Option<LoadedEngine>,
    // Loading failed: passes stay off
    failed: bool,
    warned: FxHashSet<String>,
}

impl SteelPasses {
    pub(crate) fn from_config() -> Self {
        let config = crate::config::config();
        let mut passes = SteelPasses {
            compare_fails: config.pass_compare_fail,
            ..Default::default()
        };
        if let Some(setting) = &config.passes {
            for entry in setting.split(',').map(str::trim).filter(|e| !e.is_empty()) {
                let parsed = entry
                    .split_once('=')
                    .and_then(|(name, mode)| Some((name.trim(), PassMode::parse(mode.trim())?)));
                match parsed {
                    Some((name, mode)) => passes.set_mode(name, mode),
                    None => eprintln!(
                        "STEEL_PASSES: `{entry}` isn't `name=mode` with a mode of off, steel, compare or rust"
                    ),
                }
            }
        }
        if let Some(modules) = &config.pass_modules {
            passes.modules.extend(
                modules
                    .split(',')
                    .map(str::trim)
                    .filter(|m| !m.is_empty())
                    .map(|m| {
                        std::fs::canonicalize(m)
                            .map(|p| p.to_string_lossy().into_owned())
                            .unwrap_or_else(|_| m.to_string())
                    }),
            );
        }
        passes
    }

    /// Whether compiling would load the pass engine with only the built-in
    /// passes, going by the settings
    pub(crate) fn builtin_engine_wanted() -> bool {
        let config = crate::config::config();
        if config.pass_modules.as_deref().is_some_and(|m| !m.trim().is_empty()) {
            return false;
        }
        let mut passes = SteelPasses::default();
        for entry in config.passes.iter().flat_map(|s| s.split(',')) {
            if let Some((name, mode)) = entry
                .split_once('=')
                .and_then(|(name, mode)| Some((name.trim(), PassMode::parse(mode.trim())?)))
            {
                passes.set_mode(name, mode);
            }
        }
        passes.wanted()
    }

    pub(crate) fn set_mode(&mut self, name: &str, mode: PassMode) {
        if name == "*" {
            self.all = Some(mode);
        } else {
            self.modes.insert(name.to_string(), mode);
        }
    }

    pub(crate) fn set_compare_fails(&mut self, fails: bool) {
        self.compare_fails = fails;
    }

    /// A module of passes, given as source, to load as `name`
    pub(crate) fn add_module_source(&mut self, name: String, source: String) {
        self.modules.push(name.clone());
        self.sources.push((name, source));
        // Loaded again with the next run
        self.engine = None;
        self.failed = false;
    }

    // Whether a Steel pass could run, so the registry is worth loading. The
    // built-in passes may default to `steel`, so only settings that turn
    // every pass off or to Rust avoid it.
    fn wanted(&self) -> bool {
        let steel = |mode: &PassMode| matches!(mode, PassMode::Steel | PassMode::Compare);
        !self.modules.is_empty()
            || self.modes.values().any(steel)
            || self.all.is_none_or(|mode| steel(&mode))
    }

    // Whether passes can run in this compiler now
    fn available(&self, kernel: bool) -> bool {
        kernel
            && self.wanted()
            && !self.failed
            && !IN_PASS_ENGINE.with(|f| f.get())
            && !RUST_PASSES_ONLY.with(|f| f.get())
    }

    fn mode(&self, pass: &Pass) -> PassMode {
        self.modes
            .get(&pass.name)
            .copied()
            .or(self.all)
            .unwrap_or(pass.default)
    }

    fn warn_once(&mut self, key: String, message: impl FnOnce() -> String) {
        if self.warned.insert(key) {
            eprintln!("{}", message());
        }
    }

    // The pass engine, with the registry and the pass modules loaded
    fn load(&mut self) -> Result<LoadedEngine> {
        if let Some(engine) = &self.engine {
            return Ok(engine.clone());
        }
        if self.sources.is_empty() {
            let shared = SHARED_ENGINE.with(|shared| shared.borrow().clone());
            let shared = match shared {
                Some(shared) => shared,
                None => {
                    let loaded = self.load_engine().map_err(|err| err.to_string());
                    SHARED_ENGINE.with(|shared| *shared.borrow_mut() = Some(loaded.clone()));
                    if let Err(err) = &loaded {
                        eprintln!("The Steel compiler passes didn't load, so none run: {err}");
                    }
                    loaded
                }
            };
            let engine = match shared {
                Ok(engine) => engine,
                Err(err) => {
                    // Reported when it happened
                    self.failed = true;
                    return Err(SteelErr::new(ErrorKind::Generic, err));
                }
            };
            self.engine = Some(engine.clone());
            return Ok(engine);
        }
        let engine = self.load_engine()?;
        self.engine = Some(engine.clone());
        Ok(engine)
    }

    fn load_engine(&self) -> Result<LoadedEngine> {
        let _in_pass_engine = InPassEngine::enter();
        // The built-in passes alone: from an image made once
        if self.sources.is_empty() && self.modules.is_empty() {
            if let Some(loaded) = Engine::pass_engine() {
                let (engine, passes_at) = loaded?;
                return Ok(std::sync::Arc::new(std::sync::Mutex::new((engine, passes_at))));
            }
        }
        let mut engine = Engine::new();
        engine.register_module(pass_support_module());
        for (name, source) in &self.sources {
            engine.register_steel_module(name.clone(), source.clone());
        }
        let mut program = String::from(
            crate::steel_vm::engine::PASS_ENGINE_PROGRAM
                .strip_suffix("passes-at\n")
                .unwrap(),
        );
        for module in &self.modules {
            program.push_str(&format!("(require {module:?})\n"));
        }
        program.push_str("passes-at\n");
        let loaded = engine.compile_and_run_raw_program(program)?;
        let passes_at = loaded.last().cloned().ok_or_else(|| {
            SteelErr::new(ErrorKind::Generic, "the pass registry didn't load".into())
        })?;
        Ok(std::sync::Arc::new(std::sync::Mutex::new((engine, passes_at))))
    }

    fn passes(
        &mut self,
        engine: &mut Engine,
        passes_at: &SteelVal,
        point: &str,
    ) -> Result<Vec<Pass>> {
        let found = engine
            .call_function_with_args(passes_at.clone(), vec![SteelVal::SymbolV(point.into())])?;
        let SteelVal::ListV(found) = found else {
            return Ok(Vec::new());
        };
        let mut passes = Vec::new();
        for entry in found.iter() {
            let SteelVal::ListV(fields) = entry else {
                continue;
            };
            let fields: Vec<&SteelVal> = fields.iter().collect();
            if let [SteelVal::SymbolV(name), SteelVal::SymbolV(default), procedure] = fields[..] {
                passes.push(Pass {
                    name: name.as_str().to_string(),
                    default: PassMode::parse(default.as_str()).unwrap_or(PassMode::Off),
                    procedure: procedure.clone(),
                });
            }
        }
        Ok(passes)
    }

    // The Steel pass on `exprs`, or why it couldn't
    fn run_steel(
        engine: &mut Engine,
        pass: &Pass,
        point: &str,
        exprs: &[ExprKind],
    ) -> Result<Vec<ExprKind>> {
        let mut bridge = SyntaxBridge::new();
        let syntax = bridge.encode_program(exprs)?;
        let result = {
            let _facts = crate::compiler::passes::pass_queries::enter(exprs, &mut bridge);
            engine.call_function_with_args(pass.procedure.clone(), vec![syntax, context(point)])
        };
        bridge.decode_program(&result?)
    }

    // Passes one after another on the same syntax: the program goes to syntax
    // once and comes back once. A pass after the first sees what the one
    // before returned, and its queries answer about that. Any failure fails
    // the whole chain, so the caller can run the passes one at a time.
    fn run_chain(
        engine: &mut Engine,
        passes: &[&Pass],
        point: &str,
        exprs: &[ExprKind],
    ) -> Result<Vec<ExprKind>> {
        let mut bridge = SyntaxBridge::new();
        let mut syntax = bridge.encode_program(exprs)?;
        for (i, pass) in passes.iter().enumerate() {
            let result = {
                let _facts = if i == 0 {
                    crate::compiler::passes::pass_queries::enter(exprs, &mut bridge)
                } else {
                    crate::compiler::passes::pass_queries::enter_pending(&syntax, &mut bridge)
                };
                engine.call_function_with_args(
                    pass.procedure.clone(),
                    vec![syntax.clone(), context(point)],
                )?
            };
            if !matches!(result, SteelVal::ListV(_)) {
                return Err(SteelErr::new(
                    ErrorKind::ConversionError,
                    format!("the compiler pass {} returned {result}, not a program", pass.name),
                ));
            }
            syntax = result;
        }
        bridge.decode_program(&syntax)
    }

    // The Steel pass on `exprs`, warning and keeping them if it fails
    fn run_steel_or_keep(
        &mut self,
        engine: &mut Engine,
        pass: &Pass,
        point: &str,
        exprs: Vec<ExprKind>,
    ) -> Vec<ExprKind> {
        match Self::run_steel(engine, pass, point, &exprs) {
            Ok(changed) => changed,
            Err(err) => {
                self.warn_once(pass.name.clone(), || {
                    format!(
                        "The compiler pass {} failed at {point}, so it was skipped: {err}",
                        pass.name
                    )
                });
                exprs
            }
        }
    }

    // Several Steel passes in a row: as a chain, or one at a time if the
    // chain fails, so a failing pass is found and skipped
    fn run_steel_group(
        &mut self,
        engine: &mut Engine,
        group: &[&Pass],
        point: &str,
        exprs: Vec<ExprKind>,
    ) -> Vec<ExprKind> {
        if group.len() > 1 {
            if let Ok(changed) = Self::run_chain(engine, group, point, &exprs) {
                return changed;
            }
        }
        group
            .iter()
            .fold(exprs, |exprs, pass| self.run_steel_or_keep(engine, pass, point, exprs))
    }

    /// The passes registered at `point`, each by its mode
    pub(crate) fn run(
        &mut self,
        kernel: Option<&mut Kernel>,
        point: &str,
        mut exprs: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        // A compiler without a kernel is a kernel's own, and code the pass
        // engine compiles doesn't run passes either
        if !self.available(kernel.is_some()) {
            return Ok(exprs);
        }
        let Some(loaded) = self.loaded() else {
            return Ok(exprs);
        };
        let _in_pass_engine = InPassEngine::enter();
        let mut guard = loaded.lock().unwrap();
        let (engine, passes_at) = &mut *guard;
        let passes_at = passes_at.clone();
        let Some(passes) = self.passes_or_warn(engine, &passes_at, point) else {
            return Ok(exprs);
        };

        let mut i = 0;
        while i < passes.len() {
            let pass = &passes[i];
            match self.mode(pass) {
                PassMode::Off => {}
                PassMode::Rust => {
                    if let Some(rust) = rust_version(&pass.name) {
                        exprs = rust(exprs)?;
                    }
                }
                PassMode::Steel => {
                    // Passes in a row in steel mode share one round trip
                    let group: Vec<&Pass> = passes[i..]
                        .iter()
                        .take_while(|pass| self.mode(pass) == PassMode::Steel)
                        .collect();
                    i += group.len();
                    exprs = self.run_steel_group(engine, &group, point, exprs);
                    continue;
                }
                PassMode::Compare => {
                    let Some(rust) = rust_version(&pass.name) else {
                        self.warn_once(format!("{} compare", pass.name), || {
                            format!(
                                "The compiler pass {} has no Rust version to compare with, so only the Steel one runs",
                                pass.name
                            )
                        });
                        if let Ok(changed) = Self::run_steel(engine, pass, point, &exprs) {
                            exprs = changed;
                        }
                        i += 1;
                        continue;
                    };
                    let steel = Self::run_steel(engine, pass, point, &exprs);
                    let input = exprs.clone();
                    let rust = rust(exprs)?;
                    self.report_comparison(&pass.name, point, &input, &rust, steel)?;
                    exprs = rust;
                }
            }
            i += 1;
        }
        Ok(exprs)
    }
}

impl SteelPasses {
    // A compare run: the Rust result against the Steel one, on `input`
    fn report_comparison(
        &mut self,
        name: &str,
        point: &str,
        input: &[ExprKind],
        rust: &[ExprKind],
        steel: Result<Vec<ExprKind>>,
    ) -> Result<()> {
        let problem = match &steel {
            Ok(steel) => shape_difference(input, rust, steel)
                .map(|difference| format!("differs from its Rust version at {difference}")),
            Err(err) => Some(format!("failed: {err}")),
        };
        if let Some(problem) = problem {
            let message = format!("The Steel version of the compiler pass {name} at {point} {problem}");
            if self.compare_fails {
                return Err(SteelErr::new(ErrorKind::Generic, message));
            }
            eprintln!("{message}");
        }
        Ok(())
    }

    /// Passes with versions in Rust and Steel whose sites come one after
    /// another, each with its Rust version: run in order, each by its mode.
    /// Steel ones in a row share one round trip to syntax.
    pub(crate) fn run_sites(
        &mut self,
        kernel: bool,
        exprs: &mut Vec<ExprKind>,
        sites: &[(&str, &dyn Fn(&mut Vec<ExprKind>) -> bool)],
    ) -> Result<()> {
        let run_rust = |exprs: &mut Vec<ExprKind>| {
            for (_, rust) in sites {
                rust(exprs);
            }
        };
        if !self.available(kernel) {
            run_rust(exprs);
            return Ok(());
        }
        let Some(loaded) = self.loaded() else {
            run_rust(exprs);
            return Ok(());
        };
        let _in_pass_engine = InPassEngine::enter();
        let mut guard = loaded.lock().unwrap();
        let (engine, passes_at) = &mut *guard;
        let passes_at = passes_at.clone();

        // Each site's pass, if Steel has one, and its mode
        let mut plan = Vec::with_capacity(sites.len());
        for (site, rust) in sites {
            let pass = self
                .passes_or_warn(engine, &passes_at, site)
                .and_then(|passes| passes.into_iter().find(|pass| pass.name == *site));
            let mode = pass.as_ref().map_or(PassMode::Rust, |pass| self.mode(pass));
            plan.push((*site, *rust, pass, mode));
        }

        let mut i = 0;
        while i < plan.len() {
            let (site, rust, pass, mode) = &plan[i];
            match (mode, pass) {
                (PassMode::Off, _) => {}
                (PassMode::Steel, Some(_)) => {
                    let group: Vec<&Pass> = plan[i..]
                        .iter()
                        .take_while(|(_, _, pass, mode)| *mode == PassMode::Steel && pass.is_some())
                        .map(|(_, _, pass, _)| pass.as_ref().unwrap())
                        .collect();
                    let rusts: Vec<&dyn Fn(&mut Vec<ExprKind>) -> bool> =
                        plan[i..i + group.len()].iter().map(|(_, rust, _, _)| *rust).collect();
                    i += group.len();
                    let input = core::mem::take(exprs);
                    if group.len() > 1 {
                        if let Ok(changed) = Self::run_chain(engine, &group, site, &input) {
                            *exprs = changed;
                            continue;
                        }
                    }
                    // One at a time; a Steel version that fails gives way to its
                    // Rust version
                    *exprs = input;
                    for (pass, rust) in group.iter().zip(rusts) {
                        match Self::run_steel(engine, pass, &pass.name, exprs) {
                            Ok(changed) => *exprs = changed,
                            Err(err) => {
                                self.warn_once(pass.name.clone(), || {
                                    format!(
                                        "The compiler pass {} failed, so its Rust version ran instead: {err}",
                                        pass.name
                                    )
                                });
                                rust(exprs);
                            }
                        }
                    }
                    continue;
                }
                (PassMode::Compare, Some(pass)) => {
                    let steel = Self::run_steel(engine, pass, site, exprs);
                    let input = exprs.clone();
                    rust(exprs);
                    self.report_comparison(site, site, &input, exprs, steel)?;
                }
                _ => {
                    rust(exprs);
                }
            }
            i += 1;
        }
        Ok(())
    }

    fn loaded(&mut self) -> Option<LoadedEngine> {
        let shared = self.sources.is_empty();
        match self.load() {
            Ok(loaded) => Some(loaded),
            Err(err) => {
                self.failed = true;
                // The shared engine's failure is reported once, by load
                if !shared {
                    eprintln!("The Steel compiler passes didn't load, so none run: {err}");
                }
                None
            }
        }
    }

    fn passes_or_warn(
        &mut self,
        engine: &mut Engine,
        passes_at: &SteelVal,
        point: &str,
    ) -> Option<Vec<Pass>> {
        match self.passes(engine, passes_at, point) {
            Ok(passes) => Some(passes),
            Err(err) => {
                self.warn_once(format!("passes at {point}"), || {
                    format!("Couldn't list the compiler passes at {point}: {err}")
                });
                None
            }
        }
    }

    /// A pass with versions in Rust and Steel, at the place in the compiler
    /// where its Rust version runs (`site` is its name). `None` when the Rust
    /// version runs as it always has (its mode is `rust`, or there are no
    /// Steel passes here); otherwise the program has been changed by the mode
    /// (`steel`, `compare`, or `off`: neither runs), and whether it changed.
    pub(crate) fn run_site(
        &mut self,
        kernel: bool,
        site: &str,
        exprs: &mut Vec<ExprKind>,
        rust: impl FnOnce(&mut Vec<ExprKind>) -> bool,
    ) -> Result<Option<bool>> {
        if !self.available(kernel) {
            return Ok(None);
        }
        let Some(loaded) = self.loaded() else {
            return Ok(None);
        };
        let _in_pass_engine = InPassEngine::enter();
        let mut guard = loaded.lock().unwrap();
        let (engine, passes_at) = &mut *guard;
        let passes_at = passes_at.clone();
        let Some(passes) = self.passes_or_warn(engine, &passes_at, site) else {
            return Ok(None);
        };
        let Some(pass) = passes.into_iter().find(|pass| pass.name == site) else {
            return Ok(None);
        };

        match self.mode(&pass) {
            PassMode::Rust => Ok(None),
            PassMode::Off => Ok(Some(false)),
            PassMode::Steel => match Self::run_steel(engine, &pass, site, exprs) {
                Ok(changed) => {
                    *exprs = changed;
                    Ok(Some(true))
                }
                Err(err) => {
                    self.warn_once(pass.name.clone(), || {
                        format!(
                            "The compiler pass {site} failed, so its Rust version ran instead: {err}"
                        )
                    });
                    Ok(None)
                }
            },
            PassMode::Compare => {
                let steel = Self::run_steel(engine, &pass, site, exprs);
                let input = exprs.clone();
                let changed = rust(exprs);
                self.report_comparison(site, site, &input, exprs, steel)?;
                Ok(Some(changed))
            }
        }
    }
}

// Rust helpers for passes, only in the pass engine. `#%private/steel/passes`
// provides them.
pub(crate) fn pass_support_module() -> crate::steel_vm::builtin::BuiltInModule {
    use crate::rvals::Syntax;
    use crate::steel_vm::register_fn::RegisterFn;

    let mut module = crate::steel_vm::builtin::BuiltInModule::new("#%private/steel/pass-support");
    // `(rebuild form parts)`: `form` with `parts` instead, keeping its span,
    // scopes and its place in the program
    module.register_fn(
        "rebuild",
        |form: SteelVal, parts: SteelVal| -> Result<SteelVal> {
            let form = match form {
                SteelVal::SyntaxObject(form) => form,
                // A list a pass wrote as plain data, which a pass after it in a
                // chain gets as it was
                SteelVal::ListV(_) => {
                    return match parts {
                        SteelVal::ListV(_) => Ok(parts),
                        other => Err(SteelErr::new(
                            ErrorKind::TypeMismatch,
                            format!("rebuild: expects a list of parts, found {other}"),
                        )),
                    }
                }
                other => {
                    return Err(SteelErr::new(
                        ErrorKind::TypeMismatch,
                        format!("rebuild: expects syntax, found {other}"),
                    ))
                }
            };
            let parts = match parts {
                SteelVal::ListV(_) | SteelVal::VectorV(_) => parts,
                other => {
                    return Err(SteelErr::new(
                        ErrorKind::TypeMismatch,
                        format!("rebuild: expects a list of parts, found {other}"),
                    ))
                }
            };
            Ok(Syntax::rebuilt(&form, parts).into())
        },
    );
    // What a pass can ask about its program: pass_queries.rs
    crate::compiler::passes::pass_queries::register(&mut module);
    // `(constant-primitive name)`: the primitive `name` (a `#%prim.` name) if
    // calling it with literals can be done at compile time, else #f
    module.register_fn("constant-primitive", |name: SteelVal| match name {
        SteelVal::SymbolV(name) => crate::steel_vm::primitives::constant_primitives()
            .get(&steel_parser::interner::InternedString::from(name.as_str()))
            .cloned()
            .unwrap_or(SteelVal::BoolV(false)),
        _ => SteelVal::BoolV(false),
    });
    // `(program-literal? value)`: whether `value` can be put in the program
    // as a literal
    module.register_fn("program-literal?", |value: SteelVal| {
        matches!(
            value,
            SteelVal::BoolV(_)
                | SteelVal::IntV(_)
                | SteelVal::NumV(_)
                | SteelVal::CharV(_)
                | SteelVal::StringV(_)
        )
    });
    module
}

// What a pass gets besides the program: for now, where it runs
fn context(point: &str) -> SteelVal {
    let mut context = crate::values::HashMap::new();
    context.insert(
        SteelVal::SymbolV("point".into()),
        SteelVal::SymbolV(point.into()),
    );
    SteelVal::HashMapV(Gc::new(context).into())
}
