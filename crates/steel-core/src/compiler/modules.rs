use crate::rvals::Result;
use crate::{
    compiler::{
        passes::{analysis::SemanticAnalysis, VisitorMutRefUnit},
        program::PROVIDE,
    },
    parser::{
        ast::{Atom, Begin, Define, ExprKind, List, Macro, PatternPair, Quote, SyntaxRules},
        expand_visitor::{expand_foreign, ExpanderMany, GlobalMap, RequiredMacroMap},
        interner::InternedString,
        kernel::{Kernel, MacroModule, Phase1Import, RenameInstance},
        parser::{
            lower_entire_ast, lower_macro_and_require_definitions, ParseError, Parser, Sources,
            SyntaxObject,
        },
        tokens::TokenType,
    },
    steel_vm::{
        engine::{default_prelude_macros, ModuleContainer},
        transducers::interleave,
    },
};

use compact_str::CompactString;
use crossbeam_channel::Sender;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rustc_hash::{FxHashMap, FxHashSet};
// use smallvec::SmallVec;
use steel_parser::{ast::PROTO_HASH_GET, expr_list, parser::SourceId, span::Span};

use thin_vec::{thin_vec, ThinVec};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    io::Read,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use crate::time::SystemTime;

use crate::parser::expand_visitor::{
    define_generated_macros, expand, extract_macro_defs, lift_macro_defs_from_begins,
    MacroRedefinitions,
};

use super::{
    compiler::KernelDefMacroSpec,
    passes::{
        analysis::is_a_builtin_definition,
        begin::FlattenBegin,
        mangle::{collect_globals, NameMangler},
    },
    program::{BEGIN, FOR_SYNTAX, ONLY_IN, PREFIX_IN, REQUIRE_IDENT_SPEC},
};

macro_rules! time {
    ($label:expr, $e:expr) => {{
        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let e = $e;

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "{}: {:?}", $label, now.elapsed());

        e
    }};
}

macro_rules! declare_builtins {
    ( $( $name:expr => $path:expr ), *) => {
        pub static BUILT_INS: &[(&str, &str)] = &[
            $( ($name, include_str!($path)), )*
        ];

        // TODO: Why was this an issue? Bugs with static string internment issues?
        pub(crate) fn intern_modules() {
            $(
                let _ = InternedString::from($name);
                let _ = InternedString::from($path);
            )*
        }
    };
}

pub(crate) const MANGLER_SEPARATOR: &str = "__%#__";

macro_rules! create_prelude {
    (
        $( $module:literal, )*
        $( for_syntax $module_for_syntax:literal ),*
    ) => {
        pub static PRELUDE_MODULES: &[&str] = &[
            $( $module, )*
            $( $module_for_syntax, )*
        ];

        pub static PRELUDE_WITHOUT_BASE: &str = concat!(
            $( "(require \"", $module, "\")\n", )*
            $( "(require (for-syntax \"", $module_for_syntax, "\"))\n", )*
        );

        pub static PRELUDE_STRING: &str = concat!(
            "(require-builtin steel/base)\n",
            $( "(require \"", $module, "\")\n", )*
            $( "(require (for-syntax \"", $module_for_syntax, "\"))\n", )*
        );
    }
}

// TODO: These need to be set up and interned in a stable position.
declare_builtins!(
    "steel/option" => "../scheme/modules/option.scm",
    "steel/result" => "../scheme/modules/result.scm",
    "steel/iterators" => "../scheme/modules/iterators.scm",
    "steel/async" => "../scheme/modules/async.scm",
    "steel/sync" => "../scheme/modules/sync.scm",
    "#%private/steel/contract" => "../scheme/modules/contracts.scm",
    "#%private/steel/print" => "../scheme/print.scm",
    "#%private/steel/ports" => "../scheme/modules/ports.scm",
    "#%private/steel/control" => "../scheme/modules/parameters.scm",
    "#%private/steel/reader" => "../scheme/modules/reader.scm",
    "#%private/steel/stdlib" => "../scheme/stdlib.scm",
    "#%private/steel/match" => "../scheme/modules/match.scm",
    "#%private/steel/sort" => "../scheme/modules/sort.scm",
    "#%private/steel/streams" => "../scheme/modules/streams.scm",
    "#%private/steel/transducers" => "../scheme/modules/transducers.scm",
    "steel/serde" => "../scheme/modules/serde.scm",
    "steel/signatures" => "../scheme/modules/signatures.scm"
);

create_prelude!(
    "#%private/steel/stdlib",
    "#%private/steel/control",
    "#%private/steel/contract",
    "#%private/steel/print",
    "#%private/steel/ports",
    "#%private/steel/reader",
    "#%private/steel/match",
    "#%private/steel/sort",
    "#%private/steel/streams",
    "#%private/steel/transducers",
);

/// Takes a fully qualified path, and resolves it to a relative path,
/// for use in determining a unique module name irrespective of where the
/// module was loaded.
pub(crate) fn fully_qualified_to_relative(
    path: PathBuf,
    search_dirs: &[PathBuf],
) -> std::io::Result<PathBuf> {
    // First, check if the path is relative:
    let working_dir = std::env::current_dir()?;

    if let Ok(p) = path.strip_prefix(&working_dir) {
        if p.exists() {
            return Ok(p.to_path_buf());
        }
    }

    for p in search_dirs {
        if let Ok(p) = p.strip_prefix(&working_dir) {
            if p.exists() {
                return Ok(p.to_path_buf());
            }
        }
    }

    Ok(path)
}

#[cfg(not(target_family = "wasm"))]
pub static STEEL_SEARCH_PATHS: Lazy<Option<Vec<PathBuf>>> = Lazy::new(|| {
    std::env::var("STEEL_SEARCH_PATHS")
        .ok()
        .map(|x| std::env::split_paths(x.as_str()).collect::<Vec<_>>())
});

pub fn steel_search_dirs() -> Vec<PathBuf> {
    #[cfg(not(target_family = "wasm"))]
    return STEEL_SEARCH_PATHS.clone().unwrap_or_default();

    #[cfg(target_family = "wasm")]
    return Vec::new();
}

#[cfg(not(target_family = "wasm"))]
pub static STEEL_HOME: Lazy<Option<String>> = Lazy::new(|| {
    std::env::var("STEEL_HOME").ok().or_else(|| {
        let home = env_home::env_home_dir().map(|x| x.join(".steel"));

        if let Some(home) = home {
            if home.exists() {
                return Some(home.into_os_string().into_string().unwrap());
            }

            #[cfg(target_os = "windows")]
            {
                if let Err(e) = std::fs::create_dir(&home) {
                    eprintln!("Unable to create steel home directory {:?}: {}", home, e)
                }

                return Some(home.into_os_string().into_string().unwrap());
            }
        }

        #[cfg(not(target_os = "windows"))]
        {
            let bd = xdg::BaseDirectories::new();
            let home = bd.data_home;

            home.map(|mut x: PathBuf| {
                x.push("steel");

                // Just go ahead and initialize the directory, even though
                // this is probably not the best place to do this. This almost
                // assuredly could be lifted out of this check since failing here
                // could cause some annoyance.
                if !x.exists() {
                    if let Err(e) = std::fs::create_dir(&x) {
                        eprintln!("Unable to create steel home directory {:?}: {}", x, e)
                    }
                }

                x.into_os_string().into_string().unwrap()
            })
        }

        #[cfg(target_os = "windows")]
        None
    })
});

#[cfg(target_family = "wasm")]
pub static STEEL_HOME: Lazy<Option<String>> = Lazy::new(|| None);

pub fn steel_home() -> Option<String> {
    STEEL_HOME.clone()
}

/// The directory a module's path is relative to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ModuleRoot {
    /// The directory of the first module the main program required.
    Entry,
    /// The cogs directory of the steel home.
    Home,
    /// One of the search directories.
    Search,
}

/// Where a module was found when it was required.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleOrigin {
    pub path: PathBuf,
    pub root: ModuleRoot,
    pub root_dir: PathBuf,
    /// The path from the root directory, separated by `/`.
    pub relative: String,
}

// How a require was resolved.
enum Found<'a> {
    Beside,
    Home(&'a Path),
    Search(&'a Path),
}

/// Module origins by prefix path, and prefix paths by root and relative
/// path.
#[derive(Clone, Default)]
pub struct ModuleOrigins {
    entry: Option<PathBuf>,
    by_prefix_path: FxHashMap<String, ModuleOrigin>,
    by_relative: FxHashMap<(ModuleRoot, String), String>,
}

impl ModuleOrigins {
    pub fn get(&self, path: &Path) -> Option<&ModuleOrigin> {
        self.by_prefix_path.get(prefix_path(path))
    }

    pub(crate) fn by_prefix_path(&self, prefix_path: &str) -> Option<&ModuleOrigin> {
        self.by_prefix_path.get(prefix_path)
    }

    pub(crate) fn prefix_path_for(&self, root: ModuleRoot, relative: &str) -> Option<&str> {
        self.by_relative
            .get(&(root, relative.to_string()))
            .map(|x| x.as_str())
    }

    // Keeps the first origin recorded for a module.
    fn record(&mut self, requirer: &Path, main: bool, found: Found, spec: &str, path: &Path) {
        let Some(key) = path.to_str().map(|_| prefix_path(path).to_string()) else {
            return;
        };

        if self.by_prefix_path.contains_key(&key) {
            return;
        }

        let origin = match found {
            Found::Beside if main => {
                if self.entry.is_none() {
                    self.entry = path.parent().map(|x| x.to_path_buf());
                }

                self.entry.clone().and_then(|root_dir| {
                    let relative = relative_to_root(&root_dir, path)?;
                    Some((ModuleRoot::Entry, root_dir, relative))
                })
            }
            Found::Beside => self.get(requirer).and_then(|parent| {
                let dir = parent.relative.rsplit_once('/').map_or("", |x| x.0);
                relative_from(dir, Path::new(spec))
                    .map(|relative| (parent.root, parent.root_dir.clone(), relative))
            }),
            Found::Home(dir) => relative_from("", Path::new(spec))
                .map(|relative| (ModuleRoot::Home, dir.to_path_buf(), relative)),
            Found::Search(dir) => relative_from("", Path::new(spec))
                .map(|relative| (ModuleRoot::Search, dir.to_path_buf(), relative)),
        };

        let Some((root, root_dir, relative)) = origin else {
            return;
        };

        // Another file already has this name under the root.
        if self.by_relative.contains_key(&(root, relative.clone())) {
            return;
        }

        self.by_relative
            .insert((root, relative.clone()), key.clone());
        self.by_prefix_path.insert(
            key,
            ModuleOrigin {
                path: path.to_path_buf(),
                root,
                root_dir,
                relative,
            },
        );
    }
}

// `path` relative to `dir`, or None if `path` is absolute. A path that
// leaves the root keeps its leading `..`.
fn relative_from(dir: &str, path: &Path) -> Option<String> {
    use std::path::Component;

    let mut parts: Vec<&str> = dir.split('/').filter(|x| !x.is_empty()).collect();

    for component in path.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_str()?),
            Component::CurDir => {}
            Component::ParentDir => match parts.last() {
                Some(&"..") | None => parts.push(".."),
                Some(_) => {
                    parts.pop();
                }
            },
            Component::RootDir | Component::Prefix(_) => return None,
        }
    }

    Some(parts.join("/"))
}

// `path` relative to `root`. Both are absolute.
fn relative_to_root(root: &Path, path: &Path) -> Option<String> {
    let shared = root
        .components()
        .zip(path.components())
        .take_while(|(a, b)| a == b)
        .count();

    let mut parts = vec![".."; root.components().count() - shared];
    for component in path.components().skip(shared) {
        parts.push(component.as_os_str().to_str()?);
    }

    Some(parts.join("/"))
}

/// Manages the modules
/// keeps some visited state on the manager for traversal
/// Also keeps track of the metadata for each file in order to determine
/// if it needs to be recompiled
#[derive(Clone)]
pub struct ModuleManager {
    pub(crate) compiled_modules: CompiledModuleCache,
    pub(crate) file_metadata: crate::HashMap<PathBuf, SystemTime>,
    visited: FxHashSet<PathBuf>,
    custom_builtins: HashMap<String, String>,
    rollback_metadata: crate::HashMap<PathBuf, SystemTime>,
    // Phase 0 code that modules had kept when the last compile began
    rollback_deferred: crate::HashMap<PathBuf, DeferredCode>,
    // #[serde(skip_serializing, skip_deserializing)]
    module_resolvers: Vec<Arc<dyn SourceModuleResolver>>,

    prelude_string: Cow<'static, str>,

    pub(crate) origins: ModuleOrigins,
}

pub trait SourceModuleResolver: Send + Sync {
    fn resolve(&self, key: &str) -> Option<String>;
    fn exists(&self, key: &str) -> bool;
}

impl ModuleManager {
    pub(crate) fn new(
        compiled_modules: crate::HashMap<PathBuf, CompiledModule>,
        file_metadata: crate::HashMap<PathBuf, SystemTime>,
    ) -> Self {
        ModuleManager {
            compiled_modules: CompiledModuleCache::new(compiled_modules),
            file_metadata,
            visited: FxHashSet::default(),
            custom_builtins: HashMap::new(),
            rollback_metadata: crate::HashMap::new(),
            rollback_deferred: crate::HashMap::new(),
            module_resolvers: Vec::new(),
            prelude_string: Cow::Borrowed(PRELUDE_STRING),
            origins: ModuleOrigins::default(),
        }
    }

    pub fn origins(&self) -> &ModuleOrigins {
        &self.origins
    }

    pub fn add_builtin_module(&mut self, module_name: String, mut text: String) {
        // Custom external builtins should be loaded with the prelude first, otherwise
        // they'll need to handle a bunch of imports
        text.insert_str(0, &self.prelude_string);

        self.custom_builtins.insert(module_name, text);
    }

    pub fn modules(&self) -> &crate::HashMap<PathBuf, CompiledModule> {
        &self.compiled_modules.compiled_modules
    }

    pub fn modules_mut(&mut self) -> &mut crate::HashMap<PathBuf, CompiledModule> {
        &mut self.compiled_modules.compiled_modules
    }

    pub(crate) fn default() -> Self {
        Self::new(crate::HashMap::default(), crate::HashMap::default())
    }

    pub fn set_prelude_string(&mut self, prelude_string: Cow<'static, str>) {
        self.prelude_string = prelude_string;
    }

    pub fn use_default_prelude_string(&mut self) {
        self.prelude_string = Cow::Borrowed(PRELUDE_STRING);
    }

    pub(crate) fn register_module_resolver(
        &mut self,
        resolver: impl SourceModuleResolver + 'static,
    ) {
        self.module_resolvers.push(Arc::new(resolver));
    }

    // Add the module directly to the compiled module cache
    pub(crate) fn add_module(
        &mut self,
        path: PathBuf,
        global_macro_map: &mut FxHashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<()> {
        self.visited.clear();

        // TODO: Expand macros on the fly when visiting a module. Don't wait till the end
        // Macro expansion should happen as we enter a module.
        let mut module_builder = ModuleBuilder::new_from_path(
            path,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            &self.custom_builtins,
            &[],
            &self.module_resolvers,
            &self.prelude_string,
            &mut self.origins,
        )?;

        module_builder.compile()?;

        Ok(())
    }

    pub(crate) fn rollback_metadata(&mut self) {
        self.file_metadata = self.rollback_metadata.clone();
        self.compiled_modules.deferred = self.rollback_deferred.clone();
    }

    // #[allow(unused)]
    pub(crate) fn compile_main(
        &mut self,
        global_macro_map: &mut FxHashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        mut exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        builtin_modules: ModuleContainer,
        lifted_kernel_environments: &mut HashMap<String, KernelDefMacroSpec>,
        phase1_imports: &mut Vec<Phase1Import>,
        lifted_macro_environments: &mut HashMap<PathBuf, HashSet<InternedString>>,
        search_dirs: &[PathBuf],
        global_map: &FxHashMap<InternedString, usize>,
    ) -> Result<Vec<ExprKind>> {
        // Wipe the visited set on entry
        self.visited.clear();

        self.rollback_metadata = self.file_metadata.clone();
        self.rollback_deferred = self.compiled_modules.deferred.clone();

        // TODO
        // This is also explicitly wrong -> we should separate the global macro map from the macros found locally in this module
        // For instance, (cond) is global, but (define-syntax blagh) might be local to main
        // if a module then defines a function (blagh) that is used inside its scope, this would expand the macro in that scope
        // which we do not want
        // The program's own definitions shadow what it requires
        let mut redefinitions = MacroRedefinitions::default();
        let mut defined: FxHashSet<InternedString> =
            extract_macro_defs(&mut exprs, global_macro_map, &mut redefinitions)?
                .into_iter()
                .collect();

        defined.extend(collect_globals(&exprs));
        exprs.iter().for_each(|x| defined_names(x, &mut defined));
        exprs
            .iter()
            .for_each(|x| crate::parser::expand_visitor::defined_macro_names(x, &mut defined));

        let mut module_builder = ModuleBuilder::main(
            path,
            exprs,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
            sources,
            kernel,
            builtin_modules.clone(),
            global_macro_map,
            &self.custom_builtins,
            search_dirs,
            &self.module_resolvers,
            &self.prelude_string,
            &mut self.origins,
        )?;

        let mut module_statements = module_builder.compile()?;

        module_builder.collect_provides()?;

        // What the program's names refer to, for macros' literals and free-identifier=?
        let mut imports = FxHashMap::default();
        for require_object in module_builder
            .require_objects
            .iter()
            .filter(|x| !x.for_syntax)
        {
            let path = require_object.path.get_path();
            if let Some(module) = module_builder
                .compiled_modules
                .compiled_modules
                .get(path.as_ref())
            {
                let from: InternedString = module.name.to_string_lossy().as_ref().into();
                for (local, original) in imported_names(require_object, module) {
                    imports.insert(local, (from, original));
                }
            }
        }
        let top_level = InternedString::from_static("top-level");
        crate::parser::expand_visitor::record_module_bindings(
            top_level,
            crate::parser::expand_visitor::ModuleBindings {
                own: defined.iter().copied().collect(),
                imports,
            },
        );
        let _current = crate::parser::expand_visitor::CurrentModule::set(top_level);

        let mut ast = core::mem::take(&mut module_builder.source_ast);

        let mut require_defines = Vec::new();

        let mut explicit_requires = HashMap::new();

        for require_object in module_builder
            .require_objects
            .iter()
            .filter(|x| !x.for_syntax)
        {
            let path = require_object.path.get_path();
            explicit_requires.clear();

            // If there _are_ explicit identifiers to import, limit what we import to what
            // is in the set
            for ident in &require_object.idents_to_import {
                match ident {
                    MaybeRenamed::Normal(i) => {
                        explicit_requires.insert(i.atom_identifier().unwrap().clone(), None);
                    }

                    MaybeRenamed::Renamed(from, to) => {
                        explicit_requires.insert(
                            from.atom_identifier().unwrap().clone(),
                            Some(to.atom_identifier().unwrap().clone()),
                        );
                    }
                }
            }

            // println!("{:?}", path);
            let module = if let Some(module) = module_builder
                .compiled_modules
                .get(path.as_ref(), module_builder.sources)
            {
                module
            } else {
                // log::debug!(target: "modules", "No provides found for module, skipping: {:?}", path);

                continue;
            };

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    let other_module_prefix = module.prefix();

                    // TODO: Expand the contract out into something we expect
                    // Otherwise, this is going to blow up
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match *qualifier {
                                    x if x == *REQUIRE_IDENT_SPEC => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();

                                        if !explicit_requires.is_empty()
                                            && !name
                                                .atom_identifier()
                                                .map(|x| explicit_requires.contains_key(x))
                                                .unwrap_or_default()
                                        {
                                            continue;
                                        }

                                        if module
                                            .macro_map
                                            .contains_key(name.atom_identifier().unwrap())
                                        {
                                            continue;
                                        }

                                        let hash_get = expr_list![
                                            ExprKind::atom(*PROTO_HASH_GET),
                                            ExprKind::atom(
                                                CompactString::new(MODULE_PREFIX)
                                                    + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let mut owned_name = name.clone();

                                        // If we have the alias listed, we should use it
                                        if !explicit_requires.is_empty() {
                                            if let Some(alias) = explicit_requires
                                                .get(name.atom_identifier().unwrap())
                                                .copied()
                                                .flatten()
                                            {
                                                *owned_name.atom_identifier_mut().unwrap() =
                                                    alias.clone();
                                            }
                                        }

                                        if let Some(prefix) = &require_object.prefix {
                                            if let Some(existing) = owned_name.atom_identifier_mut()
                                            {
                                                let mut prefixed_identifier = prefix.clone();
                                                prefixed_identifier.push_str(existing.resolve());

                                                // Update the existing identifier to point to a new one with the prefix applied
                                                *existing = prefixed_identifier.into();
                                            }
                                        }

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            owned_name,
                                            hash_get,
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        require_defines.push(define);
                                    }
                                    _ => {
                                        stop!(TypeMismatch => format!("provide expects either an identifier, (for-syntax <ident>), or (contract/out ...), found: {}", provide))
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            if !explicit_requires.is_empty()
                                && !provide
                                    .atom_identifier()
                                    .map(|x| explicit_requires.contains_key(x))
                                    .unwrap_or_default()
                            {
                                continue;
                            }

                            if module
                                .macro_map
                                .contains_key(provide.atom_identifier().unwrap())
                            {
                                continue;
                            }

                            let hash_get = expr_list![
                                ExprKind::atom(*PROTO_HASH_GET),
                                ExprKind::atom(
                                    CompactString::new(MODULE_PREFIX) + &other_module_prefix
                                ),
                                ExprKind::Quote(Box::new(Quote::new(
                                    provide.clone(),
                                    SyntaxObject::default(TokenType::Quote)
                                ))),
                            ];

                            let mut owned_provide = provide.clone();

                            // If we have the alias listed, we should use it
                            if !explicit_requires.is_empty() {
                                if let Some(alias) = explicit_requires
                                    .get(provide.atom_identifier().unwrap())
                                    .copied()
                                    .flatten()
                                {
                                    *owned_provide.atom_identifier_mut().unwrap() = alias.clone();
                                }
                            }

                            // TODO: If there is a prefix applied, use it here?
                            if let Some(prefix) = &require_object.prefix {
                                // println!("Found prefix: {}", prefix);

                                if let Some(existing) = owned_provide.atom_identifier_mut() {
                                    let mut prefixed_identifier = prefix.clone();
                                    prefixed_identifier.push_str(existing.resolve());

                                    // Update the existing identifier to point to a new one with the prefix applied
                                    *existing = prefixed_identifier.into();
                                }
                            }

                            let define = ExprKind::Define(Box::new(Define::new(
                                owned_provide,
                                hash_get,
                                SyntaxObject::default(TokenType::Define),
                            )));

                            require_defines.push(define);
                        }
                        _ => {
                            stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                        }
                    }
                }
            }
        }

        let mut mangled_asts = Vec::with_capacity(ast.len());

        let require_objects = core::mem::take(&mut module_builder.require_objects);
        drop(module_builder);

        if let Some(kernel) = kernel.as_mut() {
            let mut instantiated = FxHashMap::default();

            for require_object in require_objects.iter().filter(|x| x.for_syntax) {
                let path = require_object.path.get_path().into_owned();
                let mut unused = Vec::new();
                let (module, macros, _) = Self::find_in_scope_macros(
                    &mut self.compiled_modules,
                    Some(&mut *kernel),
                    &path,
                    require_object,
                    &mut unused,
                );

                let procedural = procedural_for_syntax(kernel, module, require_object);

                let instance = instantiate_for_syntax(
                    kernel,
                    &self.compiled_modules.compiled_modules,
                    global_macro_map,
                    &builtin_modules,
                    require_object,
                    true,
                    &macros,
                    &mut instantiated,
                )?;

                for import in instance.imports {
                    phase1_imports.retain(|x| x.local != import.local);
                    phase1_imports.push(import);
                }

                kernel.add_phase1_macros("top-level", macros, procedural, instance.renames);
            }
        }

        // TODO: Move this to the lower level as well
        // It seems we're only doing this expansion at the top level, but we _should_ do this at the lower level as well
        // Also require the macros that the requires re-export, from the modules that define them
        let reexported: Vec<RequireObject> = require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .flat_map(|x| reexport_requires(&self.compiled_modules.compiled_modules, x))
            .collect();

        // Gather each require's macros first, so the program can be expanded one form at a
        // time, and a macro defined by one form's macros hides the imported ones from then on
        struct RequiredMacros {
            path: PathBuf,
            module_name: String,
            in_scope_macros: FxHashMap<InternedString, SteelMacro>,
            name_mangler: NameMangler,
            exported: FxHashMap<InternedString, InternedString>,
            private: HashSet<InternedString>,
        }

        let mut required = Vec::new();

        for require_object in require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .chain(reexported.iter())
        {
            let require_for_syntax = require_object.path.get_path();

            let (module, mut in_scope_macros, name_mangler) = Self::find_in_scope_macros(
                &mut self.compiled_modules,
                kernel.as_mut(),
                require_for_syntax.as_ref(),
                require_object,
                &mut mangled_asts,
            );

            in_scope_macros.retain(|name, _| !defined.contains(name));

            let module_name = module.name.to_str().unwrap().to_string();

            for import in provided_phase1_bindings(kernel.as_ref(), module, require_object) {
                if !phase1_imports.iter().any(|x| x.local == import.local) {
                    phase1_imports.push(import);
                }
            }

            let has_kernel_macros = kernel
                .as_ref()
                .is_some_and(|kernel| kernel.has_procedural_macros(&module_name));

            let exported = provided_kernel_macros(kernel.as_ref(), module, require_object);

            // A procedural macro required again replaces any macro of the same name defined
            // since, of either kind
            for name in exported.keys() {
                if !defined.contains(name) {
                    crate::parser::expand_visitor::remove_macro(global_macro_map, name);

                    if let Some(kernel) = kernel.as_mut() {
                        kernel.forget_macro("top-level", name);
                    }
                }
            }

            // Even if it provides none, the module's macros can expand to its private ones
            if has_kernel_macros {
                let exported = exported.clone();

                lifted_kernel_environments
                    .entry(module_name.clone())
                    .or_insert_with(|| KernelDefMacroSpec {
                        _env: module_name.clone(),
                        exported: FxHashMap::default(),
                        name_mangler: name_mangler.clone(),
                    })
                    .exported
                    .extend(exported);
            }

            // Code here can't use the module's private macros. The module's own macros refer to
            // them with its prefix
            let prefix = module.prefix();
            let provided = provided_names(module);
            let private: HashSet<InternedString> = module
                .macro_map
                .keys()
                .filter(|name| !provided.contains(*name))
                .filter(|name| !name.resolve().starts_with(prefix.as_str()))
                .copied()
                .collect();

            required.push(RequiredMacros {
                path: module.name.clone(),
                module_name,
                in_scope_macros,
                name_mangler,
                exported,
                private,
            });
        }

        // Procedural macros from the requires and the program get their arguments as written
        let _procedural = crate::parser::expand_visitor::ProceduralHeads::set(
            lifted_kernel_environments
                .values()
                .flat_map(|x| x.exported.keys().copied())
                .chain(
                    kernel
                        .as_ref()
                        .map(|kernel| kernel.procedural_names("top-level"))
                        .unwrap_or_default(),
                )
                .collect(),
            &ast,
        );

        for expr in ast.iter_mut() {
            for index in 0..required.len() {
                // @matt 12/8/2023
                // The easiest thing to do here, is to go to the other module, and find
                // what defmacros have been exposed on the require for syntax. Once those
                // have been found, we run a pass with kernel expansion, limiting the
                // expander to only use the macros that we've exposed. After that,
                // we run the expansion again, using the full suite of defmacro capabilities.
                //
                // The question that remains - how to define the neat phases of what kinds
                // of macros can expand into what? Can defmacro -> syntax-rules -> defmacro?
                // This could eventually prove to be cumbersome, but it is still early
                // for defmacro. Plus, I need to create a syntax-case or syntax-parse
                // frontend before the defmacro style macros become too pervasive.
                //
                // TODO: Replicate this behavior over to builtin modules
                let RequiredMacros {
                    path,
                    module_name,
                    in_scope_macros,
                    name_mangler,
                    exported,
                    ..
                } = &mut required[index];

                // First expand the in scope macros
                if !expand_foreign(expr, in_scope_macros, GlobalMap::Set(&name_mangler.globals))? {
                    continue;
                }

                let _source_id = sources.get_source_id(path).unwrap();

                // Their expansions refer to the module's other macros of both kinds with its
                // prefix, but only to the procedural ones it provides
                let globals = name_mangler.globals.clone();
                crate::parser::expand_visitor::expand_with_kernel_defining(
                    expr,
                    in_scope_macros,
                    GlobalMap::Set(&globals),
                    crate::parser::expand_visitor::KernelExpander::for_module(
                        kernel.as_mut(),
                        module_name,
                        Some(exported),
                        name_mangler,
                    ),
                    false,
                )?;

                let mut generated = FxHashSet::default();
                crate::parser::expand_visitor::defined_macro_names(expr, &mut generated);

                for name in generated {
                    if defined.insert(name) {
                        for other in required.iter_mut() {
                            other.in_scope_macros.remove(&name);
                            other.exported.remove(&name);
                        }
                    }
                }
            }
        }

        for RequiredMacros {
            path,
            in_scope_macros,
            mut private,
            ..
        } in required
        {
            // Global macro map - also need to expand with ALL macros
            // post expansion in the target environment, which means we can't _just_
            // extend the global macro map with the target in scope macros, we need to
            // do something like the two pass expansion
            // An imported macro with a prelude macro's name doesn't change what the prelude's
            // templates use
            for (name, m) in in_scope_macros {
                crate::parser::expand_visitor::define_macro(global_macro_map, name, m);
            }

            // Also hide the macros the program shadows with its own definitions
            private.extend(defined.iter().copied());

            lifted_macro_environments.insert(path, private);
        }

        // Include the defines from the modules now imported
        module_statements.append(&mut require_defines);

        // The next two lines here expand _all_ of the source code with the top level macros
        // This is necessary because of the std library macros, although this should be able to be
        // solved with scoped imports of the standard library explicitly
        module_statements.append(&mut ast);

        time!("Top level macro evaluation time", {
            for expr in module_statements.iter_mut() {
                if redefinitions.reached(global_macro_map, expr) {
                    continue;
                }

                if let ExprKind::List(l) = expr {
                    crate::parser::expand_visitor::forget_replaced_macro(global_macro_map, l);
                }

                let found_in_scope = expand(expr, global_macro_map, GlobalMap::Map(global_map))?;

                for item in found_in_scope.iter() {
                    crate::parser::expand_visitor::remove_macro(global_macro_map, item);

                    for (_, shadowed_vars) in lifted_macro_environments.iter_mut() {
                        // If this was recently shadowed, then we don't want it any more.
                        shadowed_vars.insert(*item);
                    }
                }
            }

            redefinitions.finish(global_macro_map);
        });

        Ok(module_statements)
    }

    fn find_in_scope_macros<'a>(
        compiled_modules: &'a mut CompiledModuleCache,
        mut kernel: Option<&mut Kernel>,
        require_for_syntax: &'a PathBuf,
        require_object: &'a RequireObject,
        mangled_asts: &'a mut Vec<ExprKind>,
    ) -> (
        &'a CompiledModule,
        FxHashMap<InternedString, SteelMacro>,
        NameMangler,
    ) {
        let module = compiled_modules
            .compiled_modules
            .get_mut(require_for_syntax)
            .unwrap_or_else(|| panic!("Module missing!: {:?}", require_for_syntax));

        let prefix = module.prefix();

        // Collect globals and also, anything that is imported
        let mut globals = collect_globals(&module.ast);

        // Bring in what they provided first, but then if we can't find that, we'll have to
        // find all of the remaining ones by fetching all of their dependencies, and then
        // reborrowing
        globals.extend(
            module
                .require_objects
                .iter()
                .filter(|x| !x.for_syntax)
                .flat_map(|x| x.as_identifiers()),
        );

        let modules_to_check = module
            .require_objects
            .iter()
            .filter_map(|x| {
                if x.idents_to_import.is_empty() && !x.for_syntax {
                    Some(x.path.get_path().into_owned())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let requires: Vec<RequireObject> = module
            .require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .cloned()
            .collect();

        let module_name = module.name.to_str().unwrap_or_default().to_owned();

        // Macros the module imports. Its code refers to them with its prefix, unless it
        // defines the same names itself
        let mut rules = FxHashMap::default();
        let mut procedural = FxHashMap::default();
        let mut own = collect_globals(&module.ast);
        own.extend(module.own_macros.iter().copied());

        for required in &requires {
            let Some(other) = compiled_modules
                .compiled_modules
                .get(required.path.get_path().as_ref())
            else {
                continue;
            };

            for (local, name) in as_required(&provided_names(other), required) {
                if let Some(m) = other.macro_map.get(&name).filter(|_| !own.contains(&local)) {
                    rules.insert(local, m.clone());
                }
            }

            let environment = other.name.to_str().unwrap_or_default().to_owned();

            for (local, name) in provided_kernel_macros(kernel.as_deref(), other, required) {
                if !own.contains(&local) {
                    procedural.insert(local, (environment.clone(), name));
                }
            }
        }

        for importing_module in modules_to_check {
            let other_module = compiled_modules
                .compiled_modules
                .get_mut(&importing_module)
                .unwrap();

            for provide_expr in &other_module.provides {
                // TODO: Handle provide spec stuff!
                for provide in &provide_expr.list().unwrap().args[1..] {
                    if let Some(ident) = provide.atom_identifier() {
                        globals.insert(*ident);
                    }

                    if let Some(list) = provide.list().and_then(|x| x.second_ident()) {
                        globals.insert(*list);
                    }
                }
            }
        }

        let module = compiled_modules
            .compiled_modules
            .get_mut(require_for_syntax)
            .unwrap_or_else(|| panic!("Module missing!: {:?}", require_for_syntax));

        let mut name_mangler = NameMangler::new(globals, prefix.clone()).in_module(&module_name);

        // If the module hasn't been emitted already, then include it here
        if !module.emitted {
            let mut module_ast = module.ast.clone();

            name_mangler.mangle_vars(&mut module_ast);

            mangled_asts.append(&mut module_ast);
        }

        // The module's code refers to its imported macros and its own procedural macros with
        // its prefix, using the names it imports them as
        let own_procedural = kernel
            .as_deref()
            .and_then(|kernel| kernel.exported_defmacros(&module_name))
            .unwrap_or_default();

        procedural.extend(
            own_procedural
                .iter()
                .map(|name| (*name, (module_name.clone(), *name))),
        );

        name_mangler.add_macros(procedural.keys().chain(rules.keys()).copied());

        // Its templates also refer to its own syntax-rules macros with its prefix, so a macro
        // of the same name at the use site can't replace them. Its macros can also be found
        // under those names.
        let own_rules: Vec<InternedString> = module
            .macro_map
            .iter()
            .filter(|(name, m)| !m.special_mangled && module.own_macros.contains(*name))
            .map(|(name, _)| *name)
            .collect();

        name_mangler.add_macros(own_rules.iter().copied());

        {
            let mut should_mangle = false;
            for (_, smacro) in module.macro_map.iter() {
                if !smacro.special_mangled && !smacro.is_mangled() {
                    should_mangle = true;
                    break;
                }
            }

            if should_mangle {
                for (_, smacro) in Arc::make_mut(&mut module.macro_map).iter_mut() {
                    if !smacro.special_mangled && !smacro.is_mangled() {
                        for expr in smacro.exprs_mut() {
                            name_mangler.visit(expr);
                        }
                        smacro.mark_mangled();
                        smacro.mark_definition_resolved(&module_name);
                    }
                }

                let aliases: Vec<(InternedString, SteelMacro)> = own_rules
                    .iter()
                    .filter_map(|name| {
                        let m = module.macro_map.get(name)?.clone();
                        Some(((prefix.clone() + name.resolve()).into(), m))
                    })
                    .collect();

                Arc::make_mut(&mut module.macro_map).extend(aliases);
            }
        }

        // let provided_macros = module.provides_for
        // let expander = Expander::new(&module.macro_map);
        // TODO
        // expand expressions one by one
        // if expansion with _just_ public macros from the required module doesn't do anything, stop
        // if it _does_ change, do another pass with all of the macros in scope
        // do this for each of the expressions in the file in this loop
        // TODO -> try not cloning this
        // TODO -> do this in the module expansion as well
        let mut in_scope_macros = module
            .provides_for_syntax
            .iter()
            // Chain with just the normal provides!
            .chain(module.provides.iter().flat_map(|x| {
                x.list().unwrap().args[1..]
                    .iter()
                    .filter_map(|x| x.atom_identifier())
            }))
            .filter_map(|x| {
                let smacro = module.macro_map.get(x);
                if let Some(smacro) = smacro {
                    Some((*x, smacro.clone()))
                } else {
                    None
                }
            })
            .collect::<FxHashMap<_, _>>();

        // If the require_object specifically imports things, we should reference it

        if !require_object.idents_to_import.is_empty() {
            // Only the macros the require names
            let provided = core::mem::take(&mut in_scope_macros);

            for maybe in &require_object.idents_to_import {
                match maybe {
                    MaybeRenamed::Normal(n) => {
                        if let Some(ident) = n.atom_identifier() {
                            // If the macro is not provided, we should
                            // not bring it in
                            if !provided.contains_key(ident) {
                                continue;
                            }

                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros
                                        .insert((prefix.to_string() + ident.resolve()).into(), m);

                                    in_scope_macros.remove(ident);
                                } else {
                                    in_scope_macros.insert(*ident, m);
                                }
                            }
                        }
                    }
                    MaybeRenamed::Renamed(from, to) => {
                        if let Some(ident) = from.atom_identifier() {
                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }
                                // TODO: Remove this unwrap
                                // in_scope_macros.insert(*to.atom_identifier().unwrap(), m);

                                if let Some(ident) = from.atom_identifier() {
                                    if !provided.contains_key(ident) {
                                        continue;
                                    }
                                }

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros.insert(
                                        (prefix.to_string()
                                            + to.atom_identifier().unwrap().resolve())
                                        .into(),
                                        m,
                                    );
                                } else {
                                    in_scope_macros.insert(*to.atom_identifier().unwrap(), m);
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // Pull in all of the macros that the module exposes

            for provide_expr in &module.provides {
                if let Some(provide_expr) = provide_expr.list() {
                    for ident in provide_expr.args.split_first().unwrap().1 {
                        if let Some(ident) = ident.atom_identifier() {
                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros
                                        .insert((prefix.to_string() + ident.resolve()).into(), m);

                                    in_scope_macros.remove(ident);
                                } else {
                                    in_scope_macros.insert(*ident, m);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Imported syntax-rules macros. Its templates refer to them with its prefix, like its
        // globals
        let imported: Vec<(InternedString, SteelMacro)> = rules
            .iter()
            .map(|(local, m)| ((prefix.clone() + local.resolve()).into(), m.clone()))
            .filter(|(alias, _)| !module.macro_map.contains_key(alias))
            .collect();

        if !imported.is_empty() {
            Arc::make_mut(&mut module.macro_map).extend(imported);
        }

        // Code using the module needs the prefixed names its macros expand to
        in_scope_macros.extend(
            module
                .macro_map
                .iter()
                .filter(|(name, _)| name.resolve().starts_with(prefix.as_str()))
                .map(|(name, m)| (*name, m.clone())),
        );

        rules.extend(
            own_rules
                .iter()
                .filter_map(|name| Some((*name, module.macro_map.get(name)?.clone()))),
        );

        if let Some(kernel) = kernel.as_deref_mut() {
            kernel.add_macro_module(
                prefix,
                MacroModule {
                    environment: module_name,
                    mangler: name_mangler.clone(),
                    rules,
                    procedural,
                },
            );
        }

        (module, in_scope_macros, name_mangler)
    }
}

// Pre-compile module to bytecode? Is it even possible?
// Dynamically linking the module would then make it relatively
// easy to just load everything up at the start.
// Compiled module _should_ be possible now. Just create a target
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompiledModule {
    name: PathBuf,
    provides: Vec<ExprKind>,
    require_objects: Vec<RequireObject>,
    provides_for_syntax: Vec<InternedString>,
    pub(crate) macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
    pub(crate) ast: Vec<ExprKind>,
    emitted: bool,
    cached_prefix: CompactString,
    downstream: Vec<PathBuf>,
    downstream_builtins: Vec<PathBuf>,

    pub(crate) compiled_ast: Option<ExprKind>,
    // The syntax-rules macros it defines. The rest of its macro map comes from the prelude
    #[serde(default)]
    own_macros: FxHashSet<InternedString>,
    // Macros it imports and re-exports
    #[serde(default)]
    reexports: Vec<Reexport>,
}

// A module's code can nest too deep to drop recursively
impl Drop for CompiledModule {
    fn drop(&mut self) {
        steel_parser::ast::drop_deep(core::mem::take(&mut self.ast));
        steel_parser::ast::drop_deep(core::mem::take(&mut self.provides));
        steel_parser::ast::drop_deep(self.compiled_ast.take());
    }
}

// A macro that a module provides but another module defines
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Reexport {
    source: PathOrBuiltIn,
    // Its name in the defining module
    name: InternedString,
    // The name the module provides it as
    provided: InternedString,
}

// Requires for the macros a required module re-exports: one require of each defining module,
// for those macros only
fn reexport_requires(
    modules: &crate::HashMap<PathBuf, CompiledModule>,
    require_object: &RequireObject,
) -> Vec<RequireObject> {
    let Some(module) = modules.get(require_object.path.get_path().as_ref()) else {
        return Vec::new();
    };

    let provided: FxHashSet<InternedString> = module.reexports.iter().map(|x| x.provided).collect();

    as_required(&provided, require_object)
        .into_iter()
        .filter_map(|(local, provided)| {
            let reexport = module.reexports.iter().find(|x| x.provided == provided)?;

            Some(RequireObject {
                path: reexport.source.clone(),
                for_syntax: false,
                idents_to_import: vec![MaybeRenamed::Renamed(
                    ExprKind::atom(reexport.name),
                    ExprKind::atom(local),
                )],
                prefix: None,
                span: require_object.span,
                except: Vec::new(),
                renames: Vec::new(),
            })
        })
        .collect()
}

// Specs with the ones a macro expanded to `(begin spec ...)` spliced in
fn spliced_specs<'a>(specs: impl Iterator<Item = &'a ExprKind>, spliced: &mut Vec<ExprKind>) {
    for spec in specs {
        match spec {
            ExprKind::Begin(b) => spliced_specs(b.exprs.iter(), spliced),
            ExprKind::List(l)
                if l.first()
                    .and_then(ExprKind::atom_syntax_object)
                    .is_some_and(|x| x.ty == TokenType::Begin) =>
            {
                spliced_specs(l.args[1..].iter(), spliced)
            }
            _ => spliced.push(spec.clone()),
        }
    }
}

// A name provided both as itself and under contract/out is provided with its contract
fn drop_uncontracted_duplicates(provides: &mut [ExprKind]) {
    let contracted: FxHashSet<InternedString> = provides
        .iter()
        .filter_map(ExprKind::list)
        .flat_map(|l| l.args.iter())
        .filter_map(ExprKind::list)
        .filter(|x| x.first_ident() == Some(&*REQUIRE_IDENT_SPEC))
        .filter_map(|x| x.args.get(1)?.atom_identifier().copied())
        .collect();
    if contracted.is_empty() {
        return;
    }
    for l in provides.iter_mut().filter_map(ExprKind::list_mut) {
        l.args
            .retain(|x| x.atom_identifier().is_none_or(|x| !contracted.contains(x)));
    }
}

pub static MANGLER_PREFIX: &str = "##mm";
pub static MODULE_PREFIX: &str = "__module-";
pub static MANGLED_MODULE_PREFIX: &str = "__module-##mm";
// The names a require brings in, each with its name in the module it comes from
fn imported_names(
    require_object: &RequireObject,
    module: &CompiledModule,
) -> Vec<(InternedString, InternedString)> {
    let mut explicit = FxHashMap::default();
    for ident in &require_object.idents_to_import {
        match ident {
            MaybeRenamed::Normal(i) => {
                if let Some(i) = i.atom_identifier() {
                    explicit.insert(*i, *i);
                }
            }
            MaybeRenamed::Renamed(from, to) => {
                if let (Some(from), Some(to)) = (from.atom_identifier(), to.atom_identifier()) {
                    explicit.insert(*from, *to);
                }
            }
        }
    }

    let mut names = Vec::new();
    for provide_expr in &module.provides {
        let Some(provides) = provide_expr.list() else {
            continue;
        };
        for provide in provides.args.iter().skip(1) {
            let name = match provide {
                ExprKind::Atom(_) => provide.atom_identifier(),
                ExprKind::List(l) if l.first_ident() == Some(&*REQUIRE_IDENT_SPEC) => {
                    l.args.get(1).and_then(ExprKind::atom_identifier)
                }
                _ => None,
            };
            let Some(name) = name.copied() else {
                continue;
            };
            let local = if explicit.is_empty() {
                name
            } else {
                match explicit.get(&name) {
                    Some(local) => *local,
                    None => continue,
                }
            };
            let local = match &require_object.prefix {
                Some(prefix) => (prefix.clone() + local.resolve()).into(),
                None => local,
            };
            names.push((local, name));
        }
    }
    names
}

// The module path without the steel home. The module's prefix is keyed on it.
fn prefix_path(name: &Path) -> &str {
    let name = name.to_str().unwrap();

    match STEEL_HOME.as_ref() {
        Some(steel_home) => name.trim_start_matches(steel_home.as_str()),
        None => name,
    }
}

fn push_prefix_id(base: &mut CompactString, name: &Path) {
    let interned = InternedString::from_str(prefix_path(name));
    let id = interned.get().into_inner();

    base.push_str(&id.to_string());
    base.push_str(MANGLER_SEPARATOR);
}

pub fn path_to_prefix_name(name: PathBuf) -> String {
    let mut base = CompactString::new(MANGLER_PREFIX);
    push_prefix_id(&mut base, &name);
    base.into_string()
}

pub fn path_to_module_name(name: PathBuf) -> String {
    let mut base = CompactString::new(MANGLED_MODULE_PREFIX);
    push_prefix_id(&mut base, &name);
    base.into_string()
}

// TODO: @Matt 6/12/23 - This _should_ be serializable. If possible, we can try to store intermediate objects down to some file.
impl CompiledModule {
    pub fn new(
        name: PathBuf,
        provides: Vec<ExprKind>,
        require_objects: Vec<RequireObject>,
        provides_for_syntax: Vec<InternedString>,
        macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
        ast: Vec<ExprKind>,
        downstream: Vec<PathBuf>,
        downstream_builtins: Vec<PathBuf>,
    ) -> Self {
        let mut base = CompactString::new(MANGLER_PREFIX);
        push_prefix_id(&mut base, &name);

        Self {
            name,
            provides,
            require_objects,
            provides_for_syntax,
            macro_map,
            ast,
            emitted: false,
            cached_prefix: base,
            downstream,
            downstream_builtins,
            compiled_ast: None,
            own_macros: FxHashSet::default(),
            reexports: Vec::new(),
        }
    }

    // TODO: Should cache this
    pub fn prefix(&self) -> CompactString {
        self.cached_prefix.clone()
    }

    pub fn get_compiled_ast(&self) -> &Option<ExprKind> {
        &self.compiled_ast
    }

    pub fn get_macros(&self) -> Arc<FxHashMap<InternedString, SteelMacro>> {
        self.macro_map.clone()
    }

    pub fn get_ast(&self) -> &[ExprKind] {
        &self.ast
    }

    pub fn name(&self) -> &PathBuf {
        &self.name
    }

    pub fn get_provides(&self) -> &[ExprKind] {
        &self.provides
    }

    pub fn get_requires(&self) -> &[RequireObject] {
        &self.require_objects
    }

    // pub fn get_requires(&self) -> &[PathBuf] {
    //     &self.requires
    // }

    pub fn set_emitted(&mut self, emitted: bool) {
        self.emitted = emitted;
    }

    fn to_top_level_module(
        &mut self,
        modules: &crate::HashMap<PathBuf, CompiledModule>,
        global_macro_map: &FxHashMap<InternedString, SteelMacro>,
    ) -> Result<ExprKind> {
        let mut globals = collect_globals(&self.ast);

        let mut exprs = self.ast.clone();

        let mut provide_definitions = Vec::new();

        // TODO: Change this to not use the full path. Unfortunately that isn't portable,
        // so we should use something that is more agnostic of the target location
        // if we were to load.
        //
        // Probably a better idea would be to somehow assign a unique ID to each module;
        // the path from the $STEEL_HOME would be relatively safe. That way we can just strip
        // the $STEEL_HOME root away from the path if it starts with it, and then have
        // that resolve to be the "name" of the module.
        let prefix = self.prefix();

        // Now we should be able to set up a series of requires with the right style
        // ;; Refresh the module definition in this namespace
        // (define a-module.rkt-b (hash-get 'b b-module.rkt-b))

        let mut explicit_requires = HashMap::new();

        // TODO: This is the same as the top level, they should be merged
        for require_object in self.require_objects.iter().filter(|x| !x.for_syntax) {
            let path = require_object.path.get_path();

            explicit_requires.clear();

            for ident in &require_object.idents_to_import {
                match ident {
                    MaybeRenamed::Normal(i) => {
                        explicit_requires.insert(i.atom_identifier().unwrap().clone(), None);
                    }
                    MaybeRenamed::Renamed(from, to) => {
                        explicit_requires.insert(
                            from.atom_identifier().unwrap().clone(),
                            Some(to.atom_identifier().unwrap().clone()),
                        );
                    }
                }
            }

            // println!("{:?}", path);
            // println!("{:?}", modules.keys().collect::<Vec<_>>());
            let module = modules.get(path.as_ref()).unwrap();

            let other_module_prefix = module.prefix();

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                if module.macro_map.contains_key(qualifier) {
                                    continue;
                                }

                                match *qualifier {
                                    x if x == *REQUIRE_IDENT_SPEC => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();

                                        if !explicit_requires.is_empty()
                                            && !name
                                                .atom_identifier()
                                                .map(|x| explicit_requires.contains_key(x))
                                                .unwrap_or_default()
                                        {
                                            continue;
                                        }

                                        if module
                                            .macro_map
                                            .contains_key(name.atom_identifier().unwrap())
                                        {
                                            continue;
                                        }

                                        let hash_get = expr_list![
                                            ExprKind::atom(*PROTO_HASH_GET),
                                            ExprKind::atom(
                                                CompactString::new(MODULE_PREFIX)
                                                    + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let mut owned_name = name.clone();

                                        // If we have the alias listed, we should use it
                                        if !explicit_requires.is_empty() {
                                            if let Some(alias) = explicit_requires
                                                .get(name.atom_identifier().unwrap())
                                                .copied()
                                                .flatten()
                                            {
                                                *owned_name.atom_identifier_mut().unwrap() =
                                                    alias.clone();
                                            }
                                        }

                                        if let Some(prefix) = &require_object.prefix {
                                            if let Some(existing) = owned_name.atom_identifier_mut()
                                            {
                                                let mut prefixed_identifier = prefix.clone();
                                                prefixed_identifier.push_str(existing.resolve());

                                                // Update the existing identifier to point to a new one with the prefix applied
                                                *existing = prefixed_identifier.into();
                                            }
                                        }

                                        globals.insert(*name.atom_identifier().unwrap());

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            owned_name,
                                            hash_get,
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        provide_definitions.push(define);
                                    }

                                    _ => {
                                        stop!(TypeMismatch => format!("provide expects either an identifier, (for-syntax <ident>), or (contract/out ...) - found: {}", provide))
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            if !explicit_requires.is_empty()
                                && !provide
                                    .atom_identifier()
                                    .map(|x| explicit_requires.contains_key(x))
                                    .unwrap_or_default()
                            {
                                continue;
                            }

                            if module
                                .macro_map
                                .contains_key(provide.atom_identifier().unwrap())
                            {
                                continue;
                            }

                            // Mangle with a prefix if necessary
                            let mut provide = provide.clone();
                            let raw_provide = provide.clone();

                            // If we have the alias listed, we should use it
                            if !explicit_requires.is_empty() {
                                if let Some(alias) = explicit_requires
                                    .get(provide.atom_identifier().unwrap())
                                    .copied()
                                    .flatten()
                                {
                                    *provide.atom_identifier_mut().unwrap() = alias.clone();
                                }
                            }

                            if let Some(prefix) = &require_object.prefix {
                                if let Some(existing) = provide.atom_identifier_mut() {
                                    let mut prefixed_identifier = prefix.clone();
                                    prefixed_identifier.push_str(existing.resolve());

                                    // Update the existing identifier to point to a new one with the prefix applied
                                    *existing = prefixed_identifier.into();
                                }
                            }

                            let provide_ident = provide.atom_identifier().unwrap();

                            // Since this is now bound to be in the scope of the current working module, we also want
                            // this to be mangled. In the event we do something like, qualify the import, then we might
                            // have to mangle this differently
                            globals.insert(*provide_ident);

                            let define = ExprKind::Define(Box::new(Define::new(
                                ExprKind::atom(prefix.clone() + provide_ident.resolve()),
                                expr_list![
                                    ExprKind::atom(*PROTO_HASH_GET),
                                    ExprKind::atom(
                                        CompactString::new(MODULE_PREFIX) + &other_module_prefix
                                    ),
                                    ExprKind::Quote(Box::new(Quote::new(
                                        raw_provide.clone(),
                                        SyntaxObject::default(TokenType::Quote)
                                    )))
                                ],
                                SyntaxObject::default(TokenType::Define),
                            )));

                            // if require_object.prefix.is_some() {
                            //     println!("{}", define);
                            // }

                            provide_definitions.push(define);
                        }
                        _ => {
                            stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                        }
                    }
                }
            }
        }

        // Mangle all of the variables that are either:
        // 1. Defined locally in this file
        // 2. Required by another file
        let mut name_mangler =
            NameMangler::new(globals, prefix.clone()).in_module(&self.name.to_string_lossy());

        // Afterwards, walk through and unmangle any quoted values, since these
        // were intended to be used with non mangled values.
        // let name_unmangler = NameUnMangler::new(&prefix);

        name_mangler.mangle_vars(&mut exprs);

        // The provide definitions should also be mangled
        name_mangler.mangle_vars(&mut provide_definitions);

        // let mut hash_builder = Vec::new();

        // These are gonna be the pairs
        // hash_builder.push(());

        // Construct the series of provides as well, we'll want these to refer to the correct values
        //
        let mut provides: smallvec::SmallVec<[(ExprKind, ExprKind); 24]> = self
            .provides
            .iter()
            .flat_map(|x| &x.list().unwrap().args[1..])
            .cloned()
            .map(|x| (x.clone(), x))
            .collect();

        for provide in &mut provides {
            match &provide.1 {
                ExprKind::List(l) => {
                    if let Some(qualifier) = l.first_ident() {
                        match qualifier {
                            x if *x == *REQUIRE_IDENT_SPEC => {
                                // *provide = expand(l.get(2).unwrap().clone(), global_macro_map)?;

                                // *provide = expand(l.)

                                let (Some(name), Some(value)) = (l.get(1), l.get(2)) else {
                                    stop!(BadSyntax => "provide: contract/out expects a name and a contract"; l.location)
                                };

                                if name.atom_identifier().is_none() {
                                    stop!(BadSyntax => "provide: contract/out expects an identifier, found: {}", name; l.location)
                                }

                                provide.0 = name.clone();

                                let mut provide_expr = value.clone();
                                expand(
                                    &mut provide_expr,
                                    global_macro_map,
                                    GlobalMap::Set(&name_mangler.globals),
                                )?;

                                provide.1 = provide_expr;

                                continue;

                                // name_unmangler.unmangle_expr(provide);
                            }
                            unknown => {
                                stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...), found: {}", unknown; l.location)
                            }
                        }
                    } else {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)"; l.location)
                    }
                }
                ExprKind::Atom(_) => {
                    continue;
                }
                other => {
                    stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier"; other.span())
                }
            }
        }

        // Drop all of the macro references here
        provides.retain(|x| !self.macro_map.contains_key(x.0.atom_identifier().unwrap()));

        // We want one without the mangled version, for the actual provides
        let un_mangled = provides.clone();

        let left_unmangled: Vec<_> = un_mangled.into_iter().map(|x| x.0).collect();

        let mut right: Vec<_> = provides.into_iter().map(|x| x.1).collect();

        name_mangler.mangle_vars(&mut right);
        // name_unmangler.unmangle_vars(&mut provides);

        let mut hash_body = thin_vec![ExprKind::ident("%proto-hash%")];

        // We can put the module name in there, but that doesn't help us get the docs out...
        // Probably need to just store the docs directly in the module itself as well?
        // hash_body.push(ExprKind::atom("#:module-name"));
        // hash_body.push(ExprKind::atom(prefix.clone()));

        // left_unmangled.pretty_print();

        hash_body.extend(interleave(
            left_unmangled.into_iter().map(|x| {
                if let ExprKind::Atom(_) = x {
                    ExprKind::Quote(Box::new(Quote::new(
                        x,
                        SyntaxObject::default(TokenType::Quote),
                    )))
                } else if let ExprKind::List(l) = x {
                    if let Some(qualifier) = l.first_ident() {
                        match qualifier {
                            x if *x == *REQUIRE_IDENT_SPEC => {
                                todo!()
                            }
                            _ => {
                                return ExprKind::Quote(Box::new(Quote::new(
                                    l.get(2).unwrap().clone(),
                                    SyntaxObject::default(TokenType::Quote),
                                )))
                            }
                        }
                    }

                    // Then this is a contract out, and we should handle it here

                    ExprKind::Quote(Box::new(Quote::new(
                        l.get(2).unwrap().clone(),
                        SyntaxObject::default(TokenType::Quote),
                    )))
                    // ExprKind::Quote(Box::new(Quote::new(
                    //     x,
                    //     SyntaxObject::default(TokenType::Quote),
                    // )))
                } else {
                    panic!("TODO this shouldn't be possible")
                }
            }),
            right,
        ));

        let module_define = ExprKind::Define(Box::new(Define::new(
            ExprKind::atom(CompactString::new(MODULE_PREFIX) + &prefix),
            ExprKind::List(List::new(hash_body)),
            SyntaxObject::default(TokenType::Quote),
        )));
        // } else {
        //     println!("Didn't find any provides: {:?} - {}", self.name, prefix);

        //     let res = ExprKind::List(List::new(hash_body));

        //     res
        // };

        // println!("{}", module_define);

        // let mut offset = None;

        // Find offset of first non builtin require definition:
        // for (idx, expr) in exprs.iter().enumerate() {
        //     if let ExprKind::Define(d) = expr {
        //         // if !is_a_builtin_definition(d) || !is_a_require_definition(d) {
        //         if !is_a_builtin_definition(d) {
        //             // println!("Found offset at: {:?}", offset);

        //             offset = Some(idx);
        //             println!("Found offset at: {:?}", offset);
        //             break;
        //         }
        //     }
        // }

        exprs.push(module_define);

        // exprs.append(&mut provide_definitions);

        let mut builtin_definitions = Vec::new();

        exprs.retain_mut(|expr| {
            if let ExprKind::Define(d) = expr {
                if is_a_builtin_definition(d) {
                    builtin_definitions.push(core::mem::take(expr));
                    false
                } else {
                    true
                }
            } else {
                true
            }
        });

        builtin_definitions.append(&mut provide_definitions);

        let module = self.name.as_os_str().to_str().unwrap().to_owned();

        // Introduce a call to set the current module context?
        builtin_definitions.push(expr_list!(
            ExprKind::atom("#%prim.#%push-module-context"),
            ExprKind::string_lit(module)
        ));

        builtin_definitions.append(&mut exprs);

        builtin_definitions.push(expr_list!(ExprKind::atom("#%prim.#%pop-module-context"),));

        // provide_definitions.append(&mut builtin_definitions);
        // provide_definitions.append(&mut exprs);

        exprs = builtin_definitions;

        // if let Some(offset) = offset {
        // for (idx, expr) in provide_definitions.into_iter().enumerate() {
        //     exprs.insert(offset + idx, expr);
        // }
        // } else {
        // provide_definitions.append(&mut exprs);
        // }

        // println!("MODULE DEFINITIONS----");

        // exprs.pretty_print();

        // println!("END MODULE DEFINITIONS");

        // exprs.pretty_print();

        // exprs.push(module_define);

        // Construct the overall definition
        // TODO: Perhaps mangle these as well, especially if they have contracts associated with them

        // if offset.is_none() {
        //     provide_definitions.append(&mut exprs);
        // }

        let res = ExprKind::Begin(Box::new(Begin::new(
            exprs,
            SyntaxObject::default(TokenType::Begin),
        )));

        // Don't include this... yet
        // self.compiled_ast = Some(res.clone());

        Ok(res)
    }

    // Turn the module into the AST node that represents the macro module in the stdlib
    fn _to_module_ast_node(&self) -> ExprKind {
        let mut body = thin_vec![
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "module".into(),
            )))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                ("___".to_string() + self.name.to_str().unwrap()).into(),
            )))),
        ];

        // Put any provides at the top
        body.extend(self.provides.clone());

        // Include any dependencies here
        // body.append(&mut self.requires.clone());

        // TODO: @Matt 10/8/22
        // Reconsider how to address this expansion.
        // We really don't want to pollute the module space - perhaps disallow shadowed built-ins so we don't need this?
        // That would probably be annoying
        // let steel_base = ExprKind::List(List::new(vec![ExprKind::atom("steel/base".to_string())]));

        // self.ast.pretty_print();

        // body.push(steel_base);

        // Put the ast nodes inside the macro
        body.extend(self.ast.clone());

        // TODO clean this up
        let res = ExprKind::List(List::new(body));

        // if log_enabled!(target: "requires", log::Level::Debug) {
        //     debug!(target: "requires", "Module ast node: {}", res.to_string());
        // }

        res
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum MaybeRenamed {
    Normal(ExprKind),
    Renamed(ExprKind, ExprKind),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RequireObject {
    pub path: PathOrBuiltIn,
    pub for_syntax: bool,
    pub idents_to_import: Vec<MaybeRenamed>,
    pub prefix: Option<String>,
    pub span: Span,
    // Names left out by except-in, kept until the module's provides are known
    #[serde(default)]
    except: Vec<Excluded>,
    // Names rename-in gives other names, applied then too
    #[serde(default)]
    renames: Vec<(InternedString, InternedString)>,
}

impl RequireObject {
    fn as_identifiers(&self) -> Vec<InternedString> {
        let mut out = Vec::new();
        for identifier in &self.idents_to_import {
            if let MaybeRenamed::Normal(n) = identifier {
                if let Some(ident) = n.atom_identifier() {
                    if let Some(prefix) = self.prefix.as_ref() {
                        out.push((prefix.clone() + ident.resolve()).into());
                    }
                }
            }
        }

        out
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PathOrBuiltIn {
    BuiltIn(Cow<'static, str>),
    Path(PathBuf),
}

impl PathOrBuiltIn {
    pub fn get_path(&self) -> Cow<'_, PathBuf> {
        match self {
            Self::Path(p) => Cow::Borrowed(p),
            Self::BuiltIn(p) => Cow::Owned(PathBuf::from(p.to_string())),
        }
    }
}

#[derive(Default, Debug, Clone)]
struct RequireObjectBuilder {
    path: Option<PathOrBuiltIn>,
    for_syntax: bool,
    idents_to_import: Vec<MaybeRenamed>,
    // Built up prefix
    prefix: Option<String>,
    span: Span,
    except: Vec<Excluded>,
    renames: Vec<(InternedString, InternedString)>,
}

// A name an except-in leaves out
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Excluded {
    name: InternedString,
    // The prefix the name is written with in the except-in
    prefix: String,
    // Whether it's left out of an only-in inside the except-in, rather than out of everything
    // the module provides
    of_only_in: bool,
}

impl RequireObjectBuilder {
    fn build(self) -> Result<RequireObject> {
        let path = self
            .path
            .ok_or_else(crate::throw!(Generic => "require must have a path!"))?;

        Ok(RequireObject {
            path,
            for_syntax: self.for_syntax,
            idents_to_import: self.idents_to_import,
            prefix: self.prefix,
            span: self.span,
            except: self.except,
            renames: self.renames,
        })
    }
}

fn try_canonicalize(path: PathBuf) -> PathBuf {
    std::fs::canonicalize(&path).unwrap_or_else(|_| path)
}

/*
#[derive(Default, Clone)]
struct DependencyGraph {
    downstream: HashMap<PathBuf, Vec<PathBuf>>,
}

impl DependencyGraph {
    // Adding edges downward.
    pub fn add_edges(&mut self, parent: PathBuf, children: Vec<PathBuf>) {
        self.downstream.insert(parent, children);
    }

    pub fn remove(&mut self, parent: &PathBuf) {
        self.downstream.remove(parent);
    }

    pub fn add_edge(&mut self, parent: &PathBuf, child: PathBuf) {
        if let Some(children) = self.downstream.get_mut(parent) {
            children.push(child);
        } else {
            self.downstream.insert(parent.clone(), vec![child]);
        }
    }

    // Check everything downstream of this, to see if anything needs to be invalidated
    pub fn check_downstream_changes(
        &self,
        root: &PathBuf,
        updated_at: &crate::HashMap<PathBuf, SystemTime>,
    ) -> std::io::Result<bool> {
        let mut stack = vec![root];

        while let Some(next) = stack.pop() {
            let meta = std::fs::metadata(next)?;

            if let Some(prev) = updated_at.get(next) {
                if *prev != meta.modified()? {
                    return Ok(true);
                }
            }

            if let Some(children) = self.downstream.get(next) {
                for child in children {
                    stack.push(child);
                }
            }
        }

        Ok(false)
    }
}
*/

#[derive(Clone)]
#[allow(unused)]
pub(crate) struct CompiledModuleCache {
    cache_dir: Option<Arc<PathBuf>>,
    compiled_modules: crate::HashMap<PathBuf, CompiledModule>,
    // Phase 0 code of modules only required for syntax so far. It runs once the module is
    // required normally
    deferred: crate::HashMap<PathBuf, DeferredCode>,
}

// A module's code kept for later. It can nest too deep to drop recursively
#[derive(Clone)]
pub(crate) struct DeferredCode(ExprKind);

impl DeferredCode {
    fn into_inner(mut self) -> ExprKind {
        core::mem::take(&mut self.0)
    }
}

impl Drop for DeferredCode {
    fn drop(&mut self) {
        steel_parser::ast::drop_deep([core::mem::take(&mut self.0)]);
    }
}

// impl Drop for ModuleManager {
// fn drop(&mut self) {
// self.compiled_modules.cache_modules();
// }
// }

#[allow(unused)]
impl CompiledModuleCache {
    fn cache_module(&mut self, key: &PathBuf, module: &CompiledModule) {
        if let Some(cache_dir) = &self.cache_dir {
            let key = Self::create_key(&cache_dir, key);

            if key.exists() {
                return;
            }

            let encoded = bincode::serialize(&module).unwrap();
            log::info!("Writing to key: {:?}", key);
            let parent = key.parent().unwrap();
            if !parent.exists() {
                std::fs::create_dir_all(parent).unwrap();
            }
            if !key.exists() {
                std::fs::write(key, encoded).unwrap();
            }
        }
    }

    pub fn create_key(cache_dir: &PathBuf, key: &PathBuf) -> PathBuf {
        if key.exists() {
            let parts: PathBuf = key.components().skip(1).collect();
            cache_dir.join(parts)
        } else {
            cache_dir.join(key)
        }
    }

    pub fn get_cached(&mut self, path: &PathBuf, sources: &mut Sources) -> Option<&CompiledModule> {
        self.read_cached_module(path, sources)?;
        self.compiled_modules.get(path)
    }

    pub fn get_cached_mut(
        &mut self,
        path: &PathBuf,
        sources: &mut Sources,
    ) -> Option<&mut CompiledModule> {
        self.read_cached_module(path, sources)?;
        self.compiled_modules.get_mut(path)
    }

    // Note: Some modules simply cannot be cached.
    // If there are modules where we need the side effects of compilation, i.e.
    // anything to do with the kernel, we should designate that some side
    // effect too place and make sure to not include those in the cache.
    fn read_cached_module(&mut self, path: &PathBuf, sources: &mut Sources) -> Option<()> {
        // Load the module in the background from the cache
        static BACKGROUND_DESERIALIZING: Lazy<(
            Mutex<HashMap<PathBuf, Option<CompiledModule>>>,
            Sender<PathBuf>,
        )> = Lazy::new(|| {
            let (sender, receiver) = crossbeam_channel::unbounded();

            std::thread::spawn(move || {
                for key in receiver {
                    log::info!("Background thread received: {:?}", key);
                    let guard = BACKGROUND_DESERIALIZING.0.lock();
                    if !guard.contains_key(&key) {
                        drop(guard);
                        if let Ok(contents) = std::fs::read(&key) {
                            let value: bincode::Result<CompiledModule> =
                                bincode::deserialize(&contents);
                            match value {
                                Ok(mut value) => {
                                    value.emitted = false;
                                    value.cached_prefix =
                                        path_to_prefix_name(value.name.to_path_buf()).into();

                                    log::info!(
                                        "Successfully read cached module in background -> {:?}",
                                        key
                                    );

                                    BACKGROUND_DESERIALIZING.0.lock().insert(key, Some(value));
                                }
                                Err(e) => {
                                    log::info!("Error: {:?}", e);
                                }
                            }
                        }
                    }
                }

                log::info!("Thread finished");
            });

            (Mutex::new(HashMap::new()), sender)
        });

        fn drain_module(key: &PathBuf) -> Option<CompiledModule> {
            BACKGROUND_DESERIALIZING
                .0
                .lock()
                .get_mut(key)
                .and_then(|x| x.take())
        }

        fn enqueue_module(key: PathBuf) {
            BACKGROUND_DESERIALIZING.1.send(key).unwrap();
        }

        let now = crate::time::Instant::now();

        if let Some(cache_dir) = &self.cache_dir {
            let key = Self::create_key(cache_dir, path);

            if let Some(value) = drain_module(&key) {
                self.compiled_modules.insert(path.to_owned(), value);
                return Some(());
            }

            // Check the cache first, otherwise
            // kick off loading the background values:

            let contents = std::fs::read(&key).ok()?;
            let mut value: CompiledModule = bincode::deserialize(&contents).unwrap();

            value.emitted = false;
            value.cached_prefix = path_to_prefix_name(value.name.to_path_buf()).into();

            for downstream in value
                .downstream
                .iter()
                .chain(value.downstream_builtins.iter())
            {
                enqueue_module(Self::create_key(&cache_dir, downstream));
            }

            log::info!("Downstream: {:?}", value.downstream);
            log::info!("Downstream builtins: {:?}", value.downstream_builtins);

            self.compiled_modules.insert(path.to_owned(), value);

            log::info!(
                "Successfully read cached module: {:?} - {:?}",
                path,
                now.elapsed()
            );

            Some(())
        } else {
            None
        }
    }

    #[cfg(not(target_family = "wasm"))]
    pub fn new(compiled_modules: crate::HashMap<PathBuf, CompiledModule>) -> Self {
        let home = PathBuf::from(steel_home().unwrap());

        let cache_dir = Arc::new(home.join("cached-modules"));

        if !cache_dir.exists() {
            std::fs::create_dir(cache_dir.as_path()).ok();
        }

        Self {
            cache_dir: Some(cache_dir),
            compiled_modules,
            deferred: crate::HashMap::default(),
        }
    }

    #[cfg(target_family = "wasm")]
    pub fn new(compiled_modules: crate::HashMap<PathBuf, CompiledModule>) -> Self {
        Self {
            cache_dir: None,
            compiled_modules,
            deferred: crate::HashMap::default(),
        }
    }

    pub fn get(&mut self, key: &PathBuf, sources: &mut Sources) -> Option<&CompiledModule> {
        // if !self.compiled_modules.contains_key(key) {
        //     return self.get_cached(key, sources);
        // }

        self.compiled_modules.get(key)
    }

    pub fn cached_mut(
        &mut self,
        key: &PathBuf,
        sources: &mut Sources,
    ) -> Option<&mut CompiledModule> {
        if !self.compiled_modules.contains_key(key) {
            return self.get_cached_mut(key, sources);
        }

        self.compiled_modules.get_mut(key)
    }
}

// impl core::ops::Deref for CompiledModuleCache {
//     type Target = crate::HashMap<PathBuf, CompiledModule>;

//     fn deref(&self) -> &Self::Target {
//         &self.compiled_modules
//     }
// }

// impl core::ops::DerefMut for CompiledModuleCache {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         &mut self.compiled_modules
//     }
// }

// Emits the phase 0 code a module kept while it was only required for syntax, after the code
// of the modules it requires
fn emit_deferred(cache: &mut CompiledModuleCache, path: &PathBuf, new_exprs: &mut Vec<ExprKind>) {
    if let Some(expr) = cache.deferred.remove(path) {
        let requires = cache
            .compiled_modules
            .get(path)
            .map(|module| {
                module
                    .downstream_builtins
                    .iter()
                    .chain(&module.downstream)
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        for required in &requires {
            emit_deferred(cache, required, new_exprs);
        }

        new_exprs.push(expr.into_inner());
    }
}

struct ModuleBuilder<'a> {
    name: PathBuf,
    main: bool,
    // Only required for syntax, so its phase 0 code waits until something requires it
    for_syntax_only: bool,
    source_ast: Vec<ExprKind>,
    local_macros: FxHashSet<InternedString>,
    macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
    // TODO: Change the requires / requires_for_syntax to just be a require enum?
    require_objects: Vec<RequireObject>,

    provides: Vec<ExprKind>,
    provides_for_syntax: Vec<ExprKind>,
    compiled_modules: &'a mut CompiledModuleCache,
    visited: &'a mut FxHashSet<PathBuf>,
    file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
    sources: &'a mut Sources,
    kernel: &'a mut Option<Kernel>,
    builtin_modules: ModuleContainer,
    global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
    custom_builtins: &'a HashMap<String, String>,
    search_dirs: &'a [PathBuf],
    module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
    prelude_string: &'a str,
    origins: &'a mut ModuleOrigins,
}

// How many rounds the two kinds of macros can take turns expanding a form. Each level of a
// syntax-rules macro expanding to a procedural one takes a round.
const MAX_EXPANSION_ROUNDS: usize = 4096;

// The names a program's top level defines, before its defines are parsed
fn defined_names(expr: &ExprKind, names: &mut FxHashSet<InternedString>) {
    match expr {
        ExprKind::Define(d) => names.extend(d.name.atom_identifier()),
        ExprKind::Begin(b) => b.exprs.iter().for_each(|x| defined_names(x, names)),
        ExprKind::List(l) if crate::parser::expand_visitor::is_define(l) => {
            let name = match l.get(1) {
                Some(ExprKind::List(f)) => f.args.first(),
                name => name,
            };

            names.extend(name.and_then(ExprKind::atom_identifier));
        }
        ExprKind::List(l) if l.first_ident() == Some(&*BEGIN) => {
            l.args.iter().skip(1).for_each(|x| defined_names(x, names))
        }
        _ => {}
    }
}

// Whether this is what a require-builtin expands to. A module can redefine the names it imports
fn is_builtin_import(form: &ExprKind) -> bool {
    fn imports(form: &ExprKind) -> bool {
        let value = match form {
            ExprKind::Define(d) => &d.body,
            ExprKind::List(l)
                if l.first_ident().is_some_and(|x| x.resolve() == "define") && l.len() == 3 =>
            {
                &l.args[2]
            }
            _ => return false,
        };
        value
            .list()
            .and_then(|x| x.first_ident())
            .is_some_and(|x| x.resolve() == "%module-get%")
    }

    match form {
        ExprKind::Begin(b) => b.exprs.iter().any(imports),
        ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => {
            l.args[1..].iter().any(imports)
        }
        form => imports(form),
    }
}

// The name of a define-syntax, as written
fn define_syntax_name(form: &ExprKind) -> Option<&SyntaxObject> {
    match form {
        ExprKind::Macro(m) => m.name.atom_syntax_object(),
        ExprKind::List(l) if l.is_define_syntax() => match l.get(1)? {
            ExprKind::List(header) => header.first()?.atom_syntax_object(),
            name => name.atom_syntax_object(),
        },
        _ => None,
    }
}

// Spans of the macro names a form defines, keyed by the names as written
fn defined_name_spans(form: &ExprKind, spans: &mut FxHashMap<InternedString, Span>) {
    match form {
        ExprKind::Begin(b) => b.exprs.iter().for_each(|x| defined_name_spans(x, spans)),
        ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => l.args[1..]
            .iter()
            .for_each(|x| defined_name_spans(x, spans)),
        form => {
            if let Some(syn) = define_syntax_name(form) {
                if let TokenType::Identifier(name) = syn.ty {
                    spans.insert(name, syn.span);
                }
            }
        }
    }
}

// A begin written by a macro can define each macro only once, like a body
fn check_begin_definitions(forms: &[ExprKind]) -> Result<()> {
    let mut seen = FxHashSet::default();

    for syn in forms.iter().filter_map(define_syntax_name) {
        if let TokenType::Identifier(name) = syn.ty {
            if !seen.insert(name) {
                stop!(BadSyntax => format!("define-syntax: duplicate binding name: {name}"); syn.span);
            }
        }
    }

    Ok(())
}

// Takes the forms out of a non-empty begin. Macros a template defines in it are renamed for
// the begin first, as they would be inside it.
fn begin_forms(expr: &mut ExprKind) -> Option<Vec<ExprKind>> {
    let mut forms: Vec<ExprKind> = match expr {
        ExprKind::Begin(b) if !b.exprs.is_empty() => core::mem::take(&mut b.exprs),
        ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) && l.args.len() > 1 => {
            l.args.drain(1..).collect()
        }
        _ => return None,
    };

    crate::parser::expand_visitor::rename_template_macros(&mut forms, true);
    Some(forms)
}

// A written procedural define-syntax spliced out of a begin gets the same head as one written at
// the top level
fn as_written_top_level(form: &mut ExprKind) {
    if let ExprKind::List(l) = form {
        let head = l.first().and_then(ExprKind::atom_syntax_object);
        if head.is_some_and(|x| x.ty == TokenType::DefineSyntax)
            && crate::parser::expand_visitor::procedural_macro_name(l).is_some()
        {
            let span = head.map(|x| x.span).unwrap_or_default();
            l.args[0] = ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::Identifier(*steel_parser::ast::DEFINE_SYNTAX),
                span,
            )));
        }
    }
}

// Splices begins into the module's forms, like a Racket module body
fn splice_begins(forms: Vec<ExprKind>) -> Vec<ExprKind> {
    let mut spliced = Vec::with_capacity(forms.len());

    fn splice(mut form: ExprKind, out: &mut Vec<ExprKind>) {
        match begin_forms(&mut form) {
            Some(forms) => forms.into_iter().for_each(|x| splice(x, out)),
            None => out.push(form),
        }
    }

    forms.into_iter().for_each(|x| splice(x, &mut spliced));
    spliced
}

// Whether a form defines a macro, as written or as a begin of such forms
fn defines_macros(expr: &ExprKind) -> bool {
    let mut names = FxHashSet::default();
    crate::parser::expand_visitor::defined_macro_names(expr, &mut names);

    if !names.is_empty() {
        return true;
    }

    // Also a procedural one with definitions in its transformer
    let written = expr.list().and_then(|l| l.first()).is_some_and(|head| {
        matches!(
            head,
            ExprKind::Atom(Atom {
                syn: SyntaxObject {
                    ty: TokenType::DefineSyntax,
                    ..
                },
            })
        ) || head.atom_identifier().is_some_and(|x| {
            matches!(
                x.resolve(),
                "define-syntax" | "define-syntax-rule" | "defmacro"
            )
        })
    });

    if written {
        return true;
    }

    match expr {
        ExprKind::Begin(b) => b.exprs.iter().any(defines_macros),
        ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => {
            l.args[1..].iter().any(defines_macros)
        }
        _ => false,
    }
}

// Turns macro definitions in a procedural macro's output back into lists, the way the kernel
// expander leaves them for the syntax-rules expansion that follows
fn macros_as_lists(expr: &mut ExprKind) {
    fn as_list(expr: &mut ExprKind) {
        if let ExprKind::Macro(_) = expr {
            let ExprKind::Macro(m) = core::mem::replace(expr, ExprKind::empty()) else {
                unreachable!()
            };
            *expr = ExprKind::List(crate::parser::tryfrom_visitor::macro_as_list(*m));
        }
    }

    struct AsLists;

    impl VisitorMutRefUnit for AsLists {
        fn visit_list(&mut self, l: &mut List) {
            for arg in l.args.iter_mut() {
                as_list(arg);
                self.visit(arg);
            }
        }

        fn visit_begin(&mut self, b: &mut Begin) {
            for expr in b.exprs.iter_mut() {
                as_list(expr);
                self.visit(expr);
            }
        }
    }

    as_list(expr);
    AsLists.visit(expr);
}

// Expands a form's head until it's a core form, to tell whether it defines a macro. Like in a
// Racket module, every form's head is expanded, in order, before any of their bodies.
fn expand_head(
    expr: &mut ExprKind,
    expander: &mut ExpanderMany,
    environment: &str,
    globals: GlobalMap,
) -> Result<()> {
    for _ in 0..MAX_EXPANSION_ROUNDS {
        let next = match expr {
            // A begin's forms are the module's forms
            ExprKind::Begin(b) => {
                for form in b.exprs.iter_mut() {
                    expand_head(form, expander, environment, globals)?;
                }
                return Ok(());
            }
            ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => {
                for form in l.args[1..].iter_mut() {
                    expand_head(form, expander, environment, globals)?;
                }
                return Ok(());
            }
            // A macro definition is already a core form
            ExprKind::List(l)
                if l.is_define_syntax() || defines_macros(&ExprKind::List(l.clone())) =>
            {
                return Ok(());
            }
            ExprKind::List(l) => {
                let Some(head) = l.first_ident().copied() else {
                    return Ok(());
                };

                // A head written by a template means what it means where the template is. The
                // full expansion works that out
                if l.first()
                    .and_then(ExprKind::atom_syntax_object)
                    .is_some_and(|x| x.scopes.introduced())
                {
                    return Ok(());
                }

                let span = l
                    .first()
                    .and_then(ExprKind::atom_syntax_object)
                    .map(|x| x.span)
                    .unwrap_or_default();

                // Look for a required module's syntax-rules macro first, then the module's own
                let required = expander
                    .overlays
                    .iter()
                    .position(|x| x.map.in_scope_macros.contains_key(&head));
                let rules = match required {
                    Some(index) => expander.overlays[index].map.in_scope_macros.get(&head),
                    None => expander.map.get(&head),
                };

                if let Some(m) = rules {
                    let list = core::mem::replace(l, List::new(ThinVec::new()));
                    let (expanded, introduction) =
                        m.expand_marked(list, span, &Default::default(), globals)?;

                    if let Some(index) = required {
                        crate::parser::expand_visitor::record_foreign(introduction);
                        expander.overlays[index].changed = true;
                    }

                    Some(expanded)
                } else if let Some(kernel) =
                    expander.kernel.as_mut().and_then(|x| x.map.as_deref_mut())
                {
                    // A procedural macro: a required module's, expanded where it's defined, or
                    // the module's own
                    let provided = expander
                        .overlays
                        .iter()
                        .position(|x| x.map.provided_kernel_macros.contains_key(&head));

                    if let Some(index) = provided {
                        let required = &mut expander.overlays[index].map;
                        let name = required.provided_kernel_macros[&head];
                        let list = core::mem::replace(l, List::new(ThinVec::new()));
                        let mut expanded = kernel.expand_syntax_object(
                            &name,
                            ExprKind::List(list),
                            &required.module_name,
                            Some(&mut required.name_mangler),
                        )?;
                        macros_as_lists(&mut expanded);
                        Some(expanded)
                    } else if kernel.contains_syntax_object_macro(&head, Some(environment)) {
                        let list = core::mem::replace(l, List::new(ThinVec::new()));
                        let mut expanded = kernel.expand_syntax_object(
                            &head,
                            ExprKind::List(list),
                            environment,
                            None,
                        )?;
                        macros_as_lists(&mut expanded);
                        Some(expanded)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        match next {
            Some(expanded) => *expr = expanded,
            None => return Ok(()),
        }
    }

    Ok(())
}

// The names a module provides
fn provided_names(module: &CompiledModule) -> FxHashSet<InternedString> {
    module
        .provides
        .iter()
        .filter_map(ExprKind::list)
        .flat_map(|x| x.args.iter().skip(1))
        .filter_map(ExprKind::atom_identifier)
        .chain(module.provides_for_syntax.iter())
        .copied()
        .collect()
}

// The module's own macros can make begin-for-syntax forms, which have to be found before
// its transformers are loaded. Only the uses that make one are expanded this early.
fn expand_phase1_forms(
    ast: &mut [ExprKind],
    macro_map: &FxHashMap<InternedString, SteelMacro>,
) -> Result<()> {
    // Finds templates that mention `begin-for-syntax`
    struct NamesBeginForSyntax(bool);

    impl<'a> crate::compiler::passes::VisitorMutUnitRef<'a> for NamesBeginForSyntax {
        fn visit_atom(&mut self, a: &'a Atom) {
            self.0 |= a
                .ident()
                .is_some_and(|x| *x == *crate::compiler::program::BEGIN_FOR_SYNTAX);
        }
    }

    let makes_phase1 = macro_map.values().any(|m| {
        m.cases.iter().any(|case| {
            let mut names = NamesBeginForSyntax(false);
            crate::compiler::passes::VisitorMutUnitRef::visit(&mut names, &case.body);
            names.0
        })
    });

    if !makes_phase1 {
        return Ok(());
    }

    let globals = collect_globals(ast);

    // A use of one of the macros, or a begin containing one
    fn uses_macro(expr: &ExprKind, macro_map: &FxHashMap<InternedString, SteelMacro>) -> bool {
        match expr {
            ExprKind::Begin(b) => b.exprs.iter().any(|x| uses_macro(x, macro_map)),
            ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => {
                l.args[1..].iter().any(|x| uses_macro(x, macro_map))
            }
            ExprKind::List(l) => l
                .first_ident()
                .is_some_and(|name| macro_map.contains_key(name)),
            _ => false,
        }
    }

    for expr in ast.iter_mut() {
        if uses_macro(expr, macro_map) {
            let mut expanded = expr.clone();
            expand(&mut expanded, macro_map, GlobalMap::Set(&globals))?;

            if crate::parser::kernel::has_phase1(&expanded) {
                *expr = expanded;
            }
        }
    }

    Ok(())
}

fn procedural_for_syntax(
    kernel: &Kernel,
    module: &CompiledModule,
    require_object: &RequireObject,
) -> FxHashMap<InternedString, (String, InternedString)> {
    let environment = module.name.to_str().unwrap_or_default().to_owned();

    provided_kernel_macros(Some(kernel), module, require_object)
        .into_iter()
        .map(|(local, name)| (local, (environment.clone(), name)))
        .collect()
}

// The procedural macros a module provides to a require, keyed by the names they're required as
fn provided_kernel_macros(
    kernel: Option<&Kernel>,
    module: &CompiledModule,
    require_object: &RequireObject,
) -> FxHashMap<InternedString, InternedString> {
    let Some(defined) =
        kernel.and_then(|kernel| kernel.exported_defmacros(module.name.to_str().unwrap()))
    else {
        return FxHashMap::default();
    };

    let provided: FxHashSet<InternedString> = module
        .provides
        .iter()
        .filter_map(ExprKind::list)
        .flat_map(|x| x.args.iter().skip(1))
        .filter_map(ExprKind::atom_identifier)
        .chain(module.provides_for_syntax.iter())
        .filter(|name| defined.contains(*name))
        .copied()
        .collect();

    as_required(&provided, require_object)
}

// The `begin-for-syntax` bindings a module provides to a require
fn provided_phase1_bindings(
    kernel: Option<&Kernel>,
    module: &CompiledModule,
    require_object: &RequireObject,
) -> Vec<Phase1Import> {
    let Some(kernel) = kernel else {
        return Vec::new();
    };

    let environment = module.name.to_str().unwrap();

    let provided: FxHashSet<InternedString> = module
        .provides_for_syntax
        .iter()
        .filter(|name| kernel.defines_for_syntax(environment, name))
        .copied()
        .collect();

    as_required(&provided, require_object)
        .into_iter()
        .map(|(local, name)| Phase1Import::for_syntax_binding(local, environment, name))
        .collect()
}

static PHASE1_INSTANCES: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);

// The prelude and Steel's builtin modules have a single phase 1 instance, shared by all
// transformer code
fn shared_at_phase1(module: &Path) -> bool {
    let name = module.to_str().unwrap_or_default();
    PRELUDE_MODULES.contains(&name) || BUILT_INS.iter().any(|x| x.0 == name)
}

struct CollectIdentifiers(FxHashSet<InternedString>);

impl VisitorMutRefUnit for CollectIdentifiers {
    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(name) = a.syn.ty.identifier() {
            self.0.insert(*name);
        }
    }
}

struct CollectDefinitions(FxHashSet<InternedString>);

impl VisitorMutRefUnit for CollectDefinitions {
    fn visit_define(&mut self, define: &mut Define) {
        if let Some(name) = define.name.atom_identifier() {
            self.0.insert(*name);
        }
    }
}

// A module instantiated for syntax
struct ForSyntaxInstance {
    imports: Vec<Phase1Import>,
    // The code of the module and the modules it requires, under their own names
    code: Vec<ExprKind>,
    // New names the instance gave those modules, where it needed to
    renames: Vec<(String, String)>,
    // The prefixes of the modules in `code`
    prefixes: Vec<String>,
}

// Instantiates the module of a `for-syntax` require in the kernel, along with the modules it
// requires, and returns the bindings it provides to transformer code. With `rename`, the
// instance gives those modules names of its own. Without it, it uses the modules' own names,
// which is fine when compiling a module, since other modules get their own instances.
// `instantiated` holds the modules this compile already has instances of.
fn instantiate_for_syntax(
    kernel: &mut Kernel,
    modules: &crate::HashMap<PathBuf, CompiledModule>,
    global_macro_map: &FxHashMap<InternedString, SteelMacro>,
    host_modules: &ModuleContainer,
    require_object: &RequireObject,
    rename: bool,
    macros: &FxHashMap<InternedString, SteelMacro>,
    instantiated: &mut FxHashMap<PathBuf, String>,
) -> Result<ForSyntaxInstance> {
    if kernel.share_host_modules {
        for module in host_modules.inner().values() {
            kernel.share_module(module);
        }
    }

    fn dependencies_first(
        path: PathBuf,
        modules: &crate::HashMap<PathBuf, CompiledModule>,
        order: &mut Vec<PathBuf>,
    ) {
        if order.contains(&path) {
            return;
        }

        // A builtin module's code uses the prelude without requiring it
        let name = path.to_str().unwrap_or_default();

        if BUILT_INS.iter().any(|x| x.0 == name) && !PRELUDE_MODULES.contains(&name) {
            for prelude in PRELUDE_MODULES {
                dependencies_first(PathBuf::from(*prelude), modules, order);
            }
        }

        if let Some(module) = modules.get(&path) {
            for required in module.require_objects.iter().filter(|x| !x.for_syntax) {
                dependencies_first(required.path.get_path().into_owned(), modules, order);
            }
        }

        order.push(path);
    }

    let path = require_object.path.get_path().into_owned();

    let mut order = Vec::new();
    dependencies_first(path.clone(), modules, &mut order);

    let instance = PHASE1_INSTANCES.fetch_add(1, core::sync::atomic::Ordering::Relaxed);

    // The module being compiled gets one instance of each module, no matter which of its
    // requires reach it
    let fresh: Vec<PathBuf> = order
        .iter()
        .filter(|x| !shared_at_phase1(x) && !instantiated.contains_key(*x))
        .cloned()
        .collect();

    for path in &fresh {
        if let Some(module) = modules.get(path) {
            let prefix = module.prefix().to_string();
            let renamed = if rename {
                prefix.replacen(
                    MANGLER_SEPARATOR,
                    &format!("#phase1-{instance}{MANGLER_SEPARATOR}"),
                    1,
                )
            } else {
                prefix
            };
            instantiated.insert(path.clone(), renamed);
        }
    }

    let prefixes: Vec<(String, String)> = order
        .iter()
        .filter(|x| rename && !shared_at_phase1(x))
        .filter_map(|x| {
            Some((
                modules.get(x)?.prefix().to_string(),
                instantiated.get(x)?.clone(),
            ))
        })
        .collect();

    let own_prefixes: Vec<String> = fresh
        .iter()
        .filter_map(|x| modules.get(x))
        .map(|module| module.prefix().to_string())
        .collect();

    let mut code = Vec::new();
    let mut own_code = Vec::new();
    let mut shared = Vec::new();
    let mut after_prelude = None;

    for module_path in &order {
        let Some(module) = modules.get(module_path) else {
            continue;
        };

        let is_shared = shared_at_phase1(module_path);

        if !is_shared && !fresh.contains(module_path) {
            continue;
        }

        if is_shared
            && (shared.contains(module_path)
                || kernel.has_shared_instance(
                    module_path,
                    &format!("{MODULE_PREFIX}{}", module.prefix()),
                ))
        {
            continue;
        }

        let mut expr = module
            .clone()
            .to_top_level_module(modules, global_macro_map)?;

        if !PRELUDE_MODULES.contains(&module_path.to_str().unwrap_or_default()) {
            after_prelude.get_or_insert(code.len());
        }

        if is_shared {
            shared.push(module_path.clone());
        } else {
            own_code.push(expr.clone());

            RenameInstance {
                prefixes: &prefixes,
            }
            .visit(&mut expr);
        }

        code.push(expr);
    }

    // A builtin module's code refers to the prelude's provides as globals, which a program's
    // prelude defines, so define them for transformer code too
    let mut prelude = Vec::new();

    for name in PRELUDE_MODULES {
        let Some(module) = modules.get(&PathBuf::from(*name)) else {
            continue;
        };

        let table = format!("{MODULE_PREFIX}{}", module.prefix());

        if !shared.contains(&module.name) && !kernel.defines(&table) {
            continue;
        }

        for provided in provided_names(module) {
            if module.macro_map.contains_key(&provided) || kernel.defines(provided.resolve()) {
                continue;
            }

            prelude.push(ExprKind::List(List::new(thin_vec![
                ExprKind::ident("define"),
                ExprKind::atom(provided),
                ExprKind::List(List::new(thin_vec![
                    ExprKind::ident("%proto-hash-get%"),
                    ExprKind::ident(&table),
                    ExprKind::Quote(Box::new(Quote::new(
                        ExprKind::atom(provided),
                        SyntaxObject::default(TokenType::Quote),
                    ))),
                ])),
            ])));
        }
    }

    let at = after_prelude.unwrap_or(code.len());
    code.splice(at..at, prelude);

    // The compiler drops globals that nothing refers to, and some of them are only referred to
    // by the macros' templates
    let mut templates = CollectIdentifiers(FxHashSet::default());

    for m in macros.values() {
        for expr in m.clone().exprs_mut() {
            templates.visit(expr);
        }
    }

    let mut defined = CollectDefinitions(FxHashSet::default());

    for expr in code.iter_mut() {
        defined.visit(expr);
    }

    let mut kept = thin_vec![ExprKind::ident("list")];
    let mut own_kept = thin_vec![ExprKind::ident("list")];

    for name in templates.0 {
        let renamed: InternedString = prefixes
            .iter()
            .find(|(from, _)| name.resolve().contains(from.as_str()))
            .map(|(from, to)| name.resolve().replacen(from.as_str(), to, 1).into())
            .unwrap_or(name);

        if defined.0.contains(&renamed) {
            kept.push(ExprKind::atom(renamed));
            own_kept.push(ExprKind::atom(name));
        }
    }

    code.push(ExprKind::List(List::new(kept)));
    own_code.push(ExprKind::List(List::new(own_kept)));

    kernel.instantiate(code, shared).map_err(|mut err| {
        err.prepend_message(&format!(
            "instantiating {} for syntax: ",
            path.to_str().unwrap_or_default()
        ));
        err
    })?;

    let Some(module) = modules.get(&path) else {
        return Ok(ForSyntaxInstance {
            imports: Vec::new(),
            code: own_code,
            renames: prefixes,
            prefixes: own_prefixes,
        });
    };

    let table = prefixes
        .iter()
        .find(|(from, _)| *from == module.prefix().as_str())
        .map(|(_, to)| format!("{MODULE_PREFIX}{to}"))
        .unwrap_or_else(|| format!("{MODULE_PREFIX}{}", module.prefix()));

    let provided: FxHashSet<InternedString> = module
        .provides
        .iter()
        .filter_map(ExprKind::list)
        .flat_map(|x| x.args.iter().skip(1))
        .filter_map(|x| match x {
            ExprKind::List(l) => l.second_ident(),
            x => x.atom_identifier(),
        })
        .filter(|name| !module.macro_map.contains_key(*name))
        .copied()
        .collect();

    Ok(ForSyntaxInstance {
        imports: as_required(&provided, require_object)
            .into_iter()
            .map(|(local, name)| Phase1Import::provided(local, &table, name))
            .collect(),
        code: own_code,
        renames: prefixes,
        prefixes: own_prefixes,
    })
}

// Names a module provides, keyed by the names a require gives them
fn as_required(
    provided: &FxHashSet<InternedString>,
    require_object: &RequireObject,
) -> FxHashMap<InternedString, InternedString> {
    let required: FxHashMap<InternedString, InternedString> =
        if require_object.idents_to_import.is_empty() {
            provided.iter().map(|name| (*name, *name)).collect()
        } else {
            require_object
                .idents_to_import
                .iter()
                .filter_map(|maybe| match maybe {
                    MaybeRenamed::Normal(n) => n.atom_identifier().map(|name| (*name, *name)),
                    MaybeRenamed::Renamed(from, to) => {
                        Some((*to.atom_identifier()?, *from.atom_identifier()?))
                    }
                })
                .filter(|(_, name)| provided.contains(name))
                .collect()
        };

    match &require_object.prefix {
        Some(prefix) => required
            .into_iter()
            .map(|(local, name)| ((prefix.clone() + local.resolve()).into(), name))
            .collect(),
        None => required,
    }
}

#[derive(Debug)]
pub struct RequiredModuleMacros {
    pub module_key: PathBuf,
    pub module_name: String,
    pub module_macros: Arc<FxHashMap<InternedString, SteelMacro>>,
    pub in_scope_macros: FxHashMap<InternedString, SteelMacro>,
    pub name_mangler: NameMangler,
    // The procedural macros the module provides, keyed by the names they're required as
    pub provided_kernel_macros: FxHashMap<InternedString, InternedString>,
}

impl<'a> ModuleBuilder<'a> {
    #[allow(clippy::too_many_arguments)]
    #[allow(unused)]
    fn main(
        name: Option<PathBuf>,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut CompiledModuleCache,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
        prelude_string: &'a str,
        origins: &'a mut ModuleOrigins,
    ) -> Result<Self> {
        // TODO don't immediately canonicalize the path unless we _know_ its coming from a path
        // change the path to not always be required
        // if its not required we know its not coming in

        #[cfg(not(target_family = "wasm"))]
        let name = if let Some(p) = name {
            std::fs::canonicalize(p)?
        } else {
            std::env::current_dir()?
        };

        #[cfg(target_family = "wasm")]
        let name = PathBuf::new();

        Ok(ModuleBuilder {
            name,
            main: true,
            for_syntax_only: false,
            source_ast,
            local_macros: FxHashSet::default(),
            macro_map: default_prelude_macros(),
            require_objects: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
            prelude_string,
            origins,
        })
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        // println!("Compiling: {:?}", self.name);

        // debug!(target: "requires", "Visiting: {:?}", self.name);

        // @Matt - 10/3/23
        // This has a relatively fatal flaw at the moment:
        /*

            (define-syntax register-plugin
              (syntax-rules ()
                [(register-plugin plugin-path identifiers ...)
                 (begin
                   (require plugin-path
                            (only-in identifiers ...))
                   (provide identifiers ...))]))
        */
        // This will fail to compile - anything that expands into a require will fail since
        // require is more or less a top level primitive. What we need to do is figure
        // out a way to have this be recursive - the whole compilation step should
        // go again once we discover that there is another require. It shouldn't be too bad,
        // but the base case has to be figured out such that we don't get infinite recursion.
        // I think the condition is probably something along the following:
        //
        // - Did we expand anything
        // - Are there require statements
        //
        // If we expanded anything, we then should check for require statements... maybe

        let mut new_exprs = Vec::new();

        // self.source_ast.pretty_print();

        self.collect_requires()?;

        self.collect_provides()?;

        // if log_enabled!(log::Level::Info) {
        // debug!(target: "requires", "Requires: {:#?}", self.require_objects);
        // debug!(target: "requires", "Provides: {:#?}", self.provides);
        // debug!(target: "requires", "Provides for-syntax: {:?}", self.provides_for_syntax);
        // }

        if self.visited.contains(&self.name) {
            stop!(Generic => format!("circular dependency found during module resolution with: {:?}", self.name))
        }

        self.visited.insert(self.name.clone());

        if self.main {
            let exprs = core::mem::take(&mut self.source_ast);
            self.source_ast = exprs
                .into_iter()
                .filter(|x| {
                    if let ExprKind::List(l) = x {
                        if let Some(provide) = l.first_ident() {
                            return *provide != *PROVIDE;
                        }
                    }
                    true
                })
                .collect();
        }

        self.extract_macro_defs()?;

        // TODO include built ins here
        if self.require_objects.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            let expr = self.compile_module()?;
            self.emit(&mut new_exprs, expr);
        } else {
            let required_at_phase0: FxHashSet<PathBuf> = self
                .require_objects
                .iter()
                .filter(|x| !x.for_syntax)
                .map(|x| x.path.get_path().into_owned())
                .collect();

            // TODO come back for parsing built ins
            for module in self
                .require_objects
                .iter()
                .filter(|x| matches!(x.path, PathOrBuiltIn::BuiltIn(_)))
                .map(|x| x.path.get_path())
            {
                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                // If we already have compiled this module, get it from the cache
                let for_syntax_only =
                    self.for_syntax_only || !required_at_phase0.contains(module.as_ref());

                if self
                    .compiled_modules
                    .compiled_modules
                    .contains_key(module.as_ref())
                {
                    if !for_syntax_only {
                        emit_deferred(self.compiled_modules, module.as_ref(), &mut new_exprs);
                    }
                    continue;
                }

                // TODO this is some bad crap here don't do this
                let input = BUILT_INS
                    .iter()
                    .find(|x| x.0 == module.to_str().unwrap())
                    .map(|x| Cow::Borrowed(x.1))
                    .or_else(|| {
                        self.custom_builtins
                            .get(module.to_str().unwrap())
                            .map(|x| Cow::Owned(x.to_string()))
                    })
                    .or_else(|| {
                        self.module_resolvers
                            .iter()
                            .find_map(|x| x.resolve(module.to_str().unwrap()))
                            // Insert the prelude
                            .map(|mut x| {
                                x.insert_str(0, self.prelude_string);
                                x
                            })
                            .map(Cow::Owned)
                    })
                    .ok_or_else(
                        crate::throw!(Generic => "Unable to find builtin module: {:?}", module),
                    )?;

                let mut new_module = ModuleBuilder::new_built_in(
                    module.into_owned(),
                    input,
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                    self.global_macro_map,
                    self.custom_builtins,
                    self.module_resolvers,
                    self.prelude_string,
                    self.origins,
                )?;

                new_module.for_syntax_only = for_syntax_only;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                new_exprs.append(&mut module_exprs);

                // Probably want to evaluate a module even if it has no provides?
                if !new_module.provides.is_empty() {
                    let expr = new_module.compile_module()?;
                    new_module.emit(&mut new_exprs, expr);
                } else if !new_module
                    .compiled_modules
                    .compiled_modules
                    .contains_key(&new_module.name)
                {
                    let expr = new_module.compile_module()?;
                    new_module.emit(&mut new_exprs, expr);
                }
            }

            // At this point, requires should be fully qualified (absolute) paths

            for (module, require_statement_span) in self
                .require_objects
                .iter()
                .filter(|x| matches!(x.path, PathOrBuiltIn::Path(_)))
                .map(|x| (x.path.get_path(), x.span))
            {
                if cfg!(target_family = "wasm") {
                    stop!(Generic => "requiring modules is not supported for wasm");
                }

                let last_modified = std::fs::metadata(module.as_ref())
                    .map_err(|err| {
                        let mut err = crate::SteelErr::from(err);
                        err.prepend_message(&format!(
                            "Attempting to load module from: {:?} ",
                            module
                        ));
                        err.set_span(require_statement_span)
                    })?
                    .modified()?;

                // Check if we should compile based on the last time modified
                // If we're unable to get information, we want to compile
                let should_recompile =
                    if let Some(cached_modified) = self.file_metadata.get(module.as_ref()) {
                        last_modified != *cached_modified
                    } else {
                        true
                    };

                let mut downstream_validated = true;

                let for_syntax_only =
                    self.for_syntax_only || !required_at_phase0.contains(module.as_ref());

                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                if !should_recompile {
                    // If we already have compiled this module, get it from the cache
                    if let Some(m) = self.compiled_modules.get(module.as_ref(), self.sources) {
                        // debug!("Getting {:?} from the module cache", module);
                        // println!("Already found in the cache: {:?}", module);
                        // new_exprs.push(m.to_module_ast_node());
                        // No need to do anything

                        // Check the dependencies all the way down.
                        // if any are invalidated, re-compile the whole thing.
                        let mut stack = m.downstream.clone();

                        while let Some(next) = stack.pop() {
                            let meta = std::fs::metadata(&next)?;

                            if let Some(prev) = self.file_metadata.get(&next) {
                                if *prev != meta.modified()? {
                                    // println!(
                                    //     "Detected change in {:?}, recompiling root starting from {:?}",
                                    //     next,
                                    //     module.as_ref()
                                    // );
                                    downstream_validated = false;
                                    break;
                                }
                            }

                            if let Some(module) = self.compiled_modules.get(&next, self.sources) {
                                for child in &module.downstream {
                                    stack.push(child.to_owned());
                                }
                            }
                        }

                        if downstream_validated {
                            if !for_syntax_only {
                                emit_deferred(
                                    self.compiled_modules,
                                    module.as_ref(),
                                    &mut new_exprs,
                                );
                            }
                            continue;
                        }
                    }
                }

                // TODO: Parallelize this?
                // There is quite the opportunity to parallelize a chunk
                // of the compilation.

                let mut new_module = ModuleBuilder::new_from_path(
                    module.into_owned(),
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                    self.global_macro_map,
                    self.custom_builtins,
                    self.search_dirs,
                    self.module_resolvers,
                    self.prelude_string,
                    self.origins,
                )?;

                new_module.for_syntax_only = for_syntax_only;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                new_exprs.append(&mut module_exprs);

                // If we need to, revisit because there are new provides
                if !new_module.provides.is_empty() {
                    let expr = new_module.compile_module()?;
                    new_module.emit(&mut new_exprs, expr);
                // If the module hasn't yet been compiled, compile it anyway
                } else if !new_module
                    .compiled_modules
                    .compiled_modules
                    .contains_key(&new_module.name)
                {
                    // else if !new_module.compiled_modules.contains_key(&new_module.name) {
                    let expr = new_module.compile_module()?;
                    new_module.emit(&mut new_exprs, expr);
                } else if !downstream_validated {
                    let expr = new_module.compile_module()?;
                    new_module.emit(&mut new_exprs, expr);
                }
            }
        }

        self.resolve_except_in()?;

        // log::info!("Compiling module: {:?} took {:?}", self.name, now.elapsed());
        // new_exprs.pretty_print();

        Ok(new_exprs)
    }

    // Where the macro the module imports as `name` is defined
    fn reexport(&self, name: &InternedString) -> Option<Reexport> {
        let kernel = self.kernel.as_ref();

        self.require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .find_map(|require_object| {
                let module = self
                    .compiled_modules
                    .compiled_modules
                    .get(require_object.path.get_path().as_ref())?;

                let defined = kernel
                    .and_then(|kernel| kernel.exported_defmacros(module.name.to_str()?))
                    .unwrap_or_default();

                let imported = as_required(&provided_names(module), require_object);

                if let Some(original) = imported.get(name) {
                    if module.macro_map.contains_key(original) || defined.contains(original) {
                        return Some(Reexport {
                            source: require_object.path.clone(),
                            name: *original,
                            provided: *name,
                        });
                    }
                }

                // Re-exported by the module it's required from
                let provided: FxHashSet<InternedString> =
                    module.reexports.iter().map(|x| x.provided).collect();
                let original = as_required(&provided, require_object).remove(name)?;
                let reexport = module.reexports.iter().find(|x| x.provided == original)?;

                Some(Reexport {
                    source: reexport.source.clone(),
                    name: reexport.name,
                    provided: *name,
                })
            })
    }

    // Now that the module is compiled, an except-in imports everything it provides except the
    // names left out, and a rename-in everything with the names it gives
    fn resolve_except_in(&mut self) -> Result<()> {
        for require_object in self.require_objects.iter_mut() {
            if require_object.except.is_empty() && require_object.renames.is_empty() {
                continue;
            }

            let path = require_object.path.get_path().into_owned();

            let Some(module) = self.compiled_modules.get(&path, self.sources) else {
                continue;
            };

            let provided: Vec<MaybeRenamed> = module
                .provides
                .iter()
                .filter_map(ExprKind::list)
                .flat_map(|provide| provide.args.iter().skip(1))
                .filter_map(|provide| {
                    provide
                        .atom_identifier()
                        .or_else(|| provide.list().and_then(|x| x.second_ident()))
                        .copied()
                })
                .chain(module.provides_for_syntax.iter().copied())
                .chain(module.reexports.iter().map(|x| x.provided))
                .map(|name| MaybeRenamed::Normal(ExprKind::atom(name)))
                .collect();

            let only_in = core::mem::take(&mut require_object.idents_to_import);

            let local = |maybe: &MaybeRenamed| match maybe {
                MaybeRenamed::Normal(name) => name.atom_identifier().copied(),
                MaybeRenamed::Renamed(_, to) => to.atom_identifier().copied(),
            };

            // The name the module provides it under
            let source = |maybe: &MaybeRenamed| match maybe {
                MaybeRenamed::Normal(name) => name.atom_identifier().copied(),
                MaybeRenamed::Renamed(from, _) => from.atom_identifier().copied(),
            };

            let written = |excluded: &Excluded, name: InternedString| {
                excluded.name.resolve() == format!("{}{}", excluded.prefix, name.resolve())
            };

            // Each excluded name has to be imported by the spec inside the except-in
            for excluded in &require_object.except {
                let inside = if excluded.of_only_in {
                    &only_in
                } else {
                    &provided
                };

                if !inside
                    .iter()
                    .filter_map(local)
                    .any(|name| written(excluded, name))
                {
                    stop!(BadSyntax => format!("except-in: {} isn't provided by {:?}", excluded.name, path); require_object.span);
                }
            }

            // An only-in around an except-in can't name what the except-in left out
            for excluded in require_object.except.iter().filter(|x| !x.of_only_in) {
                if only_in
                    .iter()
                    .filter_map(source)
                    .any(|name| written(excluded, name))
                {
                    stop!(BadSyntax => format!("only-in: {} is left out by the except-in inside it", excluded.name); require_object.span);
                }
            }

            let candidates = if only_in.is_empty() {
                provided
            } else {
                only_in
            };

            // An except-in around an only-in excludes by the name the only-in gives, otherwise
            // by the name the module provides
            let excluded = |x: &MaybeRenamed| {
                require_object.except.iter().any(|excluded| {
                    let name = if excluded.of_only_in {
                        local(x)
                    } else {
                        source(x)
                    };
                    name.is_some_and(|name| written(excluded, name))
                })
            };

            let mut imported: Vec<MaybeRenamed> =
                candidates.into_iter().filter(|x| !excluded(x)).collect();

            // rename-in gives an imported name another
            for (from, to) in &require_object.renames {
                let Some(found) = imported.iter_mut().find(|x| local(x) == Some(*from)) else {
                    stop!(BadSyntax => format!("rename-in: {} isn't imported from {:?}", from, path); require_object.span);
                };
                let original = source(found).unwrap_or(*from);
                *found = MaybeRenamed::Renamed(ExprKind::atom(original), ExprKind::atom(*to));
            }

            // An empty list means everything, so import a name no module provides instead
            if imported.is_empty() {
                imported.push(MaybeRenamed::Normal(ExprKind::atom("#%nothing")));
            }

            require_object.idents_to_import = imported;
            require_object.except.clear();
            require_object.renames.clear();
        }

        Ok(())
    }

    // A module only required for syntax keeps its phase 0 code until something requires it
    fn emit(&mut self, new_exprs: &mut Vec<ExprKind>, expr: ExprKind) {
        if self.for_syntax_only {
            self.compiled_modules
                .deferred
                .insert(self.name.clone(), DeferredCode(expr));
        } else {
            self.compiled_modules.deferred.remove(&self.name);
            new_exprs.push(expr);
        }
    }

    // TODO: This should run again on itself, probably
    fn compile_module(&mut self) -> Result<ExprKind> {
        if let Some(kernel) = self.kernel.as_mut() {
            kernel.begin_compiling(self.name.to_str().unwrap().to_string());
        }

        let result = self.compile_this_module();

        if let Some(kernel) = self.kernel.as_mut() {
            kernel.end_compiling();
        }

        result
    }

    fn compile_this_module(&mut self) -> Result<ExprKind> {
        // A begin's forms are module forms, so its procedural macros are loaded together with
        // the module's written ones
        let mut ast = splice_begins(core::mem::take(&mut self.source_ast));
        ast.iter_mut().for_each(as_written_top_level);

        let mut own_names = collect_globals(&ast);
        own_names.extend(self.local_macros.iter().copied());

        // Locations of its written syntax-rules macros
        let written_rule_spans: FxHashMap<InternedString, Span> = self
            .local_macros
            .iter()
            .filter_map(|name| Some((*name, self.macro_map.get(name)?.location)))
            .collect();

        // Its procedural macros are its own names too
        let mut procedural = FxHashSet::default();
        ast.iter()
            .for_each(|x| crate::parser::expand_visitor::defined_macro_names(x, &mut procedural));
        own_names.extend(procedural);
        let mut provides = core::mem::take(&mut self.provides);
        // Clone the requires... I suppose
        let requires = self.require_objects.clone();

        // info!(
        //     target: "requires",
        //     "Into compiled module: provides for syntax: {:?}",
        //     self.provides_for_syntax
        // );

        expand_phase1_forms(&mut ast, &self.macro_map)?;

        let prev_length = ast.len();

        // Attempt extracting the syntax transformers from this module
        if let Some(kernel) = self.kernel.as_mut() {
            let mut imports: Vec<Phase1Import> = self
                .require_objects
                .iter()
                .filter(|x| !x.for_syntax)
                .filter_map(|require_object| {
                    let module = self
                        .compiled_modules
                        .compiled_modules
                        .get(require_object.path.get_path().as_ref())?;

                    Some(provided_phase1_bindings(
                        Some(&*kernel),
                        module,
                        require_object,
                    ))
                })
                .flatten()
                .collect();

            let mut for_syntax = Vec::new();
            let mut for_syntax_prefixes = Vec::new();
            let mut phase1_macros = FxHashMap::default();
            let mut phase1_procedural = FxHashMap::default();
            let mut instantiated = FxHashMap::default();

            for require_object in self.require_objects.iter().filter(|x| x.for_syntax) {
                // The required module's macros, for this module's transformer code
                let path = require_object.path.get_path().into_owned();
                let mut unused = Vec::new();
                let (module, macros, _) = ModuleManager::find_in_scope_macros(
                    &mut self.compiled_modules,
                    Some(&mut *kernel),
                    &path,
                    require_object,
                    &mut unused,
                );

                phase1_procedural.extend(procedural_for_syntax(kernel, module, require_object));

                let instance = instantiate_for_syntax(
                    kernel,
                    &self.compiled_modules.compiled_modules,
                    self.global_macro_map,
                    &self.builtin_modules,
                    require_object,
                    false,
                    &macros,
                    &mut instantiated,
                )?;

                imports.extend(instance.imports);
                for_syntax.extend(instance.code);
                for_syntax_prefixes.extend(instance.prefixes);
                phase1_macros.extend(macros);
            }

            kernel.add_phase1_macros(
                self.name.to_str().unwrap(),
                phase1_macros,
                phase1_procedural,
                Vec::new(),
            );

            // Templates in phase 1 code refer to this module's definitions
            {
                let mut mangler = NameMangler::new(
                    collect_globals(&ast),
                    path_to_prefix_name(self.name.to_path_buf()).into(),
                )
                .in_module(&self.name.to_string_lossy());
                for expr in ast.iter_mut() {
                    if let ExprKind::List(l) = expr {
                        if l.first_ident() == Some(&*crate::compiler::program::BEGIN_FOR_SYNTAX) {
                            for form in l.args.iter_mut().skip(1) {
                                mangler.visit_phase1_code(form);
                            }
                        }
                    }
                }
            }

            kernel.load_syntax_transformers(
                &mut ast,
                self.name.to_str().unwrap().to_string(),
                &imports,
                for_syntax,
                for_syntax_prefixes,
            )?
        };

        let loaded_syntax_transformers = prev_length != ast.len();

        let globals = collect_globals(&ast);

        // What its names refer to, for macros' literals and free-identifier=?
        let module_name: InternedString = self.name.to_string_lossy().as_ref().into();
        let mut imports = FxHashMap::default();
        for require_object in self.require_objects.iter().filter(|x| !x.for_syntax) {
            let path = require_object.path.get_path();
            if let Some(module) = self.compiled_modules.compiled_modules.get(path.as_ref()) {
                let from: InternedString = module.name.to_string_lossy().as_ref().into();
                for (local, original) in imported_names(require_object, module) {
                    imports.insert(local, (from, original));
                }
            }
        }
        crate::parser::expand_visitor::record_module_bindings(
            module_name,
            crate::parser::expand_visitor::ModuleBindings {
                own: globals.iter().chain(own_names.iter()).copied().collect(),
                imports,
            },
        );
        let _current = crate::parser::expand_visitor::CurrentModule::set(module_name);

        let globals = GlobalMap::Set(&globals);

        // TODO: Revisit if caching is something we want here
        // if self.kernel.is_some() && !loaded_syntax_transformers {
        //     let modules = self.compiled_modules.compiled_modules.clone();
        //     if let Some(module) = self.compiled_modules.cached_mut(&self.name, self.sources) {
        //         log::info!("Using cached module: {:?}", self.name);

        //         module.set_emitted(true);

        //         let top_level_time = crate::time::Instant::now();

        //         let res = module.to_top_level_module(&modules, self.global_macro_map);

        //         log::info!("Cached top level time: {:?}", top_level_time.elapsed());

        //         log::info!("Using cached module took: {:?}", now.elapsed());

        //         return res;
        //     } else {
        //         log::info!("Module not found in the cache: {:?}", self.name);
        //     }
        // }

        // This needs to be overlayed with imported macros. So the idea being that this
        // expansion _also_ interweaves with the other requires... seems to make sense.
        //
        // And it also seems like it needs to happen at the same time. So the lookups
        // should happen together.

        if self.require_objects.is_empty() {
            for expr in ast.iter_mut() {
                // Expand both kinds of macros, each where it's used
                crate::parser::expand_visitor::expand_with_kernel(
                    expr,
                    &self.macro_map,
                    globals,
                    crate::parser::expand_visitor::KernelExpander::in_environment(
                        self.kernel.as_mut(),
                        self.builtin_modules.clone(),
                        self.name.to_str().unwrap(),
                    ),
                )?;
            }
        }

        let mut mangled_asts = Vec::with_capacity(ast.len() + 16);
        let mut downstream = Vec::new();
        let mut downstream_builtins = Vec::new();

        let mut required_macros = Vec::new();

        let macro_map = Arc::make_mut(&mut self.macro_map);

        for require_object in self.require_objects.iter() {
            let require_for_syntax = require_object.path.get_path();

            if let PathOrBuiltIn::Path(_) = &require_object.path {
                downstream.push(require_for_syntax.clone().into_owned());
            }

            if let PathOrBuiltIn::BuiltIn(_) = &require_object.path {
                downstream_builtins.push(require_for_syntax.clone().into_owned());
            }

            if require_object.for_syntax {
                continue;
            }

            // Stash this?
            let (module, in_scope_macros, name_mangler) = ModuleManager::find_in_scope_macros(
                &mut self.compiled_modules,
                self.kernel.as_mut(),
                require_for_syntax.as_ref(),
                require_object,
                &mut mangled_asts,
            );

            let module_name = module.name.to_str().unwrap().to_owned();
            let mut provided_kernel_macros =
                provided_kernel_macros(self.kernel.as_ref(), module, require_object);

            // The module's own definitions shadow what it requires
            let mut in_scope_macros = in_scope_macros;
            in_scope_macros.retain(|name, _| !own_names.contains(name));
            provided_kernel_macros.retain(|name, _| !own_names.contains(name));

            required_macros.push(RequiredModuleMacros {
                module_key: require_for_syntax.clone().into_owned(),
                module_macros: module.macro_map.clone(),
                in_scope_macros,
                name_mangler,
                module_name,
                provided_kernel_macros,
            });

            if require_object.idents_to_import.is_empty() {
                if let Some(m) = self
                    .compiled_modules
                    .get(&require_object.path.get_path(), self.sources)
                {
                    for provide_expr in &m.provides {
                        // TODO: Handle provide spec stuff!
                        // An import replaces a prelude macro of the same name, but not one the
                        // module defines
                        for provide in &provide_expr.list().unwrap().args[1..] {
                            if let Some(ident) = provide.atom_identifier() {
                                if !own_names.contains(ident) {
                                    crate::parser::expand_visitor::remove_macro(macro_map, ident);
                                }
                            }

                            if let Some(list) = provide.list().and_then(|x| x.second_ident()) {
                                if !own_names.contains(list) {
                                    crate::parser::expand_visitor::remove_macro(macro_map, list);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Also require the macros that the requires re-export, from the modules that define them
        let reexported: Vec<RequireObject> = self
            .require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .flat_map(|x| reexport_requires(&self.compiled_modules.compiled_modules, x))
            .collect();

        for require_object in &reexported {
            let path = require_object.path.get_path();

            let (module, mut in_scope_macros, name_mangler) = ModuleManager::find_in_scope_macros(
                &mut self.compiled_modules,
                self.kernel.as_mut(),
                path.as_ref(),
                require_object,
                &mut mangled_asts,
            );

            let module_name = module.name.to_str().unwrap().to_owned();
            let mut provided_kernel_macros =
                provided_kernel_macros(self.kernel.as_ref(), module, require_object);

            in_scope_macros.retain(|name, _| !own_names.contains(name));
            provided_kernel_macros.retain(|name, _| !own_names.contains(name));

            for name in in_scope_macros.keys().chain(provided_kernel_macros.keys()) {
                crate::parser::expand_visitor::remove_macro(macro_map, name);
            }

            required_macros.push(RequiredModuleMacros {
                module_key: path.clone().into_owned(),
                module_macros: module.macro_map.clone(),
                in_scope_macros,
                name_mangler,
                module_name,
                provided_kernel_macros,
            });
        }

        let overlays = required_macros
            .iter_mut()
            .map(|x| RequiredMacroMap {
                map: x,
                changed: false,
            })
            .collect::<Vec<_>>();

        // Procedural macros from the module and its requires get their arguments as written
        let _procedural = crate::parser::expand_visitor::ProceduralHeads::set(
            overlays
                .iter()
                .flat_map(|x| x.map.provided_kernel_macros.keys().copied())
                .chain(
                    self.kernel
                        .as_ref()
                        .map(|kernel| kernel.procedural_names(self.name.to_str().unwrap()))
                        .unwrap_or_default(),
                )
                .collect(),
            &ast,
        );

        // Expand both kinds of macros, each where it's used
        let module_name = self.name.to_str().unwrap();
        let mut many_expander = ExpanderMany::many(macro_map, overlays, globals).with_kernel(
            crate::parser::expand_visitor::KernelExpander::in_environment(
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                module_name,
            ),
        );
        many_expander.define_top_level_procedural = true;
        let mut generated = Vec::new();

        // Like in a Racket module, every form's head is expanded first, in order, and forms
        // that turn out to define macros are expanded right away, so those macros are visible
        // in the whole module. Then the rest are expanded, staying in place. Finally the forms
        // before the last one that defined a macro are expanded again, in case a macro was
        // defined deeper inside a form. A begin's forms are module forms, so its definitions
        // and expressions are expanded along with the module's
        let mut ast = splice_begins(ast);
        let mut expanded = vec![false; ast.len()];

        // The procedural macros each form defines as written, and the form spans
        let mut written_procedural: Vec<FxHashSet<InternedString>> = ast
            .iter()
            .map(|form| {
                let mut names = FxHashSet::default();
                crate::parser::expand_visitor::defined_procedural_macro_names(form, &mut names);
                names
            })
            .collect();
        let mut written_spans: Vec<Span> = ast
            .iter()
            .map(crate::parser::span_visitor::get_span)
            .collect();

        // A written procedural macro can only be defined once, including inside a begin
        let mut written_once = FxHashSet::default();
        for (form, names) in ast.iter().zip(written_procedural.iter()) {
            for name in names {
                if !written_once.insert(*name) {
                    let mut spans = FxHashMap::default();
                    defined_name_spans(form, &mut spans);
                    let at = spans
                        .get(name)
                        .copied()
                        .unwrap_or_else(|| crate::parser::span_visitor::get_span(form));
                    stop!(BadSyntax => format!("Macro re defined within the top level definition: {name}"); at);
                }
            }
        }
        let mut last_defining = None;

        for pass in 0..3 {
            let mut index = 0;
            while index < ast.len() {
                index += 1;
                let index = index - 1;

                let skip = match pass {
                    // Expand each form's head, in order. Forms that define a macro are expanded
                    // fully now.
                    0 => {
                        expand_head(&mut ast[index], &mut many_expander, module_name, globals)?;

                        // Splice in a begin it expanded to, and handle its forms in turn
                        if let Some(forms) = begin_forms(&mut ast[index]) {
                            check_begin_definitions(&forms)?;
                            let count = forms.len();
                            let span = written_spans[index];
                            ast.splice(index..=index, forms);
                            expanded.splice(index..=index, vec![false; count]);
                            written_procedural
                                .splice(index..=index, vec![FxHashSet::default(); count]);
                            written_spans.splice(index..=index, vec![span; count]);
                            continue;
                        }

                        !defines_macros(&ast[index])
                    }
                    1 => expanded[index],
                    // Every form before the last one that defined a macro, in either pass
                    _ => last_defining.map_or(true, |last| index >= last),
                };

                if skip {
                    continue;
                }

                expanded[index] = true;
                let expr = &mut ast[index];
                let span = crate::parser::span_visitor::get_span(expr);

                // The two kinds of macros take turns until none are left, like at the top level
                let mut form_defined = FxHashSet::default();

                for round in 0.. {
                    if round == MAX_EXPANSION_ROUNDS {
                        stop!(BadSyntax => "macro expansion doesn't finish"; span);
                    }

                    many_expander.changed = false;
                    many_expander.expand(expr)?;
                    // Only do another round if the form defined a macro
                    let mut changed = false;

                    _procedural.add(expr);
                    // A macro defined by one of this module's macros belongs to the module, so
                    // its other macros can refer to it wherever they're used. The last pass
                    // only expands. Record the name spans first, since the definitions are
                    // removed once defined
                    let mut name_spans = FxHashMap::default();
                    defined_name_spans(expr, &mut name_spans);

                    let mut defined = if pass < 2 {
                        define_generated_macros(
                            expr,
                            many_expander.map.macros_mut().unwrap(),
                            globals,
                        )?
                    } else {
                        Default::default()
                    };

                    // The same goes for procedural macros the module's macros define
                    let mut procedural = FxHashSet::default();
                    if pass < 2 {
                        crate::parser::expand_visitor::defined_macro_names(expr, &mut procedural);
                    }
                    // Including the ones the expander defined where they were made
                    let made = core::mem::take(&mut many_expander.defined_procedural);
                    if pass < 2 {
                        procedural.extend(made.iter().map(|x| x.0));
                        name_spans.extend(made);
                    }
                    // A definition stays in the form until the kernel's last pass, so later
                    // rounds see it again. One the form wrote itself is a written macro.
                    procedural.retain(|x| {
                        !form_defined.contains(x) && !written_procedural[index].contains(x)
                    });

                    // A name the form defines twice, as either kind of macro
                    let mut repeated = None;
                    let mut once = FxHashSet::default();
                    for name in defined.iter().chain(procedural.iter()) {
                        if !once.insert(*name) {
                            repeated = Some(*name);
                        }
                    }

                    defined.extend(
                        procedural
                            .into_iter()
                            .filter(|x| !defined.contains(x))
                            .collect::<Vec<_>>(),
                    );
                    form_defined.extend(defined.iter().copied());
                    changed |= !defined.is_empty();

                    if !defined.is_empty() {
                        last_defining = last_defining.max(Some(index));
                    }

                    // Like a written macro, a macro defined by the module's macros can only be
                    // defined once. The error points at the second definition.
                    for name in defined.iter().chain(repeated.iter()) {
                        let written = written_procedural
                            .iter()
                            .position(|x| x.contains(name))
                            .map(|i| written_spans[i])
                            .or_else(|| written_rule_spans.get(name).copied());

                        if written.is_some()
                            || self.local_macros.contains(name)
                            || generated.contains(name)
                            || repeated == Some(*name)
                        {
                            // Point at where the form is written, since its expansion has the
                            // template's spans
                            let at = written
                                .filter(|written| written.start() > written_spans[index].start())
                                .or_else(|| name_spans.get(name).copied())
                                .unwrap_or(span);
                            stop!(BadSyntax => format!("Macro re defined within the top level definition: {name}"); at);
                        }
                    }

                    // Like a written macro, a macro the module defines hides an imported one with
                    // the same name
                    for overlay in many_expander.overlays.iter_mut() {
                        for name in &defined {
                            overlay.map.in_scope_macros.remove(name);
                            overlay.map.provided_kernel_macros.remove(name);
                        }
                    }

                    generated.extend(defined);

                    if !changed {
                        break;
                    }
                }
            }
        }

        // Like in Racket, provide specs are expanded after the module's forms, so they can use
        // the macros those forms define
        for expr in provides.iter_mut() {
            many_expander.expand(expr)?;
        }

        self.local_macros.extend(generated);

        // Like in Racket, a name can only be defined once, as either a value or a macro
        let mut macros: FxHashSet<InternedString> = self.local_macros.iter().copied().collect();
        written_procedural
            .iter()
            .for_each(|x| macros.extend(x.iter().copied()));
        for form in ast.iter().filter(|x| !is_builtin_import(x)) {
            if let Some(name) = collect_globals(core::slice::from_ref(form))
                .into_iter()
                .find(|x| macros.contains(x))
            {
                stop!(BadSyntax => format!("define: identifier already defined as a macro: {name}");
                    crate::parser::span_visitor::get_span(form));
            }
        }

        // TODO: Check HERE for whether there are more requires than were previously found.
        // If so, we should go back and compile the module again

        for expr in &mut ast {
            lower_entire_ast(expr)?;
            FlattenBegin::flatten(expr);
        }

        // And then finally, expand the keyword arguments
        for expr in &mut ast {
            crate::parser::expand_visitor::strip_loaded_procedural_definitions(expr);
            crate::parser::expand_visitor::expand_keyword_arguments(expr)?;
        }

        for provide in provides.iter_mut().filter_map(ExprKind::list_mut) {
            let args = core::mem::take(&mut provide.args);
            provide.args = self.filter_out_for_syntax_provides(args)?;
        }

        // Collect the provides the expansion made along with the written ones, before sorting
        {
            self.source_ast = ast;
            self.provides = provides;

            self.collect_provides()?;

            provides = core::mem::take(&mut self.provides);
            ast = core::mem::take(&mut self.source_ast);
        }

        // Like Racket's provide transformers, these run once the module is expanded
        // `(all-from-out "path")` provides what the module imports from that require
        for provide in provides.iter_mut() {
            let Some(provide) = provide.list_mut() else {
                continue;
            };
            if !provide.args.iter().any(|x| {
                x.list()
                    .and_then(|x| x.first_ident())
                    .is_some_and(|x| x.resolve() == "all-from-out")
            }) {
                continue;
            }
            let mut expanded = ThinVec::new();
            for spec in core::mem::take(&mut provide.args) {
                let Some(paths) = spec
                    .list()
                    .filter(|x| {
                        x.first_ident()
                            .is_some_and(|x| x.resolve() == "all-from-out")
                    })
                    .map(|x| x.args[1..].to_vec())
                else {
                    expanded.push(spec);
                    continue;
                };
                for path in paths {
                    let Some(written) = path.string_literal().map(|x| x.to_string()) else {
                        stop!(BadSyntax => "all-from-out expects a module path"; spec.span());
                    };
                    let required =
                        self.require_objects
                            .iter()
                            .filter(|x| !x.for_syntax)
                            .find(|x| {
                                x.path
                                    .get_path()
                                    .to_string_lossy()
                                    .ends_with(written.as_str())
                            });
                    let Some(module) = required.and_then(|x| {
                        self.compiled_modules
                            .compiled_modules
                            .get(x.path.get_path().as_ref())
                            .map(|module| (x, module))
                    }) else {
                        stop!(BadSyntax => format!("all-from-out: {} isn't required", written); spec.span());
                    };
                    for (local, _) in imported_names(module.0, module.1) {
                        expanded.push(ExprKind::atom(local));
                    }
                }
            }
            provide.args = expanded;
        }
        // `(prefix-out p: name ...)` provides each name as `p:name`, and
        // `(rename-out [name other] ...)` each as the other
        for provide in provides.iter_mut() {
            let Some(provide) = provide.list_mut() else {
                continue;
            };
            let mut expanded = ThinVec::new();
            for spec in core::mem::take(&mut provide.args) {
                // The names it provides, each with the name to provide it as
                let renamed: Vec<(InternedString, InternedString)> = match spec
                    .list()
                    .and_then(|x| Some((x.first_ident()?.resolve(), x)))
                {
                    Some(("prefix-out", l)) => {
                        let Some(prefix) = l.args.get(1).and_then(ExprKind::atom_identifier) else {
                            stop!(BadSyntax => "prefix-out expects a prefix and names"; spec.span());
                        };
                        let mut names = Vec::new();
                        spliced_specs(l.args.iter().skip(2), &mut names);
                        let mut renamed = Vec::new();
                        for name in &names {
                            let Some(name) = name.atom_identifier() else {
                                stop!(BadSyntax => "prefix-out expects a prefix and names"; spec.span());
                            };
                            renamed.push((
                                *name,
                                (prefix.resolve().to_string() + name.resolve()).into(),
                            ));
                        }
                        renamed
                    }
                    Some(("rename-out", l)) => {
                        let mut renamed = Vec::new();
                        for pair in l.args.iter().skip(1) {
                            let names = pair.list().filter(|x| x.len() == 2).and_then(|x| {
                                Some((*x.args[0].atom_identifier()?, *x.args[1].atom_identifier()?))
                            });
                            match names {
                                Some(names) => renamed.push(names),
                                None => {
                                    stop!(BadSyntax => "rename-out expects pairs of a name and the name to provide it as"; pair.span())
                                }
                            }
                        }
                        renamed
                    }
                    _ => {
                        expanded.push(spec);
                        continue;
                    }
                };
                for (name, exported) in renamed {
                    let name = &name;
                    // Its own macro, or one a require brings in
                    let imported_macro = self
                        .require_objects
                        .iter()
                        .filter(|x| !x.for_syntax)
                        .filter_map(|x| {
                            let module = self
                                .compiled_modules
                                .compiled_modules
                                .get(x.path.get_path().as_ref())?;
                            Some((x, module))
                        })
                        .any(|(x, module)| {
                            imported_names(x, module).iter().any(|(local, original)| {
                                local == name && module.macro_map.contains_key(original)
                            })
                        });
                    if self.macro_map.contains_key(name) || imported_macro {
                        // A macro that passes its use on to the macro of the name
                        let rest = ExprKind::atom("rest");
                        let pattern = List::new_maybe_improper(
                            thin_vec![ExprKind::atom("_"), rest.clone()],
                            true,
                        );
                        let template =
                            List::new_maybe_improper(thin_vec![ExprKind::atom(*name), rest], true);
                        let mut alias = SteelMacro::parse_from_ast_macro(Box::new(Macro::new(
                            ExprKind::atom(exported),
                            Box::new(SyntaxRules::new(
                                ThinVec::new(),
                                thin_vec![PatternPair::new(pattern.into(), template.into())
                                    .map_err(|x| {
                                        crate::rerrs::SteelErr::new(
                                            crate::rerrs::ErrorKind::BadSyntax,
                                            x.to_string(),
                                        )
                                    },)?],
                                SyntaxObject::default(TokenType::SyntaxRules),
                            )),
                            SyntaxObject::default(TokenType::DefineSyntax),
                        )))?;
                        alias.mark_definition_resolved(&self.name.to_string_lossy());
                        Arc::make_mut(&mut self.macro_map).insert(exported, alias);
                        self.local_macros.insert(exported);
                    } else {
                        ast.push(ExprKind::Define(Box::new(Define::new(
                            ExprKind::atom(exported),
                            ExprKind::atom(*name),
                            SyntaxObject::default(TokenType::Define),
                        ))));
                    }
                    own_names.insert(exported);
                    expanded.push(ExprKind::atom(exported));
                }
            }
            provide.args = expanded;
        }
        drop_uncontracted_duplicates(&mut provides);

        // A procedural macro provided by name is provided for syntax
        if let Some(defined) = self
            .kernel
            .as_ref()
            .and_then(|kernel| kernel.exported_defmacros(self.name.to_str().unwrap()))
        {
            for provide in provides.iter_mut() {
                if let ExprKind::List(l) = provide {
                    let (macros, rest): (Vec<_>, Vec<_>) = l
                        .args
                        .drain(1..)
                        .partition(|x| x.atom_identifier().is_some_and(|x| defined.contains(x)));

                    l.args.extend(rest);
                    self.provides_for_syntax.extend(macros);
                }
            }
        }

        // A macro the module imports and provides by name is provided from the module that
        // defines it
        let mut reexports = Vec::new();

        for provide in provides.iter_mut() {
            let ExprKind::List(l) = provide else {
                continue;
            };

            l.args.retain(|x| {
                let name = match x {
                    ExprKind::List(l) if l.first_ident() == Some(&*FOR_SYNTAX) => l.second_ident(),
                    x => x.atom_identifier(),
                };

                let Some(name) = name.filter(|x| !own_names.contains(*x)) else {
                    return true;
                };

                match self.reexport(name) {
                    Some(reexport) => {
                        reexports.push(reexport);
                        false
                    }
                    None => true,
                }
            });
        }

        let mut for_syntax = Vec::new();

        for provide in core::mem::take(&mut self.provides_for_syntax) {
            let name = provide
                .atom_identifier()
                .filter(|x| !own_names.contains(*x));

            match name.and_then(|name| self.reexport(name)) {
                Some(reexport) => reexports.push(reexport),
                None => for_syntax.push(provide),
            }
        }

        self.provides_for_syntax = for_syntax;

        // Put the mangled asts at the top
        // then include the ast there
        // mangled_asts.append(&mut ast);

        for expr in mangled_asts.iter_mut() {
            lower_entire_ast(expr)?;

            FlattenBegin::flatten(expr);
        }

        mangled_asts.append(&mut ast);

        // TODO: @Matt: 7/3/25
        // Re-exporting macros needs to work here
        let mut module = CompiledModule::new(
            self.name.clone(),
            provides,
            requires,
            self.provides_for_syntax
                .iter()
                .map(|x| *x.atom_identifier().unwrap())
                .collect(),
            // core::mem::take(&mut self.macro_map),
            self.macro_map.clone(),
            mangled_asts,
            downstream,
            downstream_builtins,
        );

        module.set_emitted(true);
        module.own_macros = self.local_macros.clone();
        module.reexports = reexports;

        // Cache it
        if !loaded_syntax_transformers
            && module.provides_for_syntax.is_empty()
            && self.local_macros.is_empty()
            && module.get_provides().iter().all(|x| {
                if let ExprKind::List(l) = x {
                    let mut iter = l.args.iter();
                    iter.next();

                    for provide in iter {
                        if let Some(ident) = provide.atom_identifier() {
                            if self.local_macros.contains(ident) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }

                    return true;
                }

                true
            })
        {
            // Remove the unused defines since this module
            // won't need to reference these later from any macros
            {
                let provided = module
                    .get_provides()
                    .iter()
                    .filter_map(|provide| provide.list())
                    .flat_map(|provide| provide.args.iter().skip(1))
                    .filter_map(|name| name.atom_identifier().copied())
                    .collect();
                let mut sem = SemanticAnalysis::new(&mut module.ast);
                sem.remove_unused_define_imports(&provided);
            }

            // TODO: Revisit with some caching later
            // self.compiled_modules.cache_module(&module.name, &module);
        }

        let result = module.to_top_level_module(
            &self.compiled_modules.compiled_modules,
            self.global_macro_map,
        )?;

        self.compiled_modules
            .compiled_modules
            .insert(self.name.clone(), module);

        Ok(result)
    }

    fn extract_macro_defs(&mut self) -> Result<()> {
        // Probably don't have more than 128 macros in a module, but who knows?
        // let mut macro_indices = SmallVec::<[usize; 128]>::new();

        // let exprs = core::mem::take(&mut self.source_ast);

        lift_macro_defs_from_begins(&mut self.source_ast)?;

        let mut error = None;
        let mut procedural = FxHashSet::default();

        self.source_ast.retain_mut(|expr| {
            // A procedural macro defined twice, or also defined as a syntax-rules macro
            if let ExprKind::List(l) = expr {
                if l.is_define_syntax() {
                    if let Some(name) = crate::parser::expand_visitor::procedural_macro_name(l) {
                        if (self.local_macros.contains(&name) || !procedural.insert(name))
                            && error.is_none()
                        {
                            error = Some(
                                crate::rerrs::SteelErr::new(
                                    crate::rerrs::ErrorKind::BadSyntax,
                                    format!(
                                        "Macro re defined within the top level definition: {name}"
                                    ),
                                )
                                .with_span(l.location),
                            );
                        }
                    }
                }
            }

            if let ExprKind::Macro(_) = expr {
                // Replace with dummy begin value so we don't have to copy
                // everything other for every macro definition
                let mut taken_expr = ExprKind::Begin(Box::new(Begin::new(
                    Vec::new(),
                    SyntaxObject::default(TokenType::Begin),
                )));

                core::mem::swap(expr, &mut taken_expr);

                if let ExprKind::Macro(m) = taken_expr {
                    match SteelMacro::parse_from_ast_macro(m) {
                        Ok(mut generated_macro) => {
                            // Its free names refer to the module's globals
                            generated_macro.mark_definition_resolved(&self.name.to_string_lossy());
                            let name = generated_macro.name();

                            let name = *name;

                            // Like a variable, a macro can only be defined once in a module
                            if !self.local_macros.insert(name) || procedural.contains(&name) {
                                if error.is_none() {
                                    error = Some(crate::rerrs::SteelErr::new(
                                        crate::rerrs::ErrorKind::BadSyntax,
                                        format!(
                                            "Macro re defined within the top level definition: {name}"
                                        ),
                                    )
                                    .with_span(generated_macro.span()));
                                }

                                return false;
                            }

                            crate::parser::expand_visitor::define_macro(
                                Arc::make_mut(&mut self.macro_map),
                                name,
                                generated_macro,
                            );
                        }
                        Err(e) => {
                            if error.is_none() {
                                error = Some(e);
                            }
                            // error = Some(e);
                            return false;
                        }
                    }
                } else {
                    unreachable!();
                }

                return false;
            }

            true
        });

        if let Some(e) = error {
            return Err(e);
        }

        Ok(())
    }

    // Takes out the (for-syntax) forms from the provides
    fn filter_out_for_syntax_provides(
        &mut self,
        exprs: ThinVec<ExprKind>,
    ) -> Result<ThinVec<ExprKind>> {
        let mut normal_provides = ThinVec::new();

        for expr in exprs {
            match &expr {
                ExprKind::Atom(_) => {
                    if expr.atom_identifier().is_none() {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)"; expr.span())
                    }

                    normal_provides.push(expr);
                }
                // What a provide spec's macro expands to can be several specs
                ExprKind::List(l)
                    if l.first()
                        .and_then(ExprKind::atom_syntax_object)
                        .is_some_and(|x| x.ty == TokenType::Begin) =>
                {
                    let specs = l.args[1..].iter().cloned().collect();
                    normal_provides.extend(self.filter_out_for_syntax_provides(specs)?);
                }
                ExprKind::List(l) => {
                    if let Some(for_syntax) = l.first_ident() {
                        match *for_syntax {
                            x if x == *FOR_SYNTAX => {
                                if l.args.len() != 2 {
                                    stop!(ArityMismatch => "provide expects a single identifier in the (for-syntax <ident>)"; l.location)
                                }

                                if l.args[1].atom_identifier().is_none() {
                                    stop!(TypeMismatch => "provide expects a single identifier in the (for-syntax <ident>)"; l.location)
                                }

                                // Collect the for syntax expressions
                                // TODO -> remove this clone
                                self.provides_for_syntax.push(l.args[1].clone());
                            }
                            x if x == *REQUIRE_IDENT_SPEC => {
                                normal_provides.push(expr);
                            }
                            // x if x == *CONTRACT_OUT || x == *REQUIRE_IDENT_SPEC => {
                            //     normal_provides.push(expr);
                            // }
                            _ => {
                                normal_provides.push(expr);
                                // stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
                            }
                        }
                    } else {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)"; l.location)
                    }
                }
                ExprKind::Begin(b) => {
                    let specs = b.exprs.iter().cloned().collect();
                    normal_provides.extend(self.filter_out_for_syntax_provides(specs)?);
                }
                _ => {
                    stop!(TypeMismatch => "provide expects either a (for-syntax <ident>) or an ident"; expr.span())
                }
            }
        }

        Ok(normal_provides)
    }

    // TODO -> collect (provide (for-syntax ...))
    // I think these will already be collected for the macro, however I think for syntax should be found earlier
    // Otherwise the macro expansion will not be able to understand it
    fn collect_provides(&mut self) -> Result<()> {
        // let now = crate::time::Instant::now();

        let mut non_provides = Vec::with_capacity(self.source_ast.len());
        let exprs = core::mem::take(&mut self.source_ast);

        fn walk(
            module_builder: &mut ModuleBuilder,
            exprs: Vec<ExprKind>,
            non_provides: &mut Vec<ExprKind>,
        ) -> Result<()> {
            for mut expr in exprs {
                match &mut expr {
                    ExprKind::List(l) => {
                        if let Some(provide) = l.first_ident() {
                            if *provide == *PROVIDE {
                                // Provides nothing
                                if l.len() == 1 {
                                    continue;
                                }

                                // Swap out the value inside the list
                                let args = core::mem::take(&mut l.args);

                                let filtered =
                                    module_builder.filter_out_for_syntax_provides(args)?;

                                l.args = filtered;

                                module_builder.provides.push(expr);

                                continue;
                            }
                        }
                    }
                    ExprKind::Begin(b) => {
                        let exprs = core::mem::take(&mut b.exprs);

                        // Reserve capacity for these to be moved to the top level
                        non_provides.reserve(exprs.len());

                        walk(module_builder, exprs, non_provides)?;
                    }
                    _ => {}
                }

                non_provides.push(expr);
            }

            Ok(())
        }

        walk(self, exprs, &mut non_provides)?;

        self.source_ast = non_provides;

        // log::debug!(target: "pipeline_time", "Collecting provides time: {:?}", now.elapsed());

        Ok(())
    }

    fn parse_require_object(
        &mut self,
        home: &Option<PathBuf>,
        r: &crate::parser::ast::Require,
        atom: &ExprKind,
    ) -> Result<RequireObject> {
        let mut object = RequireObjectBuilder::default();

        // Set the span so we can pass through an error if the
        // module is not found
        object.span = r.location.span;

        self.parse_require_object_inner(home, r, atom, &mut object)
            .and_then(|_| object.build())
    }

    // TODO: Recursively crunch the requires to gather up the necessary information
    fn parse_require_object_inner(
        &mut self,
        home: &Option<PathBuf>,
        r: &crate::parser::ast::Require,
        atom: &ExprKind,
        require_object: &mut RequireObjectBuilder,
    ) -> Result<()> {
        log::info!("Parsing require object from: {:?}", self.name);
        match atom {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::StringLiteral(s),
                        span,
                        ..
                    },
            }) => {
                if require_object.path.is_some() {
                    stop!(Generic => "require object only expects one path!")
                }

                // Try this?
                if let Some(lib) = BUILT_INS.iter().cloned().find(|x| x.0 == s.resolve()) {
                    require_object.path = Some(PathOrBuiltIn::BuiltIn(lib.0.into()));
                    return Ok(());
                }

                if self.custom_builtins.contains_key(s.resolve()) {
                    require_object.path =
                        Some(PathOrBuiltIn::BuiltIn(s.clone().to_string().into()));

                    return Ok(());
                }

                if self
                    .module_resolvers
                    .iter()
                    .find(|x| x.exists(s.resolve()))
                    .is_some()
                {
                    require_object.path =
                        Some(PathOrBuiltIn::BuiltIn(s.clone().to_string().into()));

                    return Ok(());
                }

                if cfg!(target_family = "wasm") {
                    stop!(Generic => "requiring modules is not supported for wasm");
                }

                let mut current = self.name.clone();
                if current.is_file() {
                    current.pop();
                }
                current.push(PathBuf::from(s.resolve()));

                let mut found = Found::Beside;

                // // If the path exists on its own, we can continue
                // // But theres the case where we're searching for a module on the STEEL_HOME
                if !current.exists() {
                    if let Some(home_dir) = home.as_ref() {
                        let mut home = home_dir.clone();
                        home.push(PathBuf::from(s.resolve()));
                        current = home;
                        found = Found::Home(home_dir);

                        log::info!("Searching STEEL_HOME for {:?}", current);

                        if !current.exists() {
                            for search_dir in self.search_dirs {
                                let mut dir = search_dir.clone();
                                dir.push(s.resolve());

                                if dir.exists() {
                                    current = dir;
                                    found = Found::Search(search_dir);
                                    break;
                                }
                            }
                        }
                    } else {
                        // TODO: Check if this module exists in STEEL_HOME first. If it does, we'll take that as our candidate
                        // and then continue on to the final module resolution part.
                        //
                        // If it doesn't exist, we should iterate through the search directories and attempt to find
                        // a matching path there.
                        for dir in self.search_dirs {
                            let mut dir = dir.clone();
                            dir.push(s.resolve());

                            if dir.exists() {
                                current = dir;
                                break;
                            }
                        }

                        stop!(Generic => format!("Module not found: {:?} with STEEL_HOME: {:?}", current, home); *span)
                    }
                }

                // Get the absolute path and store that
                // self.requires.push(current)

                let current = try_canonicalize(current);

                self.origins
                    .record(&self.name, self.main, found, s.resolve(), &current);

                require_object.path = Some(PathOrBuiltIn::Path(current));
            }

            // TODO: Requires with qualifiers, that aren't just for-syntax
            // Perhaps like:
            // (with-prefix <xyz>)
            ExprKind::List(l) => {
                match l.first_ident() {
                    Some(x) if *x == *ONLY_IN => {
                        if l.args.len() < 2 {
                            stop!(BadSyntax => "only-in expects a require-spec and optionally a list of ids to bind (maybe renamed)"; l.location);
                        }

                        self.parse_require_object_inner(home, r, &l.args[1], require_object)?;

                        for remaining in &l.args[2..] {
                            match remaining {
                                ExprKind::Atom(_) => {
                                    if remaining.atom_identifier().is_none() {
                                        stop!(BadSyntax => "only-in expected identifiers to import"; remaining.span());
                                    }

                                    require_object
                                        .idents_to_import
                                        .push(MaybeRenamed::Normal(remaining.clone()));
                                }
                                ExprKind::List(l) => {
                                    if l.len() != 2 {
                                        stop!(BadSyntax => "Expected a pair when renaming required identifiers"; l.location);
                                    }

                                    let from = &l.args[0];
                                    let to = &l.args[1];

                                    if from.atom_identifier().is_none()
                                        || to.atom_identifier().is_none()
                                    {
                                        stop!(BadSyntax => "only-in expected identifiers to rename"; l.location);
                                    }

                                    // (<from> <to>)
                                    require_object
                                        .idents_to_import
                                        .push(MaybeRenamed::Renamed(from.clone(), to.clone()));
                                }
                                _ => {
                                    stop!(BadSyntax => "unexpected syntax in only-in form during module requires")
                                }
                            }
                        }
                    }

                    Some(x) if *x == *PREFIX_IN => {
                        if l.args.len() != 3 {
                            stop!(BadSyntax => "prefix-in expects a prefix to prefix a given file or module"; l.location);
                        }

                        let prefix = &l.args[1];

                        if let Some(prefix) = prefix.atom_identifier() {
                            // Parse the inner spec first, so an except-in in it only sees the
                            // prefixes inside it
                            self.parse_require_object_inner(home, r, &l.args[2], require_object)?;

                            let inner = require_object.prefix.take().unwrap_or_default();
                            require_object.prefix = Some(prefix.resolve().to_string() + &inner);
                        } else {
                            stop!(TypeMismatch => "prefix-in expects an identifier to use for the prefix"; prefix.span());
                        }
                    }

                    // Everything the spec imports, with these names given other names
                    Some(x) if x.resolve() == "rename-in" => {
                        if l.args.len() < 2 {
                            stop!(BadSyntax => "rename-in expects a require-spec and pairs of names"; l.location);
                        }

                        self.parse_require_object_inner(home, r, &l.args[1], require_object)?;

                        for pair in &l.args[2..] {
                            let names = pair.list().filter(|x| x.len() == 2).and_then(|x| {
                                Some((*x.args[0].atom_identifier()?, *x.args[1].atom_identifier()?))
                            });
                            match names {
                                Some(names) => require_object.renames.push(names),
                                None => {
                                    stop!(BadSyntax => "rename-in expects pairs of the name it provides and the name to bind"; pair.span())
                                }
                            }
                        }
                    }

                    // Everything the module provides except these names
                    Some(x) if x.resolve() == "except-in" => {
                        if l.args.len() < 2 {
                            stop!(BadSyntax => "except-in expects a require-spec and the identifiers to leave out"; l.location);
                        }

                        self.parse_require_object_inner(home, r, &l.args[1], require_object)?;

                        for excluded in &l.args[2..] {
                            match excluded.atom_identifier() {
                                Some(name) => require_object.except.push(Excluded {
                                    name: *name,
                                    prefix: require_object.prefix.clone().unwrap_or_default(),
                                    of_only_in: !require_object.idents_to_import.is_empty(),
                                }),
                                None => {
                                    stop!(BadSyntax => "except-in expected identifiers to leave out"; excluded.span())
                                }
                            }
                        }
                    }

                    // A require for transformer code, which gets a phase 1 instance of the module
                    Some(x) if *x == *FOR_SYNTAX => {
                        if l.args.len() != 2 {
                            stop!(BadSyntax => "for-syntax expects one module to require"; l.location);
                        }

                        self.parse_require_object_inner(home, r, &l.args[1], require_object)?;
                        require_object.for_syntax = true;
                    }
                    _ => {
                        stop!(BadSyntax => "require accepts either a string literal, a for-syntax expression or an only-in expression"; l.location)
                    }
                }
            }

            unknown => {
                stop!(Generic => format!("require object expected a string literal referring to a file/module, found: {}", unknown); atom.span())
            }
        }

        Ok(())
    }

    fn collect_requires(&mut self) -> Result<()> {
        // unimplemented!()

        let mut exprs_without_requires = Vec::new();
        let exprs = core::mem::take(&mut self.source_ast);

        let home = STEEL_HOME
            .clone()
            .map(|x| {
                // TODO: Fix this - try to hack in a root drive
                // for windows if a unix path is provided
                if cfg!(target_os = "windows") {
                    let mut result = x.trim_start_matches("/").to_string();

                    let mut iter = result.chars();
                    iter.next();
                    if matches!(iter.next(), Some(':')) {
                        return PathBuf::from(result);
                    }

                    result.insert(1, ':');
                    return PathBuf::from(result);
                }

                PathBuf::from(x)
            })
            .map(|mut x| {
                x.push("cogs");

                x
            });

        fn walk(
            module_builder: &mut ModuleBuilder,
            home: &Option<PathBuf>,
            exprs_without_requires: &mut Vec<ExprKind>,
            exprs: Vec<ExprKind>,
        ) -> Result<()> {
            for expr in exprs {
                match expr {
                    // Include require/for-syntax here
                    // This way we have some understanding of what dependencies a file has
                    ExprKind::Require(r) => {
                        for atom in &r.modules {
                            // TODO: Consider making this not a reference for r
                            let require_object =
                                module_builder.parse_require_object(home, &r, atom)?;

                            module_builder.require_objects.push(require_object);
                        }
                    }
                    ExprKind::Begin(b) => {
                        walk(module_builder, home, exprs_without_requires, b.exprs)?
                    }
                    _ => exprs_without_requires.push(expr),
                }
            }

            Ok(())
        }

        walk(self, &home, &mut exprs_without_requires, exprs)?;

        self.source_ast = exprs_without_requires;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn new_built_in(
        name: PathBuf,
        input: Cow<'static, str>,
        compiled_modules: &'a mut CompiledModuleCache,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
        prelude_string: &'a str,
        origins: &'a mut ModuleOrigins,
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            &[],
            module_resolvers,
            false,
            prelude_string,
            origins,
        )
        .parse_builtin(input)
    }

    fn new_from_path(
        name: PathBuf,
        compiled_modules: &'a mut CompiledModuleCache,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
        prelude_string: &'a str,
        origins: &'a mut ModuleOrigins,
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
            true,
            prelude_string,
            origins,
        )
        .parse_from_path()
    }

    fn raw(
        name: PathBuf,
        compiled_modules: &'a mut CompiledModuleCache,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
        canonicalize: bool,
        prelude_string: &'a str,
        origins: &'a mut ModuleOrigins,
    ) -> Self {
        // println!("New module found: {:?}", name);

        let name = if canonicalize {
            try_canonicalize(name)
        } else {
            name
        };

        ModuleBuilder {
            name,
            main: false,
            for_syntax_only: false,
            local_macros: FxHashSet::default(),
            source_ast: Vec::new(),
            // TODO: This used to be empty
            macro_map: default_prelude_macros(),
            require_objects: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
            prelude_string,
            origins,
        }
    }

    fn parse_builtin(mut self, input: Cow<'static, str>) -> Result<Self> {
        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let id = self
            .sources
            .add_source(input.clone(), Some(self.name.clone()));

        let parsed = Parser::new_from_source(&input, self.name.clone(), Some(id))
            .without_lowering()
            .map(|x| x.and_then(lower_macro_and_require_definitions))
            .collect::<core::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Parsing: {:?} - {:?}", self.name, now.elapsed());

        // self.source_ast.pretty_print();

        Ok(self)
    }

    fn parse_from_path(mut self) -> Result<Self> {
        log::info!("Opening: {:?}", self.name);
        let mut exprs = String::new();

        // If we were unable to resolve it via any of the built in module resolvers,
        // then we check the file system.
        let mut file = std::fs::File::open(&self.name).map_err(|err| {
            let mut err = crate::SteelErr::from(err);
            err.prepend_message(&format!("Attempting to load module from: {:?}", self.name));
            err
        })?;
        self.file_metadata
            .insert(self.name.clone(), file.metadata()?.modified()?);

        file.read_to_string(&mut exprs)?;

        let mut expressions = Parser::new(self.prelude_string, SourceId::none())
            .without_lowering()
            .map(|x| x.and_then(lower_macro_and_require_definitions))
            .collect::<core::result::Result<Vec<_>, ParseError>>()?;

        let id = self.sources.add_source(exprs, Some(self.name.clone()));

        {
            // Fetch the exprs after adding them to the sources
            // We did _just_ add it, so its fine to unwrap
            let guard = &self.sources.sources;

            let exprs = guard.get(id).unwrap();

            let mut parsed = Parser::new_from_source(exprs, self.name.clone(), Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect::<core::result::Result<Vec<_>, ParseError>>()?;

            expressions.append(&mut parsed);

            self.source_ast = expressions;
        }

        Ok(self)
    }
}
