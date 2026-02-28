use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, RwLock},
};

use quickscope::ScopeSet;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

#[cfg(feature = "sync")]
use once_cell::sync::Lazy;
use steel_parser::tokens::TokenType;

use crate::{
    compiler::{
        passes::analysis::SemanticAnalysis,
        program::{BEGIN_FOR_SYNTAX, DEFMACRO},
    },
    custom_reference,
    gc::unsafe_erased_pointers::CustomReference,
    parser::{
        ast::{Atom, Set},
        expand_visitor::GlobalMap,
        parser::SyntaxObject,
    },
    rvals::{Result, SteelString},
    steel_vm::register_fn::RegisterFn,
    values::lists::List,
};
use crate::{stdlib::KERNEL, steel_vm::engine::Engine, SteelVal};

use steel_parser::expr_list;
use thin_vec::{thin_vec, ThinVec};

use super::{
    ast::{ExprKind, TryFromSteelValVisitorForExprKind},
    interner::InternedString,
    span_visitor::get_span,
};

thread_local! {
    pub(crate) static KERNEL_IMAGE: Engine = Engine::new_bootstrap_kernel(false);
    pub(crate) static KERNEL_IMAGE_SB: Engine = Engine::new_bootstrap_kernel(true);
}

#[cfg(feature = "sync")]
pub static STATIC_KERNEL_IMAGE: Lazy<Engine> = Lazy::new(|| Engine::new_bootstrap_kernel(false));
#[cfg(feature = "sync")]
pub static STATIC_KERNEL_IMAGE_SB: Lazy<Engine> = Lazy::new(|| Engine::new_bootstrap_kernel(true));

pub(crate) fn fresh_kernel_image(sandbox: bool) -> Engine {
    // Just deep clone the env coming out

    if sandbox {
        #[cfg(feature = "sync")]
        {
            STATIC_KERNEL_IMAGE_SB.deep_clone()
        }

        #[cfg(not(feature = "sync"))]
        {
            KERNEL_IMAGE_SB.with(|x| x.deep_clone())
        }
    } else {
        #[cfg(feature = "sync")]
        {
            STATIC_KERNEL_IMAGE.deep_clone()
        }

        #[cfg(not(feature = "sync"))]
        {
            KERNEL_IMAGE.with(|x| x.deep_clone())
        }
    }
}

type TransformerMap = FxHashMap<String, FxHashSet<InternedString>>;

// Internal set of transformers that we'll embed
#[derive(Clone, Debug)]
struct Transformers {
    set: Arc<RwLock<TransformerMap>>,
}

/// The Kernel is an engine context used to evaluate defmacro style macros
/// It lives inside the compiler, so in theory there could be tiers of kernels
/// Note: this will be called along side syntax rules style macros. In this case macro
/// expansion then is a one step process, dispatching on the macros. However, at the moment this
/// will not support user defined def macros -> this kernel will only support the built in def macros
/// for structs.
#[derive(Clone)]
pub struct Kernel {
    transformers: Transformers,
    constants: HashSet<InternedString>,
    pub(crate) engine: Box<Engine>,
}

impl Default for Kernel {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct GlobalSymbolMap<'a> {
    pub(crate) map: &'a GlobalMap<'a>,
}

impl<'a> CustomReference for GlobalSymbolMap<'a> {}
custom_reference!(GlobalSymbolMap<'a>);

pub(crate) struct ParentScopeSet<'a> {
    pub(crate) set: &'a ScopeSet<InternedString, FxBuildHasher>,
}

impl<'a> CustomReference for ParentScopeSet<'a> {}
custom_reference!(ParentScopeSet<'a>);

pub(crate) static TOP_LEVEL_SCOPE_MAP: &str = "#%top-level-scope-map";
pub(crate) static TOP_LEVEL_GLOBAL_MAP: &str = "#%top-level-global-map";

impl Kernel {
    pub fn new() -> Self {
        // Does... sandboxing help here?
        let mut engine = fresh_kernel_image(!cfg!(feature = "unsandboxed-kernel"));

        let transformers = Transformers {
            set: Arc::new(RwLock::new(HashMap::default())),
        };

        let embedded_transformer_object = transformers.clone();
        engine.register_fn(
            "register-macro-transformer!",
            move |name: String, env: String| {
                embedded_transformer_object
                    .set
                    .write()
                    .unwrap()
                    .entry(env)
                    .or_default()
                    .insert(name.as_str().into())
            },
        );

        let embedded_transformer_object = transformers.clone();
        engine.register_fn(
            "current-macro-transformers!",
            move |env: SteelString| -> SteelVal {
                embedded_transformer_object
                    .set
                    .read()
                    .unwrap()
                    .get(env.as_str())
                    .map(|set| {
                        set.iter()
                            .map(|x| x.resolve().to_string())
                            .map(|x| SteelVal::SymbolV(x.into()))
                            .collect::<crate::values::lists::List<SteelVal>>()
                            .into()
                    })
                    .unwrap_or_else(|| SteelVal::ListV(List::new()))
            },
        );

        // engine.register_value(TOP_LEVEL_SCOPE_MAP, SteelVal::Void);
        // engine.register_value(TOP_LEVEL_GLOBAL_MAP, SteelVal::Void);

        // Load in parameters.
        // TODO: Merge this with the path in modules.rs
        if let Err(err) =
            engine.compile_and_run_raw_program(include_str!("../scheme/modules/parameters.scm"))
        {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        if let Err(err) =
            engine.compile_and_run_raw_program(include_str!("../scheme/modules/ports.scm"))
        {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        // Run the script for building the core interface for structs
        if let Err(err) = engine.compile_and_run_raw_program(KERNEL) {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        // let mut macros = HashSet::new();
        // macros.insert("%better-lambda%".to_string());
        // macros.insert(*STRUCT_KEYWORD);
        // macros.insert(*DEFINE_VALUES);

        Kernel {
            // macros,
            transformers,
            constants: HashSet::new(),
            engine: Box::new(engine),
        }
    }

    // pub(crate) fn bootstrap(_engine: Engine) -> (Self, RawProgramWithSymbols) {
    //     todo!()

    // let transformers = Transformers {
    //     set: Arc::new(RwLock::new(HashSet::default())),
    // };

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("register-macro-transformer!", move |name: String| {
    //     embedded_transformer_object
    //         .set
    //         .write()
    //         .unwrap()
    //         .insert(name.as_str().into())
    // });

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("current-macro-transformers!", move || -> SteelVal {
    //     embedded_transformer_object
    //         .set
    //         .read()
    //         .unwrap()
    //         .iter()
    //         .map(|x| x.resolve().to_string())
    //         .map(|x| SteelVal::SymbolV(x.into()))
    //         .collect::<crate::values::lists::List<SteelVal>>()
    //         .into()
    // });

    // Run the script for building the core interface for structs
    // engine.compile_and_run_raw_program(KERNEL).unwrap();

    // let raw_program = engine.emit_raw_program_no_path(KERNEL).unwrap();
    // engine.run_raw_program(raw_program.clone()).unwrap();

    // let mut macros = HashSet::new();
    // macros.insert("%better-lambda%".to_string());
    // macros.insert(*STRUCT_KEYWORD);
    // macros.insert(*DEFINE_VALUES);

    // (
    //     Kernel {
    //         // macros,
    //         transformers,
    //         constants: HashSet::new(),
    //         engine: Box::new(engine),
    //     },
    //     raw_program,
    // )
    // }

    // pub(crate) fn initialize_post_bootstrap(_engine: Engine) -> Self {
    // let transformers = Transformers {
    //     set: Arc::new(RwLock::new(HashSet::default())),
    // };

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("register-macro-transformer!", move |name: String| {
    //     embedded_transformer_object
    //         .set
    //         .write()
    //         .unwrap()
    //         .insert(name.as_str().into())
    // });

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("current-macro-transformers!", move || -> SteelVal {
    //     embedded_transformer_object
    //         .set
    //         .read()
    //         .unwrap()
    //         .iter()
    //         .map(|x| x.resolve().to_string())
    //         .map(|x| SteelVal::SymbolV(x.into()))
    //         .collect::<crate::values::lists::List<SteelVal>>()
    //         .into()
    // });

    // Kernel {
    //     // macros,
    //     transformers,
    //     constants: HashSet::new(),
    //     engine: Box::new(engine),
    // }

    //     todo!()
    // }

    pub fn is_constant(&self, ident: &InternedString) -> bool {
        self.constants.contains(ident)
    }

    // Define environment? Each module has its own set of transformers?
    pub fn load_syntax_transformers(
        &mut self,
        exprs: &mut Vec<ExprKind>,
        environment: String,
    ) -> Result<()> {
        enum IndexKind {
            DefMacro(usize),
            BeginForSyntax(usize),
        }

        let mut def_macro_expr_indices = smallvec::SmallVec::<[IndexKind; 5]>::new();

        let mut provide_definitions = thin_vec![ExprKind::ident("provide")];
        // TOOD: Consider using Arc<String> instead of just plain String!
        let environment_ident = ExprKind::string_lit(environment.clone());

        for (i, expr) in exprs.iter_mut().enumerate() {
            let first_ident = expr.list().and_then(|l| l.first_ident());

            match first_ident {
                Some(ident) if *ident == *DEFMACRO => {
                    let underlying = expr.list_mut_or_else(|| unreachable!()).unwrap();

                    match &underlying[1] {
                        ExprKind::List(l) => provide_definitions.push(l[0].clone()),
                        ExprKind::Atom(_) => {
                            provide_definitions.push(underlying.args[1].clone());
                        }
                        _ => {
                            let span = underlying[0].atom_syntax_object().unwrap().span;
                            stop!(BadSyntax => "malformed defmacro"; span);
                        }
                    }

                    underlying.args.insert(1, environment_ident.clone());

                    def_macro_expr_indices.push(IndexKind::DefMacro(i));
                }

                Some(ident) if *ident == *BEGIN_FOR_SYNTAX => {
                    def_macro_expr_indices.push(IndexKind::BeginForSyntax(i));
                }

                _ => {}
            }
        }

        // if def_macro_expr_indices.is_empty() {
        //     return Ok(());
        // }

        // Fill up the define macro expressions with the correct ones
        // Lets do some debug logging to make sure this even makes sense
        // in the first place
        let mut def_macro_exprs = ThinVec::with_capacity(def_macro_expr_indices.len() + 3);

        let mut macro_of_choice: InternedString = "#%syntax-transformer-module".into();

        if environment == "top-level" {
            if self.engine.extract_value("top-level").is_ok() {
                macro_of_choice = "#%syntax-transformer-module-update".into();
            }
        }

        def_macro_exprs.push(ExprKind::atom(macro_of_choice));
        def_macro_exprs.push(ExprKind::ident(&environment));

        if !def_macro_expr_indices.is_empty() {
            def_macro_exprs.push(ExprKind::List(steel_parser::ast::List::new(
                provide_definitions,
            )));
        }

        let mut exprs_buffer = ThinVec::with_capacity(def_macro_expr_indices.len());

        while let Some(i) = def_macro_expr_indices.pop() {
            match i {
                IndexKind::DefMacro(i) => {
                    let def_macro = exprs.remove(i);

                    exprs_buffer.push(def_macro);
                }
                IndexKind::BeginForSyntax(i) => {
                    let begin_for_syntax = exprs.remove(i);

                    let mut iter = begin_for_syntax.into_list().args.into_iter();
                    iter.next();

                    exprs_buffer.extend(iter);
                }
            }
        }

        exprs_buffer.reverse();

        def_macro_exprs.append(&mut exprs_buffer);

        // Create the generated module
        let generated_module = ExprKind::List(steel_parser::ast::List::new(def_macro_exprs));

        // self.engine.run(format!(
        //     "(set! #%loading-current-module \"{}\")",
        //     if cfg!(windows) {
        //         environment.replace("\\", "/")
        //     } else {
        //         environment
        //     }
        // ))?;

        self.engine.call_function_by_name_with_args_from_mut_slice(
            "#%set-module",
            &mut [SteelVal::StringV(
                if cfg!(windows) {
                    environment.replace("\\", "/")
                } else {
                    environment
                }
                .into(),
            )],
        )?;

        // TODO: Load this as a module instead, so that way we have some real
        // separation from each other.
        //
        // Meaning something like:
        // (create-environment <environment> (provide <defmacros> ...) <defmacro definitions> ...)
        //
        // Expands into something like:
        //
        // (define env (let () <defmacro definitions> (hash <defmacro defs>)))
        //
        // Then, expansion would work by just interacting with that environment - something like
        // (call-function-in-environment foo env)
        // Which will extract the function from the module, and call it with the arguments. In theory
        // that should give us the necessary isolation between modules that we want
        // for the purposes of this.
        self.engine
            .run_raw_program_from_exprs(vec![generated_module])?;

        // self.engine
        //     .run("(set! #%loading-current-module \"default\")".to_owned())?;

        self.engine.call_function_by_name_with_args_from_mut_slice(
            "#%set-module",
            &mut [SteelVal::StringV("default".into())],
        )?;

        Ok(())
    }

    // TODO: Have this report errors
    pub fn load_program_for_comptime(
        &mut self,
        constants: crate::values::HashMap<InternedString, SteelVal, FxBuildHasher>,
        exprs: &mut Vec<ExprKind>,
    ) -> Result<()> {
        let mut analysis = SemanticAnalysis::new(exprs);

        let result = analysis.run_black_box(constants, false);

        // Only select expressions which are identified as constant
        let subset = analysis
            .exprs
            .iter()
            .filter_map(|expr| {
                match expr {
                    ExprKind::Define(define) => {
                        if let ExprKind::LambdaFunction(_) = &define.body {
                            let name = define.name.atom_identifier().unwrap();

                            return if result.contains_key(name) {
                                Some(expr.clone())
                            } else if self.engine.global_exists(name.resolve()) {
                                // If this global exists, nuke it in the engine

                                self.engine.register_value(name.resolve(), SteelVal::Void);

                                None
                            } else {
                                None
                            };
                        }
                    }
                    ExprKind::Begin(b) => {
                        let begin = b
                            .exprs
                            .iter()
                            .filter(|expr| {
                                if let ExprKind::Define(define) = expr {
                                    if let ExprKind::LambdaFunction(_) = &define.body {
                                        let name = define.name.atom_identifier().unwrap();

                                        return result.contains_key(name);

                                        // return if result.contains_key(&name) {
                                        //     true
                                        // } else if self.engine.global_exists(name.resolve()) {
                                        //     false
                                        // } else {
                                        //     false
                                        // };
                                    }
                                }

                                false
                            })
                            .cloned()
                            .collect();

                        return Some(ExprKind::Begin(Box::new(crate::parser::ast::Begin::new(
                            begin,
                            b.location.clone(),
                        ))));
                    }

                    _ => {}
                }

                None

                // TODO: Also check begin statements here as well
                // if let ExprKind::Define(define) = expr {
                //     if let ExprKind::LambdaFunction(_) = &define.body {
                //         let name = define.name.atom_identifier().unwrap().clone();

                //         return result.contains_key(&name);
                //     }
                // }
            })
            // .cloned()
            .collect::<Vec<_>>();

        log::debug!("Loading constant functions");

        if subset.is_empty() {
            log::debug!("Found no constant functions");

            return Ok(());
        }

        self.engine.run_raw_program_from_exprs(subset)?;

        fn set(var: ExprKind, expr: ExprKind) -> ExprKind {
            ExprKind::Set(Box::new(Set::new(
                var,
                expr,
                SyntaxObject::default(TokenType::Set),
            )))
        }

        let mut set_idents = Vec::with_capacity(result.len());

        let memoize: InternedString = "%make-memoize".into();

        for ident in result.keys() {
            self.constants.insert(*ident);

            let set_ident = ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::Identifier(ident.clone()),
            )));
            let memoize_expr = expr_list!(
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                    memoize.clone()
                )))),
                set_ident.clone()
            );

            set_idents.push(set(set_ident, memoize_expr));
        }

        self.engine.run_raw_program_from_exprs(set_idents)?;

        log::debug!("Constant functions loaded");

        // dbg!(&self
        //     .constants
        //     .iter()
        //     .map(|x| x.resolve())
        //     .collect::<Vec<_>>());

        Ok(())

        // todo!("Run through every expression, and memoize them by calling (set! <ident> (make-memoize <ident>))")
    }

    pub fn exported_defmacros(&self, environment: &str) -> Option<FxHashSet<InternedString>> {
        self.transformers
            .set
            .read()
            .unwrap()
            .get(environment)
            .cloned()
    }

    pub fn contains_syntax_object_macro(
        &self,
        ident: &InternedString,
        environment: Option<&str>,
    ) -> bool {
        if let Some(environment) = environment {
            // TODO: Come back to this - but we really just
            // want a chain of environment that we've added to
            // transformers
            let base_environment = self
                .transformers
                .set
                .read()
                .unwrap()
                .get(environment)
                .map(|x| x.contains(ident))
                .unwrap_or_default();

            base_environment
                || self
                    .transformers
                    .set
                    .read()
                    .unwrap()
                    .get("default")
                    .map(|x| x.contains(ident))
                    .unwrap_or_default()
        } else {
            self.transformers
                .set
                .read()
                .unwrap()
                .get("default")
                .map(|x| x.contains(ident))
                .unwrap_or_default()
        }
    }

    pub fn call_function(&mut self, ident: &InternedString, args: &[SteelVal]) -> Result<SteelVal> {
        let function = self.engine.extract_value(ident.resolve())?;

        self.engine.call_function_with_args(function, args.to_vec())
    }

    pub(crate) fn expand_syntax_object(
        &mut self,
        ident: &InternedString,
        expr: ExprKind,
        environment: &str,
        in_scope: &ScopeSet<InternedString, FxBuildHasher>,
        globals: GlobalMap,
    ) -> Result<ExprKind> {
        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let span = get_span(&expr);

        let syntax_objects =
            super::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(expr)?;

        let function = if environment == "default" {
            // TODO: This actually needs to go through the proper resolution process,
            // probably some sort of transformer map.
            self.engine.extract_value(ident.resolve())?
        } else {
            // Check if there is anything to expand in this environment
            if let Ok(SteelVal::HashMapV(map)) = self.engine.extract_value(environment) {
                if let Some(func) = map.get(&SteelVal::SymbolV(ident.resolve().to_string().into()))
                {
                    func.clone()
                } else {
                    self.engine.extract_value(ident.resolve())?
                }
            } else {
                self.engine.extract_value(ident.resolve())?
            }
        };

        let in_scope = ParentScopeSet { set: in_scope };
        let globals = GlobalSymbolMap { map: &globals };

        let result = self
            .engine
            .with_immutable_reference(&in_scope)
            .with_immutable_reference(&globals)
            .consume_once(|engine, args| {
                let mut args_iter = args.into_iter();

                engine.update_value(TOP_LEVEL_SCOPE_MAP, args_iter.next().unwrap());
                engine.update_value(TOP_LEVEL_GLOBAL_MAP, args_iter.next().unwrap());

                engine
                    .call_function_with_args_from_mut_slice(function, &mut [syntax_objects])
                    .map_err(|x| x.set_span(span))
            })?;

        // This shouldn't be lowering all the way. It should just be back to list right?
        let res = TryFromSteelValVisitorForExprKind::root(&result);

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Kernel expansion time: {:?}", now.elapsed());

        res
    }
}
