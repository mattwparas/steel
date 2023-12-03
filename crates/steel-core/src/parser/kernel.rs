use std::{
    collections::HashSet,
    sync::{Arc, RwLock},
};

use steel_parser::tokens::TokenType;

use crate::{
    compiler::{passes::analysis::SemanticAnalysis, program::RawProgramWithSymbols},
    parser::{
        ast::{Atom, Set},
        parser::SyntaxObject,
    },
    rvals::Result,
    steel_vm::register_fn::RegisterFn,
};
use crate::{stdlib::KERNEL, steel_vm::engine::Engine, SteelVal};

use steel_parser::expr_list;

use super::{
    ast::{ExprKind, TryFromSteelValVisitorForExprKind},
    interner::InternedString,
    span_visitor::get_span,
};

thread_local! {
    pub(crate) static KERNEL_IMAGE: Engine = Engine::new_bootstrap_kernel();
}

pub(crate) fn fresh_kernel_image() -> Engine {
    KERNEL_IMAGE.with(|x| x.clone())
}

// Internal set of transformers that we'll embed
#[derive(Clone, Debug)]
struct Transformers {
    set: Arc<RwLock<HashSet<InternedString>>>,
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

impl Kernel {
    pub fn new() -> Self {
        let mut engine = fresh_kernel_image();

        let transformers = Transformers {
            set: Arc::new(RwLock::new(HashSet::default())),
        };

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("register-macro-transformer!", move |name: String| {
            embedded_transformer_object
                .set
                .write()
                .unwrap()
                .insert(name.as_str().into())
        });

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("current-macro-transformers!", move || -> SteelVal {
            embedded_transformer_object
                .set
                .read()
                .unwrap()
                .iter()
                .map(|x| x.resolve().to_string())
                .map(|x| SteelVal::SymbolV(x.into()))
                .collect::<crate::values::lists::List<SteelVal>>()
                .into()
        });

        // Run the script for building the core interface for structs
        engine.compile_and_run_raw_program(KERNEL).unwrap();

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

    pub(crate) fn bootstrap(mut engine: Engine) -> (Self, RawProgramWithSymbols) {
        let transformers = Transformers {
            set: Arc::new(RwLock::new(HashSet::default())),
        };

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("register-macro-transformer!", move |name: String| {
            embedded_transformer_object
                .set
                .write()
                .unwrap()
                .insert(name.as_str().into())
        });

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("current-macro-transformers!", move || -> SteelVal {
            embedded_transformer_object
                .set
                .read()
                .unwrap()
                .iter()
                .map(|x| x.resolve().to_string())
                .map(|x| SteelVal::SymbolV(x.into()))
                .collect::<crate::values::lists::List<SteelVal>>()
                .into()
        });

        // Run the script for building the core interface for structs
        // engine.compile_and_run_raw_program(KERNEL).unwrap();

        let raw_program = engine.emit_raw_program_no_path(KERNEL).unwrap();
        engine.run_raw_program(raw_program.clone()).unwrap();

        // let mut macros = HashSet::new();
        // macros.insert("%better-lambda%".to_string());
        // macros.insert(*STRUCT_KEYWORD);
        // macros.insert(*DEFINE_VALUES);

        (
            Kernel {
                // macros,
                transformers,
                constants: HashSet::new(),
                engine: Box::new(engine),
            },
            raw_program,
        )
    }

    pub(crate) fn initialize_post_bootstrap(mut engine: Engine) -> Self {
        let transformers = Transformers {
            set: Arc::new(RwLock::new(HashSet::default())),
        };

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("register-macro-transformer!", move |name: String| {
            embedded_transformer_object
                .set
                .write()
                .unwrap()
                .insert(name.as_str().into())
        });

        let embedded_transformer_object = transformers.clone();
        engine.register_fn("current-macro-transformers!", move || -> SteelVal {
            embedded_transformer_object
                .set
                .read()
                .unwrap()
                .iter()
                .map(|x| x.resolve().to_string())
                .map(|x| SteelVal::SymbolV(x.into()))
                .collect::<crate::values::lists::List<SteelVal>>()
                .into()
        });

        Kernel {
            // macros,
            transformers,
            constants: HashSet::new(),
            engine: Box::new(engine),
        }
    }

    pub fn is_constant(&self, ident: &InternedString) -> bool {
        self.constants.contains(ident)
    }

    // TODO: Have this report errors
    pub fn load_program_for_comptime(
        &mut self,
        constants: im_rc::HashMap<InternedString, SteelVal>,
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
                            let name = define.name.atom_identifier().unwrap().clone();

                            return if result.contains_key(&name) {
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
                                        let name = define.name.atom_identifier().unwrap().clone();

                                        return if result.contains_key(&name) {
                                            true
                                        } else if self.engine.global_exists(name.resolve()) {
                                            false
                                        } else {
                                            false
                                        };
                                    }
                                }

                                false
                            })
                            .cloned()
                            .collect();

                        return Some(ExprKind::Begin(crate::parser::ast::Begin::new(
                            begin,
                            b.location.clone(),
                        )));
                    }

                    _ => {}
                }

                return None;

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
            self.constants.insert(ident.clone());

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

    pub fn contains_syntax_object_macro(&self, ident: &InternedString) -> bool {
        // self.syntax_object_macros.contains(ident)

        self.transformers.set.read().unwrap().contains(ident)

        // self.engine.extract_value()

        // todo!()
    }

    pub fn call_function(&mut self, ident: &InternedString, args: &[SteelVal]) -> Result<SteelVal> {
        let function = self.engine.extract_value(ident.resolve())?;

        self.engine.call_function_with_args(function, args.to_vec())
    }

    pub(crate) fn expand_syntax_object(
        &mut self,
        ident: &InternedString,
        expr: ExprKind,
    ) -> Result<ExprKind> {
        // println!("EXPANDING: {}", ident);

        let span = get_span(&expr);

        let syntax_objects =
            super::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(expr.clone())?;

        let function = self.engine.extract_value(ident.resolve())?;

        // if let SteelVal::ListV(list) = syntax_objects {
        // let mut iter = list.into_iter();
        // Drop the macro name
        // iter.next();

        // let arguments = iter.collect();

        // log::info!(target: "kernel", "Expanding: {:?} with arguments: {:?}", ident, arguments);

        let result = self
            .engine
            .call_function_with_args(function, vec![syntax_objects])
            .map_err(|x| x.set_span(span))?;

        // let expr = ExprKind::try_from(&result);

        // println!("Expanded to: {:?}", result.to_string());

        // println!("Expanded to: {:#?}", expr);

        // TODO: try to understand what is actually happening here
        // let ast_version = ExprKind::try_from(&result)
        //     .map(from_list_repr_to_ast)
        //     .unwrap()
        //     .unwrap();

        let ast_version = TryFromSteelValVisitorForExprKind::root(&result).unwrap();

        // println!("{}")
        // println!("{}", ast_version.to_pretty(60));

        Ok(ast_version)

        // Ok(
        //     crate::parser::parser::Parser::parse(result.to_string().trim_start_matches('\''))?
        //         .into_iter()
        //         .next()
        //         .unwrap(),
        // )
        // } else {
        // stop!(TypeMismatch => "call-function-in-env expects a list for the arguments")
        // }
    }
}
