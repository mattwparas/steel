use std::{collections::HashSet, convert::TryFrom};

use crate::{compiler::program::STRUCT_KEYWORD, parser::ast::from_list_repr_to_ast, rvals::Result};
use crate::{stdlib::KERNEL, steel_vm::engine::Engine, SteelVal};

use super::{ast::ExprKind, interner::InternedString, span_visitor::get_span};

thread_local! {
    // pub(crate) static KERNEL_IMAGE: Engine = Engine::new_kernel();
    pub(crate) static KERNEL_IMAGE: Engine = Engine::new_bootstrap_kernel();
}

pub(crate) fn fresh_kernel_image() -> Engine {
    KERNEL_IMAGE.with(|x| x.clone())
}

/// The Kernel is an engine context used to evaluate defmacro style macros
/// It lives inside the compiler, so in theory there could be tiers of kernels
/// Note: this will be called along side syntax rules style macros. In this case macro
/// expansion then is a one step process, dispatching on the macros. However, at the moment this
/// will not support user defined def macros -> this kernel will only support the built in def macros
/// for structs.
#[derive(Clone)]
pub struct Kernel {
    macros: HashSet<InternedString>,
    engine: Box<Engine>,
}

impl Default for Kernel {
    fn default() -> Self {
        Self::new()
    }
}

impl Kernel {
    pub fn new() -> Self {
        let mut engine = fresh_kernel_image();

        // Run the script for building the core interface for structs
        engine.compile_and_run_raw_program(KERNEL).unwrap();

        let mut macros = HashSet::new();
        // macros.insert("%better-lambda%".to_string());
        macros.insert(*STRUCT_KEYWORD);

        Kernel {
            macros,
            engine: Box::new(engine),
        }
    }

    pub fn contains_macro(&self, ident: &InternedString) -> bool {
        self.macros.contains(ident)
    }

    pub fn expand(&mut self, ident: &InternedString, expr: ExprKind) -> Result<ExprKind> {
        let span = get_span(&expr);

        // let syntax_objects = SyntaxObjectFromExprKind::try_from_expr_kind(expr.clone())?;

        // println!("{:?}", syntax_objects);

        // println!(
        //     "{:?}",
        //     crate::rvals::Syntax::steelval_to_exprkind(&syntax_objects).map(from_list_repr_to_ast)
        // );

        let args = SteelVal::try_from(expr)?;

        let function = self.engine.extract_value(ident.resolve())?;

        if let SteelVal::ListV(list) = args {
            let mut iter = list.into_iter();
            // Drop the macro name
            iter.next();

            let arguments = iter.collect();

            log::info!(target: "kernel", "Expanding: {:?} with arguments: {:?}", ident, arguments);

            let result = self
                .engine
                .call_function_with_args(function, arguments)
                .map_err(|x| x.set_span(span))?;

            // let expr = ExprKind::try_from(&result);

            // println!("Expanded to: {:?}", result.to_string());

            // println!("Expanded to: {:#?}", expr);

            // TODO: try to understand what is actually happening here
            let ast_version = ExprKind::try_from(&result)
                .map(from_list_repr_to_ast)
                .unwrap()
                .unwrap();

            // println!("{}")
            // println!("{}", ast_version.to_pretty(60));

            Ok(ast_version)

            // Ok(
            //     crate::parser::parser::Parser::parse(result.to_string().trim_start_matches('\''))?
            //         .into_iter()
            //         .next()
            //         .unwrap(),
            // )
        } else {
            stop!(TypeMismatch => "call-function-in-env expects a list for the arguments")
        }
    }
}
