use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
};

use crate::{parser::ast::from_list_repr_to_ast, rvals::Result};
use crate::{stdlib::KERNEL, steel_vm::engine::Engine, SteelVal};

use super::{ast::ExprKind, span_visitor::get_span};

/// The Kernel is an engine context used to evaluate defmacro style macros
/// It lives inside the compiler, so in theory there could be tiers of kernels
/// Note: this will be called along side syntax rules style macros. In this case macro
/// expansion then is a one step process, dispatching on the macros. However, at the moment this
/// will not support user defined def macros -> this kernel will only support the built in def macros
/// for structs.
pub struct Kernel {
    macros: HashSet<String>,
    engine: Box<Engine>,
}

impl Kernel {
    pub fn new() -> Self {
        let mut engine = Engine::new_kernel();

        // Run the script for building the core interface for structs
        engine.compile_and_run_raw_program(KERNEL).unwrap();

        let mut macros = HashSet::new();
        macros.insert("make-struct".to_string());
        macros.insert("%lambda%".to_string());

        // let mut aliases = HashMap::new();
        // aliases.insert("lambda".to_string(), "%lambda%".to_string());

        Kernel {
            macros,
            engine: Box::new(engine),
        }
    }

    pub fn contains_macro(&self, ident: &str) -> bool {
        self.macros.contains(ident)
    }

    pub fn expand(&mut self, ident: &str, expr: ExprKind) -> Result<ExprKind> {
        let span = get_span(&expr);

        let args = SteelVal::try_from(expr)?;

        let function = self.engine.extract_value(ident)?;

        if let SteelVal::ListV(list) = args {
            let mut iter = list.into_iter();
            // Drop the macro name
            iter.next();

            let arguments = iter.collect();

            // println!("Expanding: {:?} with arguments: {:?}", ident, arguments);

            let result = self
                .engine
                .call_function_with_args(function, arguments)
                .map_err(|x| x.set_span(span))?;

            // let expr = ExprKind::try_from(&result);

            // println!("Expanded to: {:#?}", expr);

            // TODO: try to understand what is actually happening here
            // let ast_version = ExprKind::try_from(&result)
            //     .map(from_list_repr_to_ast)
            //     .unwrap()
            //     .unwrap();

            // println!("Expanded to: {}", ast_version);

            // Ok(ast_version)

            Ok(
                crate::parser::parser::Parser::parse(result.to_string().trim_start_matches('\''))?
                    .into_iter()
                    .next()
                    .unwrap(),
            )
        } else {
            stop!(TypeMismatch => "call-function-in-env expects a list for the arguments")
        }
    }
}
