use crate::parser::{
    ast::{ExprKind, List},
    interner::InternedString,
};

use super::{Folder, VisitorMutRefUnit};

pub struct MultipleArityFunctions {
    dot: InternedString,
}

impl MultipleArityFunctions {
    pub fn new() -> Self {
        MultipleArityFunctions { dot: ".".into() }
    }

    pub fn expand_multiple_arity_functions(exprs: &mut Vec<ExprKind>) {
        let dot = ".".into();

        for expr in exprs.iter_mut() {
            MultipleArityFunctions { dot }.visit(expr)
        }
    }
}

impl VisitorMutRefUnit for MultipleArityFunctions {
    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        // Visit the body
        // lambda_function.body = self.visit(lambda_function.body);

        self.visit(&mut lambda_function.body);

        let mut dot_count = 0;

        lambda_function
            .args
            .iter()
            .filter_map(|x| x.atom_identifier())
            .for_each(|x| {
                if *x == self.dot {
                    dot_count += 1;
                }
            });

        // If we found one dot in the correct position
        // Adjust this lambda function to be a rest function
        if dot_count == 1 {
            let dot_index = lambda_function.args.len() - 2;

            if let Some(dot) = lambda_function.args[dot_index].atom_identifier() {
                if *dot == self.dot {
                    lambda_function.args.remove(dot_index);
                    lambda_function.rest = true;

                    // log::debug!(target: "reader-macros", "transformed multi-arity function");
                }
                // else {
                //     return ExprKind::LambdaFunction(lambda_function);
                // }
            }
        }

        // ExprKind::LambdaFunction(lambda_function)
    }
}
