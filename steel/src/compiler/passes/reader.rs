use crate::parser::ast::{ExprKind, List};

use super::Folder;

pub struct ExpandMethodCalls {}

impl ExpandMethodCalls {
    pub fn expand_methods(exprs: Vec<ExprKind>) -> Vec<ExprKind> {
        ExpandMethodCalls {}.fold(exprs)
    }
}

impl Folder for ExpandMethodCalls {
    #[inline]
    fn visit_list(&mut self, mut l: List) -> ExprKind {
        if l.args.is_empty() {
            l.args = l.args.into_iter().map(|e| self.visit(e)).collect();
            return ExprKind::List(l);
        }

        // println!("Visiting list: {:?}", l);

        if let Some(func) = l.first_ident().map(|x| x.to_string()) {
            // println!("Inside this if statement");
            // println!("Func: {:?}", func);

            if func.contains('.') {
                let words = func.split('.').collect::<Vec<_>>();

                if words.len() != 2 {
                    l.args = l.args.into_iter().map(|e| self.visit(e)).collect();
                    return ExprKind::List(l);
                } else {
                    // Here we're going to transform (struct.method ...)
                    // into (method struct)
                    let mut first = l.args.remove(0);
                    let mut second = first.clone();

                    // method
                    first.update_string_in_atom(words[0].to_string());

                    // struct
                    second.update_string_in_atom(words[1].to_string());

                    // method pushed on first
                    l.args.insert(0, first);

                    // then the struct
                    l.args.insert(0, second);

                    l.args = l.args.into_iter().map(|e| self.visit(e)).collect();

                    // println!("Expanded method call: {:?}", l);

                    return ExprKind::List(l);
                }
            }
        }

        l.args = l.args.into_iter().map(|e| self.visit(e)).collect();
        ExprKind::List(l)
    }
}

pub struct MultipleArityFunctions {}

impl MultipleArityFunctions {
    pub fn expand_multiple_arity_functions(exprs: Vec<ExprKind>) -> Vec<ExprKind> {
        MultipleArityFunctions {}.fold(exprs)
    }
}

impl Folder for MultipleArityFunctions {
    #[inline]
    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<crate::parser::ast::LambdaFunction>,
    ) -> ExprKind {
        // Visit the body
        lambda_function.body = self.visit(lambda_function.body);

        let mut dot_count = 0;

        lambda_function
            .args
            .iter()
            .filter_map(|x| x.atom_identifier())
            .for_each(|x| {
                if x == "." {
                    dot_count += 1;
                }
            });

        // If we found one dot in the correct position
        // Adjust this lambda function to be a rest function
        if dot_count == 1 {
            let dot_index = lambda_function.args.len() - 2;

            if let Some(dot) = lambda_function.args[dot_index].atom_identifier() {
                if dot == "." {
                    lambda_function.args.remove(dot_index);
                    lambda_function.rest = true;

                    log::debug!(target: "reader-macros", "transformed multi-arity function");
                } else {
                    return ExprKind::LambdaFunction(lambda_function);
                }
            }
        }

        ExprKind::LambdaFunction(lambda_function)
    }
}
