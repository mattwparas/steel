use std::collections::{HashMap, HashSet};

use quickscope::ScopeMap;

use crate::parser::{
    ast::ExprKind,
    parser::{IdentifierMetadata, SyntaxObject},
    visitors::VisitorMutRef,
};

use super::{VisitorMutRefUnit, VisitorMutUnitRef};

#[derive(Debug)]
enum IdentifierStatus {
    Global,
    Local,
    Captured,
}

#[derive(Debug)]
pub struct LexicalInformation {
    kind: IdentifierStatus,
    set_bang: bool,
}

// Populate the metadata about individual
#[derive(Default, Debug)]
pub struct Analysis {
    info: HashMap<usize, LexicalInformation>,
}

impl Analysis {
    pub fn run(&mut self, exprs: &mut [ExprKind]) {
        for expr in exprs {
            let mut pass = AnalysisPass::new(self);
            pass.visit(expr);
        }
    }

    pub fn insert(&mut self, object: &SyntaxObject, metadata: LexicalInformation) {
        self.info.insert(object.syntax_object_id, metadata);
    }

    pub fn get(&self, object: &SyntaxObject) -> Option<&LexicalInformation> {
        self.info.get(&object.syntax_object_id)
    }
}

#[derive(Default)]
pub struct MetaDataPopulator {
    depth: usize,
}

impl VisitorMutRefUnit for MetaDataPopulator {
    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) {
        self.depth += 1;
        l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.depth -= 1;
        self.visit(&mut l.body_expr);
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        for var in &mut lambda_function.args {
            self.visit(var);
        }
        self.depth += 1;
        self.visit(&mut lambda_function.body);
        self.depth -= 1;
    }

    fn visit_atom(&mut self, a: &mut crate::parser::ast::Atom) {
        // Populate metadata on the identifier if its not there
        match &mut a.syn.metadata {
            Some(m) => m.depth = self.depth,
            m @ None => *m = Some(IdentifierMetadata { depth: self.depth }),
        }
    }
}

struct AnalysisPass<'a> {
    info: &'a mut Analysis,
    scope: ScopeMap<String, bool>,
}

impl<'a> AnalysisPass<'a> {
    pub fn new(info: &'a mut Analysis) -> Self {
        AnalysisPass {
            info,
            scope: ScopeMap::new(),
        }
    }
}

impl<'a> VisitorMutRefUnit for AnalysisPass<'a> {
    fn visit_define(&mut self, define: &mut crate::parser::ast::Define) {
        self.visit(&mut define.name);
        self.visit(&mut define.body);
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        // We're entering a new scope since we've entered a lambda function
        self.scope.push_layer();

        let let_level_bindings = lambda_function
            .args
            .iter()
            .map(|x| x.atom_identifier().unwrap())
            .collect::<Vec<_>>();

        for arg in lambda_function
            .args
            .iter()
            .map(|x| x.atom_identifier().unwrap())
        {
            self.scope.define(arg.to_string(), false);
        }

        self.visit(&mut lambda_function.body);

        // We've exited the scope, we're done
        // self.scope.pop_layer();

        let captured_vars = self
            .scope
            .iter()
            .filter(|x| *x.1)
            .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
            .map(|x| x.0.to_string())
            .collect::<HashSet<_>>();

        self.scope.pop_layer();

        println!("Captured vars: {:?}", captured_vars);
        println!("Let level bindings: {:?}", let_level_bindings);

        for var in &lambda_function.args {
            println!("Var: {:?}", var);
            let kind = if captured_vars.contains(var.atom_identifier().unwrap()) {
                IdentifierStatus::Captured
            } else {
                IdentifierStatus::Local
            };

            self.info.insert(
                &var.atom_syntax_object().unwrap(),
                LexicalInformation {
                    kind,
                    set_bang: false,
                },
            );
        }
    }

    fn visit_atom(&mut self, a: &mut crate::parser::ast::Atom) {
        let name = a.ident();

        if let Some(ident) = name {
            // If this contains a key at the top, then it shouldn't be marked as captured by this scope
            if self.scope.contains_key_at_top(ident) {
                // Set it to not be captured if its contained at the top level
                *(self.scope.get_mut(ident).unwrap()) = false;
                self.info.insert(
                    &a.syn,
                    LexicalInformation {
                        kind: IdentifierStatus::Local,
                        set_bang: false,
                    },
                );
                return;
            }

            // Otherwise, go ahead and mark it as captured if we can find a reference to it
            if let Some(is_captured) = self.scope.get_mut(ident) {
                *is_captured = true;
                self.info.insert(
                    &a.syn,
                    LexicalInformation {
                        kind: IdentifierStatus::Captured,
                        set_bang: false,
                    },
                );
                return;
            }

            // Otherwise, we've hit a global variable
            self.info.insert(
                &a.syn,
                LexicalInformation {
                    kind: IdentifierStatus::Global,
                    set_bang: false,
                },
            );
        }
    }

    // TODO: merge the representations of the anonymous function call
    // and the normal visit lambda
    fn visit_list(&mut self, l: &mut crate::parser::ast::List) {
        if l.is_anonymous_function_call() {
            if let Some(arguments) = l.rest_mut() {
                for argument in arguments {
                    self.visit(argument)
                }
            }

            // l.rest_mut()
            //     .map(|x| x.iter_mut().for_each(|x| self.visit(x)));

            // Since we're effectively visiting a let, enter the scope
            self.scope.push_layer();

            let func = l.first_func_mut().unwrap();

            // func.args.iter().map(|x| x.atom_identifier().unwrap())

            let let_level_bindings = func
                .args
                .iter()
                .map(|x| x.atom_identifier().unwrap())
                .collect::<Vec<_>>();

            for arg in &let_level_bindings {
                self.scope.define(arg.to_string(), false);
            }

            self.visit(&mut func.body);

            let captured_vars = self
                .scope
                .iter()
                .filter(|x| *x.1)
                .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
                .map(|x| x.0.to_string())
                .collect::<HashSet<_>>();

            self.scope.pop_layer();

            for var in &func.args {
                let kind = if captured_vars.contains(var.atom_identifier().unwrap()) {
                    IdentifierStatus::Captured
                } else {
                    IdentifierStatus::Local
                };

                self.info.insert(
                    &var.atom_syntax_object().unwrap(),
                    LexicalInformation {
                        kind,
                        set_bang: false,
                    },
                );
            }
        } else {
            for arg in &mut l.args {
                self.visit(arg)
            }
        }
    }
}

// struct PrinterPass {}

impl<'a> VisitorMutUnitRef<'a> for Analysis {
    fn visit_atom(&mut self, a: &'a crate::parser::ast::Atom) {
        println!(
            "Atom: {:?}, Lexical Information: {:?}",
            a.syn.ty,
            self.get(&a.syn)
        );
    }

    fn visit_lambda_function(&mut self, lambda_function: &'a crate::parser::ast::LambdaFunction) {
        for arg in &lambda_function.args {
            if let Some(arg) = arg.atom_syntax_object() {
                println!(
                    "Atom in function argument: {:?}, Lexical Information: {:?}",
                    arg.ty,
                    self.get(&arg)
                );
            }
        }

        self.visit(&lambda_function.body);
    }
}

#[cfg(test)]
mod analysis_pass_tests {
    use crate::parser::parser::Parser;

    use super::*;

    #[test]
    fn check_analysis_pass() {
        let script = r#"
        (define (foo x y z)
            (lambda (extra-arg) (+ x y z)))
        "#;

        let mut analysis = Analysis::default();
        let mut exprs = Parser::parse(script).unwrap();

        analysis.run(&mut exprs);

        analysis.visit(&exprs[0]);

        // println!("{:?}", analysis.info);
    }
}
