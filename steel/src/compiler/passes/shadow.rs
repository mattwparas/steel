use quickscope::{ScopeMap, ScopeSet};

use crate::parser::ast::Atom;

use super::VisitorMutRefUnit;

pub struct RenameShadowedVariables {
    modified: bool,
    scope: ScopeSet<String>,
    // Modify the variable with the depth
    shadows: ScopeMap<String, usize>,
}

impl RenameShadowedVariables {
    pub fn new() -> Self {
        Self {
            scope: ScopeSet::new(),
            shadows: ScopeMap::new(),
            modified: false,
        }
    }
}

impl VisitorMutRefUnit for RenameShadowedVariables {
    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        self.scope.push_layer();
        self.shadows.push_layer();

        // TODO: Insert the code here to mark these variables as in scope

        for variable in lambda_function.arguments_mut() {
            if self.scope.contains(variable) {
                let modifier = self.scope.depth();
                self.shadows.define(variable.to_string(), modifier);
                variable.push(std::char::from_digit(modifier as u32, 10).unwrap())
            }

            self.scope.define(variable.to_string());
        }

        self.visit(&mut lambda_function.body);

        self.scope.pop_layer();
        self.shadows.pop_layer();
    }

    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(ident) = a.ident_mut() {
            if let Some(modifier) = self.shadows.get(ident.as_str()) {
                // Append the variable with the depth it occurs at.
                // Now, shadowing shouldn't actually _be_ a problem
                ident.push(std::char::from_digit(*modifier as u32, 10).unwrap());
                self.modified = true;
            }
        }
    }

    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) {
        self.scope.push_layer();
        self.shadows.push_layer();

        // TODO: Insert the code here to mark these variables as in scope

        self.scope.pop_layer();
        self.shadows.pop_layer();

        todo!()
    }
}

#[cfg(test)]
mod shadow_mangling_tests {
    use crate::parser::{ast::AstTools, parser::Parser};

    use super::*;

    #[test]
    fn check_basic_shadowing() {
        let script = r#"
            (define (foo x)
                (let ((x x))
                    (+ x 10)))
        "#;

        let mut exprs = Parser::parse(script).unwrap();
        let mut renamer = RenameShadowedVariables::new();

        for expr in exprs.iter_mut() {
            renamer.visit(expr);
        }

        exprs.pretty_print();
    }

    #[test]
    fn check_basic_shadowing_multiple_levels() {
        let script = r#"
            (define (foo x)
                (let ((x x))
                    (let ((x x))
                        (+ x 10))))
        "#;

        let mut exprs = Parser::parse(script).unwrap();
        let mut renamer = RenameShadowedVariables::new();

        for expr in exprs.iter_mut() {
            renamer.visit(expr);
        }

        exprs.pretty_print();
    }
}
