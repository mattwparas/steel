use std::collections::HashMap;

use quickscope::{ScopeMap, ScopeSet};

use crate::parser::{
    ast::{Atom, ExprKind},
    interner::InternedString,
};

use super::VisitorMutRefUnit;

pub struct RenameShadowedVariables {
    modified: bool,
    scope: ScopeSet<InternedString>,
    // Modify the variable with the depth
    shadows: ScopeMap<InternedString, usize>,
    str_modifiers: HashMap<usize, String>,
}

impl Default for RenameShadowedVariables {
    fn default() -> Self {
        Self::new()
    }
}

impl RenameShadowedVariables {
    pub fn new() -> Self {
        Self {
            scope: ScopeSet::new(),
            shadows: ScopeMap::new(),
            modified: false,
            str_modifiers: HashMap::new(),
        }
    }

    pub fn rename_shadowed_vars(exprs: &mut [ExprKind]) {
        let mut renamer = Self::new();

        for expr in exprs.iter_mut() {
            renamer.visit(expr);
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
                self.shadows.define(*variable, modifier);

                // Create a mutable string to mangle
                let mut mut_var = "##".to_string() + variable.resolve();

                if let Some(char_modifier) = char::from_digit(modifier as u32, 10) {
                    mut_var.push(char_modifier);
                } else if let Some(str_modifier) = self.str_modifiers.get(&modifier) {
                    mut_var.push_str(str_modifier);
                } else {
                    self.str_modifiers.insert(modifier, modifier.to_string());
                    mut_var.push_str(self.str_modifiers.get(&modifier).unwrap());
                }

                // println!("Mangling variable: {}", mut_var);

                *variable = mut_var.into();

                self.scope.define(*variable);

                continue;
            }

            self.scope.define(*variable);
        }

        self.visit(&mut lambda_function.body);

        self.scope.pop_layer();
        self.shadows.pop_layer();
    }

    fn visit_quote(&mut self, _quote: &mut steel_parser::ast::Quote) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(ident) = a.ident_mut() {
            if let Some(modifier) = self.shadows.get(ident) {
                // Append the variable with the depth it occurs at.
                // Now, shadowing shouldn't actually _be_ a problem
                // ident.push(char::from_digit(*modifier as u32, 10).unwrap());

                let mut mut_ident = "##".to_string() + ident.resolve();

                if let Some(char_modifier) = char::from_digit(*modifier as u32, 10) {
                    mut_ident.push(char_modifier)
                } else if let Some(str_modifier) = self.str_modifiers.get(modifier) {
                    mut_ident.push_str(str_modifier)
                } else {
                    panic!("The modifier should be defined by now")
                }

                *ident = mut_ident.into();

                self.modified = true;
            }
        }
    }

    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) {
        self.scope.push_layer();
        self.shadows.push_layer();

        for variable in l
            .bindings
            .iter_mut()
            .filter_map(|x| x.0.atom_identifier_mut())
        {
            if self.scope.contains(variable) {
                let modifier = self.scope.depth();
                self.shadows.define(*variable, modifier);

                let mut mut_var = "##".to_string() + variable.resolve();

                if let Some(char_modifier) = char::from_digit(modifier as u32, 10) {
                    mut_var.push(char_modifier);
                } else if let Some(str_modifier) = self.str_modifiers.get(&modifier) {
                    mut_var.push_str(str_modifier);
                } else {
                    self.str_modifiers.insert(modifier, modifier.to_string());
                    mut_var.push_str(self.str_modifiers.get(&modifier).unwrap());
                }

                *variable = mut_var.into();

                self.scope.define(*variable);
                continue;
            }

            self.scope.define(*variable);
        }

        //
        l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.visit(&mut l.body_expr);

        // TODO: Insert the code here to mark these variables as in scope

        self.scope.pop_layer();
        self.shadows.pop_layer();
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
