use fxhash::{FxBuildHasher, FxHashMap};
use quickscope::{ScopeMap, ScopeSet};

use crate::parser::{
    ast::{Atom, ExprKind},
    interner::InternedString,
};

use super::VisitorMutRefUnit;

#[derive(Clone)]
pub struct RenameShadowedVariables {
    modified: bool,
    // HACK: Pointing the identifier to its old, premangled self
    scope: ScopeSet<InternedString, FxBuildHasher>,
    reverse_map: ScopeMap<InternedString, InternedString>,
    // Modify the variable with the depth
    shadows: ScopeMap<InternedString, usize, FxBuildHasher>,
    str_modifiers: FxHashMap<usize, String>,
    rename_all: bool,
}

impl Default for RenameShadowedVariables {
    fn default() -> Self {
        Self::new()
    }
}

impl RenameShadowedVariables {
    pub fn new() -> Self {
        Self {
            scope: ScopeSet::default(),
            shadows: ScopeMap::default(),
            reverse_map: ScopeMap::default(),
            modified: false,
            str_modifiers: FxHashMap::default(),
            rename_all: false,
        }
    }

    fn clear(&mut self) {
        self.scope.clear_all();
        self.shadows.clear_all();
        self.modified = false;
        self.str_modifiers.clear();
    }

    pub fn rename_shadowed_variables(&mut self, exprs: &mut [ExprKind], rename_all: bool) {
        self.clear();
        self.rename_all = rename_all;

        // Global scope?
        // self.scope.push_layer();
        // self.shadows.push_layer();

        for expr in exprs.iter_mut() {
            // println!(
            //     "Top level scope: {:?}",
            //     self.scope.iter().map(|x| x.resolve()).collect::<Vec<_>>()
            // );

            self.visit(expr);
        }

        // self.scope.pop_layer();
        // self.shadows.pop_layer();

        self.rename_all = false;
    }

    pub fn rename_shadowed_vars(exprs: &mut [ExprKind]) {
        let mut renamer = Self::new();

        for expr in exprs.iter_mut() {
            renamer.visit(expr);
        }
    }
}

impl VisitorMutRefUnit for RenameShadowedVariables {
    // fn visit_begin(&mut self, begin: &mut steel_parser::ast::Begin) {
    //     for expr in &mut begin.exprs {
    //         if self.scope.depth() > 1 {
    //             // TODO: Flatten begins _first_
    //             if let ExprKind::Define(define) = expr {
    //                 if let ExprKind::Atom(a) = &mut define.name {
    //                     let variable = if let Some(variable) = a.syn.ty.identifier_mut() {
    //                         variable
    //                     } else {
    //                         return;
    //                     };

    //                     if self.rename_all || self.scope.contains(variable) {
    //                         let modifier = self.scope.depth();
    //                         self.shadows.define(*variable, modifier);
    //                         // println!("Renaming: {} -> {}", variable.resolve());

    //                         // Create a mutable string to mangle
    //                         let mut mut_var = "##".to_string() + variable.resolve();

    //                         if let Some(char_modifier) = char::from_digit(modifier as u32, 10) {
    //                             mut_var.push(char_modifier);
    //                         } else if let Some(str_modifier) = self.str_modifiers.get(&modifier) {
    //                             mut_var.push_str(str_modifier);
    //                         } else {
    //                             self.str_modifiers.insert(modifier, modifier.to_string());
    //                             mut_var.push_str(self.str_modifiers.get(&modifier).unwrap());
    //                         }
    //                         println!(
    //                             "define - Renaming: {} -> {} @ depth: {}",
    //                             variable.resolve(),
    //                             mut_var,
    //                             modifier
    //                         );

    //                         *variable = mut_var.into();
    //                     }

    //                     self.scope.define(*variable);
    //                 }
    //             }
    //         }
    //     }

    //     for expr in &mut begin.exprs {
    //         self.visit(expr);
    //     }
    // }

    fn visit_define(&mut self, define: &mut steel_parser::ast::Define) {
        if self.scope.depth() > 1 {
            if let ExprKind::Atom(a) = &mut define.name {
                let variable = if let Some(variable) = a.syn.ty.identifier_mut() {
                    variable
                } else {
                    return;
                };

                if self.rename_all || self.scope.contains(variable) {
                    let modifier = self.scope.depth();
                    self.shadows.define(*variable, modifier);
                    // println!("Renaming: {} -> {}", variable.resolve());

                    let prev = *variable;

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
                    // println!(
                    //     "define - Renaming: {} -> {} @ depth: {}",
                    //     variable.resolve(),
                    //     mut_var,
                    //     modifier
                    // );

                    *variable = mut_var.into();

                    if a.syn.introduced_via_macro {
                        // println!("Defining: {} -> {}", prev.resolve(), variable.resolve());
                        self.reverse_map.define(prev, *variable);
                    }
                }

                // self.reverse_map.define(, )
                self.scope.define(*variable);
            }
        }
        self.visit(&mut define.body);
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        self.scope.push_layer();
        self.shadows.push_layer();

        // for variable in lambda_function.arguments_mut() {
        for syn in lambda_function.syntax_objects_arguments_mut() {
            // if depth == 1 {
            //     println!("Would be defining: {}", variable.resolve());
            // }

            let variable = if let Some(variable) = syn.ty.identifier_mut() {
                variable
            } else {
                continue;
            };

            if self.rename_all || self.scope.contains(variable) {
                // println!("Renaming: {}", variable.resolve());

                let modifier = self.scope.depth();
                self.shadows.define(*variable, modifier);

                let prev = *variable;

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
                // println!("Renaming: {} -> {}", variable.resolve(), mut_var);

                *variable = mut_var.into();

                self.scope.define(*variable);

                if syn.introduced_via_macro {
                    // println!("Defining: {} -> {}", prev.resolve(), variable.resolve());
                    self.reverse_map.define(prev, *variable);
                }

                // if let Some(prev) = syn.previous_name {
                //     println!("Defining: {} -> {}", prev.resolve(), variable.resolve());
                //     self.reverse_map.define(prev, *variable);
                // }

                continue;
            }

            self.scope.define(*variable);
        }

        self.visit(&mut lambda_function.body);

        self.scope.pop_layer();
        self.shadows.pop_layer();
        self.reverse_map.pop_layer();
    }

    fn visit_quote(&mut self, _quote: &mut steel_parser::ast::Quote) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        // if let Some(ident) = a.ident_mut().copied() {
        // Set up this atom to point to the previous one
        // if let Some(prev) = a.syn.previous_name {
        //     println!("Defining: {} -> {}", prev.resolve(), ident.resolve());
        //     self.reverse_map.define(prev, ident);
        // }
        // }

        // Don't mangle it if its an unresolved identifier!
        // if a.syn.unresolved {
        //     println!("Skipping mangling: {}", a);
        //     println!("Introduced via macro: {}", a.syn.introduced_via_macro);
        //     return;
        // }

        let unresolved = a.syn.unresolved;

        // println!("Visiting {} @ depth: {}", a, self.scope.depth());
        // println!(
        //     "Scope: {:?}",
        //     self.scope.iter().map(|x| x.resolve()).collect::<Vec<_>>()
        // );
        // println!("Shadows: {:?}", self.shadows.iter().map(|x| x.resolve()).collect::<Vec<_>>());

        if let Some(ident) = a.ident_mut() {
            if let Some(modifier) = self.shadows.get(ident) {
                // If the value is unresolved, but there is in scope a value
                // that was also introduced via macro, then we want to resolve
                // this variable to point to that one as well.
                if unresolved && !self.reverse_map.contains_key(ident) {
                    // println!("Skipping mangling: {}", a);
                    // println!("Introduced via macro: {}", a.syn.introduced_via_macro);
                    return;
                }

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

    // fn visit_define(&mut self, define: &mut steel_parser::ast::Define) {

    // }

    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) {
        l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.scope.push_layer();
        self.shadows.push_layer();

        for syn in l
            .bindings
            .iter_mut()
            .filter_map(|x| x.0.atom_syntax_object_mut())
        {
            let variable = if let Some(variable) = syn.ty.identifier_mut() {
                variable
            } else {
                continue;
            };

            if self.rename_all || self.scope.contains(variable) {
                let modifier = self.scope.depth();
                self.shadows.define(*variable, modifier);
                let prev = *variable;

                let mut mut_var = "##".to_string() + variable.resolve();

                if let Some(char_modifier) = char::from_digit(modifier as u32, 10) {
                    mut_var.push(char_modifier);
                } else if let Some(str_modifier) = self.str_modifiers.get(&modifier) {
                    mut_var.push_str(str_modifier);
                } else {
                    self.str_modifiers.insert(modifier, modifier.to_string());
                    mut_var.push_str(self.str_modifiers.get(&modifier).unwrap());
                }

                // println!("Renaming: {} -> {}", variable.resolve(), mut_var);
                *variable = mut_var.into();

                self.scope.define(*variable);

                if syn.introduced_via_macro {
                    // println!("Defining: {} -> {}", prev.resolve(), variable.resolve());
                    self.reverse_map.define(prev, *variable);
                }

                continue;
            }

            self.scope.define(*variable);
        }

        //
        // l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.visit(&mut l.body_expr);

        // TODO: Insert the code here to mark these variables as in scope

        self.scope.pop_layer();
        self.shadows.pop_layer();
        self.reverse_map.pop_layer();
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
