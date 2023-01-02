use std::time::Instant;

use crate::parser::{
    ast::{Atom, Define, ExprKind, LambdaFunction, List},
    parser::SyntaxObject,
    tokens::TokenType,
};

use super::Folder;

use log::{debug, log_enabled};
// use itertools::Itertools;
use quickscope::ScopeMap;
// use std::collections::HashSet;

const GENSYM_PREFIX: &'static str = "##-##lambda-lifter-";

#[derive(Default)]
pub struct GenSym {
    counter: usize,
    defining_context: Option<String>,
}

impl GenSym {
    // Crappy way to get the new name for a lambda lifted function
    pub fn generate(&mut self) -> String {
        self.counter += 1;
        GENSYM_PREFIX.to_string()
            + self
                .defining_context
                .as_ref()
                .unwrap_or(&"anonymous".to_string())
            + &(self.counter.to_string())
    }

    pub fn set_defining_context(&mut self, context: String) {
        self.defining_context = Some(context);
    }
}

#[derive(Default)]
pub struct LambdaLifter {
    scope: ScopeMap<String, bool>,
    constructed_functions: Vec<ExprKind>,
    gen_sym: GenSym,
    is_set_context: bool,
}

impl LambdaLifter {
    pub fn lift(ast: Vec<ExprKind>) -> Vec<ExprKind> {
        let lambda_lifting_time = Instant::now();

        let res = LambdaLifter::default().fold(ast);

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(
                target: "pipeline_time",
                "Lambda Lifting time: {:?}",
                lambda_lifting_time.elapsed()
            );
        }

        res
    }
}

#[derive(Debug, PartialEq)]
pub struct CapturedVar {
    depth: usize,
    var: String,
}

fn construct_function(args: Vec<ExprKind>, body: ExprKind) -> ExprKind {
    ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
        args,
        body,
        SyntaxObject::default(TokenType::Lambda),
    )))
}

fn atom(name: String) -> ExprKind {
    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
        name,
    ))))
}

// TODO - move this function into some shared scope
// Transforms a let into a function call
fn let_to_function_call(l: Box<crate::parser::ast::Let>) -> ExprKind {
    let original_args = l.bindings.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
    let mut original_application_args = l.bindings.iter().map(|x| x.1.clone()).collect::<Vec<_>>();

    let function = construct_function(original_args, l.body_expr);

    original_application_args.insert(0, function);

    ExprKind::List(List::new(original_application_args))
}

// TODO -> handle errors better here
fn function_call_to_let(mut l: List) -> Box<crate::parser::ast::Let> {
    let function = l.args.remove(0);
    let function = function.unwrap_function().unwrap();

    let bound_vars = function.args;
    let application_args = l.args;

    let zipped = bound_vars.into_iter().zip(application_args).collect();

    // TOOD -> pass the source location in correctly
    crate::parser::ast::Let::new(zipped, function.body, SyntaxObject::default(TokenType::Let))
        .into()
}

impl Folder for LambdaLifter {
    fn fold(&mut self, ast: Vec<ExprKind>) -> Vec<ExprKind> {
        let mut original_exprs: Vec<ExprKind> = ast
            .into_iter()
            .map(|x| {
                let res = self.visit(x);
                // *self = Self::default();
                self.is_set_context = false;
                self.scope = ScopeMap::default();
                res
            })
            .collect();

        self.constructed_functions.append(&mut original_exprs);
        std::mem::take(&mut self.constructed_functions)

        // Initialize the top level scope to contain all global definitions
    }

    // If we hit a set, we want to mark this as a failure
    fn visit_set(&mut self, s: Box<crate::parser::ast::Set>) -> ExprKind {
        self.is_set_context = true;
        ExprKind::Set(s)
    }

    fn visit_define(&mut self, mut define: Box<Define>) -> ExprKind {
        if let ExprKind::LambdaFunction(_) = &define.body {
            // TODO move this into a method
            self.gen_sym.defining_context = define.name.atom_identifier().map(|x| x.to_string());
        };

        define.body = self.visit(define.body);
        ExprKind::Define(define)
    }

    fn visit_atom(&mut self, a: crate::parser::ast::Atom) -> ExprKind {
        let name = a.ident();

        if let Some(ident) = name {
            // If this contains a key at the top, then it shouldn't be marked as captured by this scope
            if self.scope.contains_key_at_top(ident) {
                // Set it to not be captured if its contained at the top level
                *(self.scope.get_mut(ident).unwrap()) = false;
                return ExprKind::Atom(a);
            }

            // Otherwise, go ahead and mark it as captured if we can find a reference to it
            if let Some(is_captured) = self.scope.get_mut(ident) {
                *is_captured = true;
                return ExprKind::Atom(a);
            }
        }

        // If we get to this case, then its a global and we're not worried about passing in
        // global functions at the moment
        ExprKind::Atom(a)
    }

    // TODO -> consider using scope map
    // traverse up to "mark captured"
    fn visit_let(&mut self, mut l: Box<crate::parser::ast::Let>) -> ExprKind {
        l.bindings = l
            .bindings
            .into_iter()
            .map(|x| (x.0, self.visit(x.1)))
            .collect();

        // Enter new scope with the let
        self.scope.push_layer();

        let let_level_bindings = l
            .bindings
            .iter()
            .map(|x| x.0.atom_identifier().unwrap())
            .collect::<Vec<_>>();

        // Go through and mark these as bound
        for arg in &let_level_bindings {
            self.scope.define(arg.to_string(), false);
        }

        l.body_expr = self.visit(l.body_expr);

        // After visiting - if we're in a set context just bail out
        if self.is_set_context {
            return let_to_function_call(l);
        }

        // Exit the scope
        self.scope.pop_layer();

        // These will correspond
        let mut original_args = l.bindings.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
        let mut original_application_args =
            l.bindings.iter().map(|x| x.1.clone()).collect::<Vec<_>>();

        let captured_atoms = self
            .scope
            .iter()
            .filter(|x| *x.1)
            .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
            .map(|x| x.0.to_string())
            .map(atom)
            .collect::<Vec<_>>();

        for var in &captured_atoms {
            // Append the free args
            original_args.push(var.clone());

            // Append the free application arguments to the application arguments
            original_application_args.push(var.clone())
        }

        let constructed_body = l.body_expr;

        let constructed_function = construct_function(original_args, constructed_body);

        // This is the constructed name that we'll refer to later
        let generated_name = atom(self.gen_sym.generate());

        let definition = Define::new(
            generated_name.clone(),
            constructed_function,
            SyntaxObject::default(TokenType::Define),
        );

        // Push the new function in
        self.constructed_functions.push(definition.into());

        // Call the function, return this one now
        original_application_args.insert(0, generated_name);
        ExprKind::List(List::new(original_application_args))
    }

    // If we're visiting a list, we want to check if this is actually a let expr - the immediate application
    // of a let
    //
    // ((lambda (x) x) 10) -> the same as (let ((x 10)) x)
    // This implementation should do the same thing
    fn visit_list(&mut self, mut l: List) -> ExprKind {
        if l.is_anonymous_function_call() {
            let constructed_let = function_call_to_let(l);
            self.visit_let(constructed_let)
        } else {
            l.args = l.args.into_iter().map(|e| self.visit(e)).collect();
            ExprKind::List(l)
        }
    }

    // Visiting a function, we want to add the current items to the scope
    // anything that is explicitly captured
    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<crate::parser::ast::LambdaFunction>,
    ) -> ExprKind {
        // We're entering a new scope since we've entered a lambda function
        self.scope.push_layer();

        for arg in lambda_function
            .args
            .iter()
            .map(|x| x.atom_identifier().unwrap())
        {
            self.scope.define(arg.to_string(), false);
        }

        lambda_function.body = self.visit(lambda_function.body);

        // We've exited the scope, we're done
        self.scope.pop_layer();

        ExprKind::LambdaFunction(lambda_function)
    }
}
