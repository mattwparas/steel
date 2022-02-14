use crate::parser::{
    ast::{Atom, Define, ExprKind, LambdaFunction, List},
    parser::SyntaxObject,
    tokens::TokenType,
};

use super::Folder;

use itertools::Itertools;
use quickscope::{ScopeMap, ScopeSet};
use std::collections::HashSet;

#[derive(Default)]
pub struct GenSym {
    counter: usize,
    defining_context: Option<String>,
}

impl GenSym {
    // Crappy way to get the new name for a lambda lifted function
    pub fn generate(&mut self) -> String {
        self.counter += 1;
        "##--##lambda-lifter-".to_string()
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
    captured: ScopedVars,
    constructed_functions: Vec<ExprKind>,
    gen_sym: GenSym,
    is_set_context: bool,
}

impl LambdaLifter {
    pub fn lift(ast: Vec<ExprKind>) -> Vec<ExprKind> {
        LambdaLifter::default().fold(ast)
    }
}

#[derive(Debug, PartialEq)]
pub struct CapturedVar {
    depth: usize,
    var: String,
}

#[derive(Default, Debug)]
pub struct ScopedVars {
    vars: Vec<CapturedVar>,
}

impl ScopedVars {
    pub fn remove(&mut self, var: &str, depth: usize) {
        // println!("Trying to remove: {:?} at depth: {:?}", var, depth);
        // println!("Scoped vars state now: {:?}", self);

        if let Some(index) = self
            .vars
            .iter()
            .rposition(|x| x.var == var && x.depth >= depth)
        {
            self.vars.remove(index);
        }
    }

    pub fn remove_if_out_of_scope(&mut self, depth: usize) {
        self.vars.retain(|x| x.depth < depth);
    }

    // TODO -> if the variable is already captured, don't mark it added again
    pub fn add(&mut self, var: String, depth: usize) {
        // if let Some(index) = self.vars.iter().rposition(|x| )

        let captured_var = CapturedVar { depth, var };

        if !self.vars.contains(&captured_var) {
            self.vars.push(captured_var);
        }
    }
}

/*

(define (test x) (test-let ((y 10)) (test-let ((z 20)) (+ x y z))))

(define (test x) (##testlet1 10 x))

(define (##testlet1 x y) )

*/

// LambdaFunction::new(
//     bound_names,
//     ExprKind::Begin(Begin::new(
//         set_expressions,
//         SyntaxObject::default(TokenType::Begin),
//     )),
//     SyntaxObject::default(TokenType::Lambda),
// );

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

    return ExprKind::List(List::new(original_application_args));
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
                self.captured = ScopedVars::default();
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
        match &define.body {
            ExprKind::LambdaFunction(_) => {
                // TODO move this into a method
                self.gen_sym.defining_context =
                    define.name.atom_identifier().map(|x| x.to_string());
            }
            _ => {}
        }

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

            // if self.scope.contains(ident) {
            //     let value = self.scope.get_mut(ident);;

            // }
        }

        // If we get to this case, then its a global and we're not worried about passing in
        // global functions at the moment
        ExprKind::Atom(a)

        // todo!()
    }

    // TODO -> consider using scope map
    // traverse up to "mark captured"
    fn visit_let(&mut self, mut l: Box<crate::parser::ast::Let>) -> ExprKind {
        // TODO -> checking if its a set context here isn't the right spot
        // Need to do it so that set contexts don't make any transforms at all

        // if self.is_set_context {
        //     return let_to_function_call(l);
        // }

        // self.captured.push_layer();

        // Visit the arguments of the functions as well...
        // TODO -> figure out how to get bindings visited properly

        {
            println!("----------------------------------");

            let prior_captured_vars = self
                .scope
                .iter()
                .filter(|x| *x.1)
                .map(|x| x.0)
                .collect::<Vec<_>>();

            println!("Captured vars up to this point: {:?}", prior_captured_vars);
        }

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

        // Go through and mark these as
        for arg in &let_level_bindings {
            self.scope.define(arg.to_string(), false);
        }

        println!(
            "let level bindings before visiting, scope: {:?}",
            self.scope.iter().collect::<Vec<_>>()
        );

        l.body_expr = self.visit(l.body_expr);

        println!(
            "let level bindings after visiting, scope: {:?}",
            self.scope.iter().collect::<Vec<_>>()
        );

        // After visiting - if we're in a set context just bail out
        if self.is_set_context {
            return let_to_function_call(l);
        }

        // for binding in &let_level_bindings {
        //     self.captured.remove(*binding, self.scope.depth());
        // }

        let captured_vars = self
            .scope
            .iter()
            .filter(|x| *x.1)
            .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
            .map(|x| x.0.to_string())
            .collect::<Vec<_>>();

        // Exit the scope
        self.scope.pop_layer();

        // let captured_vars = self
        //     .captured
        //     .vars
        //     .iter()
        //     .map(|x| x.clone())
        //     .collect::<Vec<_>>();

        // let mut seen = HashSet::new();
        // let mut deduped_captured_vars = Vec::new();

        // for arg in captured_vars {
        //     if seen.insert(&arg.var) {
        //         deduped_captured_vars.push(arg);
        //     }
        // }

        // These will correspond
        let mut original_args = l.bindings.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
        let mut original_application_args =
            l.bindings.iter().map(|x| x.1.clone()).collect::<Vec<_>>();

        println!("Captured vars: {captured_vars:?}");
        println!("Original args: {original_args:?}");

        let captured_atoms = captured_vars
            .into_iter()
            .map(|x| atom(x.to_string()))
            .collect::<Vec<_>>();

        // let captured_atoms = deduped_captured_vars
        //     .into_iter()
        //     .map(|x| atom(x.var.to_string()))
        //     .collect::<Vec<_>>();

        // println!("Captured atoms: {:?}", captured_atoms);

        for var in &captured_atoms {
            // Append the free args
            original_args.push(var.clone());

            // Append the free application arguments to the application arguments
            original_application_args.push(var.clone())
        }

        // let all_atoms: Vec<_> = original_args
        //     .iter()
        //     .filter_map(|x| x.atom_identifier().map(|x| x.to_string()))
        //     .collect();

        // if all_atoms.len() < original_ar

        // println!("Before deduping: {:?}", original_args);

        // let mut seen = HashSet::new();

        // let mut deduped_args = Vec::new();

        // TODO <- check out this dedup
        // let original_args = original_args
        //     .into_iter()
        //     .duplicates_by(|x| x.atom_identifier().map(|x| x.to_string()))
        //     .collect();

        // for arg in original_args {
        //     if seen.insert(arg.atom_identifier().map(|x| x.to_string())) {
        //         deduped_args.push(arg);
        //     }
        // }

        // println!("After deduping: {:?}", deduped_args);

        let constructed_body = l.body_expr;

        let constructed_function = construct_function(original_args, constructed_body);

        // This is the constructed name that we'll refer to later
        let generated_name = atom(self.gen_sym.generate());

        let definition = Define::new(
            generated_name.clone(),
            constructed_function,
            SyntaxObject::default(TokenType::Define),
        );

        println!("Constructed function: {}", definition);

        // Push the new function in
        self.constructed_functions.push(definition.into());

        // Call the function, return this one now
        original_application_args.insert(0, generated_name);
        let constructed_function_application = ExprKind::List(List::new(original_application_args));

        // println!("{}", constructed_function_application);

        // println!("Let level bindings: {:?}", let_level_bindings);

        // If something in scope was captured below, we need to mark it as captured here
        // for captured_var in captured_vars {
        //     // if let_level_bindings.contains(&)

        //     // TODO -> time complexing of searching let level bindings is suspect
        //     if !let_level_bindings.contains(&captured_var.as_str()) {
        //         self.captured.define(captured_var);
        //     }
        // }

        // We've exited the scope, we're done
        // self.scope.pop_layer();

        // Drop out of scope stuff
        // self.captured.remove_if_out_of_scope(self.scope.depth());

        // ExprKind::Let(l)

        constructed_function_application

        // (define (test x) (test-let ((y 10)) (test-let ((z 20)) (+ x y z))))

        // todo!()
    }

    // If we're visiting a list, we want to check if this is actually a let expr - the immediate application
    // of a let
    //
    // ((lambda (x) x) 10) -> the same as (let ((x 10)) x)
    // This implementation should do the same thing
    fn visit_list(&mut self, mut l: List) -> ExprKind {
        if l.is_anonymous_function_call() {
            println!(
                "trying to transform anonymous function call into let: {}",
                l
            );

            // println!("Function call before: {}", l);

            let mut constructed_let = function_call_to_let(l);

            // Visit the argument expressions
            // constructed_let.bindings = constructed_let
            //     .bindings
            //     .into_iter()
            //     .map(|x| (x.0, self.visit(x.1)))
            //     .collect();

            // println!("Let after: {}", constructed_let);

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
        // let args = ;

        // Each layer is going to be solely owned by variables in this function
        // self.captured.push_layer();

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

        // Once we're done, pop off the scope here
        for arg in lambda_function
            .args
            .iter()
            .map(|x| x.atom_identifier().unwrap())
        {
            self.captured.remove(arg, self.scope.depth());
        }

        // We've exited the scope, we're done
        self.scope.pop_layer();

        // let captured_vars = self.captured.pop_layer().unwrap();

        // self.captured.pop_layer();

        ExprKind::LambdaFunction(lambda_function)

        // todo!()
    }
}

#[test]
fn check_contains_at_top() {
    let mut scope = ScopeMap::new();

    scope.define("a", false);

    scope.push_layer();

    scope.define("b", false);

    scope.push_layer();

    scope.define("b", true);

    println!("{:?}", scope.iter().collect::<Vec<_>>());

    // println!("{:?}", scope.contains_at_top("a"));
}
