use std::collections::{HashMap, HashSet};

use quickscope::ScopeMap;

use crate::parser::{
    ast::ExprKind,
    parser::{IdentifierMetadata, SyntaxObject},
    visitors::VisitorMutRef,
};

use super::{VisitorMutRefUnit, VisitorMutUnitRef};

#[derive(Debug, PartialEq)]
pub enum IdentifierStatus {
    Global,
    Local,
    Captured,
    Free,
}

#[derive(Debug)]
pub struct LexicalInformation {
    kind: IdentifierStatus,
    set_bang: bool,
    depth: usize,
    shadows: Option<usize>,
}

impl LexicalInformation {
    pub fn new(kind: IdentifierStatus, depth: usize) -> Self {
        Self {
            kind,
            set_bang: false,
            depth,
            shadows: None,
        }
    }

    pub fn shadows(mut self, id: usize) -> Self {
        self.shadows = Some(id);
        self
    }
}

// Populate the metadata about individual
#[derive(Default, Debug)]
pub struct Analysis {
    info: HashMap<usize, LexicalInformation>,
}

impl Analysis {
    pub fn run(&mut self, exprs: &[ExprKind]) {
        let mut scope = ScopeMap::new();

        for expr in exprs.iter() {
            if let ExprKind::Define(define) = expr {
                let name = define.name.atom_identifier().unwrap();

                define_global(&mut scope, &define);

                let mut lexical_info = LexicalInformation::new(IdentifierStatus::Global, 0);

                // If this variable name is already in scope, we should mark that this variable
                // shadows the previous id
                if let Some(shadowed_var) = scope.get(name) {
                    lexical_info = lexical_info.shadows(shadowed_var.id)
                }

                self.insert(&define.name.atom_syntax_object().unwrap(), lexical_info);
            }
        }

        for expr in exprs {
            let mut pass = AnalysisPass::new(self, &mut scope);
            pass.visit(expr);
        }

        log::info!("Global scope: {:?}", scope.iter_top().collect::<Vec<_>>());
    }

    pub fn insert(&mut self, object: &SyntaxObject, metadata: LexicalInformation) {
        self.info.insert(object.syntax_object_id, metadata);
    }

    pub fn update_with(&mut self, object: &SyntaxObject, metadata: LexicalInformation) {
        let mut existing = self.info.get_mut(&object.syntax_object_id).unwrap();
        existing.kind = metadata.kind;
        existing.set_bang = existing.set_bang || metadata.set_bang;
        existing.shadows = metadata.shadows;
        existing.depth = metadata.depth;
    }

    pub fn get(&self, object: &SyntaxObject) -> Option<&LexicalInformation> {
        self.info.get(&object.syntax_object_id)
    }

    pub fn get_mut(&mut self, id: &usize) -> Option<&mut LexicalInformation> {
        self.info.get_mut(id)
    }
}

#[derive(Debug)]
struct ScopeInfo {
    id: usize,
    captured: bool,
}

struct AnalysisPass<'a> {
    info: &'a mut Analysis,
    scope: &'a mut ScopeMap<String, ScopeInfo>,
}

fn define_global(scope: &mut ScopeMap<String, ScopeInfo>, define: &crate::parser::ast::Define) {
    log::info!("Defining global: {:?}", define.name);

    scope.define(
        define.name.atom_identifier().unwrap().to_string(),
        ScopeInfo {
            id: define.name.atom_syntax_object().unwrap().syntax_object_id,
            captured: false,
        },
    );
}

impl<'a> AnalysisPass<'a> {
    pub fn new(info: &'a mut Analysis, scope: &'a mut ScopeMap<String, ScopeInfo>) -> Self {
        AnalysisPass { info, scope }
    }
}

impl<'a> AnalysisPass<'a> {
    fn get_captured_vars(&self, let_level_bindings: &[&str]) -> HashSet<String> {
        self.scope
            .iter()
            .filter(|x| x.1.captured)
            .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
            .map(|x| x.0.to_string())
            .collect::<HashSet<_>>()
    }
}

impl<'a> VisitorMutUnitRef<'a> for AnalysisPass<'a> {
    fn visit_define(&mut self, define: &'a crate::parser::ast::Define) {
        let name = define.name.atom_identifier().unwrap();

        define_global(&mut self.scope, &define);

        let mut lexical_info =
            LexicalInformation::new(IdentifierStatus::Global, self.scope.depth());

        // If this variable name is already in scope, we should mark that this variable
        // shadows the previous id
        if let Some(shadowed_var) = self.scope.get(name) {
            lexical_info = lexical_info.shadows(shadowed_var.id)
        }

        self.info
            .insert(&define.name.atom_syntax_object().unwrap(), lexical_info);

        self.visit(&define.name);
        self.visit(&define.body);
    }

    fn visit_lambda_function(&mut self, lambda_function: &'a crate::parser::ast::LambdaFunction) {
        // We're entering a new scope since we've entered a lambda function
        self.scope.push_layer();

        let let_level_bindings = lambda_function.arguments().unwrap();

        let depth = self.scope.depth();

        for arg in &lambda_function.args {
            let name = arg.atom_identifier().unwrap();
            let id = arg.atom_syntax_object().unwrap().syntax_object_id;

            self.scope.define(
                name.to_string(),
                ScopeInfo {
                    id,
                    captured: false,
                },
            );

            // Throw in a dummy info so that no matter what, we have something to refer to
            // in the event of a set!
            // Later on in this function this gets updated accordingly
            self.info.insert(
                &arg.atom_syntax_object().unwrap(),
                LexicalInformation::new(IdentifierStatus::Local, depth),
            );
        }

        self.visit(&lambda_function.body);

        let captured_vars = self.get_captured_vars(&let_level_bindings);

        // Pop the layer here - now, we check if any of the arguments below actually already exist
        // in scope. If thats the case, we've shadowed and should mark it accordingly.
        self.scope.pop_layer();

        for var in &lambda_function.args {
            let ident = var.atom_identifier().unwrap();
            let kind = if captured_vars.contains(ident) {
                IdentifierStatus::Captured
            } else {
                IdentifierStatus::Local
            };

            let mut lexical_info = LexicalInformation::new(kind, depth);

            // If this variable name is already in scope, we should mark that this variable
            // shadows the previous id
            if let Some(shadowed_var) = self.scope.get(ident) {
                lexical_info = lexical_info.shadows(shadowed_var.id)
            }

            self.info
                .update_with(&var.atom_syntax_object().unwrap(), lexical_info);
        }
    }

    fn visit_set(&mut self, s: &'a crate::parser::ast::Set) {
        let name = s.variable.atom_identifier();

        if let Some(name) = name {
            // Gather the id of the variable that is in fact mutated
            if let Some(scope_info) = self.scope.get(name) {
                let id = scope_info.id;
                if let Some(var) = self.info.get_mut(&id) {
                    var.set_bang = true;
                } else {
                    println!("Unable to find var: {} in info map to update to set!", name);
                }
            } else {
                println!("Variable not yet in scope: {}", name);
            }
        }
    }

    fn visit_atom(&mut self, a: &'a crate::parser::ast::Atom) {
        let name = a.ident();
        let depth = self.scope.depth();

        if let Some(ident) = name {
            // Check if its a global var - otherwise, we want to check if its a free
            // identifier
            if let Some(depth) = self.scope.height_of(ident) {
                if depth == 0 {
                    self.info.insert(
                        &a.syn,
                        LexicalInformation::new(IdentifierStatus::Global, depth),
                    );
                    return;
                }
            }

            // If this contains a key at the top, then it shouldn't be marked as captured by this scope
            if self.scope.contains_key_at_top(ident) {
                // Set it to not be captured if its contained at the top level
                self.scope.get_mut(ident).unwrap().captured = false;
                self.info.insert(
                    &a.syn,
                    LexicalInformation::new(IdentifierStatus::Local, depth),
                );
                return;
            }

            // Otherwise, go ahead and mark it as captured if we can find a reference to it
            if let Some(is_captured) = self.scope.get_mut(ident) {
                is_captured.captured = true;
                self.info.insert(
                    &a.syn,
                    LexicalInformation::new(IdentifierStatus::Captured, depth),
                );
                return;
            }

            // Otherwise, we've hit a free variable at this point
            self.info.insert(
                &a.syn,
                LexicalInformation::new(IdentifierStatus::Free, depth),
            );
        }
    }
}

// struct PrinterPass {}

impl<'a> VisitorMutUnitRef<'a> for Analysis {
    fn visit_atom(&mut self, a: &'a crate::parser::ast::Atom) {
        log::info!(
            "Id: {:?}, Atom: {:?}, Lexical Information: {:?}",
            a.syn.syntax_object_id,
            a.syn.ty,
            self.get(&a.syn)
        );
    }

    fn visit_lambda_function(&mut self, lambda_function: &'a crate::parser::ast::LambdaFunction) {
        for arg in &lambda_function.args {
            if let Some(arg) = arg.atom_syntax_object() {
                log::info!(
                    "Id: {:?}, Atom in function argument: {:?}, Lexical Information: {:?}",
                    arg.syntax_object_id,
                    arg.ty,
                    self.get(&arg)
                );
            }
        }

        self.visit(&lambda_function.body);
    }
}

pub fn query_top_level_define<'a, A: AsRef<str>>(
    exprs: &'a [ExprKind],
    name: A,
) -> Option<&'a crate::parser::ast::Define> {
    for expr in exprs {
        if let ExprKind::Define(d) = expr {
            match d.name.atom_identifier() {
                Some(n) if name.as_ref() == n => return Some(d),
                _ => {}
            }
        }
    }

    None
}

pub fn find_call_sites_and_call<F>(
    name: &str,
    analysis: &Analysis,
    expressions: &[ExprKind],
    function: F,
) where
    F: FnMut(&crate::parser::ast::List) -> (),
{
    let mut find_call_sites = FindCallSites::new(name, analysis, function);

    for expr in expressions {
        find_call_sites.visit(expr);
    }
}

pub fn find_call_sites_and_modify_with<F>(
    name: &str,
    analysis: &Analysis,
    expressions: &mut [ExprKind],
    function: F,
) where
    F: FnMut(&mut crate::parser::ast::List) -> (),
{
    let mut find_call_sites = FindCallSites::new(name, analysis, function);

    for expr in expressions {
        find_call_sites.visit(expr);
    }
}

struct FindCallSites<'a, F> {
    name: &'a str,
    analysis: &'a Analysis,
    func: F,
}

impl<'a, F> FindCallSites<'a, F> {
    pub fn new(name: &'a str, analysis: &'a Analysis, func: F) -> Self {
        Self {
            name,
            analysis,
            func,
        }
    }
}

impl<'a, F> VisitorMutUnitRef<'a> for FindCallSites<'a, F>
where
    F: FnMut(&crate::parser::ast::List) -> (),
{
    fn visit_list(&mut self, l: &crate::parser::ast::List) {
        if let Some(name) = l.first_ident() {
            if let Some(lexical_info) = self.analysis.get(&l.args[0].atom_syntax_object().unwrap())
            {
                if name == self.name && lexical_info.kind == IdentifierStatus::Global {
                    (self.func)(l)
                }
            }
        }
    }
}

impl<'a, F> VisitorMutRefUnit for FindCallSites<'a, F>
where
    F: FnMut(&mut crate::parser::ast::List) -> (),
{
    fn visit_list(&mut self, l: &mut crate::parser::ast::List) {
        if let Some(name) = l.first_ident() {
            if let Some(lexical_info) = self.analysis.get(&l.args[0].atom_syntax_object().unwrap())
            {
                if name == self.name && lexical_info.kind == IdentifierStatus::Global {
                    (self.func)(l)
                }
            }
        }
    }
}

#[cfg(test)]
mod analysis_pass_tests {
    use env_logger::Builder;
    use log::LevelFilter;

    use crate::parser::parser::Parser;

    use super::*;

    #[test]
    fn check_analysis_pass() {
        let mut builder = Builder::new();

        builder
            .is_test(true)
            .filter(
                Some("steel::compiler::passes::analysis"),
                LevelFilter::Trace,
            )
            .init();

        let script = r#"

        (define + (require-builtins steel/math))

        (define (foo x y z)
            (let ((x x) (y y))
                (lambda (extra-arg x) 
                    (set! x 100)
                    (set! foo "hello world")
                    (+ x y z))))

        (define (test x y z)
            (let ((foo foo))
                (foo x y z)
                (foo 1 2 3)
                (foo 10 20 30)))
        
        (foo "applesauce" "bananas" "sauce")
        "#;

        let mut analysis = Analysis::default();
        let mut exprs = Parser::parse(script).unwrap();

        analysis.run(&exprs);

        for expr in &exprs {
            analysis.visit(expr);
        }

        // find_call_sites_and_modify_with("foo", &analysis, &mut exprs, |l| {
        //     log::info!("Found a call site: {:?}", l.to_string())
        // });
    }
}
