use std::collections::{HashMap, HashSet};

use quickscope::ScopeMap;

use crate::parser::{
    ast::{ExprKind, LambdaFunction, List},
    parser::{IdentifierMetadata, SyntaxObject},
    span::Span,
    visitors::VisitorMutRef,
};

use super::{VisitorMutRefUnit, VisitorMutUnitRef};

#[derive(Debug, PartialEq)]
pub enum IdentifierStatus {
    Global,
    Local,
    LocallyDefinedFunction,
    LetVar,
    Captured,
    Free,
}

#[derive(Debug)]
pub struct SemanticInformation {
    kind: IdentifierStatus,
    set_bang: bool,
    depth: usize,
    shadows: Option<usize>,
    usage_count: usize,
    span: Span,
    refers_to: Option<usize>,
    aliases_to: Option<usize>,
    builtin: bool,
    last_usage: bool,
}

impl SemanticInformation {
    pub fn new(kind: IdentifierStatus, depth: usize, span: Span) -> Self {
        Self {
            kind,
            set_bang: false,
            depth,
            shadows: None,
            usage_count: 0,
            span,
            refers_to: None,
            aliases_to: None,
            builtin: false,
            last_usage: false,
        }
    }

    pub fn shadows(mut self, id: usize) -> Self {
        self.shadows = Some(id);
        self
    }

    pub fn with_usage_count(mut self, count: usize) -> Self {
        self.usage_count = count;
        self
    }

    pub fn refers_to(mut self, id: usize) -> Self {
        self.refers_to = Some(id);
        self
    }

    pub fn aliases_to(mut self, id: usize) -> Self {
        self.aliases_to = Some(id);
        self
    }

    pub fn mark_builtin(&mut self) {
        self.builtin = true;
    }
}

#[derive(Debug)]
pub struct FunctionInformation {
    captured_vars: HashMap<String, ScopeInfo>,
}

impl FunctionInformation {
    pub fn new(captured_vars: HashMap<String, ScopeInfo>) -> Self {
        Self { captured_vars }
    }
}

// Populate the metadata about individual
#[derive(Default, Debug)]
pub struct Analysis {
    info: HashMap<usize, SemanticInformation>,
    function_info: HashMap<usize, FunctionInformation>,
}

impl Analysis {
    pub fn from_exprs(exprs: &[ExprKind]) -> Self {
        let mut analysis = Analysis::default();
        analysis.run(&exprs);
        analysis
    }

    pub fn resolve_alias(&self, mut id: usize) -> Option<usize> {
        while let Some(next) = self
            .info
            .get(&id)
            .and_then(|x| x.aliases_to)
            .and_then(|x| self.info.get(&x))
            .and_then(|x| x.refers_to)
        {
            id = next;
        }

        return Some(id);
    }

    pub fn visit_top_level_define_function_without_body(
        &mut self,
        scope: &mut ScopeMap<String, ScopeInfo>,
        define: &crate::parser::ast::Define,
    ) {
        let name = define.name.atom_identifier().unwrap();

        let mut semantic_info = SemanticInformation::new(
            IdentifierStatus::Global,
            1,
            define.name.atom_syntax_object().unwrap().span,
        );

        if define.is_a_builtin_definition() {
            semantic_info.mark_builtin();
        }

        // If this variable name is already in scope, we should mark that this variable
        // shadows the previous id
        if let Some(shadowed_var) = scope.get(name) {
            semantic_info = semantic_info.shadows(shadowed_var.id)
        }

        log::info!("Defining global: {:?}", define.name);
        define_var(scope, &define);

        self.insert(&define.name.atom_syntax_object().unwrap(), semantic_info);
    }

    pub fn run(&mut self, exprs: &[ExprKind]) {
        let mut scope: ScopeMap<String, ScopeInfo> = ScopeMap::new();

        // TODO: Functions should be globally resolvable but top level identifiers cannot be used before they are defined
        // The way this is implemented right now doesn't respect that
        for expr in exprs.iter() {
            if let ExprKind::Define(define) = expr {
                if define.body.lambda_function().is_some() {
                    self.visit_top_level_define_function_without_body(&mut scope, define);
                }
            }
        }

        for expr in exprs {
            let mut pass = AnalysisPass::new(self, &mut scope);
            // pass.visit(expr);

            if let ExprKind::Define(define) = expr {
                if define.body.lambda_function().is_some() {
                    // Continue with the rest of the body here
                    pass.visit(&define.body);
                } else {
                    pass.visit_top_level_define_value_without_body(define);
                    pass.visit(&define.body);
                }
            } else {
                pass.visit(expr);
            }
        }

        log::info!("Global scope: {:?}", scope.iter_top().collect::<Vec<_>>());
    }

    pub fn get_function_info(&self, function: &LambdaFunction) -> Option<&FunctionInformation> {
        self.function_info.get(&function.syntax_object_id)
    }

    pub fn insert(&mut self, object: &SyntaxObject, metadata: SemanticInformation) {
        self.info.insert(object.syntax_object_id, metadata);
    }

    pub fn update_with(&mut self, object: &SyntaxObject, metadata: SemanticInformation) {
        let mut existing = self.info.get_mut(&object.syntax_object_id).unwrap();
        existing.kind = metadata.kind;
        existing.set_bang = existing.set_bang || metadata.set_bang;
        existing.shadows = metadata.shadows;
        existing.depth = metadata.depth;
        existing.usage_count = metadata.usage_count;
        existing.aliases_to = metadata.aliases_to;
        existing.refers_to = metadata.refers_to;
        existing.builtin = metadata.builtin;
    }

    pub fn get(&self, object: &SyntaxObject) -> Option<&SemanticInformation> {
        self.info.get(&object.syntax_object_id)
    }

    pub fn get_mut(&mut self, id: &usize) -> Option<&mut SemanticInformation> {
        self.info.get_mut(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeInfo {
    id: usize,
    captured: bool,
    usage_count: usize,
    /// Last touched by this ID
    last_used: Option<usize>,
}

impl ScopeInfo {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            captured: false,
            usage_count: 0,
            last_used: None,
        }
    }
}

struct AnalysisPass<'a> {
    info: &'a mut Analysis,
    scope: &'a mut ScopeMap<String, ScopeInfo>,
}

fn define_var(scope: &mut ScopeMap<String, ScopeInfo>, define: &crate::parser::ast::Define) {
    scope.define(
        define.name.atom_identifier().unwrap().to_string(),
        ScopeInfo::new(define.name.atom_syntax_object().unwrap().syntax_object_id),
    );
}

impl<'a> AnalysisPass<'a> {
    pub fn new(info: &'a mut Analysis, scope: &'a mut ScopeMap<String, ScopeInfo>) -> Self {
        AnalysisPass { info, scope }
    }
}

impl<'a> AnalysisPass<'a> {
    fn get_captured_vars(&self, let_level_bindings: &[&str]) -> HashMap<String, ScopeInfo> {
        self.scope
            .iter()
            .filter(|x| x.1.captured)
            .filter(|x| !let_level_bindings.contains(&x.0.as_str()))
            .map(|x| (x.0.clone(), x.1.clone()))
            .collect()
    }

    fn visit_top_level_define_value_without_body(&mut self, define: &crate::parser::ast::Define) {
        let name = define.name.atom_identifier().unwrap();

        let name_syntax_object = define.name.atom_syntax_object().unwrap();

        let mut semantic_info =
            SemanticInformation::new(IdentifierStatus::Global, 1, name_syntax_object.span);

        // If this variable name is already in scope, we should mark that this variable
        // shadows the previous id
        if let Some(shadowed_var) = self.scope.get(name) {
            semantic_info = semantic_info.shadows(shadowed_var.id)
        }

        if define.is_a_builtin_definition() {
            semantic_info.mark_builtin();
        }

        if let Some(aliases) = define.is_an_alias_definition() {
            log::info!(
                "Found definition that aliases - {} aliases {}: {} -> {}",
                define.name,
                define.body,
                name_syntax_object.syntax_object_id,
                define.body.atom_syntax_object().unwrap().syntax_object_id,
            );
            semantic_info = semantic_info.aliases_to(aliases);
        }

        log::info!("Defining global: {:?}", define.name);
        define_var(self.scope, &define);

        self.info.insert(&name_syntax_object, semantic_info);
    }

    // TODO: I really hate this identifier status if local nonsense
    fn visit_define_without_body(
        &mut self,
        define: &crate::parser::ast::Define,
        identifier_status_if_local: IdentifierStatus,
    ) {
        let name = define.name.atom_identifier().unwrap();

        let mut semantic_info = SemanticInformation::new(
            if self.scope.depth() == 1 {
                IdentifierStatus::Global
            } else {
                identifier_status_if_local
            },
            self.scope.depth(),
            define.name.atom_syntax_object().unwrap().span,
        );

        if define.is_a_builtin_definition() {
            semantic_info.mark_builtin();
        }

        // If this variable name is already in scope, we should mark that this variable
        // shadows the previous id
        if let Some(shadowed_var) = self.scope.get(name) {
            log::warn!("Redefining previous variable: {:?}", name);
            semantic_info = semantic_info.shadows(shadowed_var.id)
        }

        define_var(&mut self.scope, &define);

        self.info
            .insert(&define.name.atom_syntax_object().unwrap(), semantic_info);
    }

    // Visit the function arguments, marking these as defining in our scope
    // and also defaulting them to be local identifiers. This way, in the event of a set!
    // we have something to refer to
    fn visit_func_args(&mut self, lambda_function: &LambdaFunction, depth: usize) {
        for arg in &lambda_function.args {
            let name = arg.atom_identifier().unwrap();
            let id = arg.atom_syntax_object().unwrap().syntax_object_id;

            self.scope.define(name.to_string(), ScopeInfo::new(id));

            // Throw in a dummy info so that no matter what, we have something to refer to
            // in the event of a set!
            // Later on in this function this gets updated accordingly
            self.info.insert(
                &arg.atom_syntax_object().unwrap(),
                SemanticInformation::new(
                    IdentifierStatus::Local,
                    depth,
                    arg.atom_syntax_object().unwrap().span,
                ),
            );
        }
    }

    fn pop_top_layer(&mut self) -> HashMap<String, ScopeInfo> {
        let arguments = self
            .scope
            .iter_top()
            // .cloned()
            .map(|x| (x.0.clone(), x.1.clone()))
            .collect::<HashMap<_, _>>();

        self.scope.pop_layer();

        arguments
    }

    fn find_and_mark_captured_arguments(
        &mut self,
        lambda_function: &LambdaFunction,
        captured_vars: &HashMap<String, ScopeInfo>,
        depth: usize,
        arguments: HashMap<String, ScopeInfo>,
    ) {
        for var in &lambda_function.args {
            let ident = var.atom_identifier().unwrap();
            let kind = if captured_vars.contains_key(ident) {
                IdentifierStatus::Captured
            } else {
                IdentifierStatus::Local
            };

            let mut semantic_info =
                SemanticInformation::new(kind, depth, var.atom_syntax_object().unwrap().span);

            // Update the usage count to collect how many times the variable was referenced
            // Inside of the scope in which the variable existed
            let count = arguments.get(ident).unwrap().usage_count;

            if count == 0 {
                // TODO: Emit warning with the span
                log::warn!("Found unused argument: {:?}", ident);
            }

            semantic_info = semantic_info.with_usage_count(count);

            // If this variable name is already in scope, we should mark that this variable
            // shadows the previous id
            if let Some(shadowed_var) = self.scope.get(ident) {
                semantic_info = semantic_info.shadows(shadowed_var.id)
            }

            self.info
                .update_with(&var.atom_syntax_object().unwrap(), semantic_info);
        }
    }
}

impl<'a> VisitorMutUnitRef<'a> for AnalysisPass<'a> {
    fn visit_define(&mut self, define: &'a crate::parser::ast::Define) {
        self.visit_define_without_body(&define, IdentifierStatus::Local);

        // self.visit(&define.name);
        self.visit(&define.body);
    }

    fn visit_begin(&mut self, begin: &'a crate::parser::ast::Begin) {
        // Collect all of the defines inside of the body first
        for expr in &begin.exprs {
            if let ExprKind::Define(define) = expr {
                if define.body.lambda_function().is_some() {
                    self.visit_define_without_body(
                        define,
                        IdentifierStatus::LocallyDefinedFunction,
                    );
                }
            }
        }

        // After that, we can continue with everything but those
        for expr in &begin.exprs {
            if let ExprKind::Define(define) = expr {
                if define.body.lambda_function().is_some() {
                    // Continue with the rest of the body here
                    self.visit(&define.body);
                } else {
                    self.visit(expr);
                }
            } else {
                self.visit(expr);
            }
        }
    }

    fn visit_lambda_function(&mut self, lambda_function: &'a crate::parser::ast::LambdaFunction) {
        // We're entering a new scope since we've entered a lambda function
        self.scope.push_layer();

        let let_level_bindings = lambda_function.arguments().unwrap();
        let depth = self.scope.depth();

        self.visit_func_args(lambda_function, depth);
        self.visit(&lambda_function.body);

        // TODO: combine the captured_vars and arguments into one thing
        // We don't need to generate a hashset and a hashmap - we can just do it once
        let captured_vars = self.get_captured_vars(&let_level_bindings);

        log::info!("Captured variables: {:?}", captured_vars);

        // Get the arguments to get the counts
        // Pop the layer here - now, we check if any of the arguments below actually already exist
        // in scope. If thats the case, we've shadowed and should mark it accordingly.
        let arguments = self.pop_top_layer();

        // Mark the last usage of the variable after the values go out of scope
        for id in arguments.values().filter_map(|x| x.last_used) {
            self.info.get_mut(&id).unwrap().last_usage = true;
        }

        // Using the arguments, mark the vars that have been captured
        self.find_and_mark_captured_arguments(lambda_function, &captured_vars, depth, arguments);

        // Capture the information and store it in the semantic analysis for this individual function
        self.info.function_info.insert(
            lambda_function.syntax_object_id,
            FunctionInformation::new(captured_vars),
        );
    }

    fn visit_set(&mut self, s: &'a crate::parser::ast::Set) {
        let name = s.variable.atom_identifier();

        if let Some(name) = name {
            // Gather the id of the variable that is in fact mutated
            if let Some(scope_info) = self.scope.get_mut(name) {
                // Bump the usage count
                // TODO Also mark this as mutated
                scope_info.usage_count += 1;

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
        // Snag the current id of this node - we're gonna want this for later
        let current_id = a.syn.syntax_object_id;

        if let Some(ident) = name {
            // Check if its a global var - otherwise, we want to check if its a free
            // identifier
            if let Some(depth) = self.scope.height_of(ident) {
                if depth == 0 {
                    // Mark the parent as used
                    let global_var = self.scope.get_mut(ident).unwrap();
                    global_var.usage_count += 1;

                    self.info.get_mut(&global_var.id).unwrap().usage_count += 1;

                    let semantic_information =
                        SemanticInformation::new(IdentifierStatus::Global, depth, a.syn.span)
                            .with_usage_count(1)
                            .refers_to(global_var.id);

                    self.info.insert(&a.syn, semantic_information);

                    return;
                }
            }

            // If this contains a key at the top, then it shouldn't be marked as captured by this scope
            if self.scope.contains_key_at_top(ident) {
                // Set it to not be captured if its contained at the top level
                // self.scope.get_mut(ident).unwrap().captured = false;

                let mut_ref = self.scope.get_mut(ident).unwrap();

                mut_ref.captured = false;
                mut_ref.usage_count += 1;

                // Mark this as last touched by this identifier
                mut_ref.last_used = Some(current_id);

                // In the event there is a local define, we want to count the usage here
                if let Some(local_define) = self.info.get_mut(&mut_ref.id) {
                    local_define.usage_count = mut_ref.usage_count;
                }

                let semantic_info =
                    SemanticInformation::new(IdentifierStatus::Local, depth, a.syn.span)
                        .with_usage_count(1)
                        .refers_to(mut_ref.id);

                // println!("Variable {} refers to {}", ident, mut_ref.id);

                self.info.insert(&a.syn, semantic_info);

                return;
            }

            // Otherwise, go ahead and mark it as captured if we can find a reference to it
            if let Some(is_captured) = self.scope.get_mut(ident) {
                is_captured.captured = true;
                is_captured.usage_count += 1;

                // TODO: Make sure we want to mark this identifier as last used
                is_captured.last_used = Some(current_id);

                let mut identifier_status = IdentifierStatus::Captured;

                if let Some(local_define) = self.info.get_mut(&is_captured.id) {
                    local_define.usage_count = is_captured.usage_count;

                    // If this _is_ in fact a locally defined function, we don't want to capture it
                    // This is something that is going to get lifted to the top environment anyway
                    if local_define.kind == IdentifierStatus::LocallyDefinedFunction {
                        is_captured.captured = false;
                        identifier_status = IdentifierStatus::LocallyDefinedFunction;
                    }
                }

                let semantic_info = SemanticInformation::new(identifier_status, depth, a.syn.span)
                    .with_usage_count(1)
                    .refers_to(is_captured.id);

                // println!("Variable {} refers to {}", ident, is_captured.id);

                self.info.insert(&a.syn, semantic_info);

                return;
            }

            // Otherwise, we've hit a free variable at this point
            self.info.insert(
                &a.syn,
                SemanticInformation::new(IdentifierStatus::Free, depth, a.syn.span),
            );

            log::warn!("Found free var: {}", a);
        }
    }
}

impl<'a> VisitorMutUnitRef<'a> for Analysis {
    fn visit_atom(&mut self, a: &'a crate::parser::ast::Atom) {
        log::info!(
            "Id: {:?}, Atom: {:?}, Semantic Information: {:?}",
            a.syn.syntax_object_id,
            a.syn.ty,
            self.get(&a.syn)
        );
    }

    fn visit_lambda_function(&mut self, lambda_function: &'a crate::parser::ast::LambdaFunction) {
        for arg in &lambda_function.args {
            if let Some(arg) = arg.atom_syntax_object() {
                log::info!(
                    "Id: {:?}, Atom in function argument: {:?}, Semantic Information: {:?}",
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
    let mut found_defines = Vec::new();
    for expr in exprs {
        if let ExprKind::Define(d) = expr {
            match d.name.atom_identifier() {
                Some(n) if name.as_ref() == n => found_defines.push(d.as_ref()),
                _ => {}
            }
        }
    }

    if found_defines.len() > 1 {
        log::info!(
            "Multiple defines found, unable to find one unique value to associate with a name"
        );
        return None;
    }

    if found_defines.len() == 1 {
        return found_defines.into_iter().next();
    }

    None
}

struct FindCallSiteById<'a, F> {
    id: usize,
    analysis: &'a Analysis,
    func: F,
    modified: bool,
}

impl<'a, F> FindCallSiteById<'a, F> {
    pub fn new(id: usize, analysis: &'a Analysis, func: F) -> Self {
        Self {
            id,
            analysis,
            func,
            modified: false,
        }
    }

    // TODO: clean this up a bit
    pub fn is_required_call_site(&self, l: &List) -> bool {
        if let Some(refers_to) = l
            .args
            .first()
            .and_then(|x| x.atom_syntax_object())
            .and_then(|x| self.analysis.get(&x))
            .and_then(|x| x.refers_to)
        {
            return refers_to == self.id;
        }

        false
    }
}

impl<'a, F> VisitorMutRefUnit for FindCallSiteById<'a, F>
where
    F: FnMut(&Analysis, &mut crate::parser::ast::List) -> bool,
{
    fn visit_list(&mut self, l: &mut List) {
        // Go downward and visit each of the arguments (including the function call)
        for arg in &mut l.args {
            self.visit(arg);
        }

        // If we're a match, call the function
        if self.is_required_call_site(l) {
            self.modified |= (self.func)(&self.analysis, l)
        }
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
impl<'a, F> FindCallSites<'a, F> {
    fn is_required_global_function_call(&self, l: &List) -> bool {
        if let Some(name) = l.first_ident() {
            if let Some(semantic_info) = self.analysis.get(&l.args[0].atom_syntax_object().unwrap())
            {
                return name == self.name && semantic_info.kind == IdentifierStatus::Global;
            }
        }

        false
    }
}

impl<'a, F> VisitorMutUnitRef<'a> for FindCallSites<'a, F>
where
    F: FnMut(&Analysis, &crate::parser::ast::List) -> (),
{
    fn visit_list(&mut self, l: &'a crate::parser::ast::List) {
        if self.is_required_global_function_call(l) {
            (self.func)(&self.analysis, l)
        }

        for arg in &l.args {
            self.visit(arg);
        }
    }
}

impl<'a, F> VisitorMutRefUnit for FindCallSites<'a, F>
where
    F: FnMut(&Analysis, &mut crate::parser::ast::List) -> (),
{
    fn visit_list(&mut self, l: &mut crate::parser::ast::List) {
        if self.is_required_global_function_call(l) {
            (self.func)(&self.analysis, l)
        }

        for arg in &mut l.args {
            self.visit(arg);
        }
    }
}

struct MutateCallSites<'a, F> {
    name: &'a str,
    analysis: &'a Analysis,
    func: F,
}

impl<'a, F> MutateCallSites<'a, F> {
    pub fn new(name: &'a str, analysis: &'a Analysis, func: F) -> Self {
        Self {
            name,
            analysis,
            func,
        }
    }
}

impl<'a, F> VisitorMutRefUnit for MutateCallSites<'a, F>
where
    F: FnMut(&Analysis, &mut ExprKind) -> (),
{
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Struct(s) => self.visit_struct(s),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            list @ ExprKind::List(_) => {
                if let ExprKind::List(l) = &list {
                    if let Some(name) = l.first_ident() {
                        if let Some(semantic_info) =
                            self.analysis.get(&l.args[0].atom_syntax_object().unwrap())
                        {
                            if name == self.name && semantic_info.kind == IdentifierStatus::Global {
                                // At this point, call out to the user given function - if we do in fact mutate
                                // where the value points to, we should return a full node that needs to be visited
                                (self.func)(&self.analysis, list);

                                // TODO: Analysis should maybe be re run here - mutations might invalidate the analysis
                                // This might make it worth rerunning the analysis

                                return self.visit(list);
                            }
                        }
                    }
                }

                if let ExprKind::List(l) = list {
                    return self.visit_list(l);
                }

                unreachable!()
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::CallCC(cc) => self.visit_callcc(cc),
            ExprKind::Let(l) => self.visit_let(l),
        }
    }
}

// TODO: This will need to get changed in the event we actually modify _what_ the mutable pointer points to
// Right now, if we want to modify a call site, we can only change it to a call site - what we _should_ do is have it be able to point
// back to any arbitrary value, and subsequently change l to point to that value by doing another level of the recursion
// Something like:
// where F: FnMut(&mut ExprKind) -> ()
// To do this, the main visit loop would need to be goofed with in the visitor, and we pass in the reference to the wrapped object, rather than the underlying one
struct AnonymousFunctionCallSites<'a, F> {
    analysis: &'a Analysis,
    func: F,
}

impl<'a, F> AnonymousFunctionCallSites<'a, F> {
    pub fn new(analysis: &'a Analysis, func: F) -> Self {
        Self { analysis, func }
    }
}

impl<'a, F> VisitorMutRefUnit for AnonymousFunctionCallSites<'a, F>
where
    F: FnMut(&Analysis, &mut ExprKind) -> bool,
{
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Struct(s) => self.visit_struct(s),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            list @ ExprKind::List(_) => {
                // Bottom up approach - visit everything first, then, on the way back up,
                // modify the value
                if let ExprKind::List(l) = list {
                    self.visit_list(l);
                }

                if let ExprKind::List(l) = &list {
                    if l.is_anonymous_function_call() {
                        // TODO: rerunning analysis might be worth it here - we want to be able to trigger a re run if a mutation would cause a change
                        // In the state of the analysis
                        if (self.func)(&self.analysis, list) {
                            // return self.visit(list);
                            log::info!("Modified anonymous function call site!");
                        }
                    }
                }
            }
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::CallCC(cc) => self.visit_callcc(cc),
            ExprKind::Let(l) => self.visit_let(l),
        }
    }
}

struct UnusedArguments<'a> {
    analysis: &'a Analysis,
    unused_args: Vec<Span>,
}

impl<'a> UnusedArguments<'a> {
    pub fn new(analysis: &'a Analysis) -> Self {
        Self {
            analysis,
            unused_args: Vec::new(),
        }
    }
}

impl<'a> VisitorMutUnitRef<'a> for UnusedArguments<'a> {
    fn visit_lambda_function(&mut self, lambda_function: &'a LambdaFunction) {
        for arg in &lambda_function.args {
            if let Some(syntax_object) = arg.atom_syntax_object() {
                if let Some(info) = self.analysis.get(syntax_object) {
                    // println!("Ident: {}, Info: {:?}", arg, info);
                    if info.usage_count == 0 {
                        self.unused_args.push(syntax_object.span);
                    }
                }
            }
        }

        self.visit(&lambda_function.body);
    }
}

struct LiftLocallyDefinedFunctions<'a> {
    analysis: &'a Analysis,
    lifted_functions: Vec<ExprKind>,
}

impl<'a> LiftLocallyDefinedFunctions<'a> {
    pub fn new(analysis: &'a Analysis) -> Self {
        Self {
            analysis,
            lifted_functions: Vec::new(),
        }
    }
}

impl<'a> VisitorMutRefUnit for LiftLocallyDefinedFunctions<'a> {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Struct(s) => self.visit_struct(s),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::CallCC(cc) => self.visit_callcc(cc),
            ExprKind::Let(l) => self.visit_let(l),
        }
    }

    fn visit_begin(&mut self, begin: &mut crate::parser::ast::Begin) {
        // Traverse down the tree first - start bubbling up the lifted functions
        // on the way back up
        for expr in &mut begin.exprs {
            self.visit(expr);
        }

        let mut functions = Vec::new();

        for (index, expr) in begin.exprs.iter().enumerate() {
            if let ExprKind::Define(define) = expr {
                let ident = define.name.atom_syntax_object().unwrap();

                if let Some(func) = define.body.lambda_function() {
                    if let Some(info) = self.analysis.get_function_info(func) {
                        let ident_info = self.analysis.get(&ident).unwrap();

                        if ident_info.depth > 1 {
                            if !info.captured_vars.is_empty() {
                                log::info!(
                                    "Found a local function which captures variables: {} - captures vars: {:#?}",
                                    define.name,
                                    info.captured_vars
                                );
                            } else {
                                log::info!("Found a pure local function: {}", define.name);
                                functions.push(index);
                            }
                        }
                    }
                }
            }
        }

        for index in functions.into_iter().rev() {
            let removed_function = begin.exprs.remove(index);
            self.lifted_functions.push(removed_function);
        }
    }
}

// TODO: There might be opportunity to parallelize this here - perhaps shard the analysis between threads
// across some subset of expressions and then merge afterwards
pub struct SemanticAnalysis<'a> {
    // We want to reserve the right to add or remove expressions from the program as needed
    exprs: &'a mut Vec<ExprKind>,
    analysis: Analysis,
}

impl<'a> SemanticAnalysis<'a> {
    pub fn new(exprs: &'a mut Vec<ExprKind>) -> Self {
        let analysis = Analysis::from_exprs(exprs);
        Self { exprs, analysis }
    }

    pub fn get(&self, object: &SyntaxObject) -> Option<&SemanticInformation> {
        self.analysis.get(object)
    }

    pub fn query_top_level_define<A: AsRef<str>>(
        &self,
        name: A,
    ) -> Option<&crate::parser::ast::Define> {
        query_top_level_define(&self.exprs, name)
    }

    pub fn get_global_id<A: AsRef<str>>(&self, name: A) -> Option<usize> {
        self.query_top_level_define(name)?
            .name
            .atom_syntax_object()
            .map(|x| x.syntax_object_id)
    }

    /// In this case, `let` also translates directly to an anonymous function call
    pub fn find_anonymous_function_calls_and_mutate_with<F>(&mut self, func: F)
    where
        F: FnMut(&Analysis, &mut ExprKind) -> bool,
    {
        let mut anonymous_function_call_sites =
            AnonymousFunctionCallSites::new(&self.analysis, func);
        for expr in self.exprs.iter_mut() {
            anonymous_function_call_sites.visit(expr);
        }
    }

    /// Find anonymous function calls with no arguments that don't capture anything,
    /// and replace this with just the body of the function. For instance:
    ///
    /// ```scheme
    /// (let () (+ 1 2 3 4 5)) ;; => (+ 1 2 3 4 5)
    ///
    /// ```
    pub fn replace_pure_empty_lets_with_body(&mut self) -> &mut Self {
        let mut re_run_analysis = false;

        let func = |analysis: &Analysis, anon: &mut ExprKind| {
            if let ExprKind::List(l) = anon {
                let arg_count = l.args.len() - 1;
                let function = l.args.get_mut(0).unwrap();

                if let ExprKind::LambdaFunction(f) = function {
                    let analysis = analysis.get_function_info(&f).unwrap();

                    if analysis.captured_vars.is_empty() {
                        log::info!("Found a function that does not capture variables");

                        if f.args.is_empty() && arg_count == 0 {
                            // Take out the body of the function - we're going to want to use that now
                            let mut dummy = ExprKind::List(List::new(Vec::new()));
                            std::mem::swap(&mut f.body, &mut dummy);
                            *anon = dummy;

                            re_run_analysis = true;

                            // We changed the function call - we should adjust accordingly
                            return true;
                        }
                    }
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }

            false
        };

        self.find_anonymous_function_calls_and_mutate_with(func);

        if re_run_analysis {
            log::info!("Re-running the semantic analysis after modifications");

            self.analysis = Analysis::from_exprs(self.exprs);
        }

        self
    }

    // Modify the call site to point to another kind of expression
    pub fn find_call_sites_and_mutate_with<F>(&mut self, name: &str, func: F)
    where
        F: FnMut(&Analysis, &mut ExprKind) -> (),
    {
        let mut find_call_sites = MutateCallSites::new(name, &self.analysis, func);

        for expr in self.exprs.iter_mut() {
            find_call_sites.visit(expr);
        }
    }

    // Locate the call sites of the given global function name, and calls the given function
    // on the node
    pub fn find_call_sites_and_call<F>(&self, name: &str, func: F)
    where
        F: FnMut(&Analysis, &crate::parser::ast::List) -> (),
    {
        let mut find_call_sites = FindCallSites::new(name, &self.analysis, func);

        for expr in self.exprs.iter() {
            find_call_sites.visit(expr);
        }
    }

    // Locate the call sites of the given global function, and calls the given function
    // on the node
    pub fn find_call_sites_and_modify_with<F>(&mut self, name: &str, func: F)
    where
        F: FnMut(&Analysis, &mut crate::parser::ast::List) -> (),
    {
        let mut find_call_sites = FindCallSites::new(name, &self.analysis, func);

        for expr in self.exprs.iter_mut() {
            find_call_sites.visit(expr);
        }
    }

    pub fn last_usages(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis.info.values().filter(|x| x.last_usage)
    }

    pub fn free_identifiers(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis
            .info
            .values()
            .filter(|x| x.kind == IdentifierStatus::Free)
    }

    pub fn unused_variables(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis.info.values().filter(|x| {
            x.usage_count == 0
                && matches!(x.kind, IdentifierStatus::Local | IdentifierStatus::Global)
        })
    }

    pub fn global_defs(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis
            .info
            .values()
            .filter(|x| x.kind == IdentifierStatus::Global)
    }

    pub fn built_ins(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis.info.values().filter(|x| x.builtin)
    }

    pub fn find_free_identifiers(&self) -> impl Iterator<Item = &'_ SemanticInformation> {
        self.analysis
            .info
            .values()
            .filter(|x| x.kind == IdentifierStatus::Free)
    }

    pub fn find_unused_arguments(&self) -> Vec<Span> {
        let mut unused = UnusedArguments::new(&self.analysis);

        for expr in self.exprs.iter() {
            unused.visit(expr);
        }

        unused.unused_args
    }

    // TODO: Right now this lifts and renames, but it does not handle
    // The extra arguments necessary for this to work
    pub fn lift_local_functions(&mut self) {
        let mut overall_lifted = Vec::new();
        let mut re_run_analysis = false;

        let exprs_len = self.exprs.len();

        // Window in to the individual values - we're going to want to get access to each of the other ones individually
        for i in 0..exprs_len {
            let mut local_funcs = LiftLocallyDefinedFunctions::new(&self.analysis);

            // Just borrow for exactly how long we need it for
            local_funcs.visit(self.exprs.get_mut(i).unwrap());

            if !local_funcs.lifted_functions.is_empty() {
                re_run_analysis = true;
            }

            // Move out the local functions
            let mut local_functions = local_funcs.lifted_functions;

            let ids = local_functions
                .iter()
                .map(|x| {
                    if let ExprKind::Define(d) = x {
                        println!("Found a local function to lift: {}", d.name);
                        d.name.atom_syntax_object().unwrap().syntax_object_id
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>();

            for id in ids {
                let mut find_call_site_by_id = FindCallSiteById::new(
                    id,
                    &self.analysis,
                    |_: &Analysis, call_site: &mut List| {
                        if let Some(first_ident) = call_site.first_ident_mut() {
                            *first_ident = "##lambda-lifting##".to_string()
                                + first_ident
                                + id.to_string().as_str();
                            true
                        } else {
                            false
                        }
                    },
                );

                // Mutate the call sites of the original expression we were visiting
                // Similar to here - just borrow exactly how long we need it for
                find_call_site_by_id.visit(self.exprs.get_mut(i).unwrap());
                // Now also mutate the bodies of any of the functions as well
                for local_function in &mut local_functions {
                    find_call_site_by_id.visit(local_function);
                    // println!("Function: {}", local_function);

                    // Also update the name
                    if let ExprKind::Define(define) = local_function {
                        if let Some(syntax_object) = define.name.atom_syntax_object() {
                            if syntax_object.syntax_object_id != id {
                                continue;
                            }
                        }

                        if let Some(name) = define.name.atom_identifier_mut() {
                            *name =
                                "##lambda-lifting##".to_string() + name + id.to_string().as_str();
                        } else {
                            unreachable!("This should explicitly be an identifier here - perhaps a macro was invalid?");
                        }
                    } else {
                        unreachable!("These should only be defines by design");
                    }
                }

                // Visit the other ones
                for j in 0..exprs_len {
                    if i != j {
                        // println!("Revisiting: {}", self.exprs.get_mut(j).unwrap());
                        find_call_site_by_id.visit(self.exprs.get_mut(j).unwrap());
                    }
                }

                // Also visit those that have been lifted out
                // for expr in &mut overall_lifted {
                //     println!("Visiting: {}", expr);
                //     find_call_site_by_id.visit(expr);
                // }

                // Check if we need to re run this analysis - if we've made any modifications up to this point, we
                // Need to re run the analysis afterwards
                re_run_analysis |= find_call_site_by_id.modified;
            }

            // Put the lifted expressions back at the end - they _probably_ should go to the front, but for now
            // Lets just put them at the back
            overall_lifted.append(&mut local_functions);
        }

        // Same as the above - just getting around a double mutable borrow.
        // Move the lifted functions to the back of the original expression list
        self.exprs.append(&mut overall_lifted);

        if re_run_analysis {
            log::info!(
                "Re-running the semantic analysis after modifications during lambda lifting"
            );

            self.analysis = Analysis::from_exprs(self.exprs);
        }
    }

    pub fn resolve_alias(&self, id: usize) -> Option<usize> {
        self.analysis.resolve_alias(id)
    }
}

#[cfg(test)]
mod analysis_pass_tests {
    use env_logger::Builder;
    use log::LevelFilter;

    use crate::{
        parser::{ast::List, parser::Parser},
        rerrs::ErrorKind,
    };

    use super::*;

    #[test]
    fn find_last_usages() {
        let script = r#"
            (define (loop value accum)
                (+ value accum)
                (loop value (cons value accum)))
        "#;

        let mut exprs = Parser::parse(script).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);

        let last_usages = analysis.last_usages().collect::<Vec<_>>();
        // In this case, we should be identifying just the two usages for
        // value and accum in the recursive call
        assert_eq!(last_usages.len(), 2);
    }

    #[test]
    fn analysis_pass_finds_call_sites() {
        let script = r#"
            (define (foo) (+ 1 2 3 4 5))
            (define (test) (let ((a 10) (b 20)) (foo)))
            (foo)
            (foo)
            (begin
                (foo)
                (foo)
                (foo))
        "#;

        let mut exprs = Parser::parse(script).unwrap();
        let analysis = SemanticAnalysis::new(&mut exprs);

        let mut count = 0;

        analysis.find_call_sites_and_call("foo", |_, _| count += 1);

        assert_eq!(count, 6);
    }

    #[test]
    fn resolve_alias() {
        let script = r#"
            (define list (%module-get%))
            (define alias-list list)
            (define alias-list2 alias-list)
            (define alias-list3 alias-list2)
            (define alias-list4 alias-list3)
        "#;

        // let mut analysis = Analysis::default();
        let mut exprs = Parser::parse(script).unwrap();
        {
            let analysis = SemanticAnalysis::new(&mut exprs);

            let list_id = analysis
                .query_top_level_define("list")
                .and_then(|x| x.name_id())
                .unwrap();

            let alias_list_4_id = analysis
                .query_top_level_define("alias-list4")
                .and_then(|x| x.name_id())
                .unwrap();

            let found = analysis.resolve_alias(alias_list_4_id);

            assert_eq!(list_id, found.unwrap());
        }
    }

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

        (define + (%module-get%))
        (define list (%module-get%))

        (define alias-list list)
        (define alias-list2 alias-list)
        (define alias-list3 alias-list2)
        (define alias-list4 alias-list3)

        (let ()
            (let ()
                (let ()
                    (let () (+ 1 2 3 4 5)))))

        (+ applesauce 20)

        (define applesauce 100)

        ;(let ((a 10) (b 20))
        ;    (+ a b c))

        ;(define (foo x y z)
        ;    (define (inner-func-that-captures a b c)
        ;        (inner-func-that-captures x y z))
        ;    (inner-func-that-captures 1 2 3))

        (define (test)
            (define (foo)
                    (bar))
            (define (bar)
                    (foo))
            (foo))

        (define (loop value accum)
            (loop (cons value accum)))

        ;(define (foo x y z)
        ;    (let ((x x) (y y))
        ;        (lambda (extra-arg x) 
        ;            (set! x 100)
        ;            (set! foo "hello world")
        ;            (+ z z z))))

        ;(define (test x y z)
        ;    (let ((foo foo))
        ;        (foo x y z)
        ;        (foo 1 2 3)
        ;        (foo 10 20 30)))
        
        ;(foo "applesauce" "bananas" "sauce")
        "#;

        // let mut analysis = Analysis::default();
        let mut exprs = Parser::parse(script).unwrap();
        {
            let mut analysis = SemanticAnalysis::new(&mut exprs);
            analysis.replace_pure_empty_lets_with_body();

            // Log the free identifiers
            let free_vars = analysis.find_free_identifiers();

            for var in free_vars {
                crate::rerrs::report_error(
                    ErrorKind::FreeIdentifier.to_error_code(),
                    "input.rkt",
                    script,
                    format!("Free identifier"),
                    var.span,
                );
            }

            let unused_args = analysis.find_unused_arguments();

            println!("Unused args: {:?}", unused_args);

            for var in analysis.unused_variables() {
                crate::rerrs::report_warning(
                    ErrorKind::FreeIdentifier.to_error_code(),
                    "input.rkt",
                    script,
                    format!("Unused variable"),
                    var.span,
                );
            }

            for var in analysis.global_defs() {
                crate::rerrs::report_info(
                    ErrorKind::FreeIdentifier.to_error_code(),
                    "input.rkt",
                    script,
                    format!("global var"),
                    var.span,
                );
            }

            // for var in analysis.built_ins() {
            //     crate::rerrs::report_info(
            //         ErrorKind::FreeIdentifier.to_error_code(),
            //         "input.rkt",
            //         script,
            //         format!("built in function"),
            //         var.span,
            //     );
            // }

            for var in analysis.last_usages() {
                crate::rerrs::report_info(
                    ErrorKind::FreeIdentifier.to_error_code(),
                    "input.rkt",
                    script,
                    format!("last usage of variable"),
                    var.span,
                );
            }

            analysis.lift_local_functions();
            // analysis.lift_local_functions();

            for expr in analysis.exprs.iter() {
                println!("{}", expr);
            }

            let list_id = analysis
                .query_top_level_define("list")
                .unwrap()
                .name
                .atom_syntax_object()
                .unwrap()
                .syntax_object_id;

            let alias_list_4_id = analysis
                .query_top_level_define("alias-list4")
                .unwrap()
                .name
                .atom_syntax_object()
                .unwrap()
                .syntax_object_id;

            let found = analysis.resolve_alias(alias_list_4_id);

            println!(
                "List id: {}, list 4 id: {}, resolved alias id: {:?}",
                list_id, alias_list_4_id, found
            );
        }

        // println!("{}", exprs[0]);

        // let function_definit

        // analysis.find_call_sites_and_call(name, func)

        // analysis.run(&exprs);

        // for expr in &exprs {
        //     analysis.visit(expr);
        // }

        // find_call_sites_and_modify_with("foo", &analysis, &mut exprs, |l| {
        //     log::info!("Found a call site: {:?}", l.to_string())
        // });
    }
}
