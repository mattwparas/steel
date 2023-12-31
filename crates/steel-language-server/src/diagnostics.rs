#![allow(unused)]

use std::{
    collections::{BTreeSet, HashMap},
    iter::FlatMap,
    path::PathBuf,
};

use dashmap::DashSet;
use ropey::Rope;
use steel::{
    compiler::passes::{
        analysis::{
            query_top_level_define, query_top_level_define_on_condition,
            RequiredIdentifierInformation, SemanticAnalysis,
        },
        VisitorMutUnitRef,
    },
    define_primitive_symbols, define_symbols,
    parser::{
        ast::{Define, ExprKind},
        interner::InternedString,
        parser::{SourceId, SyntaxObjectId},
        span::Span,
    },
    steel_vm::{builtin::Arity, engine::Engine},
    stop, SteelVal,
};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Range, Url};

use crate::backend::{make_error, offset_to_position};

pub struct DiagnosticContext<'a> {
    pub engine: &'a Engine,
    pub analysis: &'a SemanticAnalysis<'a>,
    pub uri: &'a Url,
    pub source_id: Option<SourceId>,
    pub rope: Rope,
    pub globals_set: &'a DashSet<InternedString>,
    pub ignore_set: &'a DashSet<InternedString>,
}

pub struct FreeIdentifiersAndUnusedIdentifiers;

impl DiagnosticGenerator for FreeIdentifiersAndUnusedIdentifiers {
    fn diagnose(&mut self, context: &mut DiagnosticContext) -> Vec<Diagnostic> {
        context
            .analysis
            .free_identifiers_with_globals(context.engine.symbol_map())
            .into_iter()
            .map(|(ident, info)| (ident, info.span))
            .flat_map(|(ident, span)| {
                if span.source_id() != context.source_id {
                    return None;
                }

                if context.globals_set.contains(&ident) {
                    return None;
                }

                let resolved = ident.resolve();

                // We can just ignore those as well
                if resolved.starts_with("mangler") {
                    return None;
                }

                let start_position = offset_to_position(span.start, &context.rope)?;
                let end_position = offset_to_position(span.end, &context.rope)?;

                // TODO: Publish the diagnostics for each file separately, if we have them
                Some(make_error(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    format!("free identifier: {}", resolved),
                )))
            })
            .chain(
                context
                    .analysis
                    .find_unused_arguments()
                    .into_iter()
                    .flat_map(|(ident, span)| {
                        if span.source_id() != context.source_id {
                            return None;
                        }

                        if context.ignore_set.contains(&ident) {
                            return None;
                        }

                        let resolved = ident.resolve();

                        // This identifier has been renamed - so we can unmangle it and see if it
                        // is in the ignore set
                        if resolved.starts_with("##") && resolved.ends_with(char::is_numeric) {
                            if context.ignore_set.contains(
                                &resolved
                                    .trim_start_matches("##")
                                    .trim_end_matches(char::is_numeric)
                                    .into(),
                            ) {
                                return None;
                            }
                        }

                        let start_position = offset_to_position(span.start, &context.rope)?;
                        let end_position = offset_to_position(span.end, &context.rope)?;

                        let mut diagnostic = Diagnostic::new_simple(
                            Range::new(start_position, end_position),
                            format!("unused variable"),
                        );

                        diagnostic.severity = Some(DiagnosticSeverity::INFORMATION);

                        Some(diagnostic)
                    }),
            )
            .collect()
    }
}

pub struct StaticArityChecker;

impl DiagnosticGenerator for StaticArityChecker {
    fn diagnose(&mut self, context: &mut DiagnosticContext) -> Vec<Diagnostic> {
        let mut arity_checker = StaticArityChecking {
            known_functions: HashMap::new(),
            analysis: &context.analysis,
            known_contracts: HashMap::new(),
        };

        for expr in context.analysis.exprs.iter() {
            arity_checker.visit(&expr);
        }

        // TODO: Resolve all required values to their original
        // values in the other module, and link those known
        // functions there.

        let mut identifiers = context
            .analysis
            .analysis
            .identifier_info()
            .iter()
            .filter_map(|(key, info)| {
                if info.is_required_identifier {
                    Some(*key)
                } else {
                    None
                }
            })
            .collect();

        let required_identifier_information =
            context.analysis.resolve_required_identifiers(identifiers);

        for (id, required_identifier) in required_identifier_information {
            match required_identifier {
                // This is probably going to be annoying?
                RequiredIdentifierInformation::Resolved(resolved) => {
                    // TODO: When the identifier is already in the given AST, meaning
                    // we don't have to go to another AST in order to resolve it,
                    // we should just do that here.
                }
                RequiredIdentifierInformation::Unresolved(interned, name) => {
                    let module_path_to_check = name
                        .trim_start_matches("mangler")
                        .trim_end_matches(interned.resolve())
                        .trim_end_matches("__%#__");

                    let Some(module) = context
                        .engine
                        .modules()
                        .get(&PathBuf::from(module_path_to_check))
                    else {
                        continue;
                    };

                    let module_ast = module.get_ast();

                    // TODO: This is O(n^2) behavior, and we don't want that.
                    // We should merge this and the above loop into one pass, collecting
                    // all of the things that we need. This should be fast enough for small
                    // enough modules, but it is going to blow up on larger modules.
                    let top_level_define = query_top_level_define(module_ast, interned.resolve())
                        .or_else(|| {
                            query_top_level_define_on_condition(
                                module_ast,
                                interned.resolve(),
                                |name, target| name.ends_with(target),
                            )
                        });

                    // Don't include rest args for now
                    if let Some(d) = top_level_define {
                        if let ExprKind::LambdaFunction(l) = &d.body {
                            let mut rest = false;

                            for arg in &l.args {
                                if let Some(ident) = arg.atom_identifier() {
                                    if ident.resolve() == "." {
                                        rest = true;
                                    }
                                }
                            }

                            if !rest && !l.rest {
                                arity_checker.known_functions.insert(id, l.args.len());
                            }
                        }
                    }
                }
            }
        }

        StaticCallSiteArityChecker {
            known_functions: arity_checker.known_functions,
            context,
            diagnostics: Vec::new(),
        }
        .check()
    }
}

// Rules for this:
//
// The identifier is:
//     * known, and not mutated.
//     * We know the arity
pub struct StaticArityChecking<'a> {
    // Arity check the known functions
    known_functions: HashMap<SyntaxObjectId, usize>,
    // If we can, we can attach the contract information as well,
    // to do any static error checking at the call site.
    known_contracts: HashMap<SyntaxObjectId, StaticContract>,

    analysis: &'a SemanticAnalysis<'a>,
}

impl<'a> VisitorMutUnitRef<'a> for StaticArityChecking<'a> {
    fn visit_begin(&mut self, begin: &'a steel::parser::ast::Begin) {
        // Check if this is a define/contract
        match (begin.exprs.get(0), begin.exprs.get(1)) {
            (Some(ExprKind::Define(d)), Some(ExprKind::Set(s))) => {
                if let ExprKind::LambdaFunction(l) = &d.body {
                    if let Some(contract) = function_contract(&s.expr) {
                        self.known_functions
                            .insert(d.name_id().unwrap(), l.args.len());

                        if let Ok(contract) = contract {
                            self.known_contracts.insert(d.name_id().unwrap(), contract);
                        }
                    }
                }
            }
            _ => {}
        }

        for expr in &begin.exprs {
            self.visit(expr);
        }
    }

    fn visit_define(&mut self, define: &'a Define) {
        if let Some(function_name) = define.name_id() {
            if let Some(info) = self.analysis.get_identifier(function_name) {
                // Note: This means we'll miss functions bound by define/contract.
                // It could encourage putting the contract at the function boundary,
                // which is nice.
                if !info.set_bang {
                    if let ExprKind::LambdaFunction(l) = &define.body {
                        if !l.rest {
                            self.known_functions.insert(function_name, l.args.len());
                        }
                    }
                }
            }
        }
    }
}

pub struct StaticCallSiteArityChecker<'a, 'b> {
    known_functions: HashMap<SyntaxObjectId, usize>,
    context: &'a mut DiagnosticContext<'b>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a, 'b> StaticCallSiteArityChecker<'a, 'b> {
    fn check(mut self) -> Vec<Diagnostic> {
        for expr in self.context.analysis.exprs.iter() {
            self.visit(expr);
        }

        self.diagnostics
    }
}

fn create_diagnostic(rope: &Rope, span: &Span, message: String) -> Option<Diagnostic> {
    let start_position = offset_to_position(span.start, &rope)?;
    let end_position = offset_to_position(span.end, &rope)?;

    let mut diagnostic = Diagnostic::new_simple(Range::new(start_position, end_position), message);

    diagnostic.severity = Some(DiagnosticSeverity::INFORMATION);

    Some(diagnostic)
}

impl<'a, 'b> VisitorMutUnitRef<'a> for StaticCallSiteArityChecker<'a, 'b> {
    fn visit_list(&mut self, l: &'a steel::parser::ast::List) {
        if let Some(function_call_ident) = l
            .first()
            .and_then(|x| x.atom_syntax_object())
            .map(|x| x.syntax_object_id)
        {
            // If it is something that... doesn't happen in the file we're currently
            // looking at - we probably shouldn't report it there?
            if !l
                .first_ident()
                .map(|x| x.resolve().starts_with("mangler"))
                .unwrap_or(false)
            {
                if let Some(info) = self.context.analysis.get_identifier(function_call_ident) {
                    if info.builtin {
                        let found_arity = self
                            .context
                            .engine
                            .builtin_modules()
                            .get_metadata_by_name(l.first_ident().unwrap().resolve())
                            .map(|x| x.arity)
                            .or_else(|| {
                                // If this isn't found in the original built in modules,
                                // attempt to find it registered globally?
                                if let SteelVal::BoxedFunction(b) = self
                                    .context
                                    .engine
                                    .extract_value(l.first_ident().unwrap().resolve())
                                    .ok()?
                                {
                                    b.arity.map(Arity::Exact)
                                } else {
                                    None
                                }
                            });

                        match found_arity {
                            Some(Arity::Exact(arity)) => {
                                if l.args.len() != arity + 1 {
                                    let span =
                                        l.first().unwrap().atom_syntax_object().unwrap().span;

                                    if let Some(diagnostic) = create_diagnostic(
                                        &self.context.rope,
                                        &span,
                                        format!(
                                            "Arity mismatch: {} expects {} arguments, found {}",
                                            l.first().unwrap(),
                                            arity,
                                            l.args.len() - 1
                                        ),
                                    ) {
                                        self.diagnostics.push(diagnostic);
                                    }
                                }
                            }
                            Some(Arity::AtLeast(arity)) => {
                                if l.args.len() < arity + 1 {
                                    let span =
                                        l.first().unwrap().atom_syntax_object().unwrap().span;

                                    if let Some(diagnostic) = create_diagnostic(
                                    &self.context.rope,
                                    &span,
                                    format!(
                                        "Arity mismatch: {} expects at least {} arguments, found {}",
                                        l.first().unwrap(),
                                        arity,
                                        l.args.len() - 1
                                    ),
                                ) {
                                    self.diagnostics.push(diagnostic);
                                }
                                }
                            }
                            Some(Arity::AtMost(arity)) => {
                                if l.args.len() > arity + 1 {
                                    let span =
                                        l.first().unwrap().atom_syntax_object().unwrap().span;

                                    if let Some(diagnostic) = create_diagnostic(
                                        &self.context.rope,
                                        &span,
                                        format!(
                                        "Arity mismatch: {} expects at most {} arguments, found {}",
                                        l.first().unwrap(),
                                        arity,
                                        l.args.len() - 1
                                    ),
                                    ) {
                                        self.diagnostics.push(diagnostic);
                                    }
                                }
                            }
                            // TODO: Handle this arity if it comes up
                            Some(Arity::Range(n)) => {}
                            None => {}
                        }
                    } else if let Some(refers_to_id) = info.refers_to {
                        if let Some(arity) = self.known_functions.get(&refers_to_id) {
                            if l.args.len() != arity + 1 {
                                let span = l.first().unwrap().atom_syntax_object().unwrap().span;

                                if let Some(diagnostic) = create_diagnostic(
                                    &self.context.rope,
                                    &span,
                                    format!(
                                        "Arity mismatch: {} expects {} arguments, found {}",
                                        l.first().unwrap(),
                                        arity,
                                        l.args.len() - 1
                                    ),
                                ) {
                                    self.diagnostics.push(diagnostic);
                                }
                            }
                        }
                    }
                }
            }
        }

        for arg in &l.args {
            self.visit(arg);
        }
    }
}

// Produces LSP compatible diagnostic
pub trait DiagnosticGenerator {
    fn diagnose(&mut self, context: &mut DiagnosticContext) -> Vec<Diagnostic>;
}

// Attempt to bind the contracted values, if possible
define_symbols! {
    BIND_C => "bind/c",
    PRIM_BIND_C => "mangler#%private/steel/contract__%#__bind/c",
    MAKE_FUNCTION_C => "make-function/c",
    PRIM_MAKE_FUNCTION_C => "mangler#%private/steel/contract__%#__make-function/c",
    MAKE_C => "make/c",
    PRIM_MAKE_C => "mangler#%private/steel/contract__%#__make/c",
    LIST_OF => "listof",
    PRIM_LIST_OF => "mangler#%private/steel/contract__%#__listof",
}

define_primitive_symbols! {
    (PRIM_INTP, INTP) => "int?",
    (PRIM_INTEGERP, INTEGERP) => "integer?",
    (PRIM_STRINGP, STRINGP) => "string?",
}

fn is_bind_c(ident: &InternedString) -> bool {
    *ident == *BIND_C || *ident == *PRIM_BIND_C
}

fn is_make_function_c(ident: &InternedString) -> bool {
    *ident == *MAKE_FUNCTION_C || *ident == *PRIM_MAKE_FUNCTION_C
}

fn is_make_c(ident: &InternedString) -> bool {
    *ident == *MAKE_C || *ident == *PRIM_MAKE_C
}

fn is_list_of(ident: &InternedString) -> bool {
    *ident == *LIST_OF || *ident == *PRIM_LIST_OF
}

// Is this expression referring to a contract (is this a bind/c instance)
fn is_contract(expr: &ExprKind) -> bool {
    fn is_contract_option(expr: &ExprKind) -> Option<bool> {
        expr.list()?.first_ident().map(is_bind_c)
    }

    if let Some(inner) = is_contract_option(expr) {
        inner
    } else {
        false
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum BaseTypeKind {
    Any,
    Int,
    String,
    Float,
    Boolean,
    Character,
    Symbol,
    Void,
    Other(InternedString),
}

impl BaseTypeKind {
    // TODO -> don't have this just refer directly to unknown
    fn to_type_info(&self) -> TypeInfo {
        match self {
            BaseTypeKind::Any => TypeInfo::Any,
            BaseTypeKind::Int => TypeInfo::Int,
            BaseTypeKind::String => TypeInfo::String,
            BaseTypeKind::Float => TypeInfo::Float,
            BaseTypeKind::Boolean => TypeInfo::Boolean,
            BaseTypeKind::Character => TypeInfo::Char,
            BaseTypeKind::Void => TypeInfo::Void,
            BaseTypeKind::Symbol => TypeInfo::Symbol,
            BaseTypeKind::Other(_) => TypeInfo::Unknown,
        }
    }

    fn from_str(input: InternedString) -> BaseTypeKind {
        match input {
            input
                if input == *INTP
                    || input == *INTEGERP
                    || input == *PRIM_INTP
                    || input == *PRIM_INTEGERP =>
            {
                BaseTypeKind::Int
            }

            input if input == *PRIM_STRINGP || input == *STRINGP => BaseTypeKind::String,
            _ => BaseTypeKind::Other(input),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum StaticContract {
    Atom(BaseTypeKind),
    ListOf(Box<StaticContract>),
    UnionOf(Vec<StaticContract>),
    IntersectionOf(Vec<StaticContract>),
    Function(Vec<StaticContract>, Box<StaticContract>),
    // For now just unify the internal representation of built ins vs not
    AnyArityFunction(Box<StaticContract>, Box<StaticContract>),
}

// Is this bind/c instance referring to a make-function/c instance
fn function_contract(expr: &ExprKind) -> Option<steel::rvals::Result<StaticContract>> {
    let body = expr.list()?;

    if is_bind_c(body.first_ident()?) {
        let make_function = body.get(1)?;

        if is_make_function_c(make_function.list()?.first_ident()?) {
            // This just deconstructs the (bind/c into a individual struct for access)
            // return Some(BindContract::new(make_function, body.get(2)?));

            return Some(StaticContract::from_exprkind(make_function));
        }
    }

    None
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum TypeInfo {
    // We don't have enough information to say what this type is
    // Either, the function has come externally or it was unable to be inferred for some reason
    Unknown,
    Any,
    Void,
    Int,
    Float,
    Boolean,
    Char,
    String,
    Symbol,
    ListOf(Box<TypeInfo>),
    // Basic function usage, this has some fixed number of arguments with contracts attached
    FixedArityFunction(Vec<TypeInfo>, Box<TypeInfo>),
    // This is something like addition, which has a contract with any arity in the preconditions
    AnyArityFunction(Box<TypeInfo>, Box<TypeInfo>),

    // Function where the input type directly creates the output type in some fashion
    DependentFunction(fn(Vec<TypeInfo>) -> TypeInfo),

    // If a function can return multiple things, or the return value can satisfy multiple contracts (or/c)
    UnionOf(BTreeSet<TypeInfo>),
    // If the return value of a function must satisfy multiple contracts (and/c)
    IntersectionOf(BTreeSet<TypeInfo>),
}

impl StaticContract {
    fn to_type_info(&self) -> TypeInfo {
        match self {
            StaticContract::Atom(a) => a.to_type_info(),
            StaticContract::ListOf(l) => TypeInfo::ListOf(Box::new(l.to_type_info())),
            StaticContract::Function(pre, post) => TypeInfo::FixedArityFunction(
                pre.iter().map(|x| x.to_type_info()).collect(),
                Box::new(post.to_type_info()),
            ),
            StaticContract::AnyArityFunction(pre, post) => TypeInfo::AnyArityFunction(
                Box::new(pre.to_type_info()),
                Box::new(post.to_type_info()),
            ),
            StaticContract::UnionOf(u) => {
                TypeInfo::UnionOf(u.iter().map(|x| x.to_type_info()).collect())
            }
            StaticContract::IntersectionOf(u) => {
                TypeInfo::IntersectionOf(u.iter().map(|x| x.to_type_info()).collect())
            }
        }
    }

    fn from_exprkind(expr: &ExprKind) -> steel::rvals::Result<StaticContract> {
        match expr {
            ExprKind::Atom(a) => {
                let name = BaseTypeKind::from_str(*a.ident().unwrap()); // TODO -> clean up this error handling

                Ok(StaticContract::Atom(name))
            }
            ExprKind::List(l) => match l.first_ident() {
                Some(x) if is_list_of(x) => {
                    let body = l.get(1).unwrap();

                    Ok(StaticContract::ListOf(Box::new(Self::from_exprkind(body)?)))
                }
                Some(x) if is_make_c(x) => {
                    // Just recur on the actual contract here
                    Self::from_exprkind(l.get(1).unwrap())
                }
                Some(x) if is_make_function_c(x) => {
                    // Pre conditions of the function contract
                    let pre_conditions = &l.args[1..l.args.len() - 1];
                    // Post condition of the function contract
                    let post_condition = &l.args[l.args.len() - 1];

                    let pre_condition_contracts = pre_conditions
                        .iter()
                        .map(Self::from_exprkind)
                        .collect::<steel::rvals::Result<Vec<_>>>()?;

                    let post_condition_contract = Self::from_exprkind(post_condition)?;

                    Ok(StaticContract::Function(
                        pre_condition_contracts,
                        Box::new(post_condition_contract),
                    ))
                }
                _ => {
                    println!("{expr}");
                    stop!(Generic => "Unexpected contract combinator")
                }
            },
            _ => {
                stop!(BadSyntax => "contracts can either be atoms or lists")
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct GlobalContractCollector {
    contracts: HashMap<InternedString, StaticContract>,
    syntax_id_to_string: HashMap<SyntaxObjectId, InternedString>,
}

impl<'a> VisitorMutUnitRef<'a> for GlobalContractCollector {
    fn visit_define(&mut self, define: &'a Define) {
        if let Some(contract) = function_contract(&define.body) {
            let atom_identifier = define.name.atom_syntax_object().unwrap();

            let name = define.name.atom_identifier().unwrap();
            let syntax_object_id = atom_identifier.syntax_object_id;

            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts.insert(*name, contract.unwrap());

            self.syntax_id_to_string.insert(syntax_object_id, *name);
        }
    }

    fn visit_set(&mut self, s: &'a steel::parser::ast::Set) {
        if let Some(contract) = function_contract(&s.expr) {
            let atom_identifier = s.variable.atom_syntax_object().unwrap();
            let name = s.variable.atom_identifier().unwrap();
            let syntax_object_id = atom_identifier.syntax_object_id;

            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts.insert(*name, contract.unwrap());

            self.syntax_id_to_string.insert(syntax_object_id, *name);
        }
    }
}

#[test]
fn resolve_contracts() {
    let expression = r#"
(define/contract (my-fun-contracted-function x y)
  (->/c int? int? int?)
  (+ x y))        
    "#;

    let mut engine = Engine::new();

    let exprs = engine
        .emit_expanded_ast_without_optimizations(expression, None)
        .unwrap();

    let mut collector = GlobalContractCollector {
        contracts: HashMap::default(),
        syntax_id_to_string: HashMap::default(),
    };

    for expr in &exprs {
        collector.visit(expr);
    }

    println!("{:#?}", collector.contracts);

    // Now analyze the resulting AST, and build the map
}
