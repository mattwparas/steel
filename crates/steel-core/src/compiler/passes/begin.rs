use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use steel_parser::ast::{Define, If, Let, Macro, Quote, Require, Return, SyntaxRules, Vector};

use crate::parser::{
    ast::{Atom, Begin, ExprKind, LambdaFunction, List, Set},
    parser::SyntaxObject,
    visitors::VisitorMutRef,
};
use crate::parser::{interner::InternedString, tokens::TokenType};

#[cfg(feature = "profiling")]
use crate::time::Instant;

use super::{VisitorMutRefUnit, VisitorMutUnit};

use thin_vec::{thin_vec, ThinVec};

pub(crate) struct CheckDefinesAreInLegalPositions {
    depth: usize,
}
impl VisitorMutRef for CheckDefinesAreInLegalPositions {
    type Output = crate::rvals::Result<()>;

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) -> Self::Output {
        for var in &mut lambda_function.args {
            self.visit(var)?;
        }
        self.depth += 1;
        self.visit(&mut lambda_function.body)?;
        self.depth -= 1;
        Ok(())
    }

    #[inline]
    fn visit_if(&mut self, f: &mut If) -> Self::Output {
        self.depth += 1;
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;
        self.depth -= 1;

        Ok(())
    }

    #[inline]
    fn visit_let(&mut self, l: &mut Let) -> Self::Output {
        self.depth += 1;

        for x in l.bindings.iter_mut() {
            self.visit(&mut x.1)?;
        }

        self.visit(&mut l.body_expr)?;
        self.depth -= 1;

        Ok(())
    }

    #[inline]
    fn visit_define(&mut self, define: &mut Define) -> Self::Output {
        if self.depth != 0 {
            crate::stop!(BadSyntax => "Define cannot exist except at the top level, unless within another lexical context or begin expression"; define.location.span);
        }

        self.visit(&mut define.name)?;
        self.visit(&mut define.body)?;

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut Begin) -> Self::Output {
        for expr in &mut begin.exprs {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => {
                crate::compiler::passes::with_stack(|| self.visit_lambda_function(l))
            }
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    #[inline]
    fn visit_return(&mut self, r: &mut Return) -> Self::Output {
        self.visit(&mut r.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_quote(&mut self, _quote: &mut Quote) -> Self::Output {
        // self.visit(&mut quote.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_macro(&mut self, _m: &mut Macro) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_list(&mut self, l: &mut List) -> Self::Output {
        for expr in &mut l.args {
            self.visit(expr)?;
        }
        Ok(())
    }

    #[inline]
    fn visit_syntax_rules(&mut self, _l: &mut SyntaxRules) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_set(&mut self, s: &mut Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;
        Ok(())
    }

    #[inline]
    fn visit_require(&mut self, _s: &mut Require) -> Self::Output {
        Ok(())
    }

    #[inline]
    fn visit_vector(&mut self, v: &mut Vector) -> Self::Output {
        if v.bytes {
            return Ok(());
        }

        for arg in &mut v.args {
            self.visit(arg)?;
        }

        Ok(())
    }
}

pub(crate) struct FlattenBegin {}
impl FlattenBegin {
    pub(crate) fn flatten(expr: &mut ExprKind) {
        FlattenBegin {}.visit(expr)
    }
}

impl VisitorMutRefUnit for FlattenBegin {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => {
                crate::compiler::passes::with_stack(|| self.visit_lambda_function(l))
            }
            ExprKind::Begin(begin) => {
                for expr in begin.exprs.iter_mut() {
                    self.visit(expr);
                }

                if begin.exprs.len() == 1 {
                    *expr = core::mem::take(&mut begin.exprs)
                        .into_iter()
                        .next()
                        .unwrap();

                    return;
                }

                let begin_exprs = core::mem::take(&mut begin.exprs);

                let mut flattened_exprs = Vec::with_capacity(begin_exprs.len());

                for expr in begin_exprs {
                    if let ExprKind::Begin(mut b) = expr {
                        flattened_exprs.append(&mut b.exprs)
                    } else {
                        flattened_exprs.push(expr);
                    }
                }

                begin.exprs = flattened_exprs;

                if begin.exprs.len() == 1 {
                    *expr = core::mem::take(&mut begin.exprs)
                        .into_iter()
                        .next()
                        .unwrap();
                }
            }
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }
}

pub fn flatten_begins_and_expand_defines(
    mut exprs: Vec<ExprKind>,
) -> crate::rvals::Result<Vec<ExprKind>> {
    for expr in exprs.iter_mut() {
        FlattenBegin::flatten(expr);
        ConvertDefinesToLetsMut::convert_defines_mut(expr)?;
        let mut checker = CheckDefinesAreInLegalPositions { depth: 0 };
        checker.visit(expr)?;
    }

    Ok(exprs)
}

struct DefinedVars {
    defined_identifiers: smallvec::SmallVec<[InternedString; 32]>,
    output: bool,
}

impl DefinedVars {
    fn new() -> Self {
        DefinedVars {
            defined_identifiers: smallvec::SmallVec::default(),
            output: false,
        }
    }

    fn insert(&mut self, name: InternedString) {
        self.defined_identifiers.push(name);
    }

    fn check_output(&mut self) -> bool {
        let output = self.output;
        self.output = false;
        output
    }
}

impl VisitorMutUnit for DefinedVars {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            self.output = self.output || self.defined_identifiers.contains(ident);
        }
    }
}

struct ConvertDefinesToLetsMut {
    depth: usize,
    // The first define in a body that refers to a later one as it runs
    error: Option<crate::rerrs::SteelErr>,
}

impl ConvertDefinesToLetsMut {
    fn new() -> Self {
        Self {
            depth: 0,
            error: None,
        }
    }
    fn convert_defines_mut(expr: &mut ExprKind) -> crate::rvals::Result<()> {
        let mut converter = ConvertDefinesToLetsMut::new();
        converter.visit(expr);
        converter.error.map_or(Ok(()), Err)
    }
}

impl VisitorMutRefUnit for ConvertDefinesToLetsMut {
    #[inline]
    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.body);
    }

    #[inline]
    fn visit_quote(&mut self, _quote: &mut Quote) {}

    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => {
                crate::compiler::passes::with_stack(|| self.visit_lambda_function(l))
            }
            ExprKind::Begin(begin) => {
                if self.depth > 0 {
                    let contains_defines =
                        begin.exprs.iter().any(|x| matches!(x, ExprKind::Define(_)));
                    if contains_defines {
                        let mut fake_begin = Begin::new(Vec::new(), begin.location.clone());
                        core::mem::swap(&mut fake_begin, begin);

                        let boxed = Box::new(fake_begin);

                        let mut converted = convert_exprs_to_let(boxed, &mut self.error);

                        match &mut converted {
                            ExprKind::Begin(b) => {
                                self.visit_begin(b);
                            }
                            ExprKind::List(l) => {
                                self.visit_list(l);
                            }
                            ExprKind::Let(l) => {
                                self.visit_let(l);
                            }
                            other => {
                                panic!(
                                    "Something went wrong in define conversion, found: {other:?}"
                                )
                            }
                        }

                        *expr = converted;
                    } else {
                        for expr in &mut begin.exprs {
                            self.visit(expr);
                        }
                    }
                } else {
                    for expr in &mut begin.exprs {
                        self.visit(expr);
                    }
                }
            }
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
        self.depth += 1;
        self.visit(&mut lambda_function.body);
        self.depth -= 1;
    }

    #[inline]
    fn visit_let(&mut self, l: &mut steel_parser::ast::Let) {
        self.depth += 1;

        for (binding, expr) in &mut l.bindings {
            self.visit(binding);
            self.visit(expr);
        }

        self.visit(&mut l.body_expr);

        self.depth -= 1;
    }
}

#[derive(PartialEq, Debug)]
enum ExpressionType {
    DefineConst(InternedString),
    DefineFlat(InternedString),
    DefineFlatStar(InternedString),
    DefineFunction(InternedString),
    Expression,
}

impl ExpressionType {
    fn is_expression(&self) -> bool {
        matches!(self, ExpressionType::Expression)
    }

    fn is_atom(t: &SyntaxObject) -> bool {
        matches!(
            t.ty,
            TokenType::BooleanLiteral(_)
                | TokenType::Number(_)
                | TokenType::StringLiteral(_)
                | TokenType::CharacterLiteral(_)
        )
    }

    fn is_constant(expr: &ExprKind) -> bool {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => Self::is_atom(syn),
            _ => false,
        }
    }

    fn generate_expression_types(exprs: &[ExprKind]) -> Vec<ExpressionType> {
        let mut expression_types = Vec::with_capacity(exprs.len());
        let mut defined_idents = DefinedVars::new();
        // Flat defines after a deferred expression are deferred too, so the
        // defines keep their order, e.g. `(define a 10) (define b 20)`.
        let mut seen_deferred = false;

        for expr in exprs {
            match expr {
                ExprKind::Define(d) => {
                    let name = d
                        .name
                        .atom_identifier_or_else(|| {})
                        .expect(&format!("Define without a legal name: {}", d.name));

                    defined_idents.insert(*name);

                    match &d.body {
                        ExprKind::LambdaFunction(_) => {
                            expression_types.push(ExpressionType::DefineFunction(*name));
                        }
                        _ => {
                            defined_idents.visit(&d.body);
                            if defined_idents.check_output() {
                                seen_deferred = true;
                                expression_types.push(ExpressionType::DefineFlatStar(*name));
                            } else if Self::is_constant(&d.body) {
                                expression_types.push(ExpressionType::DefineConst(*name));
                            } else if seen_deferred {
                                expression_types.push(ExpressionType::DefineFlatStar(*name));
                            } else {
                                expression_types.push(ExpressionType::DefineFlat(*name));
                            }
                        }
                    }
                }
                _ => {
                    seen_deferred = true;
                    expression_types.push(ExpressionType::Expression);
                }
            }
        }

        // println!("Expression types: {:?}", expression_types);

        expression_types
    }
}

fn set(var: ExprKind, expr: ExprKind) -> ExprKind {
    ExprKind::Set(Box::new(Set::new(
        var,
        expr,
        SyntaxObject::default(TokenType::Set),
    )))
}

fn apply_ident(func: ExprKind) -> ExprKind {
    ExprKind::List(List::new(thin_vec![func]))
}

/// Collects references to a set of names, ignoring quoted data.
///
/// A struct's generated definitions include `(make-struct-type (quote node) ...)`,
/// where the quoted `node` is also the name of a later definition in the same
/// group. Counting it would make every struct look self-referential.
struct ForwardReferences {
    names: smallvec::SmallVec<[InternedString; 32]>,
    found: bool,
    // Found in a function body, which runs after the group is bound
    found_in_lambda: bool,
    // Found where it always runs as the define does, which is an error
    found_directly: bool,
    lambdas: usize,
    // Branches of an `if`, which might not run
    branches: usize,
    // The first name, of this define or a later one, it always refers to as it runs
    before_definition: Option<(InternedString, steel_parser::span::Span)>,
    // Parameters and let bindings, which aren't the group's names in their bodies
    shadowed: Vec<InternedString>,
}

impl ForwardReferences {
    fn new() -> Self {
        ForwardReferences {
            names: smallvec::SmallVec::default(),
            found: false,
            found_in_lambda: false,
            found_directly: false,
            lambdas: 0,
            branches: 0,
            before_definition: None,
            shadowed: Vec::new(),
        }
    }

    fn insert(&mut self, name: InternedString) {
        self.names.push(name);
    }
}

impl VisitorMutUnit for ForwardReferences {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            if self.names.contains(ident) && !self.shadowed.contains(ident) {
                self.found = true;
                // A branch might only run once the group is bound, as a function's body does
                self.found_in_lambda |= self.lambdas > 0 || self.branches > 0;
                self.found_directly |= self.lambdas == 0 && self.branches == 0;

                if self.lambdas == 0 && self.branches == 0 && self.before_definition.is_none() {
                    self.before_definition = Some((*ident, a.syn.span));
                }
            }
        }
    }

    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) {
        let depth = self.shadowed.len();
        self.shadowed.extend(
            lambda_function
                .args
                .iter()
                .filter_map(|x| x.atom_identifier().copied()),
        );

        self.lambdas += 1;
        self.visit(&lambda_function.body);
        self.lambdas -= 1;

        self.shadowed.truncate(depth);
    }

    fn visit_let(&mut self, l: &Let) {
        for (_, value) in &l.bindings {
            self.visit(value);
        }

        let depth = self.shadowed.len();
        self.shadowed.extend(
            l.bindings
                .iter()
                .filter_map(|x| x.0.atom_identifier().copied()),
        );
        self.visit(&l.body_expr);
        self.shadowed.truncate(depth);
    }

    fn visit_if(&mut self, f: &If) {
        self.visit(&f.test_expr);

        // With a literal test, the branch it picks always runs. The other one is still
        // counted as a branch, since its names have to be bound.
        match literal_truth(&f.test_expr) {
            Some(truth) => {
                let (taken, other) = if truth {
                    (&f.then_expr, &f.else_expr)
                } else {
                    (&f.else_expr, &f.then_expr)
                };

                self.visit(taken);
                self.branches += 1;
                self.visit(other);
                self.branches -= 1;
            }
            None => {
                self.branches += 1;
                self.visit(&f.then_expr);
                self.visit(&f.else_expr);
                self.branches -= 1;
            }
        }
    }

    fn visit_list(&mut self, l: &List) {
        // A lambda applied where it's written runs its body there
        if let Some(ExprKind::LambdaFunction(lambda_function)) = l.args.first() {
            for arg in &l.args[1..] {
                self.visit(arg);
            }

            let depth = self.shadowed.len();
            self.shadowed.extend(
                lambda_function
                    .args
                    .iter()
                    .filter_map(|x| x.atom_identifier().copied()),
            );
            self.visit(&lambda_function.body);
            self.shadowed.truncate(depth);
            return;
        }

        for arg in &l.args {
            self.visit(arg);
        }
    }

    // Quoted data doesn't refer to anything.
    fn visit_quote(&mut self, _quote: &Quote) {}
}

// Whether a literal `if` test is true, or None if it isn't a literal
fn literal_truth(expr: &ExprKind) -> Option<bool> {
    match expr {
        ExprKind::Atom(Atom { syn, .. }) if ExpressionType::is_atom(syn) => {
            Some(!matches!(syn.ty, TokenType::BooleanLiteral(false)))
        }
        ExprKind::Quote(q) => Some(!matches!(
            &q.expr,
            ExprKind::Atom(Atom { syn, .. }) if matches!(syn.ty, TokenType::BooleanLiteral(false))
        )),
        _ => None,
    }
}

// The names of the group an expression refers to anywhere, functions it makes included
struct GroupReferences<'a> {
    names: &'a FxHashMap<InternedString, usize>,
    found: Vec<usize>,
}

impl VisitorMutUnit for GroupReferences<'_> {
    fn visit_atom(&mut self, a: &Atom) {
        if let TokenType::Identifier(ident) = &a.syn.ty {
            if let Some(index) = self.names.get(ident) {
                self.found.push(*index);
            }
        }
    }

    fn visit_quote(&mut self, _quote: &Quote) {}
}

fn group_references(expr: &ExprKind, names: &FxHashMap<InternedString, usize>) -> Vec<usize> {
    let mut references = GroupReferences {
        names,
        found: Vec::new(),
    };
    references.visit(expr);
    references.found
}

// For each function define, the first form that runs code able to call it, as it runs or
// through the other functions of the group. Its body can then run before the defines from
// that form on have, so what it reads of them is checked.
fn called_early(exprs: &[ExprKind], types: &[ExpressionType]) -> Vec<Option<usize>> {
    let mut functions = FxHashMap::default();

    for (index, ty) in types.iter().enumerate() {
        if let ExpressionType::DefineFunction(name) = ty {
            functions.insert(*name, index);
        }
    }

    let mut first = vec![None; exprs.len()];

    if functions.is_empty() {
        return first;
    }

    let mut calls: Vec<Option<Vec<usize>>> = vec![None; exprs.len()];

    for (index, expr) in exprs.iter().enumerate() {
        if matches!(types[index], ExpressionType::DefineFunction(_)) {
            continue;
        }

        let runs = match expr {
            ExprKind::Define(d) => &d.body,
            other => other,
        };

        let mut pending = group_references(runs, &functions);

        while let Some(function) = pending.pop() {
            if first[function].is_some() {
                continue;
            }

            first[function] = Some(index);

            let body = calls[function].get_or_insert_with(|| match &exprs[function] {
                ExprKind::Define(d) => group_references(&d.body, &functions),
                _ => Vec::new(),
            });

            pending.extend(body.iter().copied());
        }
    }

    first
}

// Whether a function in the group refers to a name bound at or after the define it is in,
// which only the letrec encoding binds by the time the function runs, and whether a
// define refers to one as it runs, which is an error that encoding would hide
// Also the first reference that always runs before the define it names, this one or a later
// one, which is an error, as it is in Racket
fn forward_references(
    exprs: &[ExprKind],
) -> (
    bool,
    bool,
    Option<(InternedString, steel_parser::span::Span)>,
) {
    let names: Vec<_> = exprs
        .iter()
        .map(|expr| match expr {
            ExprKind::Define(d) => d.name.atom_identifier().copied(),
            _ => None,
        })
        .collect();

    let mut in_lambda = false;
    let mut directly = false;
    let mut before_definition = None;

    for (index, expr) in exprs.iter().enumerate() {
        let mut later = ForwardReferences::new();

        for name in names[index..].iter().flatten() {
            later.insert(*name);
        }

        match expr {
            ExprKind::Define(d) => later.visit(&d.body),
            other => later.visit(other),
        }

        in_lambda |= later.found_in_lambda;
        directly |= later.found_directly;
        before_definition = before_definition.or(later.before_definition);
    }

    (in_lambda, directly, before_definition)
}

/// `STEEL_SEQUENTIAL_DEFINES=0` encodes every internal define group as letrec.
fn sequential_defines_enabled() -> bool {
    crate::config::config().sequential_defines
}

/// True when no expression in the group refers to a name bound at or after its
/// own position. With no forward references and no self recursion, the group
/// can be lowered like `let*` instead of `letrec`.
///
/// The letrec encoding binds every name to a dummy, thunks each initializer so
/// it can run once all the names exist, and assigns with `set!`.
/// `replace_mutable_captured_variables_with_boxes` then sees each variable as
/// assigned and captured, and boxes it, so every read becomes an `#%unbox`. One
/// function define in the group puts the whole group on that path, as in
///
/// ```text
/// (define (seq n)
///   (define a (+ n 1))
///   (define b (* a 2))
///   (define (get) b)      ; boxes a and b
///   (+ (get) a))
/// ```
///
/// Closure lifting runs after boxing, so it doesn't undo this. A `#:mutable`
/// struct defined inside a function generates one of these groups, with
/// function defines and no forward references.
///
/// Identifiers are collected without tracking shadowing, so a name that only
/// looks like a forward reference keeps the group on the letrec path.
fn defines_are_sequential(exprs: &[ExprKind]) -> bool {
    let names: Vec<Option<InternedString>> = exprs
        .iter()
        .map(|expr| match expr {
            ExprKind::Define(d) => d.name.atom_identifier().copied(),
            _ => None,
        })
        .collect();

    // Give up on a define without a plain name.
    if exprs
        .iter()
        .zip(names.iter())
        .any(|(e, n)| matches!(e, ExprKind::Define(_)) && n.is_none())
    {
        return false;
    }

    for (index, expr) in exprs.iter().enumerate() {
        // Everything bound at or after this point. Referring to any of them is
        // either self recursion or a forward reference.
        let mut later = ForwardReferences::new();
        for name in names[index..].iter().flatten() {
            later.insert(*name);
        }

        match expr {
            ExprKind::Define(d) => later.visit(&d.body),
            other => later.visit(other),
        }

        if later.found {
            if crate::config::config().seq_defines_log {
                let what = match expr {
                    ExprKind::Define(d) => format!("define {}", d.name),
                    other => format!("expr {}", other),
                };
                let n: Vec<String> = names[index..]
                    .iter()
                    .flatten()
                    .map(|x| x.resolve().to_string())
                    .collect();
                eprintln!(
                    "seq-defines: bail at #{} ({}) refers into [{}]",
                    index,
                    what.chars().take(90).collect::<String>(),
                    n.join(" ")
                );
            }
            return false;
        }
    }

    true
}

// A name as the code wrote it, without the renames modules and macro hygiene give it
pub(crate) fn written_name(name: &str) -> String {
    use crate::compiler::modules::MANGLER_PREFIX;

    // Hygiene renames are `##<name>#<mark>` or `##<name>#<mark>#<id>`, of a name that can be
    // a module's
    let renamed = name
        .strip_prefix("##")
        .filter(|_| !name.starts_with(MANGLER_PREFIX));

    let Some(mut base) = renamed else {
        return crate::parser::expand_visitor::unmangled(name).to_string();
    };

    for _ in 0..2 {
        match base.rfind('#') {
            Some(i)
                if i > 0
                    && base[i + 1..].bytes().all(|b| b.is_ascii_digit())
                    && i + 1 < base.len() =>
            {
                base = &base[..i];
            }
            _ => break,
        }
    }

    // A template binder is `##<name>` before its rename
    let base = crate::parser::expand_visitor::unmangled(base);
    match base.strip_prefix("##") {
        Some(rest) if !base.starts_with(MANGLER_PREFIX) && !rest.is_empty() => written_name(base),
        _ => base.to_string(),
    }
}

// What a name of the group is until its define runs
fn uninitialized() -> ExprKind {
    ExprKind::atom("#%prim.#%uninitialized")
}

// A name of the group read where it can run before its define has, in a value or an
// expression of the group, is checked, so reading it early is an error, as in Racket. So is
// what a function define's body reads, when code before those defines can call it.
fn check_before_initialization(exprs: &mut [ExprKind], types: &mut [ExpressionType]) {
    let names: Vec<Option<InternedString>> = exprs
        .iter()
        .map(|expr| match expr {
            ExprKind::Define(d) => d.name.atom_identifier().copied(),
            _ => None,
        })
        .collect();

    let mut checked = Vec::new();
    let early = called_early(exprs, types);

    for (index, expr) in exprs.iter_mut().enumerate() {
        // A function's body runs when it is called, which is only early if code before a
        // later define can call it
        let from = if matches!(types[index], ExpressionType::DefineFunction(_)) {
            match early[index] {
                Some(from) => from,
                None => continue,
            }
        } else {
            index
        };

        let mut check = CheckBeforeInitialization {
            names: names[from..].iter().flatten().copied().collect(),
            shadowed: Vec::new(),
            checked: &mut checked,
        };

        match expr {
            ExprKind::Define(d) => check.visit(&mut d.body),
            other => check.visit(other),
        }
    }

    // A constant read early is bound when its define runs, not up front
    for ty in types.iter_mut() {
        if let ExpressionType::DefineConst(name) = ty {
            if checked.contains(name) {
                *ty = ExpressionType::DefineFlat(*name);
            }
        }
    }
}

struct CheckBeforeInitialization<'a> {
    names: Vec<InternedString>,
    shadowed: Vec<InternedString>,
    checked: &'a mut Vec<InternedString>,
}

impl CheckBeforeInitialization<'_> {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::Atom(a) => {
                let Some(name) = a.ident().copied() else {
                    return;
                };

                if self.names.contains(&name) && !self.shadowed.contains(&name) {
                    self.checked.push(name);

                    // The error points at the read
                    let span = a.syn.span;
                    let read = core::mem::replace(expr, ExprKind::empty());
                    let mut call = List::new(thin_vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::new(
                            TokenType::Identifier("#%prim.#%check-initialized".into()),
                            span,
                        ))),
                        read,
                        ExprKind::Quote(Box::new(Quote::new(
                            ExprKind::atom(written_name(name.resolve())),
                            SyntaxObject::new(TokenType::Quote, span),
                        ))),
                    ]);
                    call.location = span;
                    *expr = ExprKind::List(call);
                }
            }
            ExprKind::LambdaFunction(l) => {
                let depth = self.shadowed.len();
                self.shadowed
                    .extend(l.args.iter().filter_map(|x| x.atom_identifier().copied()));
                self.visit(&mut l.body);
                self.shadowed.truncate(depth);
            }
            ExprKind::Let(l) => {
                for (_, value) in l.bindings.iter_mut() {
                    self.visit(value);
                }

                let depth = self.shadowed.len();
                self.shadowed.extend(
                    l.bindings
                        .iter()
                        .filter_map(|x| x.0.atom_identifier().copied()),
                );
                self.visit(&mut l.body_expr);
                self.shadowed.truncate(depth);
            }
            ExprKind::If(f) => {
                self.visit(&mut f.test_expr);
                self.visit(&mut f.then_expr);
                self.visit(&mut f.else_expr);
            }
            ExprKind::Define(d) => self.visit(&mut d.body),
            ExprKind::Begin(b) => b.exprs.iter_mut().for_each(|x| self.visit(x)),
            ExprKind::Return(r) => self.visit(&mut r.expr),
            // Assigning before the define runs is an error too, once the value is made
            ExprKind::Set(s) => {
                self.visit(&mut s.expr);

                let Some(syn) = s.variable.atom_syntax_object() else {
                    return;
                };

                let Some(name) = syn.ty.identifier().copied() else {
                    return;
                };

                if self.names.contains(&name) && !self.shadowed.contains(&name) {
                    self.checked.push(name);

                    let span = syn.span;
                    let value = core::mem::replace(&mut s.expr, ExprKind::empty());
                    let mut call = List::new(thin_vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::new(
                            TokenType::Identifier("#%prim.#%check-assignable".into()),
                            span,
                        ))),
                        ExprKind::atom(name),
                        ExprKind::Quote(Box::new(Quote::new(
                            ExprKind::atom(written_name(name.resolve())),
                            SyntaxObject::new(TokenType::Quote, span),
                        ))),
                        value,
                    ]);
                    call.location = span;
                    s.expr = ExprKind::List(call);
                }
            }
            ExprKind::List(l) => l.args.iter_mut().for_each(|x| self.visit(x)),
            // Quoted data, vectors of it and the rest don't read names
            _ => {}
        }
    }
}

fn convert_exprs_to_let(begin: Box<Begin>, error: &mut Option<crate::rerrs::SteelErr>) -> ExprKind {
    // let defines = collect_defines_from_current_scope(&exprs);

    let mut expression_types = ExpressionType::generate_expression_types(&begin.exprs);

    // Go ahead and quit if there are only expressions here
    if expression_types.iter().all(|x| x.is_expression()) {
        return ExprKind::Begin(begin);
    }

    // A group without function defines can be lowered this way, and so can one
    // with them as long as nothing refers forward, see `defines_are_sequential`.
    let (lambda_refers_forward, refers_forward_directly, before_definition) =
        forward_references(&begin.exprs);

    // The first one is reported
    if let Some((name, span)) = before_definition.filter(|_| error.is_none()) {
        *error = Some(
            crate::rerrs::SteelErr::new(
                crate::rerrs::ErrorKind::FreeIdentifier,
                format!(
                    "Cannot reference an identifier before its definition: {}",
                    written_name(name.resolve())
                ),
            )
            .with_span(span),
        );
    }

    // Lowered this way, a define that always refers to a later one as it runs is reported.
    // Functions that refer forward need the letrec encoding, which takes precedence.
    if !expression_types
        .iter()
        .any(|x| matches!(x, ExpressionType::DefineFunction(_)))
        && !lambda_refers_forward
        || refers_forward_directly && !lambda_refers_forward
        || (sequential_defines_enabled() && defines_are_sequential(&begin.exprs))
    {
        return begin
            .exprs
            .into_iter()
            .rev()
            .reduce(|accum, expr| match expr {
                ExprKind::Define(d) => {
                    let constructed_function = ExprKind::LambdaFunction(
                        LambdaFunction::new(
                            thin_vec![d.name],
                            accum,
                            SyntaxObject::default(TokenType::Lambda),
                        )
                        .into(),
                    );

                    ExprKind::List(List::new(thin_vec![constructed_function, d.body]))
                }
                other => ExprKind::Begin(Box::new(Begin::new(
                    vec![other, accum],
                    SyntaxObject::default(TokenType::Begin),
                ))),
            })
            .expect("Empty expression");

        // return todo!();
    }

    let mut exprs = begin.exprs;

    check_before_initialization(&mut exprs, &mut expression_types);

    // let mut last_expression = expression_types.len();

    let idx = expression_types
        .iter()
        .rev()
        .position(|x| !x.is_expression())
        .expect("Convert exprs to let in define conversion found no trailing expressions in begin");

    // println!("Last expression index: {:?}", idx);

    let idx = expression_types.len() - 1 - idx;

    let mut body = exprs.split_off(idx + 1);

    // These are going to be the
    let args = exprs
        .iter()
        .map(|x| {
            if let ExprKind::Define(d) = x {
                d.body.clone()
            } else {
                x.clone()
            }
        })
        .collect::<SmallVec<[_; 8]>>();

    // This corresponds to the (let ((apple ..) (banana ..) (cucumber ..)))
    //                               ^^^^^^     ^^^^^^^      ^^^^^^^^
    let mut top_level_arguments: ThinVec<ExprKind> = ThinVec::with_capacity(idx + 1);

    // This corresponds to the set expressions
    // (set! apple #####apple0)
    // (set! banana #####banana1)
    // (set! cucumber #####cucumber1)
    let mut set_expressions: Vec<ExprKind> = Vec::with_capacity(idx + 1);

    // corresponds to #####apple0, #####banana1, #####cucumber1, etc
    let mut bound_names: ThinVec<ExprKind> = ThinVec::with_capacity(idx + 1);

    // TODO - check that the last expression does not contain any usages of the constant?
    // if expression_types[0..idx + 1]
    //     .iter()
    //     .all(|x| matches!(x, ExpressionType::DefineConst(_)))
    // {
    //     return ExprKind::Begin(Begin::new(body, begin.location));
    // }

    // Top level application with dummy arguments that will immediately get overwritten
    let mut top_level_dummy_args = ThinVec::with_capacity(idx + 1);

    let mut new_args = ThinVec::with_capacity(idx + 1);

    // println!("{:#?}", expression_types);

    // TODO:
    // If there are functions at all, go through this weird path
    // Otherwise, we should do the following transformation:
    //
    // If there are no functions at all, just do a series of lets, similar to let*

    // TODO: Move this up so we don't have to use raw_exprs anymore

    for ((i, expression), arg) in expression_types[0..idx + 1].iter().enumerate().zip(args) {
        match expression {
            ExpressionType::DefineFunction(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(uninitialized());
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));
                    let set_expr = set(d.name.clone(), name_prime.clone());
                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                    new_args.push(arg);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };

                // let name = Atom::new(SyntaxObject::new)
            }
            ExpressionType::DefineFlat(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(uninitialized());
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));
                    let set_expr = set(d.name.clone(), name_prime.clone());
                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                    new_args.push(arg);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            ExpressionType::DefineConst(_name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_dummy_args.push(arg);
                    top_level_arguments.push(d.name.clone());
                    // top_level_arguments.push(d.name.clone());
                    // let name_prime = atom("#####".to_string() + name + i.to_string().as_str());
                    // let set_expr = set(d.name.clone(), name_prime.clone());
                    // bound_names.push(name_prime);
                    // set_expressions.push(set_expr);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            ExpressionType::DefineFlatStar(name) => {
                if let ExprKind::Define(d) = &exprs[i] {
                    top_level_arguments.push(d.name.clone());
                    top_level_dummy_args.push(uninitialized());
                    let name_prime = ExprKind::atom(compact_str::format_compact!(
                        "_____{}{}",
                        name.resolve(),
                        i
                    ));

                    // Make this a (set! x (x'))
                    // Applying the function
                    let set_expr = set(d.name.clone(), apply_ident(name_prime.clone()));

                    // Set this to be an empty function (lambda () <expr>)
                    new_args.push(
                        LambdaFunction::new(
                            ThinVec::new(),
                            arg.clone(),
                            SyntaxObject::default(TokenType::Lambda),
                        )
                        .into(),
                    );

                    bound_names.push(name_prime);
                    set_expressions.push(set_expr);
                } else {
                    panic!("expected define, found: {}", &exprs[i]);
                };
            }
            // TODO: Move this down, don't put it with the lets, put it down in order with the set expressions
            // That way we're not at risk of accidentally goofing up the ordering of the expressions.
            // If will _only_ go in the right order of assignment
            ExpressionType::Expression => {
                // TODO: This is definitly not right
                // let expr = atom("#####define-conversion".to_string() + i.to_string().as_str());
                // top_level_dummy_args.push(ExprKind::Atom(Atom::new(SyntaxObject::default(
                //     TokenType::IntegerLiteral(123),
                // ))));

                // // This also gets bound in the inner function for now
                // bound_names.push(expr.clone());

                // top_level_arguments.push(expr);
                // new_args.push(arg);

                set_expressions.push(arg)
            }
        }
    }

    // // Append the body instructions to the set!
    set_expressions.append(&mut body);

    let inner_lambda = LambdaFunction::new(
        bound_names,
        ExprKind::Begin(Box::new(Begin::new(
            set_expressions,
            SyntaxObject::default(TokenType::Begin),
        ))),
        SyntaxObject::default(TokenType::Lambda),
    );

    new_args.insert(0, ExprKind::LambdaFunction(Box::new(inner_lambda)));

    let inner_application = ExprKind::List(List::new(new_args));

    let outer_lambda = LambdaFunction::new(
        top_level_arguments,
        inner_application,
        SyntaxObject::default(TokenType::Lambda),
    );

    top_level_dummy_args.insert(0, ExprKind::LambdaFunction(Box::new(outer_lambda)));

    ExprKind::List(List::new(top_level_dummy_args))

    // TODO: This is the real transformation that needs to take place once lets are fixed
    // follow-up - let rec is going to be completely broken

    // let pairs = bound_names
    //     .into_iter()
    //     .zip(new_args.into_iter())
    //     .collect::<Vec<_>>();

    // let inner_let = Let::new(
    //     pairs,
    //     ExprKind::Begin(Begin::new(
    //         set_expressions,
    //         SyntaxObject::default(TokenType::Begin),
    //     )),
    //     SyntaxObject::default(TokenType::Let),
    // );

    // let outer_pairs = top_level_arguments
    //     .into_iter()
    //     .zip(top_level_dummy_args.into_iter())
    //     .collect::<Vec<_>>();

    // let outer_let = Let::new(
    //     outer_pairs,
    //     ExprKind::Let(Box::new(inner_let)),
    //     SyntaxObject::default(TokenType::Let),
    // );

    // let output = ExprKind::Let(Box::new(outer_let));

    // println!("-----------------");
    // println!("{}", output.to_pretty(60));

    // output
}

#[cfg(test)]
mod flatten_begin_test {

    use super::*;
    use crate::parser::ast::ExprKind;
    use crate::parser::ast::{Atom, Begin, List};
    use steel_parser::tokens::IntLiteral;

    use crate::parser::parser::SyntaxObject;
    use crate::parser::tokens::TokenType;
    use crate::parser::tokens::TokenType::*;

    // #[test]
    // fn defines_translates_to_simple_let() {
    //     let expr = r#"
    //     (lambda ()
    //         (begin
    //             (define x 10)
    //             (define y 20)
    //             (define z 30)
    //             (+ x y z)))"#;

    //     let expected = r#"
    //     (lambda () ((lambda (x y z) ((lambda () (begin (+ x y z))))) 10 20 30))
    //     "#;

    //     let parsed = Parser::parse(expr).unwrap();
    //     let expected_parsed = Parser::parse(expected).unwrap();

    //     let result = flatten_begins_and_expand_defines(parsed);

    //     assert_eq!(result, expected_parsed);
    // }

    fn atom(ident: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(ident.into()))))
    }

    fn int(num: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(num).into(),
        )))
    }

    #[test]
    fn basic_flatten_one_level() {
        let mut expr = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(thin_vec![
                        atom("+"),
                        atom("x"),
                        int(10),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        let expected = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::List(List::new(thin_vec![atom("+"), atom("x"), int(10)])),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        FlattenBegin::flatten(&mut expr);

        assert_eq!(expr, expected);
    }

    #[test]
    fn basic_flatten_multiple_levels() {
        let mut expr = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(thin_vec![
                        atom("+"),
                        atom("x"),
                        int(10),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(thin_vec![
                        atom("+"),
                        atom("y"),
                        int(20),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
                ExprKind::Begin(Box::new(Begin::new(
                    vec![ExprKind::List(List::new(thin_vec![
                        atom("+"),
                        atom("z"),
                        int(30),
                    ]))],
                    SyntaxObject::default(TokenType::Begin),
                ))),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        let expected = ExprKind::Begin(Box::new(Begin::new(
            vec![
                ExprKind::List(List::new(thin_vec![atom("+"), atom("x"), int(10)])),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("y"), int(20)])),
                ExprKind::List(List::new(thin_vec![atom("+"), atom("z"), int(30)])),
            ],
            SyntaxObject::default(TokenType::Begin),
        )));

        FlattenBegin::flatten(&mut expr);

        assert_eq!(expr, expected);
    }
}

#[cfg(test)]
mod written_name_tests {
    use super::written_name;

    #[test]
    fn renames_are_undone_and_written_names_kept() {
        assert_eq!(written_name("##x#12"), "x");
        assert_eq!(written_name("##x#12#3"), "x");
        assert_eq!(written_name("##mm5__%#__x"), "x");
        assert_eq!(written_name("####mm5__%#__x#12"), "x");
        assert_eq!(written_name("x#1"), "x#1");
        assert_eq!(written_name("####tmp2#2147485264"), "tmp2");
    }
}
