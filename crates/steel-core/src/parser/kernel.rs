use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use compact_str::CompactString;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

#[cfg(feature = "sync")]
use once_cell::sync::Lazy;
use steel_parser::ast::{QUASIQUOTE, QUOTE, RAW_QUOTE};
use steel_parser::tokens::TokenType;

use crate::{
    compiler::{
        modules::{MANGLER_PREFIX, MANGLER_SEPARATOR},
        passes::mangle::{is_unquote, NameMangler},
        passes::{analysis::SemanticAnalysis, VisitorMutRefUnit, VisitorMutUnitRef},
        program::{BEGIN, BEGIN_FOR_SYNTAX, DEFMACRO},
    },
    parser::{
        ast::{Atom, Define, LambdaFunction, Let, Quote, Set},
        expand_visitor::{is_define, is_lambda_head, is_let_head},
        parser::{ExpansionMark, SyntaxObject, SyntaxObjectId},
    },
    rvals::{Result, SteelString},
    steel_vm::builtin::BuiltInModule,
    steel_vm::register_fn::RegisterFn,
    values::lists::List,
};
use crate::{stdlib::KERNEL, steel_vm::engine::Engine, SteelVal};

use steel_parser::expr_list;
use thin_vec::{thin_vec, ThinVec};

use super::{
    ast::{ExprKind, TryFromSteelValVisitorForExprKind},
    expand_visitor::GlobalMap,
    expander::SteelMacro,
    interner::InternedString,
    span_visitor::get_span,
    visitors::VisitorMutRef,
};

thread_local! {
    pub(crate) static KERNEL_IMAGE: Engine = Engine::new_bootstrap_kernel(false);
    pub(crate) static KERNEL_IMAGE_SB: Engine = Engine::new_bootstrap_kernel(true);
}

#[cfg(feature = "sync")]
pub static STATIC_KERNEL_IMAGE: Lazy<Engine> = Lazy::new(|| Engine::new_bootstrap_kernel(false));
#[cfg(feature = "sync")]
pub static STATIC_KERNEL_IMAGE_SB: Lazy<Engine> = Lazy::new(|| Engine::new_bootstrap_kernel(true));

pub(crate) fn fresh_kernel_image(sandbox: bool) -> Engine {
    // Just deep clone the env coming out

    if sandbox {
        #[cfg(feature = "sync")]
        {
            STATIC_KERNEL_IMAGE_SB.deep_clone()
        }

        #[cfg(not(feature = "sync"))]
        {
            KERNEL_IMAGE_SB.with(|x| x.deep_clone())
        }
    } else {
        #[cfg(feature = "sync")]
        {
            STATIC_KERNEL_IMAGE.deep_clone()
        }

        #[cfg(not(feature = "sync"))]
        {
            KERNEL_IMAGE.with(|x| x.deep_clone())
        }
    }
}

type TransformerMap = FxHashMap<String, FxHashSet<InternedString>>;

// Internal set of transformers that we'll embed
#[derive(Clone, Debug)]
struct Transformers {
    set: Arc<RwLock<TransformerMap>>,
    // The names each environment's transformer code can use from `begin-for-syntax`, its own
    // or imported. Their values are in the environment's phase 1 table.
    phase1: Arc<RwLock<FxHashMap<String, Vec<InternedString>>>>,
    // What builds each module's transformer environment, to build fresh ones from
    programs: Arc<RwLock<FxHashMap<String, Phase1Program>>>,
}

/// The code that builds a module's transformer environment. A module that uses its macros
/// gets an instance of its own, made from this, as in Racket.
#[derive(Clone, Debug, Default)]
struct Phase1Program {
    // The code of the modules it requires for syntax, which each instance runs afresh
    // under its own names for those modules' prefixes
    for_syntax: Vec<ExprKind>,
    for_syntax_prefixes: Vec<String>,
    imports: Vec<Phase1Import>,
    // `begin-for-syntax` and `defmacro` forms
    forms: Vec<ExprKind>,
    defmacros: Vec<InternedString>,
    // The names its transformer code can see, its own and imported
    names: Vec<InternedString>,
    // Each syntax-case macro, with its pattern matcher and body
    transformers: Vec<(InternedString, String, ExprKind)>,
    // The variables of other environments it imports, by the globals its code names them
    imported: FxHashMap<InternedString, InternedString>,
}

/// A binding that a module's transformer code imports
#[derive(Clone, Debug)]
pub(crate) struct Phase1Import {
    /// The name the importing module uses
    pub(crate) local: InternedString,
    /// Where its value comes from in the kernel
    pub(crate) value: ExprKind,
    /// For a `begin-for-syntax` binding, its environment and name
    pub(crate) source: Option<(String, InternedString)>,
}

impl Phase1Import {
    /// A `begin-for-syntax` binding of another environment
    pub(crate) fn for_syntax_binding(
        local: InternedString,
        environment: &str,
        name: InternedString,
    ) -> Self {
        Self {
            source: Some((environment.to_owned(), name)),
            ..Self::lookup(local, "hash-ref", &phase1_table(environment), name)
        }
    }

    /// A binding provided by a module instantiated in the kernel
    pub(crate) fn provided(
        local: InternedString,
        module_table: &str,
        name: InternedString,
    ) -> Self {
        Self::lookup(local, "%proto-hash-get%", module_table, name)
    }

    fn lookup(local: InternedString, getter: &str, table: &str, name: InternedString) -> Self {
        Self {
            local,
            source: None,
            value: expr_list![
                ExprKind::ident(getter),
                ExprKind::ident(table),
                ExprKind::Quote(Box::new(Quote::new(
                    ExprKind::atom(name),
                    SyntaxObject::default(TokenType::Quote)
                ))),
            ],
        }
    }
}

// Expands the macros of modules required for syntax in transformer code, leaving the code
// in templates and quoted data, which is for the phase the macros expand into
struct Phase1Expander<'a> {
    macros: &'a FxHashMap<InternedString, SteelMacro>,
    // Procedural macros from modules required for syntax, which the kernel expands
    procedural: &'a FxHashMap<InternedString, (String, InternedString)>,
    kernel: Option<&'a mut Kernel>,
    // The environment's phase 1 variables, with the kernel globals they are
    renames: &'a FxHashMap<InternedString, InternedString>,
    // The variables in scope, which hide macros of the same name. The first scope has
    // the phase 1 variables.
    locals: Vec<FxHashSet<InternedString>>,
    // How many syntax-case clauses and with-syntax bodies this is in
    patterns: usize,
    // How many macro expansions this is in
    depth: usize,
}

impl Phase1Expander<'_> {
    fn bind(&mut self, name: &ExprKind) {
        if let (Some(name), Some(scope)) = (name.atom_identifier(), self.locals.last_mut()) {
            scope.insert(*name);
        }
    }

    // A definition of a phase 1 variable defines its global
    fn define(&mut self, name: &mut ExprKind) {
        self.bind(name);

        if self.locals.len() == 1 {
            self.rename(name);
        }
    }

    fn rename(&mut self, expr: &mut ExprKind) {
        if let ExprKind::Atom(a) = expr {
            let _ = self.visit_atom(a);
        }
    }

    // Visits a function's body with its parameters bound. An optional parameter's default
    // sees the parameters before it.
    fn function(&mut self, params: &mut [ExprKind], body: &mut [ExprKind]) -> Result<()> {
        self.locals.push(FxHashSet::default());

        let result = self.function_scope(params, body);

        self.locals.pop();
        result
    }

    fn function_scope(&mut self, params: &mut [ExprKind], body: &mut [ExprKind]) -> Result<()> {
        for param in params.iter_mut() {
            match param {
                ExprKind::List(optional) => {
                    for default in optional.args.iter_mut().skip(1) {
                        self.visit(default)?;
                    }

                    if let Some(name) = optional.args.first() {
                        self.bind(name);
                    }
                }
                param => self.bind(param),
            }
        }

        self.bind_body_defines(body);

        body.iter_mut().try_for_each(|x| self.visit(x))
    }

    fn bind_body_defines(&mut self, body: &[ExprKind]) {
        let mut defined = Vec::new();

        for expr in body {
            phase1_definitions(expr, &mut defined);
        }

        if let Some(scope) = self.locals.last_mut() {
            scope.extend(defined);
        }
    }

    fn is_phase1_local(&self, name: &InternedString) -> bool {
        self.locals.iter().skip(1).any(|x| x.contains(name))
    }

    // Visits `body` with `names` bound, and the names it defines, which are in scope in
    // all of it
    fn scoped(&mut self, names: &[ExprKind], body: &mut [ExprKind]) -> Result<()> {
        self.locals.push(FxHashSet::default());

        for name in names {
            match name {
                ExprKind::List(l) => l.args.iter().for_each(|x| self.bind(x)),
                name => self.bind(name),
            }
        }

        self.bind_body_defines(body);

        let result = body.iter_mut().try_for_each(|x| self.visit(x));
        self.locals.pop();
        result
    }

    // `(let name? ([x e] ...) body ...)` and the forms like it
    fn visit_let_form(&mut self, l: &mut super::ast::List, recursive: bool) -> Result<()> {
        let named = matches!(l.args.get(1), Some(ExprKind::Atom(_)));
        let bindings_at = if named { 2 } else { 1 };

        let mut names: Vec<ExprKind> = l
            .args
            .get(1)
            .filter(|_| named)
            .cloned()
            .into_iter()
            .collect();

        if let Some(ExprKind::List(bindings)) = l.args.get(bindings_at) {
            names.extend(
                bindings
                    .args
                    .iter()
                    .filter_map(|x| x.list().and_then(|x| x.args.first()).cloned()),
            );
        }

        if recursive {
            self.locals.push(FxHashSet::default());
            names.iter().for_each(|x| self.bind(x));
        }

        if let Some(ExprKind::List(bindings)) = l.args.get_mut(bindings_at) {
            for binding in bindings.args.iter_mut() {
                if let ExprKind::List(binding) = binding {
                    for value in binding.args.iter_mut().skip(1) {
                        self.visit(value)?;
                    }
                }
            }
        }

        if recursive {
            self.locals.pop();
        }

        let body = l.args.get_mut(bindings_at + 1..).unwrap_or_default();
        self.scoped(&names, body)
    }

    fn is_local(&self, name: &InternedString) -> bool {
        self.locals.iter().any(|x| x.contains(name))
    }

    fn visit_template(&mut self, l: &mut super::ast::List) -> Result<()> {
        for expr in l.args.iter_mut() {
            match expr {
                ExprKind::List(inner)
                    if form_name(inner).is_some_and(|x| {
                        UNQUOTE_FORMS.contains(&x) || UNSYNTAX_FORMS.contains(&x)
                    }) =>
                {
                    for expr in inner.args.iter_mut().skip(1) {
                        self.visit(expr)?;
                    }
                }
                ExprKind::List(inner) => self.visit_template(inner)?,
                // The holes in quoted data and vectors are code too
                ExprKind::Quote(q) => self.visit_unsyntax_holes(&mut q.expr)?,
                ExprKind::Vector(v) => v
                    .args
                    .iter_mut()
                    .try_for_each(|x| self.visit_unsyntax_holes(x))?,
                _ => {}
            }
        }

        Ok(())
    }
}

// Evaluates the form with no pattern variables bound, as it has none around it
fn without_patterns(l: &mut super::ast::List) {
    let form = core::mem::replace(l, super::ast::List::new(ThinVec::new()));
    *l = super::ast::List::new(thin_vec![
        ExprKind::ident("#%syntax-without-patterns"),
        ExprKind::List(form),
    ]);
}

// The module table a module's code defines, which is how it's found again
fn module_table_defined_by(expr: &ExprKind) -> Option<String> {
    use crate::compiler::modules::MODULE_PREFIX;

    let forms: &[ExprKind] = match expr {
        ExprKind::Begin(b) => &b.exprs,
        expr => core::slice::from_ref(expr),
    };

    forms.iter().find_map(|form| {
        let name = match form {
            ExprKind::Define(d) => d.name.atom_identifier(),
            ExprKind::List(l) if l.args.len() == 3 && is_define(l) => l.args[1].atom_identifier(),
            _ => None,
        }?;

        name.resolve()
            .starts_with(MODULE_PREFIX)
            .then(|| name.resolve().to_owned())
    })
}

// Whether code builds syntax from a template, which reads the pattern variables
fn has_template(expr: &ExprKind) -> bool {
    match expr {
        ExprKind::List(l) => {
            matches!(
                form_name(l),
                Some("#%expand-syntax-case" | "#%quasisyntax" | "syntax" | "quasisyntax")
            ) || l.args.iter().any(has_template)
        }
        ExprKind::LambdaFunction(f) => has_template(&f.body),
        ExprKind::If(f) => {
            has_template(&f.test_expr) || has_template(&f.then_expr) || has_template(&f.else_expr)
        }
        ExprKind::Begin(b) => b.exprs.iter().any(has_template),
        ExprKind::Let(l) => {
            l.bindings.iter().any(|(_, x)| has_template(x)) || has_template(&l.body_expr)
        }
        ExprKind::Define(d) => has_template(&d.body),
        ExprKind::Set(s) => has_template(&s.expr),
        ExprKind::Return(r) => has_template(&r.expr),
        _ => false,
    }
}

// A function made in a clause reads the pattern variables of that clause when it runs,
// wherever it's called from
fn keeps_pattern_variables(l: &mut super::ast::List) {
    let lambda = core::mem::replace(l, super::ast::List::new(ThinVec::new()));
    *l = super::ast::List::new(thin_vec![
        ExprKind::ident("#%syntax-closure"),
        ExprKind::List(lambda),
    ]);
}

const QUOTE_FORMS: &[&str] = &["quote", "#%quote", "syntax", "quote-syntax"];
const QUASI_FORMS: &[&str] = &["quasiquote", "quasisyntax"];

// The forms the reader makes from `,` and `#,`. Written out as `unquote` and the like they
// are data, as quasiquote and quasisyntax treat them.
const UNQUOTE_FORMS: &[&str] = &["#%unquote", "#%unquote-splicing"];
const UNSYNTAX_FORMS: &[&str] = &["#%unsyntax", "#%unsyntax-splicing"];

// The name of a list's head, including the forms the reader makes from `'`, `#'` and the
// like, which aren't identifiers
pub(crate) fn form_name(l: &super::ast::List) -> Option<&str> {
    let head = l.first().and_then(ExprKind::atom_syntax_object)?;

    match &head.ty {
        TokenType::Identifier(name) => Some(name.resolve()),
        TokenType::Begin => Some("begin"),
        TokenType::Quote | TokenType::QuoteTick => Some("quote"),
        TokenType::QuasiQuote => Some("quasiquote"),
        TokenType::Unquote => Some("#%unquote"),
        TokenType::UnquoteSplice => Some("#%unquote-splicing"),
        TokenType::QuoteSyntax => Some("syntax"),
        TokenType::QuasiQuoteSyntax => Some("quasisyntax"),
        TokenType::UnquoteSyntax => Some("#%unsyntax"),
        TokenType::UnquoteSpliceSyntax => Some("#%unsyntax-splicing"),
        _ => None,
    }
}

impl Phase1Expander<'_> {
    // What a macro made, which can make more forever
    fn visit_expansion(
        &mut self,
        expanded: &mut ExprKind,
        span: steel_parser::span::Span,
    ) -> Result<()> {
        if self.depth >= crate::parser::expand_visitor::MAX_EXPANSION_DEPTH {
            stop!(BadSyntax => "macro expansion doesn't finish"; span);
        }

        self.depth += 1;
        let result = self.visit(expanded);
        self.depth -= 1;
        result
    }

    fn visit_unsyntax_holes(&mut self, expr: &mut ExprKind) -> Result<()> {
        match expr {
            // Quoted, `#,` reads as `unsyntax`
            ExprKind::List(l)
                if form_name(l).is_some_and(|x| {
                    UNSYNTAX_FORMS.contains(&x) || x == "unsyntax" || x == "unsyntax-splicing"
                }) =>
            {
                l.args.iter_mut().skip(1).try_for_each(|x| self.visit(x))
            }
            ExprKind::List(l) => l
                .args
                .iter_mut()
                .try_for_each(|x| self.visit_unsyntax_holes(x)),
            ExprKind::Vector(v) => v
                .args
                .iter_mut()
                .try_for_each(|x| self.visit_unsyntax_holes(x)),
            ExprKind::Quote(q) => self.visit_unsyntax_holes(&mut q.expr),
            _ => Ok(()),
        }
    }
}

impl VisitorMutRef for Phase1Expander<'_> {
    type Output = Result<()>;

    fn visit(&mut self, expr: &mut ExprKind) -> Result<()> {
        crate::compiler::passes::with_stack(|| {
            if let ExprKind::List(l) = expr {
                let head = l.first().and_then(ExprKind::atom_syntax_object);

                let template = head.is_some_and(|x| x.unresolved);

                if let Some((name, span)) =
                    head.and_then(|x| x.ty.identifier().map(|name| (*name, x.span)))
                {
                    // A local of the same name hides the macro, unless a template wrote it
                    let hidden = self.is_local(&name) && !template;

                    if let Some((environment, real)) =
                        self.procedural.get(&name).filter(|_| !hidden).cloned()
                    {
                        if let Some(kernel) = self.kernel.as_deref_mut() {
                            let list = core::mem::replace(l, super::ast::List::new(ThinVec::new()));
                            let mut expanded = kernel.expand_in_environment(
                                &real,
                                ExprKind::List(list),
                                &environment,
                            )?;
                            self.visit_expansion(&mut expanded, span)?;
                            *expr = expanded;
                            return Ok(());
                        }
                    }

                    // What they expand to names their module's macros by its prefix
                    if let Some(kernel) = self.kernel.as_deref_mut() {
                        if let Some(mut expanded) = kernel.expand_prefixed_macro(&name, l, None)? {
                            self.visit_expansion(&mut expanded, span)?;
                            *expr = expanded;
                            return Ok(());
                        }
                    }

                    if let Some(m) = self.macros.get(&name).filter(|_| !hidden) {
                        let list = core::mem::replace(l, super::ast::List::new(ThinVec::new()));
                        let mut expanded = m.expand(
                            list,
                            span,
                            &Default::default(),
                            GlobalMap::Set(&Default::default()),
                        )?;
                        self.visit_expansion(&mut expanded, span)?;
                        *expr = expanded;
                        return Ok(());
                    }
                }

                return self.visit_list(l);
            }

            match expr {
                ExprKind::Atom(a) => self.visit_atom(a),
                ExprKind::If(f) => self.visit_if(f),
                ExprKind::Define(d) => self.visit_define(d),
                ExprKind::LambdaFunction(l) => {
                    self.visit_lambda_function(l)?;

                    if self.patterns > 0 && has_template(&l.body) {
                        let lambda = core::mem::replace(expr, ExprKind::empty());
                        *expr = ExprKind::List(super::ast::List::new(thin_vec![
                            ExprKind::ident("#%syntax-closure"),
                            lambda,
                        ]));
                    }

                    Ok(())
                }
                ExprKind::Begin(b) => self.visit_begin(b),
                ExprKind::Return(r) => self.visit_return(r),
                ExprKind::Let(l) => self.visit_let(l),
                ExprKind::Set(s) => self.visit_set(s),
                ExprKind::Vector(v) => self.visit_vector(v),
                _ => Ok(()),
            }
        })
    }

    fn visit_if(&mut self, f: &mut super::ast::If) -> Result<()> {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)
    }

    fn visit_define(&mut self, define: &mut Define) -> Result<()> {
        self.define(&mut define.name);
        self.visit(&mut define.body)
    }

    fn visit_lambda_function(&mut self, lambda: &mut LambdaFunction) -> Result<()> {
        let args = lambda.args.to_vec();
        self.scoped(&args, core::slice::from_mut(&mut lambda.body))
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Result<()> {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Result<()> {
        self.visit(&mut r.expr)
    }

    // A quote in a quasisyntax template is data, apart from its `#,` holes
    fn visit_quote(&mut self, quote: &mut Quote) -> Result<()> {
        self.visit_unsyntax_holes(&mut quote.expr)
    }

    fn visit_macro(&mut self, _m: &mut super::ast::Macro) -> Result<()> {
        Ok(())
    }

    fn visit_atom(&mut self, a: &mut Atom) -> Result<()> {
        if let TokenType::Identifier(name) = &mut a.syn.ty {
            if !self.is_phase1_local(name) {
                if let Some(global) = self.renames.get(name) {
                    *name = *global;
                }
            }
        }

        Ok(())
    }

    fn visit_list(&mut self, l: &mut super::ast::List) -> Result<()> {
        match form_name(l) {
            // Pattern variables are only in scope in the clauses that bind them, not in
            // what those call
            Some("#%expand-syntax-case" | "#%quasisyntax" | "syntax" | "quasisyntax")
                if self.patterns == 0 =>
            {
                let mut template =
                    ExprKind::List(core::mem::replace(l, super::ast::List::new(ThinVec::new())));

                self.patterns += 1;
                let result = self.visit(&mut template);
                self.patterns -= 1;

                *l = super::ast::List::new(thin_vec![
                    ExprKind::ident("#%syntax-without-patterns"),
                    template,
                ]);

                result
            }
            Some("#%syntax-without-patterns") => Ok(()),
            Some(head) if QUOTE_FORMS.contains(&head) => l
                .args
                .iter_mut()
                .skip(1)
                .try_for_each(|x| self.visit_unsyntax_holes(x)),
            Some(head) if QUASI_FORMS.contains(&head) => self.visit_template(l),
            // The patterns of each clause aren't code. The expander leaves `syntax-case` alone,
            // so it's renamed to the kernel's macro for one in an expression. The define-syntax
            // takes apart the one it ends with whatever its name.
            Some("syntax-case" | "#%syntax-case") => {
                let outside = self.patterns == 0;

                if let Some(head) = l.args.first_mut() {
                    *head = ExprKind::ident("#%syntax-case");
                }

                if let Some(expr) = l.args.get_mut(1) {
                    self.visit(expr)?;
                }

                for clause in l.args.iter_mut().skip(3) {
                    if let ExprKind::List(clause) = clause {
                        let mut variables = Vec::new();

                        if let Some(pattern) = clause.args.first() {
                            pattern_variables(pattern, &mut variables);
                        }

                        let body = clause.args.get_mut(1..).unwrap_or_default();
                        self.patterns += 1;
                        let result = self.scoped(&variables, body);
                        self.patterns -= 1;
                        result?;
                    }
                }

                // Outside any other clause, what it binds is all its clauses see, also when
                // code in another clause calls it
                if outside {
                    without_patterns(l);
                }

                Ok(())
            }
            // A template is expanded where the macro is used
            Some("syntax-rules") => Ok(()),
            // The datums of each clause aren't code
            Some("case") => {
                if let Some(key) = l.args.get_mut(1) {
                    self.visit(key)?;
                }

                for clause in l.args.iter_mut().skip(2) {
                    match clause {
                        ExprKind::List(clause) => clause
                            .args
                            .iter_mut()
                            .skip(1)
                            .try_for_each(|x| self.visit(x))?,
                        clause => self.visit(clause)?,
                    }
                }

                Ok(())
            }
            // Its patterns bind pattern variables in its body
            Some("with-syntax") => {
                let outside = self.patterns == 0;
                let mut variables = Vec::new();

                if let Some(ExprKind::List(bindings)) = l.args.get_mut(1) {
                    for binding in bindings.args.iter_mut() {
                        if let ExprKind::List(binding) = binding {
                            if let Some(pattern) = binding.args.first() {
                                pattern_variables(pattern, &mut variables);
                            }

                            for value in binding.args.iter_mut().skip(1) {
                                self.visit(value)?;
                            }
                        }
                    }
                }

                let body = l.args.get_mut(2..).unwrap_or_default();
                self.patterns += 1;
                let result = self.scoped(&variables, body);
                self.patterns -= 1;

                if outside {
                    without_patterns(l);
                }

                result
            }
            _ if l.first().is_some_and(is_let_head) => self.visit_let_form(l, false),
            Some("let*" | "%plain-let") => self.visit_let_form(l, false),
            Some("letrec" | "letrec*") => self.visit_let_form(l, true),
            _ if l.first().is_some_and(is_lambda_head) => {
                let split = 2.min(l.args.len());
                let (head, body) = l.args.split_at_mut(split);

                let result = match head.get_mut(1) {
                    Some(ExprKind::List(params)) => self.function(&mut params.args, body),
                    Some(rest) => self.function(core::slice::from_mut(rest), body),
                    None => Ok(()),
                };

                if self.patterns > 0 && l.args.iter().skip(2).any(has_template) {
                    keeps_pattern_variables(l);
                }

                result
            }
            _ if is_define(l) => match l.args.get(1).cloned() {
                // A function's name is in scope where it's defined, its parameters in its body
                Some(ExprKind::List(_)) => {
                    let split = 2.min(l.args.len());
                    let (head, body) = l.args.split_at_mut(split);

                    let Some(ExprKind::List(f)) = head.get_mut(1) else {
                        return Ok(());
                    };

                    if let Some(name) = f.args.first_mut() {
                        self.define(name);
                    }

                    self.function(f.args.get_mut(1..).unwrap_or_default(), body)
                }
                Some(_) => {
                    if let Some(name) = l.args.get_mut(1) {
                        self.define(name);
                    }

                    l.args
                        .get_mut(2..)
                        .unwrap_or_default()
                        .iter_mut()
                        .try_for_each(|x| self.visit(x))
                }
                None => Ok(()),
            },
            // Racket's `datum->syntax`, where a template joins identifiers with it
            Some("datum->syntax")
                if l.args.len() == 3 && !self.is_local(&"datum->syntax".into()) =>
            {
                if let Some(ExprKind::Atom(head)) = l.args.first_mut() {
                    head.syn.ty = TokenType::Identifier("#%datum->syntax".into());
                }

                for expr in l.args.iter_mut().skip(1) {
                    self.visit(expr)?;
                }

                Ok(())
            }
            _ => {
                for expr in l.args.iter_mut() {
                    self.visit(expr)?;
                }

                Ok(())
            }
        }
    }

    fn visit_syntax_rules(&mut self, _l: &mut super::ast::SyntaxRules) -> Result<()> {
        Ok(())
    }

    fn visit_set(&mut self, s: &mut Set) -> Result<()> {
        self.rename(&mut s.variable);
        self.visit(&mut s.expr)
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Result<()> {
        Ok(())
    }

    fn visit_let(&mut self, l: &mut Let) -> Result<()> {
        for (_, expr) in l.bindings.iter_mut() {
            self.visit(expr)?;
        }

        let names: Vec<ExprKind> = l.bindings.iter().map(|(name, _)| name.clone()).collect();
        self.scoped(&names, core::slice::from_mut(&mut l.body_expr))
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Result<()> {
        for expr in v.args.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())
    }
}

// Where an environment's phase 1 bindings are kept in the kernel
fn phase1_table(environment: &str) -> String {
    format!("#%phase1:{environment}")
}

fn instance_value(name: &str) -> String {
    format!("#%phase1-instance:{name}")
}

fn quoted(name: InternedString) -> ExprKind {
    ExprKind::Quote(Box::new(Quote::new(
        ExprKind::atom(name),
        SyntaxObject::default(TokenType::Quote),
    )))
}

// The top level code of a fresh transformer environment for a module, which defines
// `value` as its macros and the bindings its transformer code sees. Its phase 1 variables
// are named for the environment, and renamed for the instance after.
fn instance_code(
    environment: &str,
    instance: &str,
    value: &str,
    program: &Phase1Program,
) -> ThinVec<ExprKind> {
    let mut code = ThinVec::new();

    for import in program.imports.iter().filter(|x| x.source.is_none()) {
        code.push(expr_list![
            ExprKind::ident("define"),
            ExprKind::atom(phase1_variable(environment, import.local)),
            import.value.clone(),
        ]);
    }

    let is_defmacro = |x: &ExprKind| x.list().and_then(|l| l.first_ident()) == Some(&*DEFMACRO);

    code.extend(program.forms.iter().filter(|x| !is_defmacro(x)).cloned());

    let mut macros = thin_vec![ExprKind::ident("hash")];

    for name in &program.defmacros {
        macros.push(quoted(*name));
        macros.push(ExprKind::atom(*name));
    }

    for (index, (name, matcher, transformer)) in program.transformers.iter().enumerate() {
        let body_name = format!("#%transformer:{instance}:{index}");

        code.push(expr_list![
            ExprKind::ident("define"),
            ExprKind::ident(&body_name),
            transformer.clone(),
        ]);

        macros.push(quoted(*name));
        macros.push(expr_list![
            ExprKind::ident("#%syntax-case-transformer"),
            ExprKind::string_lit(matcher.clone()),
            ExprKind::ident(&body_name),
        ]);
    }

    let mut names = thin_vec![ExprKind::ident("hash")];

    for name in &program.names {
        let global = program
            .imported
            .get(name)
            .copied()
            .unwrap_or_else(|| phase1_variable(environment, *name));

        names.push(quoted(*name));
        names.push(ExprKind::atom(global));
    }

    // Its defmacros are locals, since they're named as the module names them
    let mut body = thin_vec![
        ExprKind::ident("let"),
        ExprKind::List(super::ast::List::new(ThinVec::new()))
    ];

    body.extend(program.forms.iter().filter(|x| is_defmacro(x)).cloned());

    body.push(expr_list![
        ExprKind::ident("hash"),
        quoted("macros".into()),
        ExprKind::List(super::ast::List::new(macros)),
        quoted("phase1".into()),
        ExprKind::List(super::ast::List::new(names)),
    ]);

    code.push(expr_list![
        ExprKind::ident("define"),
        ExprKind::ident(value),
        ExprKind::List(super::ast::List::new(body)),
    ]);

    remove_unused_imports(&mut code);

    code
}

// A module's code defines every name it imports, and the compiler drops the unused ones at
// the top level. An instance would look them all up, some in modules the sandbox leaves
// out, so the unused ones go here.
fn remove_unused_imports(body: &mut ThinVec<ExprKind>) {
    fn is_import(expr: &ExprKind) -> Option<InternedString> {
        let ExprKind::Define(d) = expr else {
            return None;
        };

        let getter = d.body.list().and_then(|x| x.first_ident())?;

        matches!(getter.resolve(), "%module-get%" | "%proto-hash-get%")
            .then(|| d.name.atom_identifier().copied())
            .flatten()
    }

    struct Used(FxHashSet<InternedString>);

    impl VisitorMutRefUnit for Used {
        fn visit_atom(&mut self, a: &mut Atom) {
            if let Some(name) = a.syn.ty.identifier() {
                self.0.insert(*name);
            }
        }

        fn visit_define(&mut self, define: &mut Define) {
            self.visit(&mut define.body);
        }
    }

    fn prune(exprs: &mut Vec<ExprKind>, used: &FxHashSet<InternedString>) {
        exprs.retain(|x| is_import(x).is_none_or(|name| used.contains(&name)));

        for expr in exprs.iter_mut() {
            if let ExprKind::Begin(b) = expr {
                prune(&mut b.exprs, used);
            }
        }
    }

    let mut used = Used(FxHashSet::default());

    for expr in body.iter_mut() {
        used.visit(expr);
    }

    let mut exprs: Vec<ExprKind> = core::mem::take(body).into_iter().collect();
    prune(&mut exprs, &used.0);
    *body = exprs.into_iter().collect();
}

// The code in quoted data: what an unquote or unsyntax in a template evaluates
fn visit_holes<V: VisitorMutRefUnit>(visitor: &mut V, expr: &mut ExprKind) {
    match expr {
        ExprKind::List(l) => {
            let hole = form_name(l)
                .is_some_and(|x| UNQUOTE_FORMS.contains(&x) || UNSYNTAX_FORMS.contains(&x));
            if hole {
                l.args.iter_mut().skip(1).for_each(|x| visitor.visit(x));
            } else {
                l.args.iter_mut().for_each(|x| visit_holes(visitor, x));
            }
        }
        ExprKind::Vector(v) => v.args.iter_mut().for_each(|x| visit_holes(visitor, x)),
        ExprKind::Quote(q) => visit_holes(visitor, &mut q.expr),
        _ => {}
    }
}

// Gives identifiers other names
struct RenameExactly<'a> {
    names: &'a FxHashMap<InternedString, InternedString>,
}

impl VisitorMutRefUnit for RenameExactly<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        if let TokenType::Identifier(name) = &mut a.syn.ty {
            if let Some(new) = self.names.get(name) {
                *name = *new;
            }
        }
    }

    fn visit_quote(&mut self, quote: &mut Quote) {
        visit_holes(self, &mut quote.expr);
    }
}

/// Gives the identifiers of a module instance's code its instance's names
pub(crate) struct RenameInstance<'a> {
    pub(crate) prefixes: &'a [(String, String)],
}

impl VisitorMutRefUnit for RenameInstance<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        if let TokenType::Identifier(name) = &mut a.syn.ty {
            let resolved = name.resolve();

            if let Some((from, to)) = self
                .prefixes
                .iter()
                .find(|(from, _)| resolved.contains(from.as_str()))
            {
                *name = resolved.replacen(from.as_str(), to, 1).into();
            }
        }
    }

    // The holes of a template are the instance's code too
    fn visit_quote(&mut self, quote: &mut Quote) {
        visit_holes(self, &mut quote.expr);
    }
}

// The pattern variables of a `syntax-case` pattern
fn pattern_variables(pattern: &ExprKind, variables: &mut Vec<ExprKind>) {
    match pattern {
        ExprKind::Atom(_) => variables.push(pattern.clone()),
        ExprKind::List(l) => l.args.iter().for_each(|x| pattern_variables(x, variables)),
        ExprKind::Vector(v) => v.args.iter().for_each(|x| pattern_variables(x, variables)),
        _ => {}
    }
}

// The kernel global a phase 1 variable of an environment is
fn phase1_variable(environment: &str, name: InternedString) -> InternedString {
    format!("#%phase1-var:{environment}:{name}").into()
}

// The names a `begin-for-syntax` form defines
fn phase1_definitions(expr: &ExprKind, names: &mut Vec<InternedString>) {
    if let ExprKind::Define(d) = expr {
        names.extend(d.name.atom_identifier());
        return;
    }

    if let ExprKind::Begin(b) = expr {
        b.exprs.iter().for_each(|x| phase1_definitions(x, names));
        return;
    }

    let Some(l) = expr.list() else {
        return;
    };

    if is_define(l) {
        let name = match l.get(1) {
            Some(ExprKind::List(f)) => f.args.first(),
            name => name,
        };

        names.extend(name.and_then(ExprKind::atom_identifier));
    } else if form_name(l) == Some("begin") {
        l.args
            .iter()
            .skip(1)
            .for_each(|x| phase1_definitions(x, names));
    }
}

/// The Kernel is an engine context used to evaluate defmacro style macros
/// It lives inside the compiler, so in theory there could be tiers of kernels
/// Note: this will be called along side syntax rules style macros. In this case macro
/// expansion then is a one step process, dispatching on the macros. However, at the moment this
/// will not support user defined def macros -> this kernel will only support the built in def macros
/// for structs.
#[derive(Clone)]
pub struct Kernel {
    transformers: Transformers,
    constants: FxHashSet<InternedString>,
    pub(crate) engine: Box<Engine>,
    // The modules instantiated once and shared by all transformer code
    shared_instances: FxHashSet<PathBuf>,
    // Whether modules required for syntax can use the host's builtin modules
    pub(crate) share_host_modules: bool,
    // The modules being compiled, innermost last. With none, it's the top level.
    compiling: Vec<String>,
    // The transformer environment each module being compiled uses for another's macros
    instances: FxHashMap<(String, String), String>,
    // How many instances have been made, which name their copies of modules
    instance_count: usize,
    // The tag the modules required for syntax get while a module is compiled, so the
    // instances made for it share them, as Racket's do
    compile_tags: FxHashMap<String, String>,
    // The macros each environment's transformer code can use, from modules required for syntax
    phase1_macros: FxHashMap<String, FxHashMap<InternedString, SteelMacro>>,
    // The names the top level's instances of modules required for syntax have
    phase1_renames: FxHashMap<String, Vec<(String, String)>>,
    // The macros each module's templates name by its prefix
    macro_modules: FxHashMap<CompactString, MacroModule>,
    sandboxed: bool,
    // The macros the kernel defines itself, in the default environment
    builtin_macros: FxHashSet<InternedString>,
    // The macros defined in functions, with the locals where each is defined
    local_macros: FxHashMap<InternedString, FxHashSet<InternedString>>,
    // The phase 1 variables each environment imports, by the globals they are
    phase1_aliases: FxHashMap<String, FxHashMap<InternedString, InternedString>>,
    // The procedural macros each environment's transformer code can use, with the
    // environment and name they have where they're defined
    phase1_procedural: FxHashMap<String, FxHashMap<InternedString, (String, InternedString)>>,
}

// The builtin modules a sandboxed kernel leaves out
const SANDBOXED_MODULES: &[&str] = &["steel/tcp", "steel/http", "steel/polling"];

/// The macros a module's templates and procedural macros name by the module's prefix,
/// which the module defines or imports
#[derive(Clone, Debug)]
pub(crate) struct MacroModule {
    pub(crate) environment: String,
    pub(crate) mangler: NameMangler,
    pub(crate) rules: FxHashMap<InternedString, SteelMacro>,
    // The procedural ones, with the environment and name they have where they're defined
    pub(crate) procedural: FxHashMap<InternedString, (String, InternedString)>,
}

// What a kernel macro was given, so what it introduces can be told apart
struct MacroInput {
    // The marks the input was given, with the marks they replaced
    marks: FxHashMap<ExpansionMark, ExpansionMark>,
    // Set by syntax-case transformers, whose introduced identifiers are hygienic
    hygienic: bool,
}

thread_local! {
    static MACRO_INPUTS: RefCell<Vec<MacroInput>> = RefCell::new(Vec::new());
}

// Called by a syntax-case transformer on its input
pub(crate) fn hygienic_macro_input(input: SteelVal) -> SteelVal {
    MACRO_INPUTS.with(|inputs| {
        if let Some(input) = inputs.borrow_mut().last_mut() {
            input.hygienic = true;
        }
    });

    input
}

// Whether the identifier came from the input of the kernel macro being called
pub(crate) fn is_macro_input(mark: ExpansionMark) -> bool {
    MACRO_INPUTS.with(|inputs| {
        inputs
            .borrow()
            .last()
            .is_some_and(|input| input.marks.contains_key(&mark))
    })
}

// Where a macro's output binds names itself, which aren't references to globals
struct Binders<'a> {
    input: &'a MacroInput,
    binders: &'a mut FxHashSet<SyntaxObjectId>,
}

impl Binders<'_> {
    fn bind(&mut self, name: &ExprKind) {
        if let Some(syn) = name.atom_syntax_object() {
            if let TokenType::Identifier(_) = syn.ty {
                if !self.input.marks.contains_key(&syn.mark) {
                    self.binders.insert(syn.syntax_object_id);
                }
            }
        }
    }
}

impl<'a> VisitorMutUnitRef<'a> for Binders<'_> {
    fn visit_define(&mut self, define: &'a Define) {
        self.bind(&define.name);
        self.visit(&define.body);
    }

    fn visit_lambda_function(&mut self, lambda: &'a LambdaFunction) {
        lambda.args.iter().for_each(|x| self.bind(x));
        self.visit(&lambda.body);
    }

    fn visit_let(&mut self, l: &'a Let) {
        for (name, value) in &l.bindings {
            self.bind(name);
            self.visit(value);
        }

        self.visit(&l.body_expr);
    }

    fn visit_quote(&mut self, _quote: &'a Quote) {}

    // Binding forms that haven't been parsed yet
    fn visit_list(&mut self, l: &'a super::ast::List) {
        let head = l.first();

        if is_define(l) || head.is_some_and(is_lambda_head) {
            match l.get(1) {
                Some(ExprKind::List(params)) => params.args.iter().for_each(|x| self.bind(x)),
                Some(name) => self.bind(name),
                None => {}
            }
        } else if head.is_some_and(is_let_head) {
            let bindings = match l.get(1) {
                Some(name @ ExprKind::Atom(_)) => {
                    self.bind(name);
                    l.get(2)
                }
                bindings => bindings,
            };

            if let Some(ExprKind::List(bindings)) = bindings {
                for binding in &bindings.args {
                    if let Some(name) = binding.list().and_then(|x| x.args.first()) {
                        self.bind(name);
                    }
                }
            }
        }

        for expr in &l.args {
            self.visit(expr);
        }
    }
}

// Puts back the marks of what a kernel macro was given. A syntax-case transformer's
// introduced identifiers get a mark of their own, like those of a syntax-rules template,
// and a module's macro gives the ones it introduces the module's names.
struct Introduced<'a> {
    input: &'a MacroInput,
    mark: ExpansionMark,
    // Where the macro binds names itself. Its other identifiers are references, which
    // marks resolve to its binders or else the globals.
    defined: FxHashSet<SyntaxObjectId>,
    mangler: Option<&'a mut NameMangler>,
    quoted: usize,
    // Defined in a function, so its other identifiers can refer to the function's locals
    local: bool,
    macro_name: InternedString,
    // The locals where a macro defined in a function is defined, which what it introduces
    // can refer to
    captures: FxHashSet<InternedString>,
}

impl Introduced<'_> {
    fn resolve(&mut self, expr: &mut ExprKind) {
        let mut binders = Binders {
            input: self.input,
            binders: &mut self.defined,
        };
        VisitorMutUnitRef::visit(&mut binders, expr);

        self.visit(expr);
    }

    fn visit_quoted(&mut self, expr: &mut ExprKind) {
        self.quoted += 1;
        self.visit(expr);
        self.quoted -= 1;
    }
}

impl VisitorMutRefUnit for Introduced<'_> {
    fn visit_atom(&mut self, a: &mut Atom) {
        let TokenType::Identifier(_) = a.syn.ty else {
            return;
        };

        if let Some(mark) = self.input.marks.get(&a.syn.mark) {
            a.syn.mark = *mark;
            return;
        }

        if self.input.hygienic {
            a.syn.mark = self.mark;
            a.syn.unresolved = !self.local && !self.defined.contains(&a.syn.syntax_object_id);

            // The hygiene pass resolves it where the macro was defined
            if self.local && self.quoted == 0 && !self.defined.contains(&a.syn.syntax_object_id) {
                if let TokenType::Identifier(name) = &mut a.syn.ty {
                    if self.captures.contains(name) {
                        *name = crate::parser::expand_visitor::local_macro_capture(
                            &self.macro_name,
                            name,
                        );
                    }
                }
            }
        }

        if self.quoted == 0 {
            if let Some(mangler) = self.mangler.as_deref_mut() {
                mangler.visit_atom(a);
            }
        }
    }

    fn visit_quote(&mut self, quote: &mut Quote) {
        self.visit_quoted(&mut quote.expr);
    }

    // A syntax-rules macro it defines. What came from the input gets its marks back.
    fn visit_macro(&mut self, m: &mut super::ast::Macro) {
        struct RestoreInputMarks<'a>(&'a MacroInput);

        impl VisitorMutRefUnit for RestoreInputMarks<'_> {
            fn visit_atom(&mut self, a: &mut Atom) {
                if let Some(mark) = self.0.marks.get(&a.syn.mark) {
                    a.syn.mark = *mark;
                }
            }
        }

        let mut restore = RestoreInputMarks(self.input);

        // The name is the input's, or one the template wrote, which has the expansion's mark
        // like the uses it wrote
        match m.name.as_mut() {
            ExprKind::Atom(a) if !self.input.marks.contains_key(&a.syn.mark) => {
                if self.input.hygienic {
                    a.syn.mark = self.mark;
                }
            }
            name => restore.visit(name),
        }

        for pair in m.syntax_rules.patterns.iter_mut() {
            restore.visit(&mut pair.pattern);
            restore.visit(&mut pair.body);
        }
    }

    fn visit_list(&mut self, l: &mut super::ast::List) {
        let quoted = l
            .first()
            .and_then(ExprKind::atom_syntax_object)
            .is_some_and(|head| head.ty == TokenType::Quote)
            || l.first_ident()
                .is_some_and(|head| [*QUOTE, *RAW_QUOTE, *QUASIQUOTE].contains(head));

        let unquoted = l.first_ident().is_some_and(is_unquote);

        let outer = self.quoted;

        if quoted {
            self.quoted += 1;
        } else if unquoted && self.quoted > 0 {
            self.quoted -= 1;
        }

        for expr in &mut l.args {
            self.visit(expr);
        }

        self.quoted = outer;
    }
}

impl Default for Kernel {
    fn default() -> Self {
        Self::new()
    }
}

// Whether this is a begin-for-syntax, or a begin with one in it
pub(crate) fn has_phase1(expr: &ExprKind) -> bool {
    match expr {
        ExprKind::List(l) if l.first_ident() == Some(&*BEGIN_FOR_SYNTAX) => true,
        ExprKind::List(l) if crate::parser::expand_visitor::is_begin(l) => {
            l.args.iter().skip(1).any(has_phase1)
        }
        ExprKind::Begin(b) => b.exprs.iter().any(has_phase1),
        _ => false,
    }
}

// Top level begins are spliced, so a begin-for-syntax in one is at the top level
fn splice_phase1(expr: ExprKind, out: &mut Vec<ExprKind>) {
    match expr {
        ExprKind::List(l)
            if crate::parser::expand_visitor::is_begin(&l)
                && l.args.iter().skip(1).any(has_phase1) =>
        {
            l.args
                .into_iter()
                .skip(1)
                .for_each(|x| splice_phase1(x, out))
        }
        ExprKind::Begin(b) if b.exprs.iter().any(has_phase1) => {
            b.exprs.into_iter().for_each(|x| splice_phase1(x, out))
        }
        expr => out.push(expr),
    }
}

impl Kernel {
    pub fn new() -> Self {
        Self::with_sandbox(!cfg!(feature = "unsandboxed-kernel"))
    }

    /// Without the sandbox, transformer code can use the file system, processes and the
    /// network, as programs can.
    pub(crate) fn with_sandbox(sandbox: bool) -> Self {
        let mut engine = fresh_kernel_image(sandbox);

        let transformers = Transformers {
            set: Arc::new(RwLock::new(HashMap::default())),
            phase1: Arc::new(RwLock::new(FxHashMap::default())),
            programs: Arc::new(RwLock::new(FxHashMap::default())),
        };

        // A syntax-case macro of a module, for the instances other modules get
        let embedded_transformer_object = transformers.clone();
        engine.register_fn(
            "#%record-phase1-transformer!",
            move |env: String, name: SteelVal, matcher: String, body: SteelVal| -> Result<()> {
                if let Some(program) = embedded_transformer_object
                    .programs
                    .write()
                    .unwrap()
                    .get_mut(&env)
                {
                    if let SteelVal::SymbolV(name) = name {
                        let body = TryFromSteelValVisitorForExprKind::root_quoted(&body)?;
                        program
                            .transformers
                            .push((name.as_str().into(), matcher, body));
                    }
                }

                Ok(())
            },
        );

        // Code with no file, or a program's file, has its transformers in the top level
        let embedded_transformer_object = transformers.clone();
        engine.register_fn("#%phase1-environment", move |env: String| -> String {
            let phase1 = embedded_transformer_object.phase1.read().unwrap();

            if env != "default" && phase1.contains_key(&env) {
                env
            } else {
                "top-level".to_string()
            }
        });

        let embedded_transformer_object = transformers.clone();
        engine.register_fn(
            "register-macro-transformer!",
            move |name: String, env: String| {
                embedded_transformer_object
                    .set
                    .write()
                    .unwrap()
                    .entry(env)
                    .or_default()
                    .insert(name.as_str().into())
            },
        );

        let embedded_transformer_object = transformers.clone();
        engine.register_fn(
            "current-macro-transformers!",
            move |env: SteelString| -> SteelVal {
                embedded_transformer_object
                    .set
                    .read()
                    .unwrap()
                    .get(env.as_str())
                    .map(|set| {
                        set.iter()
                            .map(|x| x.resolve().to_string())
                            .map(|x| SteelVal::SymbolV(x.into()))
                            .collect::<crate::values::lists::List<SteelVal>>()
                            .into()
                    })
                    .unwrap_or_else(|| SteelVal::ListV(List::new()))
            },
        );

        // Load in parameters.
        // TODO: Merge this with the path in modules.rs
        if let Err(err) =
            engine.compile_and_run_raw_program(include_str!("../scheme/modules/parameters.scm"))
        {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        if let Err(err) =
            engine.compile_and_run_raw_program(include_str!("../scheme/modules/ports.scm"))
        {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        // Run the script for building the core interface for structs
        if let Err(err) = engine.compile_and_run_raw_program(KERNEL) {
            engine.raise_error(err);
            panic!("Kernel failed to load: This shouldn't happen!");
        }

        // The kernel's own macros, which a program's definitions leave alone
        let builtin_macros = transformers
            .set
            .read()
            .unwrap()
            .get("default")
            .cloned()
            .unwrap_or_default();

        Kernel {
            builtin_macros,
            transformers,
            constants: FxHashSet::default(),
            engine: Box::new(engine),
            shared_instances: FxHashSet::default(),
            share_host_modules: false,
            compiling: Vec::new(),
            instances: FxHashMap::default(),
            instance_count: 0,
            compile_tags: FxHashMap::default(),
            phase1_macros: FxHashMap::default(),
            phase1_renames: FxHashMap::default(),
            macro_modules: FxHashMap::default(),
            sandboxed: sandbox,
            local_macros: FxHashMap::default(),
            phase1_aliases: FxHashMap::default(),
            phase1_procedural: FxHashMap::default(),
        }
    }

    // pub(crate) fn bootstrap(_engine: Engine) -> (Self, RawProgramWithSymbols) {
    //     todo!()

    // let transformers = Transformers {
    //     set: Arc::new(RwLock::new(HashSet::default())),
    // };

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("register-macro-transformer!", move |name: String| {
    //     embedded_transformer_object
    //         .set
    //         .write()
    //         .unwrap()
    //         .insert(name.as_str().into())
    // });

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("current-macro-transformers!", move || -> SteelVal {
    //     embedded_transformer_object
    //         .set
    //         .read()
    //         .unwrap()
    //         .iter()
    //         .map(|x| x.resolve().to_string())
    //         .map(|x| SteelVal::SymbolV(x.into()))
    //         .collect::<crate::values::lists::List<SteelVal>>()
    //         .into()
    // });

    // Run the script for building the core interface for structs
    // engine.compile_and_run_raw_program(KERNEL).unwrap();

    // let raw_program = engine.emit_raw_program_no_path(KERNEL).unwrap();
    // engine.run_raw_program(raw_program.clone()).unwrap();

    // let mut macros = HashSet::new();
    // macros.insert("%better-lambda%".to_string());
    // macros.insert(*STRUCT_KEYWORD);
    // macros.insert(*DEFINE_VALUES);

    // (
    //     Kernel {
    //         // macros,
    //         transformers,
    //         constants: HashSet::new(),
    //         engine: Box::new(engine),
    //     },
    //     raw_program,
    // )
    // }

    // pub(crate) fn initialize_post_bootstrap(_engine: Engine) -> Self {
    // let transformers = Transformers {
    //     set: Arc::new(RwLock::new(HashSet::default())),
    // };

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("register-macro-transformer!", move |name: String| {
    //     embedded_transformer_object
    //         .set
    //         .write()
    //         .unwrap()
    //         .insert(name.as_str().into())
    // });

    // let embedded_transformer_object = transformers.clone();
    // engine.register_fn("current-macro-transformers!", move || -> SteelVal {
    //     embedded_transformer_object
    //         .set
    //         .read()
    //         .unwrap()
    //         .iter()
    //         .map(|x| x.resolve().to_string())
    //         .map(|x| SteelVal::SymbolV(x.into()))
    //         .collect::<crate::values::lists::List<SteelVal>>()
    //         .into()
    // });

    // Kernel {
    //     // macros,
    //     transformers,
    //     constants: HashSet::new(),
    //     engine: Box::new(engine),
    // }

    //     todo!()
    // }

    pub fn is_constant(&self, ident: &InternedString) -> bool {
        self.constants.contains(ident)
    }

    // Define environment? Each module has its own set of transformers?
    pub(crate) fn load_syntax_transformers(
        &mut self,
        exprs: &mut Vec<ExprKind>,
        environment: String,
        imports: &[Phase1Import],
        for_syntax: Vec<ExprKind>,
        for_syntax_prefixes: Vec<String>,
    ) -> Result<()> {
        enum IndexKind {
            DefMacro(usize),
            BeginForSyntax(usize),
        }

        if exprs.iter().any(has_phase1) {
            let mut spliced = Vec::with_capacity(exprs.len());

            for mut expr in core::mem::take(exprs) {
                if has_phase1(&expr) {
                    crate::parser::expand_visitor::rename_template_definitions(
                        core::slice::from_mut(&mut expr),
                    );
                }

                splice_phase1(expr, &mut spliced);
            }

            *exprs = spliced;
        }

        let mut def_macro_expr_indices = smallvec::SmallVec::<[IndexKind; 5]>::new();

        let mut provide_definitions = thin_vec![ExprKind::ident("provide")];
        // TOOD: Consider using Arc<String> instead of just plain String!
        let environment_ident = ExprKind::string_lit(environment.clone());

        for (i, expr) in exprs.iter_mut().enumerate() {
            let first_ident = expr.list().and_then(|l| l.first_ident());

            match first_ident {
                Some(ident) if *ident == *DEFMACRO => {
                    let underlying = expr.list_mut_or_else(|| unreachable!()).unwrap();

                    match &underlying[1] {
                        ExprKind::List(l) => provide_definitions.push(l[0].clone()),
                        ExprKind::Atom(_) => {
                            provide_definitions.push(underlying.args[1].clone());
                        }
                        _ => {
                            let span = underlying[0].atom_syntax_object().unwrap().span;
                            stop!(BadSyntax => "malformed defmacro"; span);
                        }
                    }

                    underlying.args.insert(1, environment_ident.clone());

                    def_macro_expr_indices.push(IndexKind::DefMacro(i));
                }

                Some(ident) if *ident == *BEGIN_FOR_SYNTAX => {
                    def_macro_expr_indices.push(IndexKind::BeginForSyntax(i));
                }

                _ => {}
            }
        }

        // if def_macro_expr_indices.is_empty() {
        //     return Ok(());
        // }

        // Fill up the define macro expressions with the correct ones
        // Lets do some debug logging to make sure this even makes sense
        // in the first place
        let mut def_macro_exprs = ThinVec::with_capacity(def_macro_expr_indices.len() + 3);

        let mut macro_of_choice: InternedString = "#%syntax-transformer-module".into();

        if environment == "top-level" {
            if self.engine.extract_value("top-level").is_ok() {
                macro_of_choice = "#%syntax-transformer-module-update".into();
            }
        }

        def_macro_exprs.push(ExprKind::atom(macro_of_choice));
        def_macro_exprs.push(ExprKind::ident(&environment));

        let has_provides = !def_macro_expr_indices.is_empty();

        if has_provides {
            def_macro_exprs.push(ExprKind::List(steel_parser::ast::List::new(
                provide_definitions.clone(),
            )));
        }

        // Each form, and whether it's a defmacro
        let mut exprs_buffer = Vec::with_capacity(def_macro_expr_indices.len());

        while let Some(i) = def_macro_expr_indices.pop() {
            match i {
                IndexKind::DefMacro(i) => {
                    let def_macro = exprs.remove(i);

                    exprs_buffer.push((true, def_macro));
                }
                IndexKind::BeginForSyntax(i) => {
                    let begin_for_syntax = exprs.remove(i);

                    let mut iter = begin_for_syntax.into_list().args.into_iter();
                    iter.next();

                    exprs_buffer.extend(iter.rev().map(|x| (false, x)));
                }
            }
        }

        exprs_buffer.reverse();

        for (_, expr) in exprs_buffer.iter_mut() {
            self.expand_phase1(
                &environment,
                core::slice::from_mut(expr),
                &FxHashMap::default(),
                None,
            )?;
        }

        // The kernel's own macros, like struct, aren't macros where phase 1 code runs, so
        // they're expanded here, which also shows the definitions they make
        for (defmacro, expr) in exprs_buffer.iter_mut() {
            if !*defmacro {
                let none = FxHashSet::default();
                crate::parser::expand_visitor::expand_with_kernel_defining(
                    expr,
                    &FxHashMap::default(),
                    crate::parser::expand_visitor::GlobalMap::Set(&none),
                    crate::parser::expand_visitor::KernelExpander::in_environment(
                        Some(&mut *self),
                        crate::steel_vm::engine::ModuleContainer::default(),
                        "default",
                    ),
                    false,
                )?;

                // What they define is defined by name, like the rest of phase 1
                struct Unmarked;

                impl VisitorMutRefUnit for Unmarked {
                    fn visit_atom(&mut self, a: &mut Atom) {
                        a.syn.mark = ExpansionMark::NONE;
                    }
                }

                Unmarked.visit(expr);
            }
        }

        let defmacros: Vec<InternedString> = provide_definitions
            .iter()
            .skip(1)
            .filter_map(ExprKind::atom_identifier)
            .copied()
            .collect();

        let mut names: Vec<InternedString> = imports.iter().map(|import| import.local).collect();

        exprs_buffer
            .iter()
            .for_each(|(_, x)| phase1_definitions(x, &mut names));

        let mut added = Vec::new();
        let defining = environment.clone();

        {
            let mut phase1 = self.transformers.phase1.write().unwrap();
            let known = phase1.entry(environment.clone()).or_default();

            for name in &names {
                if !known.contains(name) {
                    known.push(*name);
                    added.push(*name);
                }
            }
        }

        // The phase 1 variables are globals of the kernel, which the environment's macros
        // refer to
        // An imported variable is the one of the instance it comes from, not a copy
        let mut imported = FxHashMap::default();

        for import in imports {
            if let Some((source, name)) = &import.source {
                let instance = self.instance_of(source)?;
                imported.insert(import.local, phase1_variable(&instance, *name));
            }
        }

        self.phase1_aliases
            .entry(environment.clone())
            .or_default()
            .extend(imported.iter().map(|(local, global)| (*local, *global)));

        let renames = self.phase1_variables(&environment);

        for (_, expr) in exprs_buffer.iter_mut() {
            Phase1Expander {
                macros: &FxHashMap::default(),
                procedural: &FxHashMap::default(),
                kernel: None,
                renames: &renames,
                locals: vec![FxHashSet::default()],
                patterns: 0,
                depth: 0,
            }
            .visit(expr)?;
        }

        let forms: Vec<ExprKind> = exprs_buffer.iter().map(|(_, x)| x.clone()).collect();

        // The imported phase 1 bindings come first, and the table of all of them last
        let table = ExprKind::ident(&phase1_table(&environment));

        let mut phase1_exprs: Vec<ExprKind> = imports
            .iter()
            .filter(|import| import.source.is_none())
            .map(|import| {
                expr_list![
                    ExprKind::ident("define"),
                    ExprKind::atom(phase1_variable(&environment, import.local)),
                    import.value.clone(),
                ]
            })
            .collect();

        let mut defmacro_exprs = Vec::new();

        for (defmacro, expr) in exprs_buffer {
            if defmacro {
                defmacro_exprs.push(expr);
            } else {
                phase1_exprs.push(expr);
            }
        }

        if !names.is_empty() {
            let mut entries = thin_vec![ExprKind::ident("hash")];

            for name in &names {
                entries.push(quoted(*name));
                entries.push(ExprKind::atom(self.phase1_global(&environment, *name)));
            }

            phase1_exprs.push(expr_list![
                ExprKind::ident("set!"),
                table.clone(),
                expr_list![
                    ExprKind::ident("hash-union"),
                    table,
                    ExprKind::List(steel_parser::ast::List::new(entries)),
                ],
            ]);
        }

        if self
            .engine
            .extract_value(&phase1_table(&environment))
            .is_err()
        {
            self.engine.run_raw_program_from_exprs(vec![expr_list![
                ExprKind::ident("define"),
                ExprKind::ident(&phase1_table(&environment)),
                expr_list![ExprKind::ident("hash")],
            ]])?;
        }

        // A module's transformer environment can be built again for each module that uses it
        if environment != "top-level" && environment != "default" {
            self.forget_instances(&environment);
            self.transformers.programs.write().unwrap().insert(
                environment.clone(),
                Phase1Program {
                    for_syntax,
                    for_syntax_prefixes,
                    imports: imports.to_vec(),
                    forms,
                    defmacros,
                    names,
                    transformers: Vec::new(),
                    imported,
                },
            );
        }

        if !has_provides && !defmacro_exprs.is_empty() {
            def_macro_exprs.push(expr_list![ExprKind::ident("provide")]);
        }

        def_macro_exprs.extend(defmacro_exprs);

        // Create the generated module
        let generated_module = ExprKind::List(steel_parser::ast::List::new(def_macro_exprs));

        // self.engine.run(format!(
        //     "(set! #%loading-current-module \"{}\")",
        //     if cfg!(windows) {
        //         environment.replace("\\", "/")
        //     } else {
        //         environment
        //     }
        // ))?;

        self.engine.call_function_by_name_with_args_from_mut_slice(
            "#%set-module",
            &mut [SteelVal::StringV(
                if cfg!(windows) {
                    environment.replace("\\", "/")
                } else {
                    environment
                }
                .into(),
            )],
        )?;

        // TODO: Load this as a module instead, so that way we have some real
        // separation from each other.
        //
        // Meaning something like:
        // (create-environment <environment> (provide <defmacros> ...) <defmacro definitions> ...)
        //
        // Expands into something like:
        //
        // (define env (let () <defmacro definitions> (hash <defmacro defs>)))
        //
        // Then, expansion would work by just interacting with that environment - something like
        // (call-function-in-environment foo env)
        // Which will extract the function from the module, and call it with the arguments. In theory
        // that should give us the necessary isolation between modules that we want
        // for the purposes of this.
        phase1_exprs.push(generated_module);

        let result = self.engine.run_raw_program_from_exprs(phase1_exprs);

        // self.engine
        //     .run("(set! #%loading-current-module \"default\")".to_owned())?;

        self.engine.call_function_by_name_with_args_from_mut_slice(
            "#%set-module",
            &mut [SteelVal::StringV("default".into())],
        )?;

        // Names whose code didn't run aren't defined
        if result.is_err() {
            if let Some(known) = self.transformers.phase1.write().unwrap().get_mut(&defining) {
                known.retain(|name| !added.contains(name));
            }
        }

        result.map(|_| ())
    }

    // TODO: Have this report errors
    pub fn load_program_for_comptime(
        &mut self,
        constants: crate::values::HashMap<InternedString, SteelVal, FxBuildHasher>,
        exprs: &mut Vec<ExprKind>,
    ) -> Result<()> {
        let mut analysis = SemanticAnalysis::new(exprs);

        let result = analysis.run_black_box(constants, false);

        // Only select expressions which are identified as constant
        let subset = analysis
            .exprs
            .iter()
            .filter_map(|expr| {
                match expr {
                    ExprKind::Define(define) => {
                        if let ExprKind::LambdaFunction(_) = &define.body {
                            let name = define.name.atom_identifier().unwrap();

                            return if result.contains_key(name) {
                                Some(expr.clone())
                            } else if self.engine.global_exists(name.resolve()) {
                                // If this global exists, nuke it in the engine

                                self.engine.register_value(name.resolve(), SteelVal::Void);

                                None
                            } else {
                                None
                            };
                        }
                    }
                    ExprKind::Begin(b) => {
                        let begin = b
                            .exprs
                            .iter()
                            .filter(|expr| {
                                if let ExprKind::Define(define) = expr {
                                    if let ExprKind::LambdaFunction(_) = &define.body {
                                        let name = define.name.atom_identifier().unwrap();

                                        return result.contains_key(name);

                                        // return if result.contains_key(&name) {
                                        //     true
                                        // } else if self.engine.global_exists(name.resolve()) {
                                        //     false
                                        // } else {
                                        //     false
                                        // };
                                    }
                                }

                                false
                            })
                            .cloned()
                            .collect();

                        return Some(ExprKind::Begin(Box::new(crate::parser::ast::Begin::new(
                            begin,
                            b.location.clone(),
                        ))));
                    }

                    _ => {}
                }

                None

                // TODO: Also check begin statements here as well
                // if let ExprKind::Define(define) = expr {
                //     if let ExprKind::LambdaFunction(_) = &define.body {
                //         let name = define.name.atom_identifier().unwrap().clone();

                //         return result.contains_key(&name);
                //     }
                // }
            })
            // .cloned()
            .collect::<Vec<_>>();

        log::debug!("Loading constant functions");

        if subset.is_empty() {
            log::debug!("Found no constant functions");

            return Ok(());
        }

        self.engine.run_raw_program_from_exprs(subset)?;

        fn set(var: ExprKind, expr: ExprKind) -> ExprKind {
            ExprKind::Set(Box::new(Set::new(
                var,
                expr,
                SyntaxObject::default(TokenType::Set),
            )))
        }

        let mut set_idents = Vec::with_capacity(result.len());

        let memoize: InternedString = "%make-memoize".into();

        for ident in result.keys() {
            self.constants.insert(*ident);

            let set_ident = ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::Identifier(ident.clone()),
            )));
            let memoize_expr = expr_list!(
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                    memoize.clone()
                )))),
                set_ident.clone()
            );

            set_idents.push(set(set_ident, memoize_expr));
        }

        self.engine.run_raw_program_from_exprs(set_idents)?;

        log::debug!("Constant functions loaded");

        // dbg!(&self
        //     .constants
        //     .iter()
        //     .map(|x| x.resolve())
        //     .collect::<Vec<_>>());

        Ok(())

        // todo!("Run through every expression, and memoize them by calling (set! <ident> (make-memoize <ident>))")
    }

    // The kernel loads the prelude as modules of its own, under the same names
    /// Whether transformer code has a global of this name
    pub(crate) fn defines(&self, name: &str) -> bool {
        self.engine.extract_value(name).is_ok()
    }

    pub(crate) fn has_shared_instance(&self, module: &Path, table: &str) -> bool {
        self.shared_instances.contains(module) || self.engine.extract_value(table).is_ok()
    }

    /// Uses of macros from now on are part of compiling this module, which gets transformer
    /// environments of its own for other modules' macros
    pub(crate) fn begin_compiling(&mut self, module: String) {
        self.phase1_macros.remove(&module);
        self.phase1_procedural.remove(&module);
        self.phase1_renames.remove(&module);
        self.compiling.push(module);
    }

    pub(crate) fn end_compiling(&mut self) {
        let Some(module) = self.compiling.pop() else {
            return;
        };

        self.compile_tags.remove(&module);

        let done: Vec<_> = self
            .instances
            .iter()
            .filter(|((_, user), _)| *user == module)
            .map(|(key, name)| (key.clone(), name.clone()))
            .collect();

        for (key, name) in done {
            self.instances.remove(&key);

            for global in [instance_value(&name), phase1_table(&name), name] {
                if self.engine.extract_value(&global).is_ok() {
                    self.engine.register_value(&global, SteelVal::Void);
                }
            }
        }
    }

    fn user(&self) -> String {
        self.compiling
            .last()
            .cloned()
            .unwrap_or_else(|| "top-level".to_string())
    }

    /// The macros of modules required for syntax, for the environment's transformer code.
    /// `renames` are the names the instances of those modules have, when they aren't the
    /// modules' own.
    pub(crate) fn add_phase1_macros(
        &mut self,
        environment: &str,
        macros: FxHashMap<InternedString, SteelMacro>,
        procedural: FxHashMap<InternedString, (String, InternedString)>,
        renames: Vec<(String, String)>,
    ) {
        self.phase1_macros
            .entry(environment.to_owned())
            .or_default()
            .extend(macros);
        self.phase1_procedural
            .entry(environment.to_owned())
            .or_default()
            .extend(procedural);
        // A module required for syntax again has a new instance
        let known = self
            .phase1_renames
            .entry(environment.to_owned())
            .or_default();
        known.retain(|(from, _)| !renames.iter().any(|(new, _)| new == from));
        known.extend(renames);
    }

    // Forgets the instances of an environment, and of those that import from it, so they
    // are made again from its new code
    fn forget_instances(&mut self, environment: &str) {
        let mut stale = vec![environment.to_owned()];

        // A module loaded again has new code, a new module leaves the top level's instances
        let reloaded = self
            .transformers
            .programs
            .read()
            .unwrap()
            .contains_key(environment);

        {
            let programs = self.transformers.programs.read().unwrap();
            let mut i = 0;

            while let Some(source) = stale.get(i).cloned() {
                for (env, program) in programs.iter() {
                    let imports_it = program.imports.iter().any(|import| {
                        import
                            .source
                            .as_ref()
                            .is_some_and(|(from, _)| *from == source)
                    });

                    if imports_it && !stale.contains(env) {
                        stale.push(env.clone());
                    }
                }

                i += 1;
            }
        }

        self.instances.retain(|(env, _), _| !stale.contains(env));

        // The top level's modules required for syntax are made again with the new code
        if reloaded {
            self.compile_tags.remove("top-level");
        }
    }

    // The environment whose phase 1 variables code of `environment` sees. Code with no
    // file, or a program's file, has them in the top level.
    fn phase1_environment(&self, environment: &str) -> String {
        let phase1 = self.transformers.phase1.read().unwrap();

        if environment != "default" && phase1.contains_key(environment) {
            environment.to_owned()
        } else {
            "top-level".to_owned()
        }
    }

    // The globals the phase 1 variables of an environment are
    fn phase1_variables(&self, environment: &str) -> FxHashMap<InternedString, InternedString> {
        let environment = self.phase1_environment(environment);

        self.transformers
            .phase1
            .read()
            .unwrap()
            .get(&environment)
            .into_iter()
            .flatten()
            .map(|name| (*name, self.phase1_global(&environment, *name)))
            .collect()
    }

    // The global a phase 1 variable of an environment is, which for an imported one is the
    // variable of the instance it comes from
    fn phase1_global(&self, environment: &str, name: InternedString) -> InternedString {
        self.phase1_aliases
            .get(environment)
            .and_then(|aliases| aliases.get(&name))
            .copied()
            .unwrap_or_else(|| phase1_variable(environment, name))
    }

    // Expands transformer code with the environment's macros from modules required for
    // syntax, naming its phase 1 variables by their globals. `locals` are the variables
    // in scope of a body.
    fn expand_phase1(
        &mut self,
        environment: &str,
        exprs: &mut [ExprKind],
        renames: &FxHashMap<InternedString, InternedString>,
        locals: Option<FxHashSet<InternedString>>,
    ) -> Result<()> {
        // A program's code, from its file or not, uses what the top level requires for syntax
        let environment = if self.compiling.is_empty() {
            "top-level"
        } else {
            environment
        };

        let macros = self.phase1_macros.remove(environment).unwrap_or_default();
        let procedural = self
            .phase1_procedural
            .get(environment)
            .cloned()
            .unwrap_or_default();

        let mut result = Ok(());

        {
            let body = locals.is_some();

            let mut expander = Phase1Expander {
                macros: &macros,
                procedural: &procedural,
                kernel: Some(self),
                renames,
                locals: core::iter::once(FxHashSet::default())
                    .chain(locals)
                    .collect(),
                patterns: 0,
                depth: 0,
            };

            if body {
                expander.bind_body_defines(exprs);
            }

            result = exprs.iter_mut().try_for_each(|expr| expander.visit(expr));
        }

        if !macros.is_empty() {
            self.phase1_macros.insert(environment.to_owned(), macros);
        }

        result?;

        if let Some(prefixes) = self.phase1_renames.get(environment) {
            if !prefixes.is_empty() {
                for expr in exprs.iter_mut() {
                    RenameInstance { prefixes }.visit(expr);
                }
            }
        }

        Ok(())
    }

    // The transformer environment that expands the macros of `environment` for the module
    // being compiled. A module other than the one that defines them gets its own instance.
    fn instance_of(&mut self, environment: &str) -> Result<String> {
        let user = self.user();

        if environment == user {
            return Ok(environment.to_owned());
        }

        let key = (environment.to_owned(), user.clone());

        if let Some(name) = self.instances.get(&key) {
            return Ok(name.clone());
        }

        let Some(program) = self
            .transformers
            .programs
            .read()
            .unwrap()
            .get(environment)
            .cloned()
        else {
            return Ok(environment.to_owned());
        };

        // With no state at phase 1, every module can use the defining module's macros
        let is_defmacro = |x: &ExprKind| x.list().and_then(|l| l.first_ident()) == Some(&*DEFMACRO);

        if program.for_syntax.is_empty()
            && program.imports.is_empty()
            && program.forms.iter().all(is_defmacro)
        {
            return Ok(environment.to_owned());
        }

        // The variables it imports are those of the instances the same module gets
        let mut imported = FxHashMap::default();

        for import in &program.imports {
            if let (Some((source, name)), Some(global)) =
                (&import.source, program.imported.get(&import.local))
            {
                let instance = self.instance_of(source)?;
                imported.insert(*global, phase1_variable(&instance, *name));
            }
        }

        // The copies of the modules its transformer code requires that the module being
        // compiled has
        let tag = match self.compile_tags.get(&user) {
            Some(tag) => tag.clone(),
            None => {
                self.instance_count += 1;
                let tag = format!("#instance-{}", self.instance_count);
                self.compile_tags.insert(user.clone(), tag.clone());
                tag
            }
        };

        // A module the module being compiled requires for syntax itself is the copy it has
        let own_prefixes: Vec<String> = self
            .transformers
            .programs
            .read()
            .unwrap()
            .get(&user)
            .map(|x| x.for_syntax_prefixes.clone())
            .unwrap_or_default();
        let own_renames = self.phase1_renames.get(&user).cloned().unwrap_or_default();

        let prefixes: Vec<(String, String)> = program
            .for_syntax_prefixes
            .iter()
            .map(|prefix| {
                if let Some((_, renamed)) = own_renames.iter().find(|(from, _)| from == prefix) {
                    return (prefix.clone(), renamed.clone());
                }

                if own_prefixes.contains(prefix) {
                    return (prefix.clone(), prefix.clone());
                }

                let renamed =
                    prefix.replacen(MANGLER_SEPARATOR, &format!("{tag}{MANGLER_SEPARATOR}"), 1);
                (prefix.clone(), renamed)
            })
            .collect();

        let name = format!("{environment}@{user}");
        let value = instance_value(&name);

        // Its phase 1 variables are its own globals
        let mut prefixes = prefixes;
        prefixes.push((
            phase1_variable(environment, "".into()).to_string(),
            phase1_variable(&name, "".into()).to_string(),
        ));

        let mut code = program.for_syntax.clone();
        code.extend(instance_code(environment, &name, &value, &program));

        for expr in code.iter_mut() {
            RenameExactly { names: &imported }.visit(expr);
            RenameInstance {
                prefixes: &prefixes,
            }
            .visit(expr);
        }

        // A module another instance made for the same compile already ran
        code.retain(|expr| {
            module_table_defined_by(expr).map_or(true, |table| !self.defines(&table))
        });

        code.extend([
            expr_list![
                ExprKind::ident("define"),
                ExprKind::ident(&name),
                expr_list![
                    ExprKind::ident("hash-ref"),
                    ExprKind::ident(&value),
                    quoted("macros".into()),
                ],
            ],
            expr_list![
                ExprKind::ident("define"),
                ExprKind::ident(&phase1_table(&name)),
                expr_list![
                    ExprKind::ident("hash-ref"),
                    ExprKind::ident(&value),
                    quoted("phase1".into()),
                ],
            ],
        ]);

        self.engine.run_raw_program_from_exprs(code)?;

        self.instances.insert(key, name.clone());

        Ok(name)
    }

    // The transformer of a `define-syntax`, which is code for the phase its macro runs at
    fn expand_transformer_code(&mut self, environment: &str, expr: &mut ExprKind) -> Result<()> {
        let ExprKind::List(l) = expr else {
            return Ok(());
        };

        let mut params = FxHashSet::default();

        let start = match l.args.get(1) {
            Some(ExprKind::List(f)) => {
                params.extend(f.args.iter().skip(1).filter_map(ExprKind::atom_identifier));
                2
            }
            _ => {
                let rules = l.args.get(2).and_then(ExprKind::list).is_some_and(|x| {
                    x.first_ident()
                        .is_some_and(|x| x.resolve() == "syntax-rules")
                        || x.is_syntax_rules()
                });

                if rules {
                    return Ok(());
                }

                2
            }
        };

        let renames = self.phase1_variables(environment);

        let body = l.args.get_mut(start..).unwrap_or_default();
        self.expand_phase1(environment, body, &renames, Some(params))
    }

    /// Makes a module of the host available to transformer code
    pub(crate) fn share_module(&mut self, module: &BuiltInModule) {
        let name = module.name();

        if self.sandboxed && SANDBOXED_MODULES.contains(&name.as_ref()) {
            return;
        }

        if !self.engine.has_module(name.as_ref()) {
            self.engine.register_module(module.clone());
        }
    }

    /// Runs module code at phase 1. `shared` are the modules it instantiates for everyone.
    pub(crate) fn instantiate(&mut self, code: Vec<ExprKind>, shared: Vec<PathBuf>) -> Result<()> {
        self.engine.run_raw_program_from_exprs(code)?;
        self.shared_instances.extend(shared);
        Ok(())
    }

    /// Whether the environment's transformer code has a phase 1 binding of this name
    pub(crate) fn defines_for_syntax(&self, environment: &str, name: &InternedString) -> bool {
        self.transformers
            .phase1
            .read()
            .unwrap()
            .get(environment)
            .is_some_and(|names| names.contains(name))
    }

    /// Whether the environment defines procedural macros
    pub(crate) fn has_procedural_macros(&self, environment: &str) -> bool {
        self.transformers
            .set
            .read()
            .unwrap()
            .contains_key(environment)
    }

    pub fn exported_defmacros(&self, environment: &str) -> Option<FxHashSet<InternedString>> {
        self.transformers
            .set
            .read()
            .unwrap()
            .get(environment)
            .cloned()
    }

    // A definition at the top level replaces a macro of its name for what comes after it
    pub(crate) fn forget_macro(&mut self, environment: &str, name: &InternedString) {
        let mut set = self.transformers.set.write().unwrap();

        if let Some(macros) = set.get_mut(environment) {
            macros.remove(name);
        }

        // The repl's macros are in the default environment
        if !self.builtin_macros.contains(name) {
            if let Some(macros) = set.get_mut("default") {
                macros.remove(name);
            }
        }
    }

    pub(crate) fn is_local_macro(&self, name: &InternedString) -> bool {
        self.local_macros.contains_key(name)
    }

    pub(crate) fn define_local_macro(
        &mut self,
        name: InternedString,
        locals: FxHashSet<InternedString>,
    ) {
        self.local_macros.insert(name, locals);
    }

    pub(crate) fn add_macro_module(&mut self, prefix: CompactString, module: MacroModule) {
        self.macro_modules.insert(prefix, module);
    }

    /// Expands a use of a macro that a module's code names by the module's prefix, if it
    /// is one
    /// `environment` is where the code is, when known: what another module's macro writes
    /// for it means the names where that macro is.
    pub(crate) fn expand_prefixed_macro(
        &mut self,
        name: &InternedString,
        l: &mut super::ast::List,
        environment: Option<&str>,
    ) -> Result<Option<ExprKind>> {
        let resolved = name.resolve();

        let Some(end) = resolved
            .starts_with(MANGLER_PREFIX)
            .then(|| resolved.find(MANGLER_SEPARATOR))
            .flatten()
            .map(|i| i + MANGLER_SEPARATOR.len())
        else {
            return Ok(None);
        };

        let Some(local) = InternedString::try_get(&resolved[end..]) else {
            return Ok(None);
        };

        let Some(module) = self.macro_modules.get(&resolved[..end]) else {
            return Ok(None);
        };

        // A module's code names its own definitions by its prefix, never a macro one of its
        // functions defines
        if self.local_macros.contains_key(&local) && !module.rules.contains_key(&local) {
            return Ok(None);
        }

        if let Some(m) = module.rules.get(&local) {
            let span = l
                .first()
                .and_then(ExprKind::atom_syntax_object)
                .map(|x| x.span);
            let list = core::mem::replace(l, super::ast::List::new(ThinVec::new()));

            let (expanded, mark) = m.expand_marked(
                list,
                span.unwrap_or_default(),
                &Default::default(),
                GlobalMap::Set(&module.mangler.globals),
            )?;

            if environment.is_some_and(|environment| environment != module.environment) {
                crate::parser::expand_visitor::record_foreign_mark(mark);
            }

            return Ok(Some(expanded));
        }

        let Some((environment, name)) = module.procedural.get(&local).cloned() else {
            return Ok(None);
        };

        let expr = ExprKind::List(core::mem::replace(l, super::ast::List::new(ThinVec::new())));

        self.expand_in_environment(&name, expr, &environment)
            .map(Some)
    }

    // Expands a use of a procedural macro where it's defined, which gives what it
    // introduces its module's names
    fn expand_in_environment(
        &mut self,
        name: &InternedString,
        expr: ExprKind,
        environment: &str,
    ) -> Result<ExprKind> {
        let prefix = self
            .macro_modules
            .iter()
            .find(|(_, x)| x.environment == environment)
            .map(|(prefix, _)| prefix.clone());

        let mut defining = prefix
            .as_ref()
            .and_then(|prefix| self.macro_modules.remove(prefix));

        let result = self.expand_syntax_object(
            name,
            expr,
            environment,
            defining.as_mut().map(|x| &mut x.mangler),
        );

        if let (Some(prefix), Some(defining)) = (prefix, defining) {
            self.macro_modules.insert(prefix, defining);
        }

        result
    }

    // Whether the environment defines a procedural macro of this name itself
    pub(crate) fn has_own_macro(&self, environment: &str, ident: &InternedString) -> bool {
        let set = self.transformers.set.read().unwrap();

        // The top level's macros defined after the first input are the default environment's,
        // with the kernel's own
        set.get(environment).is_some_and(|x| x.contains(ident))
            || environment == "top-level"
                && !self.builtin_macros.contains(ident)
                && set.get("default").is_some_and(|x| x.contains(ident))
    }

    // The procedural macros the environment defines itself
    pub(crate) fn procedural_names(&self, environment: &str) -> FxHashSet<InternedString> {
        let set = self.transformers.set.read().unwrap();
        let mut names: FxHashSet<InternedString> = set
            .get(environment)
            .into_iter()
            .flatten()
            .copied()
            .collect();

        if environment == "top-level" {
            names.extend(
                set.get("default")
                    .into_iter()
                    .flatten()
                    .filter(|x| !self.builtin_macros.contains(*x))
                    .copied(),
            );
        }

        names
    }

    // Whether a required module's macro introduced a name with this mark
    pub(crate) fn is_foreign_mark(&self, mark: ExpansionMark) -> bool {
        crate::parser::expand_visitor::is_foreign_mark(mark)
    }

    pub fn contains_syntax_object_macro(
        &self,
        ident: &InternedString,
        environment: Option<&str>,
    ) -> bool {
        if let Some(environment) = environment {
            // TODO: Come back to this - but we really just
            // want a chain of environment that we've added to
            // transformers
            let base_environment = self
                .transformers
                .set
                .read()
                .unwrap()
                .get(environment)
                .map(|x| x.contains(ident))
                .unwrap_or_default();

            base_environment
                || self
                    .transformers
                    .set
                    .read()
                    .unwrap()
                    .get("default")
                    .map(|x| x.contains(ident))
                    .unwrap_or_default()
        } else {
            self.transformers
                .set
                .read()
                .unwrap()
                .get("default")
                .map(|x| x.contains(ident))
                .unwrap_or_default()
        }
    }

    pub fn call_function(&mut self, ident: &InternedString, args: &[SteelVal]) -> Result<SteelVal> {
        let function = self.engine.extract_value(ident.resolve())?;

        self.engine.call_function_with_args(function, args.to_vec())
    }

    // A module's macros give the identifiers they introduce the module's names with `mangler`
    pub(crate) fn expand_syntax_object(
        &mut self,
        ident: &InternedString,
        expr: ExprKind,
        environment: &str,
        mangler: Option<&mut NameMangler>,
    ) -> Result<ExprKind> {
        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let span = get_span(&expr);

        let mut expr = expr;

        if ident.resolve() == "define-syntax" {
            self.expand_transformer_code(environment, &mut expr)?;
        }

        let syntax_objects =
            super::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(expr)?;

        let mut in_environment = false;

        let instance;

        let environment = if environment == "default" {
            environment
        } else {
            instance = self.instance_of(environment)?;
            instance.as_str()
        };

        let function = if environment == "default" {
            // TODO: This actually needs to go through the proper resolution process,
            // probably some sort of transformer map.
            self.engine.extract_value(ident.resolve())?
        } else {
            // Check if there is anything to expand in this environment
            if let Ok(SteelVal::HashMapV(map)) = self.engine.extract_value(environment) {
                if let Some(func) = map.get(&SteelVal::SymbolV(ident.resolve().to_string().into()))
                {
                    in_environment = true;
                    func.clone()
                } else {
                    self.engine.extract_value(ident.resolve())?
                }
            } else {
                self.engine.extract_value(ident.resolve())?
            }
        };

        let mut given = FxHashMap::default();

        let syntax_objects = match &syntax_objects {
            SteelVal::SyntaxObject(syntax) => syntax
                .map_marks(&mut |mark| *given.entry(mark).or_insert_with(ExpansionMark::fresh))
                .into(),
            _ => syntax_objects,
        };

        MACRO_INPUTS.with(|x| {
            x.borrow_mut().push(MacroInput {
                marks: given.into_iter().map(|(old, new)| (new, old)).collect(),
                hygienic: false,
            })
        });

        let result = self
            .engine
            .call_function_with_args_from_mut_slice(function, &mut [syntax_objects]);

        let input = MACRO_INPUTS.with(|x| x.borrow_mut().pop()).unwrap();

        let result = result.map_err(|x| x.set_span(span))?;

        // This shouldn't be lowering all the way. It should just be back to list right?
        let mut res = TryFromSteelValVisitorForExprKind::root(&result);

        if let Ok(expr) = &mut res {
            super::tryfrom_visitor::syntax_rules_as_lists(expr);
        }

        if let Ok(expr) = &mut res {
            // What the macro made without a location is reported where it's used
            struct UseSite(steel_parser::span::Span);

            impl VisitorMutRefUnit for UseSite {
                fn visit_atom(&mut self, a: &mut Atom) {
                    if a.syn.span.source_id().is_none() {
                        a.syn.span = self.0;
                    }
                }
            }

            UseSite(span).visit(expr);

            // The free names of a macro defined outside any function mean the globals
            let captures = self.local_macros.get(ident).cloned();
            let local = captures.is_some();

            let mark = if local {
                ExpansionMark::fresh()
            } else {
                ExpansionMark::fresh_definition_resolved()
            };

            // Expanded for another module's code
            if mangler.is_some() {
                crate::parser::expand_visitor::record_foreign_mark(mark);
            }

            Introduced {
                input: &input,
                mark,
                defined: FxHashSet::default(),
                mangler: mangler.filter(|_| in_environment),
                quoted: 0,
                local,
                macro_name: *ident,
                captures: captures.unwrap_or_default(),
            }
            .resolve(expr);
        }

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Kernel expansion time: {:?}", now.elapsed());

        res
    }
}
