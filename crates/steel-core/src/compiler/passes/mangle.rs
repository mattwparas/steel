use compact_str::CompactString;
use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::ast::{
    List, DEFINE, QUASIQUOTE, QUOTE, RAW_QUOTE, RAW_UNQUOTE, RAW_UNQUOTE_SPLICING, UNQUOTE,
    UNQUOTE_COMMA, UNQUOTE_SPLICING,
};

use crate::{
    compiler::{modules::MANGLER_PREFIX, program::BEGIN},
    parser::{
        ast::{Atom, ExprKind, Quote},
        interner::InternedString,
        tokens::TokenType,
    },
};

use super::VisitorMutRefUnit;

fn written_by_template(name: &ExprKind) -> bool {
    name.atom_syntax_object()
        .is_some_and(|syn| syn.scopes.introduced())
}

pub fn collect_globals(exprs: &[ExprKind]) -> FxHashSet<InternedString> {
    let mut global_defs = FxHashSet::default();

    for expr in exprs {
        match expr {
            ExprKind::Define(d) => {
                if written_by_template(&d.name) {
                    continue;
                }
                if let Some(name) = d.name.atom_identifier() {
                    if name.resolve().starts_with(MANGLER_PREFIX) {
                        continue;
                    }
                    global_defs.insert(*name);
                }
            }
            ExprKind::List(l)
                if l.first_ident() == Some(&*DEFINE)
                    || l.first()
                        .and_then(|x| x.atom_syntax_object())
                        .map(|x| x.ty == TokenType::Define)
                        .unwrap_or_default() =>
            {
                match l.get(1) {
                    Some(name) if written_by_template(name) => {}
                    Some(ExprKind::Atom(_)) => {
                        if let Some(name) = l.second_ident() {
                            if name.resolve().starts_with(MANGLER_PREFIX) {
                                continue;
                            }
                            // println!("Inserting: {}", name);
                            global_defs.insert(*name);
                        }
                    }

                    Some(ExprKind::List(l)) => {
                        if let Some(name) = l.first_ident() {
                            if name.resolve().starts_with(MANGLER_PREFIX) {
                                continue;
                            }
                            // println!("Inserting: {}", name);
                            global_defs.insert(*name);
                        }
                    }

                    _ => {}
                }
            }
            ExprKind::List(l)
                if l.first_ident() == Some(&*BEGIN)
                    || l.first()
                        .and_then(|x| x.atom_syntax_object())
                        .map(|x| x.ty == TokenType::Begin)
                        .unwrap_or_default() =>
            {
                if l.len() > 1 {
                    let collected_defs = collect_globals(&l[1..]);
                    global_defs.extend(collected_defs);
                }

                // let collected_defs =collect_globals(&l[])
            }
            ExprKind::Begin(b) => {
                let collected_defs = collect_globals(&b.exprs);
                global_defs.extend(collected_defs);
            }
            _ => {}
        }
    }

    global_defs
}

pub struct NameUnMangler<'a> {
    prefix: &'a str,
}

impl<'a> NameUnMangler<'a> {
    pub fn new(prefix: &'a str) -> Self {
        Self { prefix }
    }

    pub fn unmangle_vars(&mut self, exprs: &mut [ExprKind]) {
        for expr in exprs {
            self.visit(expr);
        }
    }

    pub fn unmangle_expr(&mut self, expr: &mut ExprKind) {
        self.visit(expr);
    }
}

impl<'a> VisitorMutRefUnit for NameUnMangler<'a> {
    #[inline]
    fn visit_quote(&mut self, q: &mut Quote) {
        if let Some(expression) = q.expr.atom_identifier_mut() {
            // TODO: Should roll up this into one thing, since strip prefix checks if
            // it starts with the right value
            if expression.resolve().starts_with(self.prefix) {
                *expression = expression
                    .resolve()
                    .strip_prefix(self.prefix)
                    .unwrap()
                    .into();
            }
        }
    }
}

pub(crate) fn is_prelude_builtin(name: &InternedString) -> bool {
    crate::steel_vm::primitives::PRELUDE_INTERNED_STRINGS.with(|names| names.contains(name))
}

#[derive(Clone, Debug)]
pub struct NameMangler {
    pub(crate) globals: FxHashSet<InternedString>,
    // Macros of the module that aren't globals, which its templates name with its prefix
    macros: FxHashSet<InternedString>,
    prefix: CompactString,
    mangle_cache: FxHashMap<InternedString, InternedString>,
    // The module's scope, which what its own macros write has
    module: Option<steel_parser::scopes::Scope>,
}

impl NameMangler {
    pub fn new(globals: FxHashSet<InternedString>, prefix: CompactString) -> Self {
        Self {
            globals,
            macros: FxHashSet::default(),
            prefix,
            mangle_cache: FxHashMap::default(),
            module: None,
        }
    }

    pub(crate) fn in_module(mut self, name: &str) -> Self {
        self.module = Some(steel_parser::scopes::Scope::module(name));
        self
    }

    fn mangle(&mut self, i: &mut InternedString) {
        if let Some(new) = self.mangle_cache.get(i) {
            *i = *new;
        } else {
            let original = *i;
            *i = (self.prefix.clone() + i.resolve()).into();
            self.mangle_cache.insert(original, *i);
        }
    }

    pub(crate) fn add_macros(&mut self, macros: impl IntoIterator<Item = InternedString>) {
        self.macros.extend(macros);
    }

    pub fn mangle_vars(&mut self, exprs: &mut [ExprKind]) {
        for expr in exprs {
            self.visit(expr);
        }
    }

    fn visit_quasiquoted(&mut self, expr: &mut ExprKind, depth: usize) {
        match expr {
            ExprKind::List(l) => {
                let depth = match l.first_ident() {
                    Some(head) if *head == *QUASIQUOTE => depth + 1,
                    Some(head) if is_unquote(head) => depth - 1,
                    _ => depth,
                };
                for expr in &mut l.args {
                    if depth == 0 {
                        self.visit(expr);
                    } else {
                        self.visit_quasiquoted(expr, depth);
                    }
                }
            }
            ExprKind::Vector(v) => {
                for expr in &mut v.args {
                    self.visit_quasiquoted(expr, depth);
                }
            }
            ExprKind::Quote(q) => self.visit_quasiquoted(&mut q.expr, depth),
            _ => {}
        }
    }
}

pub(crate) fn is_unquote(head: &InternedString) -> bool {
    *head == *UNQUOTE
        || *head == *UNQUOTE_COMMA
        || *head == *RAW_UNQUOTE
        || *head == *UNQUOTE_SPLICING
        || *head == *RAW_UNQUOTE_SPLICING
}

pub fn mangle_vars_with_prefix(prefix: CompactString, exprs: &mut [ExprKind]) {
    let globals = collect_globals(exprs);

    let mut name_mangler = NameMangler::new(globals, prefix);

    for expr in exprs {
        name_mangler.visit(expr);
    }
}

// A procedural macro's definition, as a macro's template can write it
pub(crate) fn is_procedural_definition(l: &List) -> bool {
    l.first()
        .and_then(ExprKind::atom_syntax_object)
        .is_some_and(|x| match &x.ty {
            TokenType::DefineSyntax => true,
            TokenType::Identifier(x) => x.resolve() == "define-syntax",
            _ => false,
        })
        && match l.get(1) {
            Some(ExprKind::List(_)) => true,
            _ => !l.get(2).and_then(ExprKind::list).is_some_and(|x| {
                x.is_syntax_rules()
                    || x.first_ident()
                        .is_some_and(|x| x.resolve() == "syntax-rules")
            }),
        }
}

impl NameMangler {
    // Phase 1 code of the module: only what its syntax templates write is the module's code
    pub(crate) fn visit_phase1_code(&mut self, expr: &mut ExprKind) {
        let mut variables = FxHashSet::default();
        collect_pattern_variables(expr, &mut variables);
        self.visit_phase1(expr, &variables);
    }

    // Its transformer is phase 1 code, which the module's names don't name, apart from what
    // the syntax templates in it write, which is the module's code. Not their pattern
    // variables.
    fn visit_procedural_definition(&mut self, l: &mut List) {
        let start = match l.args.get_mut(1) {
            Some(ExprKind::List(signature)) => {
                if let Some(name) = signature.args.first_mut() {
                    self.visit(name);
                }
                2
            }
            Some(name) => {
                self.visit(name);
                2
            }
            None => return,
        };

        let mut variables = FxHashSet::default();
        for expr in l.args.iter().skip(start) {
            collect_pattern_variables(expr, &mut variables);
        }

        for expr in l.args.iter_mut().skip(start) {
            self.visit_phase1(expr, &variables);
        }
    }

    fn visit_phase1(&mut self, expr: &mut ExprKind, variables: &FxHashSet<InternedString>) {
        let ExprKind::List(l) = expr else {
            return;
        };
        match crate::parser::kernel::form_name(l) {
            Some("syntax" | "quote-syntax") => {
                for expr in l.args.iter_mut().skip(1) {
                    self.visit_syntax_template(expr, variables, false);
                }
            }
            Some("quasisyntax") => {
                for expr in l.args.iter_mut().skip(1) {
                    self.visit_syntax_template(expr, variables, true);
                }
            }
            Some("quote" | "#%quote") => {}
            _ => {
                for expr in l.args.iter_mut() {
                    self.visit_phase1(expr, variables);
                }
            }
        }
    }

    fn visit_syntax_template(
        &mut self,
        expr: &mut ExprKind,
        variables: &FxHashSet<InternedString>,
        quasi: bool,
    ) {
        match expr {
            ExprKind::Atom(a) => {
                if a.syn
                    .ty
                    .identifier()
                    .is_some_and(|x| !variables.contains(x))
                {
                    self.visit_atom(a);
                }
            }
            ExprKind::List(l)
                if quasi
                    && crate::parser::kernel::form_name(l)
                        .is_some_and(|x| x == "#%unsyntax" || x == "#%unsyntax-splicing") =>
            {
                for expr in l.args.iter_mut().skip(1) {
                    self.visit_phase1(expr, variables);
                }
            }
            ExprKind::List(l) => {
                for expr in l.args.iter_mut() {
                    self.visit_syntax_template(expr, variables, quasi);
                }
            }
            _ => {}
        }
    }
}

// The pattern variables of the syntax-case clauses and with-syntax bindings in transformer code
fn collect_pattern_variables(expr: &ExprKind, variables: &mut FxHashSet<InternedString>) {
    let ExprKind::List(l) = expr else {
        return;
    };
    let mut patterns = Vec::new();
    match l.first_ident().map(|x| x.resolve()) {
        Some("syntax-case" | "#%syntax-case") => {
            for clause in l.args.iter().skip(3) {
                if let Some(pattern) = clause.list().and_then(|x| x.args.first()) {
                    crate::parser::kernel::pattern_variables(pattern, &mut patterns);
                }
            }
        }
        Some("with-syntax") => {
            if let Some(ExprKind::List(bindings)) = l.args.get(1) {
                for binding in &bindings.args {
                    if let Some(pattern) = binding.list().and_then(|x| x.args.first()) {
                        crate::parser::kernel::pattern_variables(pattern, &mut patterns);
                    }
                }
            }
        }
        _ => {}
    }
    variables.extend(
        patterns
            .iter()
            .filter_map(ExprKind::atom_identifier)
            .copied(),
    );
    for expr in &l.args {
        collect_pattern_variables(expr, variables);
    }
}

impl VisitorMutRefUnit for NameMangler {
    #[inline]
    fn visit_atom(&mut self, a: &mut Atom) {
        // Written by another module's macro, where a builtin means the builtin. What this
        // module's own macros write means its globals.
        let resolved_elsewhere = match self.module {
            Some(own) => a.syn.scopes.has_module_except(own),
            None => a.syn.scopes.has_module(),
        };

        if let TokenType::Identifier(i) = &mut a.syn.ty {
            if resolved_elsewhere && is_prelude_builtin(i) {
                return;
            }

            if self.globals.contains(i) || self.macros.contains(i) {
                self.mangle(i);
            }
        }
    }

    /// We don't want quoted values to be mangled since those should match
    /// the real name given
    #[inline]
    fn visit_quote(&mut self, _q: &mut Quote) {}

    // (Unfortunately) A macro template needs to keep `(quote x)` and `(quasiquote x)` as lists
    #[inline]
    fn visit_list(&mut self, l: &mut List) {
        if is_procedural_definition(l) {
            return self.visit_procedural_definition(l);
        }

        let quote_keyword = l
            .first()
            .and_then(|head| head.atom_syntax_object())
            .is_some_and(|head| head.ty == TokenType::Quote);
        match l.first_ident() {
            _ if quote_keyword => {}
            Some(head) if *head == *QUOTE || *head == *RAW_QUOTE => {}
            Some(head) if *head == *QUASIQUOTE => {
                for expr in &mut l.args[1..] {
                    self.visit_quasiquoted(expr, 1);
                }
            }
            _ => {
                for expr in &mut l.args {
                    self.visit(expr);
                }
            }
        }
    }
}

#[cfg(test)]
mod name_mangling_tests {
    use steel_parser::visitors::Eraser;

    use super::*;

    use crate::parser::parser::Parser;

    #[test]
    fn basic_mangling() {
        let mut eraser = Eraser;

        let expr = r#"
           (define (foo x y z) (let ((a 10) (b 20)) (bar (+ x y z a b))))
           (define (bar applesauce) (+ applesauce 10))
        "#;

        let mut parsed = Parser::parse(expr).unwrap();

        mangle_vars_with_prefix("--test--".into(), &mut parsed);

        eraser.visit_many(&mut parsed);

        let mut expected = Parser::parse(
            r#"
            (define (--test--foo x y z) (let ((a 10) (b 20)) (--test--bar (+ x y z a b))))
            (define (--test--bar applesauce) (+ applesauce 10))
        "#,
        )
        .unwrap();

        eraser.visit_many(&mut expected);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn shadowed_global_still_mangled() {
        let mut eraser = Eraser;

        let expr = r#"
        (define (foo x y z) (let ((foo 10) (b 20)) (foo (+ bar y z a b))))
        (define (bar applesauce) (+ applesauce 10))
     "#;

        let mut parsed = Parser::parse(expr).unwrap();

        mangle_vars_with_prefix("--test--".into(), &mut parsed);

        eraser.visit_many(&mut parsed);

        let mut expected = Parser::parse(
            r#"
            (define (--test--foo x y z) (let ((--test--foo 10) (b 20)) (--test--foo (+ --test--bar y z a b))))
            (define (--test--bar applesauce) (+ applesauce 10))
     "#,
        )
        .unwrap();

        eraser.visit_many(&mut expected);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn still_collect_defines_in_begins() {
        let expr = r#"
        (begin 
            (begin 
                (begin 
                    (begin 
                        (define x 10)
                    ) 
                    (define y 20)
                )
            )
        )
        "#;

        let mut parsed = Parser::parse(expr).unwrap();

        mangle_vars_with_prefix("--test--".into(), &mut parsed);

        let expected = Parser::parse(
            r#"
        (begin 
            (begin 
                (begin 
                    (begin 
                        (define --test--x 10)
                    ) 
                    (define --test--y 20)
                )
            )
        )
        "#,
        )
        .unwrap();

        assert_eq!(parsed, expected);
    }
}
