use fxhash::FxHashSet;
use steel_parser::ast::DEFINE;

use crate::{
    compiler::program::BEGIN,
    parser::{
        ast::{Atom, ExprKind, Quote},
        interner::InternedString,
        tokens::TokenType,
    },
};

use super::VisitorMutRefUnit;

/*
Steps for doing having scoped macros

- A requires a macro (or macros) from B
    - Expand A with macros from A
    - Then expand A with macros from B / C / D
        - A must be done first, but then the rest can be done in phases
    - Copy the code for B, mangle it and then include it in A directly
*/

pub fn collect_globals(exprs: &[ExprKind]) -> FxHashSet<InternedString> {
    let mut global_defs = FxHashSet::default();

    for expr in exprs {
        match expr {
            ExprKind::Define(d) => {
                if let Some(name) = d.name.atom_identifier() {
                    if name.resolve().starts_with("mangler") {
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
                    Some(ExprKind::Atom(_)) => {
                        if let Some(name) = l.second_ident() {
                            if name.resolve().starts_with("mangler") {
                                continue;
                            }
                            // println!("Inserting: {}", name);
                            global_defs.insert(*name);
                        }
                    }

                    Some(ExprKind::List(l)) => {
                        if let Some(name) = l.first_ident() {
                            if name.resolve().starts_with("mangler") {
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

#[derive(Clone)]
pub struct NameMangler {
    pub(crate) globals: FxHashSet<InternedString>,
    prefix: String,
}

impl NameMangler {
    pub fn new(globals: FxHashSet<InternedString>, prefix: String) -> Self {
        Self { globals, prefix }
    }

    pub fn mangle_vars(&mut self, exprs: &mut [ExprKind]) {
        for expr in exprs {
            self.visit(expr);
        }
    }
}

pub fn mangle_vars_with_prefix(prefix: String, exprs: &mut [ExprKind]) {
    let globals = collect_globals(exprs);

    let mut name_mangler = NameMangler { globals, prefix };

    for expr in exprs {
        name_mangler.visit(expr);
    }
}

impl VisitorMutRefUnit for NameMangler {
    #[inline]
    fn visit_atom(&mut self, a: &mut Atom) {
        if let TokenType::Identifier(i) = &mut a.syn.ty {
            if self.globals.contains(i) {
                let new_str = i.resolve();

                *i = (self.prefix.clone() + new_str).into();

                // i.insert_str(0, &self.prefix);
            }
        }
    }

    /// We don't want quoted values to be mangled since those should match
    /// the real name given
    #[inline]
    fn visit_quote(&mut self, _q: &mut Quote) {}
}

#[cfg(test)]
mod name_mangling_tests {
    use super::*;

    use crate::parser::parser::Parser;

    #[test]
    fn basic_mangling() {
        let expr = r#"
           (define (foo x y z) (let ((a 10) (b 20)) (bar (+ x y z a b))))
           (define (bar applesauce) (+ applesauce 10))
        "#;

        let mut parsed = Parser::parse(expr).unwrap();

        mangle_vars_with_prefix("--test--".to_string(), &mut parsed);

        let expected = Parser::parse(
            r#"
            (define (--test--foo x y z) (let ((a 10) (b 20)) (--test--bar (+ x y z a b))))
            (define (--test--bar applesauce) (+ applesauce 10))
        "#,
        )
        .unwrap();

        assert_eq!(parsed, expected);
    }

    #[test]
    fn shadowed_global_still_mangled() {
        let expr = r#"
        (define (foo x y z) (let ((foo 10) (b 20)) (foo (+ bar y z a b))))
        (define (bar applesauce) (+ applesauce 10))
     "#;

        let mut parsed = Parser::parse(expr).unwrap();

        mangle_vars_with_prefix("--test--".to_string(), &mut parsed);

        let expected = Parser::parse(
            r#"
            (define (--test--foo x y z) (let ((--test--foo 10) (b 20)) (--test--foo (+ --test--bar y z a b))))
            (define (--test--bar applesauce) (+ applesauce 10))
     "#,
        )
        .unwrap();

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

        mangle_vars_with_prefix("--test--".to_string(), &mut parsed);

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
