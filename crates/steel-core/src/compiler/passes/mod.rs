pub mod analysis;
pub mod begin;
pub mod manager;
pub mod mangle;
pub mod reader;
pub mod shadow;

use std::ops::ControlFlow;

use crate::parser::ast::ExprKind;
// use crate::parser::ast::*;

use crate::parser::ast::{
    Atom, Begin, Define, If, LambdaFunction, Let, List, Macro, Quote, Require, Return, Set,
    SyntaxRules,
};

pub trait Folder {
    fn fold(&mut self, ast: Vec<ExprKind>) -> Vec<ExprKind> {
        ast.into_iter().map(|x| self.visit(x)).collect()
    }

    // Whether or not the pass modified the input AST
    fn modified(&self) -> bool {
        false
    }

    fn visit(&mut self, expr: ExprKind) -> ExprKind {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
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
        }
    }

    #[inline]
    fn visit_if(&mut self, mut f: Box<If>) -> ExprKind {
        f.test_expr = self.visit(f.test_expr);
        f.then_expr = self.visit(f.then_expr);
        f.else_expr = self.visit(f.else_expr);
        ExprKind::If(f)
    }

    #[inline]
    fn visit_let(&mut self, mut l: Box<Let>) -> ExprKind {
        let mut visited_bindings = Vec::new();

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding), self.visit(expr)));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr);

        ExprKind::Let(l)
    }

    #[inline]
    fn visit_define(&mut self, mut define: Box<Define>) -> ExprKind {
        define.body = self.visit(define.body);
        ExprKind::Define(define)
    }

    #[inline]
    fn visit_lambda_function(&mut self, mut lambda_function: Box<LambdaFunction>) -> ExprKind {
        lambda_function.body = self.visit(lambda_function.body);
        ExprKind::LambdaFunction(lambda_function)
    }

    #[inline]
    fn visit_begin(&mut self, mut begin: Begin) -> ExprKind {
        begin.exprs = begin.exprs.into_iter().map(|e| self.visit(e)).collect();
        ExprKind::Begin(begin)
    }

    #[inline]
    fn visit_return(&mut self, mut r: Box<Return>) -> ExprKind {
        r.expr = self.visit(r.expr);
        ExprKind::Return(r)
    }

    #[inline]
    fn visit_quote(&mut self, quote: Box<Quote>) -> ExprKind {
        // quote.expr = self.visit(quote.expr);
        ExprKind::Quote(quote)
    }

    #[inline]
    fn visit_macro(&mut self, m: Box<Macro>) -> ExprKind {
        ExprKind::Macro(m)
    }

    #[inline]
    fn visit_atom(&mut self, a: Atom) -> ExprKind {
        ExprKind::Atom(a)
    }

    #[inline]
    fn visit_list(&mut self, mut l: List) -> ExprKind {
        l.args = l.args.into_iter().map(|e| self.visit(e)).collect();
        ExprKind::List(l)
    }

    #[inline]
    fn visit_syntax_rules(&mut self, l: Box<SyntaxRules>) -> ExprKind {
        ExprKind::SyntaxRules(l)
    }

    #[inline]
    fn visit_set(&mut self, mut s: Box<Set>) -> ExprKind {
        s.variable = self.visit(s.variable);
        s.expr = self.visit(s.expr);
        ExprKind::Set(s)
    }

    #[inline]
    fn visit_require(&mut self, s: Require) -> ExprKind {
        ExprKind::Require(s)
    }
}

pub trait VisitorMutUnit {
    fn visit(&mut self, expr: &ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
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
        }
    }

    #[inline]
    fn visit_if(&mut self, f: &If) {
        self.visit(&f.test_expr);
        self.visit(&f.then_expr);
        self.visit(&f.else_expr);
    }

    #[inline]
    fn visit_let(&mut self, l: &Let) {
        l.bindings.iter().for_each(|x| self.visit(&x.1));
        self.visit(&l.body_expr);
    }

    #[inline]
    fn visit_define(&mut self, define: &Define) {
        self.visit(&define.body);
    }

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) {
        self.visit(&lambda_function.body);
    }

    #[inline]
    fn visit_begin(&mut self, begin: &Begin) {
        for expr in &begin.exprs {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_return(&mut self, r: &Return) {
        self.visit(&r.expr);
    }

    #[inline]
    fn visit_quote(&mut self, quote: &Quote) {
        self.visit(&quote.expr);
    }

    #[inline]
    fn visit_macro(&mut self, _m: &Macro) {}

    #[inline]
    fn visit_atom(&mut self, _a: &Atom) {}

    #[inline]
    fn visit_list(&mut self, l: &List) {
        for expr in &l.args {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_syntax_rules(&mut self, _l: &SyntaxRules) {}

    #[inline]
    fn visit_set(&mut self, s: &Set) {
        self.visit(&s.variable);
        self.visit(&s.expr);
    }

    #[inline]
    fn visit_require(&mut self, _s: &Require) {}
}

pub trait VisitorMutControlFlow {
    fn visit(&mut self, expr: &ExprKind) -> ControlFlow<()> {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
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
        }
    }

    #[inline]
    fn visit_if(&mut self, f: &If) -> ControlFlow<()> {
        self.visit(&f.test_expr)?;
        self.visit(&f.then_expr)?;
        self.visit(&f.else_expr)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_let(&mut self, l: &Let) -> ControlFlow<()> {
        for binding in &l.bindings {
            self.visit(&binding.1)?;
        }

        self.visit(&l.body_expr)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_define(&mut self, define: &Define) -> ControlFlow<()> {
        self.visit(&define.body)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> ControlFlow<()> {
        self.visit(&lambda_function.body)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_begin(&mut self, begin: &Begin) -> ControlFlow<()> {
        for expr in &begin.exprs {
            self.visit(expr)?;
        }

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_return(&mut self, r: &Return) -> ControlFlow<()> {
        self.visit(&r.expr)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_quote(&mut self, quote: &Quote) -> ControlFlow<()> {
        self.visit(&quote.expr)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_macro(&mut self, _m: &Macro) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_atom(&mut self, _a: &Atom) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_list(&mut self, l: &List) -> ControlFlow<()> {
        for expr in &l.args {
            self.visit(expr)?;
        }

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_syntax_rules(&mut self, _l: &SyntaxRules) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_set(&mut self, s: &Set) -> ControlFlow<()> {
        self.visit(&s.variable)?;
        self.visit(&s.expr)?;

        ControlFlow::Continue(())
    }

    #[inline]
    fn visit_require(&mut self, _s: &Require) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

pub trait VisitorMutUnitRef<'a> {
    fn visit(&mut self, expr: &'a ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
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
        }
    }

    #[inline]
    fn visit_if(&mut self, f: &'a If) {
        self.visit(&f.test_expr);
        self.visit(&f.then_expr);
        self.visit(&f.else_expr);
    }

    #[inline]
    fn visit_let(&mut self, l: &'a Let) {
        l.bindings.iter().for_each(|x| self.visit(&x.1));
        self.visit(&l.body_expr);
    }

    #[inline]
    fn visit_define(&mut self, define: &'a Define) {
        self.visit(&define.name);
        self.visit(&define.body);
    }

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &'a LambdaFunction) {
        self.visit(&lambda_function.body);
    }

    #[inline]
    fn visit_begin(&mut self, begin: &'a Begin) {
        for expr in &begin.exprs {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_return(&mut self, r: &'a Return) {
        self.visit(&r.expr);
    }

    #[inline]
    fn visit_quote(&mut self, quote: &'a Quote) {
        self.visit(&quote.expr);
    }

    #[inline]
    fn visit_macro(&mut self, m: &'a Macro) {
        self.visit_syntax_rules(&m.syntax_rules);
    }

    #[inline]
    fn visit_atom(&mut self, _a: &'a Atom) {}

    #[inline]
    fn visit_list(&mut self, l: &'a List) {
        for expr in &l.args {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_syntax_rules(&mut self, s: &'a SyntaxRules) {
        for pattern in &s.patterns {
            self.visit(&pattern.body);
        }
    }

    #[inline]
    fn visit_set(&mut self, s: &'a Set) {
        self.visit(&s.variable);
        self.visit(&s.expr);
    }

    #[inline]
    fn visit_require(&mut self, _s: &'a Require) {}
}

pub trait VisitorMutRefUnit {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
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
        }
    }

    #[inline]
    fn visit_if(&mut self, f: &mut If) {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
    }

    #[inline]
    fn visit_let(&mut self, l: &mut Let) {
        l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.visit(&mut l.body_expr);
    }

    #[inline]
    fn visit_define(&mut self, define: &mut Define) {
        self.visit(&mut define.name);
        self.visit(&mut define.body);
    }

    #[inline]
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
        for var in &mut lambda_function.args {
            self.visit(var);
        }
        self.visit(&mut lambda_function.body);
    }

    #[inline]
    fn visit_begin(&mut self, begin: &mut Begin) {
        for expr in &mut begin.exprs {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_return(&mut self, r: &mut Return) {
        self.visit(&mut r.expr);
    }

    #[inline]
    fn visit_quote(&mut self, quote: &mut Quote) {
        self.visit(&mut quote.expr);
    }

    #[inline]
    fn visit_macro(&mut self, _m: &mut Macro) {}

    #[inline]
    fn visit_atom(&mut self, _a: &mut Atom) {}

    #[inline]
    fn visit_list(&mut self, l: &mut List) {
        for expr in &mut l.args {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_syntax_rules(&mut self, _l: &mut SyntaxRules) {}

    #[inline]
    fn visit_set(&mut self, s: &mut Set) {
        self.visit(&mut s.variable);
        self.visit(&mut s.expr);
    }

    #[inline]
    fn visit_require(&mut self, _s: &mut Require) {}
}
