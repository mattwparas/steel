use crate::ast::*;

use crate::ast::{
    Atom, Begin, Define, If, LambdaFunction, Let, List, Macro, Quote, Require, Return, Set,
    SyntaxRules,
};

pub use eraser::Eraser;

mod eraser {
    use crate::span::Span;

    use super::*;

    pub struct Eraser;

    impl Eraser {
        pub fn visit_many(&mut self, exprs: &mut [ExprKind]) {
            for expr in exprs {
                self.visit(expr)
            }
        }
    }

    impl VisitorMutRef for Eraser {
        type Output = ();

        fn visit_if(&mut self, if_: &mut If) {
            if_.location.span = Span::default();

            self.visit(&mut if_.test_expr);
            self.visit(&mut if_.then_expr);
            self.visit(&mut if_.else_expr);
        }

        fn visit_define(&mut self, define: &mut Define) {
            define.location.span = Span::default();

            self.visit(&mut define.name);
            self.visit(&mut define.body);
        }

        fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) {
            lambda_function.location.span = Span::default();

            lambda_function
                .args
                .iter_mut()
                .for_each(|arg| self.visit(arg));
            self.visit(&mut lambda_function.body);
        }

        fn visit_begin(&mut self, begin: &mut Begin) {
            begin.location.span = Span::default();

            begin.exprs.iter_mut().for_each(|expr| self.visit(expr));
        }

        fn visit_return(&mut self, return_: &mut Return) {
            return_.location.span = Span::default();

            self.visit(&mut return_.expr);
        }

        fn visit_quote(&mut self, quote: &mut Quote) {
            quote.location.span = Span::default();

            self.visit(&mut quote.expr);
        }

        fn visit_macro(&mut self, macro_: &mut Macro) {
            macro_.location.span = Span::default();

            self.visit(&mut macro_.name);
            self.visit_syntax_rules(&mut macro_.syntax_rules);
        }

        fn visit_atom(&mut self, atom: &mut Atom) {
            atom.syn.span = Span::default();
        }

        fn visit_list(&mut self, list: &mut List) {
            list.location = Span::default();

            list.args.iter_mut().for_each(|expr| self.visit(expr));
        }

        fn visit_syntax_rules(&mut self, rules: &mut SyntaxRules) {
            rules.location.span = Span::default();

            rules.syntax.iter_mut().for_each(|expr| self.visit(expr));
            rules.patterns.iter_mut().for_each(|pattern| {
                self.visit(&mut pattern.pattern);
                self.visit(&mut pattern.body);
            });
        }

        fn visit_set(&mut self, set: &mut Set) {
            set.location.span = Span::default();

            self.visit(&mut set.variable);
            self.visit(&mut set.expr);
        }

        fn visit_require(&mut self, require: &mut Require) {
            require.location.span = Span::default();

            require.modules.iter_mut().for_each(|expr| self.visit(expr));
        }

        fn visit_let(&mut self, let_: &mut Let) {
            let_.location.span = Span::default();

            let_.bindings.iter_mut().for_each(|binding| {
                self.visit(&mut binding.0);
                self.visit(&mut binding.1);
            });

            self.visit(&mut let_.body_expr);
        }

        fn visit_vector(&mut self, v: &mut Vector) -> Self::Output {
            v.span = Span::default();

            for arg in &mut v.args {
                self.visit(arg);
            }
        }
    }
}

pub trait VisitorMut {
    type Output;

    fn visit(&mut self, expr: &ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&mut self, f: &If) -> Self::Output;
    fn visit_define(&mut self, define: &Define) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&mut self, begin: &Begin) -> Self::Output;
    fn visit_return(&mut self, r: &Return) -> Self::Output;
    fn visit_quote(&mut self, quote: &Quote) -> Self::Output;
    fn visit_macro(&mut self, m: &Macro) -> Self::Output;
    fn visit_atom(&mut self, a: &Atom) -> Self::Output;
    fn visit_list(&mut self, l: &List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: &SyntaxRules) -> Self::Output;
    fn visit_set(&mut self, s: &Set) -> Self::Output;
    fn visit_require(&mut self, s: &Require) -> Self::Output;
    fn visit_let(&mut self, l: &Let) -> Self::Output;
    fn visit_vector(&mut self, v: &Vector) -> Self::Output;
}

pub trait Visitor {
    type Output;

    fn visit(&self, expr: &ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&self, f: &If) -> Self::Output;
    fn visit_define(&self, define: &Define) -> Self::Output;
    fn visit_lambda_function(&self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&self, begin: &Begin) -> Self::Output;
    fn visit_return(&self, r: &Return) -> Self::Output;
    fn visit_quote(&self, quote: &Quote) -> Self::Output;
    fn visit_macro(&self, m: &Macro) -> Self::Output;
    fn visit_atom(&self, a: &Atom) -> Self::Output;
    fn visit_list(&self, l: &List) -> Self::Output;
    fn visit_syntax_rules(&self, l: &SyntaxRules) -> Self::Output;
    fn visit_set(&self, s: &Set) -> Self::Output;
    fn visit_require(&self, s: &Require) -> Self::Output;
    fn visit_let(&self, l: &Let) -> Self::Output;
    fn visit_vector(&self, v: &Vector) -> Self::Output;
}

pub trait ConsumingVisitor {
    type Output;
    fn visit(&mut self, expr: ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&mut self, f: Box<If>) -> Self::Output;
    fn visit_define(&mut self, define: Box<Define>) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: Box<LambdaFunction>) -> Self::Output;
    fn visit_begin(&mut self, begin: Box<Begin>) -> Self::Output;
    fn visit_return(&mut self, r: Box<Return>) -> Self::Output;
    fn visit_quote(&mut self, quote: Box<Quote>) -> Self::Output;
    fn visit_macro(&mut self, m: Box<Macro>) -> Self::Output;
    fn visit_atom(&mut self, a: Atom) -> Self::Output;
    fn visit_list(&mut self, l: List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: Box<SyntaxRules>) -> Self::Output;
    fn visit_set(&mut self, s: Box<Set>) -> Self::Output;
    fn visit_require(&mut self, s: Box<Require>) -> Self::Output;
    fn visit_let(&mut self, l: Box<Let>) -> Self::Output;
    fn visit_vector(&mut self, v: Vector) -> Self::Output;
}

pub trait ConsumingVisitorRef {
    type Output;
    fn visit(&self, expr: ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&self, f: Box<If>) -> Self::Output;
    fn visit_define(&self, define: Box<Define>) -> Self::Output;
    fn visit_lambda_function(&self, lambda_function: Box<LambdaFunction>) -> Self::Output;
    fn visit_begin(&self, begin: Box<Begin>) -> Self::Output;
    fn visit_return(&self, r: Box<Return>) -> Self::Output;
    fn visit_quote(&self, quote: Box<Quote>) -> Self::Output;
    fn visit_macro(&self, m: Box<Macro>) -> Self::Output;
    fn visit_atom(&self, a: Atom) -> Self::Output;
    fn visit_list(&self, l: List) -> Self::Output;
    fn visit_syntax_rules(&self, l: Box<SyntaxRules>) -> Self::Output;
    fn visit_set(&self, s: Box<Set>) -> Self::Output;
    fn visit_require(&self, s: Box<Require>) -> Self::Output;
    fn visit_let(&self, l: Box<Let>) -> Self::Output;
    fn visit_vector(&self, v: Vector) -> Self::Output;
}

pub trait VisitorMutRef {
    type Output;

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }

    fn visit_if(&mut self, f: &mut If) -> Self::Output;
    fn visit_define(&mut self, define: &mut Define) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) -> Self::Output;
    fn visit_begin(&mut self, begin: &mut Begin) -> Self::Output;
    fn visit_return(&mut self, r: &mut Return) -> Self::Output;
    fn visit_quote(&mut self, quote: &mut Quote) -> Self::Output;
    fn visit_macro(&mut self, m: &mut Macro) -> Self::Output;
    fn visit_atom(&mut self, a: &mut Atom) -> Self::Output;
    fn visit_list(&mut self, l: &mut List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: &mut SyntaxRules) -> Self::Output;
    fn visit_set(&mut self, s: &mut Set) -> Self::Output;
    fn visit_require(&mut self, s: &mut Require) -> Self::Output;
    fn visit_let(&mut self, l: &mut Let) -> Self::Output;
    fn visit_vector(&mut self, v: &mut Vector) -> Self::Output;
}
