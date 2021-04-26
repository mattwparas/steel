pub mod begin;
pub mod manager;

use crate::parser::ast::ExprKind;
use crate::parser::ast::*;
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
            ExprKind::Apply(a) => self.visit_apply(a),
            ExprKind::Panic(p) => self.visit_panic(p),
            ExprKind::Transduce(t) => self.visit_transduce(t),
            ExprKind::Read(r) => self.visit_read(r),
            ExprKind::Execute(e) => self.visit_execute(e),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Struct(s) => self.visit_struct(s),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Eval(e) => self.visit_eval(e),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::CallCC(cc) => self.visit_callcc(cc),
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
    fn visit_apply(&mut self, mut apply: Box<Apply>) -> ExprKind {
        apply.func = self.visit(apply.func);
        apply.list = self.visit(apply.list);
        ExprKind::Apply(apply)
    }

    #[inline]
    fn visit_panic(&mut self, mut p: Box<Panic>) -> ExprKind {
        p.message = self.visit(p.message);
        ExprKind::Panic(p)
    }

    #[inline]
    fn visit_transduce(&mut self, mut transduce: Box<Transduce>) -> ExprKind {
        transduce.transducer = self.visit(transduce.transducer);
        transduce.func = self.visit(transduce.func);
        transduce.initial_value = self.visit(transduce.initial_value);
        transduce.iterable = self.visit(transduce.iterable);
        ExprKind::Transduce(transduce)
    }

    #[inline]
    fn visit_read(&mut self, mut read: Box<Read>) -> ExprKind {
        read.expr = self.visit(read.expr);
        ExprKind::Read(read)
    }

    #[inline]
    fn visit_execute(&mut self, mut execute: Box<Execute>) -> ExprKind {
        execute.transducer = self.visit(execute.transducer);
        execute.collection = self.visit(execute.collection);
        execute.output_type = execute.output_type.map(|x| self.visit(x));
        ExprKind::Execute(execute)
    }

    #[inline]
    fn visit_quote(&mut self, mut quote: Box<Quote>) -> ExprKind {
        quote.expr = self.visit(quote.expr);
        ExprKind::Quote(quote)
    }

    #[inline]
    fn visit_struct(&mut self, s: Box<Struct>) -> ExprKind {
        ExprKind::Struct(s)
    }

    #[inline]
    fn visit_macro(&mut self, m: Macro) -> ExprKind {
        ExprKind::Macro(m)
    }

    #[inline]
    fn visit_eval(&mut self, mut e: Box<Eval>) -> ExprKind {
        e.expr = self.visit(e.expr);
        ExprKind::Eval(e)
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
    fn visit_syntax_rules(&mut self, l: SyntaxRules) -> ExprKind {
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

    #[inline]
    fn visit_callcc(&mut self, mut cc: Box<CallCC>) -> ExprKind {
        cc.expr = self.visit(cc.expr);
        ExprKind::CallCC(cc)
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
            ExprKind::Apply(a) => self.visit_apply(a),
            ExprKind::Panic(p) => self.visit_panic(p),
            ExprKind::Transduce(t) => self.visit_transduce(t),
            ExprKind::Read(r) => self.visit_read(r),
            ExprKind::Execute(e) => self.visit_execute(e),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Struct(s) => self.visit_struct(s),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Eval(e) => self.visit_eval(e),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::CallCC(cc) => self.visit_callcc(cc),
        }
    }

    #[inline]
    fn visit_if(&mut self, f: &If) {
        self.visit(&f.test_expr);
        self.visit(&f.then_expr);
        self.visit(&f.else_expr);
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
    fn visit_apply(&mut self, apply: &Apply) {
        self.visit(&apply.func);
        self.visit(&apply.list);
    }

    #[inline]
    fn visit_panic(&mut self, p: &Panic) {
        self.visit(&p.message);
    }

    #[inline]
    fn visit_transduce(&mut self, transduce: &Transduce) {
        self.visit(&transduce.transducer);
        self.visit(&transduce.func);
        self.visit(&transduce.initial_value);
        self.visit(&transduce.iterable);
    }

    #[inline]
    fn visit_read(&mut self, read: &Read) {
        self.visit(&read.expr);
    }

    #[inline]
    fn visit_execute(&mut self, execute: &Execute) {
        self.visit(&execute.transducer);
        self.visit(&execute.collection);
        execute.output_type.as_ref().map(|x| self.visit(x));
    }

    #[inline]
    fn visit_quote(&mut self, quote: &Quote) {
        self.visit(&quote.expr);
    }

    #[inline]
    fn visit_struct(&mut self, _s: &Struct) {}

    #[inline]
    fn visit_macro(&mut self, _m: &Macro) {}

    #[inline]
    fn visit_eval(&mut self, e: &Eval) {
        self.visit(&e.expr);
    }

    #[inline]
    fn visit_atom(&mut self, a: &Atom) {}

    #[inline]
    fn visit_list(&mut self, l: &List) {
        for expr in &l.args {
            self.visit(expr);
        }
    }

    #[inline]
    fn visit_syntax_rules(&mut self, l: &SyntaxRules) {}

    #[inline]
    fn visit_set(&mut self, s: &Set) {
        self.visit(&s.variable);
        self.visit(&s.expr);
    }

    #[inline]
    fn visit_require(&mut self, s: &Require) {}

    #[inline]
    fn visit_callcc(&mut self, cc: &CallCC) {
        self.visit(&cc.expr);
    }
}
