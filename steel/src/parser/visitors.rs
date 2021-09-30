use crate::parser::ast::*;
use crate::rvals::Result;

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
            ExprKind::Read(r) => self.visit_read(r),
            // ExprKind::Execute(e) => self.visit_execute(e),
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

    fn visit_if(&mut self, f: &If) -> Self::Output;
    fn visit_define(&mut self, define: &Define) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&mut self, begin: &Begin) -> Self::Output;
    fn visit_return(&mut self, r: &Return) -> Self::Output;
    // fn visit_apply(&mut self, apply: &Apply) -> Self::Output;
    // fn visit_panic(&mut self, p: &Panic) -> Self::Output;
    // fn visit_transduce(&mut self, transduce: &Transduce) -> Self::Output;
    fn visit_read(&mut self, read: &Read) -> Self::Output;
    // fn visit_execute(&mut self, execute: &Execute) -> Self::Output;
    fn visit_quote(&mut self, quote: &Quote) -> Self::Output;
    fn visit_struct(&mut self, s: &Struct) -> Self::Output;
    fn visit_macro(&mut self, m: &Macro) -> Self::Output;
    fn visit_eval(&mut self, e: &Eval) -> Self::Output;
    fn visit_atom(&mut self, a: &Atom) -> Self::Output;
    fn visit_list(&mut self, l: &List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: &SyntaxRules) -> Self::Output;
    fn visit_set(&mut self, s: &Set) -> Self::Output;
    fn visit_require(&mut self, s: &Require) -> Self::Output;
    fn visit_callcc(&mut self, cc: &CallCC) -> Self::Output;
    fn visit_let(&mut self, l: &Let) -> Self::Output;
}

// TODO
pub trait VisitorMutResult {
    type Output;

    fn visit(&mut self, expr: &ExprKind) -> Result<Self::Output> {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Read(r) => self.visit_read(r),
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

    fn visit_if(&mut self, f: &If) -> Result<Self::Output>;
    fn visit_define(&mut self, define: &Define) -> Result<Self::Output>;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Result<Self::Output>;
    fn visit_begin(&mut self, begin: &Begin) -> Result<Self::Output>;
    fn visit_return(&mut self, r: &Return) -> Result<Self::Output>;
    fn visit_read(&mut self, read: &Read) -> Result<Self::Output>;
    fn visit_quote(&mut self, quote: &Quote) -> Result<Self::Output>;
    fn visit_struct(&mut self, s: &Struct) -> Result<Self::Output>;
    fn visit_macro(&mut self, m: &Macro) -> Result<Self::Output>;
    fn visit_eval(&mut self, e: &Eval) -> Result<Self::Output>;
    fn visit_atom(&mut self, e: &Atom) -> Result<Self::Output>;
    fn visit_list(&mut self, l: &List) -> Result<Self::Output>;
    fn visit_syntax_rules(&mut self, l: &SyntaxRules) -> Result<Self::Output>;
    fn visit_set(&mut self, s: &Set) -> Result<Self::Output>;
    fn visit_require(&mut self, s: &Require) -> Result<Self::Output>;
    fn visit_callcc(&mut self, cc: &CallCC) -> Result<Self::Output>;
    fn visit_let(&mut self, l: &Let) -> Result<Self::Output>;
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
            ExprKind::Read(r) => self.visit_read(r),
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

    fn visit_if(&self, f: &If) -> Self::Output;
    fn visit_define(&self, define: &Define) -> Self::Output;
    fn visit_lambda_function(&self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&self, begin: &Begin) -> Self::Output;
    fn visit_return(&self, r: &Return) -> Self::Output;
    fn visit_read(&self, read: &Read) -> Self::Output;
    fn visit_quote(&self, quote: &Quote) -> Self::Output;
    fn visit_struct(&self, s: &Struct) -> Self::Output;
    fn visit_macro(&self, m: &Macro) -> Self::Output;
    fn visit_eval(&self, e: &Eval) -> Self::Output;
    fn visit_atom(&self, a: &Atom) -> Self::Output;
    fn visit_list(&self, l: &List) -> Self::Output;
    fn visit_syntax_rules(&self, l: &SyntaxRules) -> Self::Output;
    fn visit_set(&self, s: &Set) -> Self::Output;
    fn visit_require(&self, s: &Require) -> Self::Output;
    fn visit_callcc(&self, cc: &CallCC) -> Self::Output;
    fn visit_let(&self, l: &Let) -> Self::Output;
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
            ExprKind::Read(r) => self.visit_read(r),
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

    fn visit_if(&mut self, f: Box<If>) -> Self::Output;
    fn visit_define(&mut self, define: Box<Define>) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: Box<LambdaFunction>) -> Self::Output;
    fn visit_begin(&mut self, begin: Begin) -> Self::Output;
    fn visit_return(&mut self, r: Box<Return>) -> Self::Output;
    fn visit_read(&mut self, read: Box<Read>) -> Self::Output;
    fn visit_quote(&mut self, quote: Box<Quote>) -> Self::Output;
    fn visit_struct(&mut self, s: Box<Struct>) -> Self::Output;
    fn visit_macro(&mut self, m: Macro) -> Self::Output;
    fn visit_eval(&mut self, e: Box<Eval>) -> Self::Output;
    fn visit_atom(&mut self, a: Atom) -> Self::Output;
    fn visit_list(&mut self, l: List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: SyntaxRules) -> Self::Output;
    fn visit_set(&mut self, s: Box<Set>) -> Self::Output;
    fn visit_require(&mut self, s: Require) -> Self::Output;
    fn visit_callcc(&mut self, cc: Box<CallCC>) -> Self::Output;
    fn visit_let(&mut self, l: Box<Let>) -> Self::Output;
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
            ExprKind::Read(r) => self.visit_read(r),
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

    fn visit_if(&self, f: Box<If>) -> Self::Output;
    fn visit_define(&self, define: Box<Define>) -> Self::Output;
    fn visit_lambda_function(&self, lambda_function: Box<LambdaFunction>) -> Self::Output;
    fn visit_begin(&self, begin: Begin) -> Self::Output;
    fn visit_return(&self, r: Box<Return>) -> Self::Output;
    fn visit_read(&self, read: Box<Read>) -> Self::Output;
    fn visit_quote(&self, quote: Box<Quote>) -> Self::Output;
    fn visit_struct(&self, s: Box<Struct>) -> Self::Output;
    fn visit_macro(&self, m: Macro) -> Self::Output;
    fn visit_eval(&self, e: Box<Eval>) -> Self::Output;
    fn visit_atom(&self, a: Atom) -> Self::Output;
    fn visit_list(&self, l: List) -> Self::Output;
    fn visit_syntax_rules(&self, l: SyntaxRules) -> Self::Output;
    fn visit_set(&self, s: Box<Set>) -> Self::Output;
    fn visit_require(&self, s: Require) -> Self::Output;
    fn visit_callcc(&self, cc: Box<CallCC>) -> Self::Output;
    fn visit_let(&self, l: Box<Let>) -> Self::Output;
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
            ExprKind::Read(r) => self.visit_read(r),
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

    fn visit_if(&mut self, f: &mut If) -> Self::Output;
    fn visit_define(&mut self, define: &mut Define) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: &mut LambdaFunction) -> Self::Output;
    fn visit_begin(&mut self, begin: &mut Begin) -> Self::Output;
    fn visit_return(&mut self, r: &mut Return) -> Self::Output;
    fn visit_read(&mut self, read: &mut Read) -> Self::Output;
    fn visit_quote(&mut self, quote: &mut Quote) -> Self::Output;
    fn visit_struct(&mut self, s: &mut Struct) -> Self::Output;
    fn visit_macro(&mut self, m: &mut Macro) -> Self::Output;
    fn visit_eval(&mut self, e: &mut Eval) -> Self::Output;
    fn visit_atom(&mut self, a: &mut Atom) -> Self::Output;
    fn visit_list(&mut self, l: &mut List) -> Self::Output;
    fn visit_syntax_rules(&mut self, l: &mut SyntaxRules) -> Self::Output;
    fn visit_set(&mut self, s: &mut Set) -> Self::Output;
    fn visit_require(&mut self, s: &mut Require) -> Self::Output;
    fn visit_callcc(&mut self, cc: &mut CallCC) -> Self::Output;
    fn visit_let(&mut self, l: &mut Let) -> Self::Output;
}
