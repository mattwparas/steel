use crate::core::instructions::Instruction;
use crate::new_parser::ast::*;
use crate::parser::{tokens::TokenType, Expr, SyntaxObject};
use crate::rvals::Result;

// fn expression_from_expr(expr: Expr) -> Expression {
//     match expr {
//         Expr::Atom(s) => Expression::new(Box::new(Atom::new(s))),
//         Expr::VectorVal(list_of_tokens) => {
//             if let Some(f) = list_of_tokens.first() {
//                 match f {
//                     Expr::Atom(SyntaxObject {
//                         ty: TokenType::Identifier(s),
//                         ..
//                     }) if s == "quote" => Expression::new(Box::new(Quote::new(
//                         expression_from_expr(list_of_tokens[0].clone()),
//                     ))),
//                     _ => {
//                         unimplemented!();
//                     }
//                 }
//             } else {
//                 unimplemented!()
//             }
//         }
//     }
// }

// Expr::Atom(s) => {
//     instructions.push(Instruction::new(OpCode::PUSH, 0, s.clone(), true));
// }
// Expr::VectorVal(list_of_tokens) => {
//     if let Some(f) = list_of_tokens.first() {
//         match f.deref() {
//             Expr::Atom(SyntaxObject {
//                 ty: TokenType::Identifier(s),
//                 ..
//             }) if s == "quote" => {
//                 check_length("quote", &list_of_tokens, 2)?;
//                 let converted = SteelVal::try_from(list_of_tokens[1].clone())?;
//                 let idx = constant_map.add_or_get(Gc::new(converted));
//                 instructions.push(Instruction::new_push_const(idx));
//                 // instructions.push(Instruction::new_quote());
//                 return Ok(());
//             }

pub trait VisitorMut {
    type Output;

    fn visit(&mut self, expr: &ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::DefineFunction(f) => self.visit_define_function(f),
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
        }
    }

    fn visit_if(&mut self, f: &If) -> Self::Output;
    fn visit_define(&mut self, define: &Define) -> Self::Output;
    fn visit_define_function(&mut self, define: &DefineFunction) -> Self::Output;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&mut self, begin: &Begin) -> Self::Output;
    fn visit_return(&mut self, r: &Return) -> Self::Output;
    fn visit_apply(&mut self, apply: &Apply) -> Self::Output;
    fn visit_panic(&mut self, p: &Panic) -> Self::Output;
    fn visit_transduce(&mut self, transduce: &Transduce) -> Self::Output;
    fn visit_read(&mut self, read: &Read) -> Self::Output;
    fn visit_execute(&mut self, execute: &Execute) -> Self::Output;
    fn visit_quote(&mut self, quote: &Quote) -> Self::Output;
    fn visit_struct(&mut self, s: &Struct) -> Self::Output;
    fn visit_macro(&mut self, m: &Macro) -> Self::Output;
    fn visit_eval(&mut self, e: &Eval) -> Self::Output;
    fn visit_atom(&mut self, a: &Atom) -> Self::Output;
    fn visit_list(&mut self, l: &List) -> Self::Output;
}

// TODO
pub trait VisitorMutResult {
    type Output;

    fn visit(&mut self, expr: &ExprKind) -> Result<Self::Output> {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::DefineFunction(f) => self.visit_define_function(f),
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
        }
    }

    fn visit_if(&mut self, f: &If) -> Result<Self::Output>;
    fn visit_define(&mut self, define: &Define) -> Result<Self::Output>;
    fn visit_define_function(&mut self, define_function: &DefineFunction) -> Result<Self::Output>;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Result<Self::Output>;
    fn visit_begin(&mut self, begin: &Begin) -> Result<Self::Output>;
    fn visit_return(&mut self, r: &Return) -> Result<Self::Output>;
    fn visit_apply(&mut self, apply: &Apply) -> Result<Self::Output>;
    fn visit_panic(&mut self, p: &Panic) -> Result<Self::Output>;
    fn visit_transduce(&mut self, transduce: &Transduce) -> Result<Self::Output>;
    fn visit_read(&mut self, read: &Read) -> Result<Self::Output>;
    fn visit_execute(&mut self, execute: &Execute) -> Result<Self::Output>;
    fn visit_quote(&mut self, quote: &Quote) -> Result<Self::Output>;
    fn visit_struct(&mut self, s: &Struct) -> Result<Self::Output>;
    fn visit_macro(&mut self, m: &Macro) -> Result<Self::Output>;
    fn visit_eval(&mut self, e: &Eval) -> Result<Self::Output>;
    fn visit_atom(&mut self, e: &Atom) -> Result<Self::Output>;
    fn visit_list(&mut self, l: &List) -> Result<Self::Output>;
}

pub trait Visitor {
    type Output;

    fn visit(&mut self, expr: &ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::DefineFunction(f) => self.visit_define_function(f),
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
        }
    }

    fn visit_if(&self, f: &If) -> Self::Output;
    fn visit_define(&self, define: &Define) -> Self::Output;
    fn visit_define_function(&self, define_function: &DefineFunction) -> Self::Output;
    fn visit_lambda_function(&self, lambda_function: &LambdaFunction) -> Self::Output;
    fn visit_begin(&self, begin: &Begin) -> Self::Output;
    fn visit_return(&self, r: &Return) -> Self::Output;
    fn visit_apply(&self, apply: &Apply) -> Self::Output;
    fn visit_panic(&self, p: &Panic) -> Self::Output;
    fn visit_transduce(&self, transduce: &Transduce) -> Self::Output;
    fn visit_read(&self, read: &Read) -> Self::Output;
    fn visit_execute(&self, execute: &Execute) -> Self::Output;
    fn visit_quote(&self, quote: &Quote) -> Self::Output;
    fn visit_struct(&self, s: &Struct) -> Self::Output;
    fn visit_macro(&self, m: &Macro) -> Self::Output;
    fn visit_eval(&self, e: &Eval) -> Self::Output;
    fn visit_atom(&self, a: &Atom) -> Self::Output;
    fn visit_list(&self, l: &List) -> Self::Output;
}

pub trait ConsumingVisitor {
    type Output;
    fn visit(self: Box<Self>, expr: ExprKind) -> Self::Output {
        match expr {
            ExprKind::If(f) => self.visit_if(f),
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::DefineFunction(f) => self.visit_define_function(f),
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
        }
    }

    fn visit_if(self: Box<Self>, f: Box<If>) -> Self::Output;
    fn visit_define(self: Box<Self>, define: Box<Define>) -> Self::Output;
    fn visit_define_function(self: Box<Self>, define_function: Box<DefineFunction>)
        -> Self::Output;
    fn visit_lambda_function(self: Box<Self>, lambda_function: Box<LambdaFunction>)
        -> Self::Output;
    fn visit_begin(self: Box<Self>, begin: Begin) -> Self::Output;
    fn visit_return(self: Box<Self>, r: Box<Return>) -> Self::Output;
    fn visit_apply(self: Box<Self>, apply: Box<Apply>) -> Self::Output;
    fn visit_panic(self: Box<Self>, p: Box<Panic>) -> Self::Output;
    fn visit_transduce(self: Box<Self>, transduce: Box<Transduce>) -> Self::Output;
    fn visit_read(self: Box<Self>, read: Box<Read>) -> Self::Output;
    fn visit_execute(self: Box<Self>, execute: Box<Execute>) -> Self::Output;
    fn visit_quote(self: Box<Self>, quote: Box<Quote>) -> Self::Output;
    fn visit_struct(self: Box<Self>, s: Box<Struct>) -> Self::Output;
    fn visit_macro(self: Box<Self>, m: Box<Macro>) -> Self::Output;
    fn visit_eval(self: Box<Self>, e: Box<Eval>) -> Self::Output;
    fn visit_atom(self: Box<Self>, a: Atom) -> Self::Output;
    fn visit_list(self: Box<Self>, l: List) -> Self::Output;
}

// pub trait VisitChildren<T> {
//     // type Output = Result<T>;
//     fn visit_children(&self, visitor: impl Visitor) -> Result<T>;
// }

// pub trait VisitChildrenMut {
//     type Output;
//     fn visit_children(&self, visitor: &mut impl VisitorMut) -> Self::Output;
// }

// impl VisitChildren<T> for
