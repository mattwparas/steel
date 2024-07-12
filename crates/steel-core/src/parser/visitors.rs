use crate::parser::ast::*;
use crate::rvals::Result;

use crate::parser::ast::{
    Atom, Begin, Define, If, LambdaFunction, Let, List, Macro, Quote, Require, Return, Set,
    SyntaxRules,
};

use steel_parser::ast::Vector;
pub use steel_parser::visitors::{
    ConsumingVisitor, ConsumingVisitorRef, Visitor, VisitorMut, VisitorMutRef,
};

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

    fn visit_if(&mut self, f: &If) -> Result<Self::Output>;
    fn visit_define(&mut self, define: &Define) -> Result<Self::Output>;
    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Result<Self::Output>;
    fn visit_begin(&mut self, begin: &Begin) -> Result<Self::Output>;
    fn visit_return(&mut self, r: &Return) -> Result<Self::Output>;
    fn visit_quote(&mut self, quote: &Quote) -> Result<Self::Output>;
    fn visit_macro(&mut self, m: &Macro) -> Result<Self::Output>;
    fn visit_atom(&mut self, e: &Atom) -> Result<Self::Output>;
    fn visit_list(&mut self, l: &List) -> Result<Self::Output>;
    fn visit_syntax_rules(&mut self, l: &SyntaxRules) -> Result<Self::Output>;
    fn visit_set(&mut self, s: &Set) -> Result<Self::Output>;
    fn visit_require(&mut self, s: &Require) -> Result<Self::Output>;
    fn visit_let(&mut self, l: &Let) -> Result<Self::Output>;
    fn visit_vector(&mut self, v: &Vector) -> Result<Self::Output>;
}
