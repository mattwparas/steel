use smallvec::SmallVec;

use crate::parser::ast::ExprKind;
use crate::parser::span::Span;
use crate::parser::visitors::Visitor;

use super::ast::Atom;

pub fn get_span(expr: &ExprKind) -> Span {
    CoalescingSpanVisitor.visit(expr)
}

pub struct CoalescingSpanVisitor;

impl Visitor for CoalescingSpanVisitor {
    type Output = Span;

    fn visit_if(&self, f: &super::ast::If) -> Self::Output {
        let span1 = Span::merge(f.location.span, self.visit(&f.test_expr));
        let span2 = Span::merge(span1, self.visit(&f.then_expr));
        Span::merge(span2, self.visit(&f.else_expr))
    }

    fn visit_define(&self, define: &super::ast::Define) -> Self::Output {
        let span1 = Span::merge(define.location.span, self.visit(&define.name));
        Span::merge(span1, self.visit(&define.body))
    }

    fn visit_lambda_function(&self, lambda_function: &super::ast::LambdaFunction) -> Self::Output {
        Span::merge(
            lambda_function.location.span,
            self.visit(&lambda_function.body),
        )
    }

    fn visit_begin(&self, begin: &super::ast::Begin) -> Self::Output {
        let last_span = begin.exprs.last().map(|x| self.visit(x));

        if let Some(last_span) = last_span {
            Span::merge(begin.location.span, last_span)
        } else {
            begin.location.span
        }
    }

    fn visit_return(&self, r: &super::ast::Return) -> Self::Output {
        Span::merge(r.location.span, self.visit(&r.expr))
    }

    fn visit_quote(&self, quote: &super::ast::Quote) -> Self::Output {
        self.visit(&quote.expr)
    }

    fn visit_macro(&self, _m: &super::ast::Macro) -> Self::Output {
        panic!("Unexpected macro found in span visitor");
    }

    fn visit_atom(&self, a: &Atom) -> Self::Output {
        a.syn.span
    }

    fn visit_list(&self, l: &super::ast::List) -> Self::Output {
        let span_vec = l
            .args
            .iter()
            .map(|x| self.visit(x))
            .collect::<SmallVec<[_; 16]>>();

        if span_vec.is_empty() {
            l.location
        } else {
            Span::coalesce_span(&span_vec)
        }
    }

    fn visit_vector(&self, l: &super::ast::Vector) -> Self::Output {
        let span_vec = l
            .args
            .iter()
            .map(|x| self.visit(x))
            .collect::<SmallVec<[_; 16]>>();
        Span::coalesce_span(&span_vec)
    }

    fn visit_syntax_rules(&self, _l: &super::ast::SyntaxRules) -> Self::Output {
        panic!("Unexpected syntax rules found in span visitor");
    }

    fn visit_set(&self, s: &super::ast::Set) -> Self::Output {
        Span::merge(s.location.span, self.visit(&s.expr))
    }

    fn visit_require(&self, _s: &super::ast::Require) -> Self::Output {
        panic!("Unexpected require found in span visitor")
    }

    fn visit_let(&self, l: &super::ast::Let) -> Self::Output {
        Span::merge(l.location.span, self.visit(&l.body_expr))
    }
}
