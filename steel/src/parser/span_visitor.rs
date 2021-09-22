use crate::parser::ast::ExprKind;
use crate::parser::span::Span;
use crate::parser::visitors::Visitor;

use super::ast::Atom;

pub fn get_span(expr: &ExprKind) -> Span {
    CoalescingSpanVisitor {}.visit(expr)
}

struct CoalescingSpanVisitor {}

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

    // fn visit_apply(&self, apply: &super::ast::Apply) -> Self::Output {
    //     Span::merge(
    //         apply.location.span,
    //         Span::merge(self.visit(&apply.func), self.visit(&apply.list)),
    //     )
    // }

    // fn visit_panic(&self, p: &super::ast::Panic) -> Self::Output {
    //     Span::merge(p.location.span, self.visit(&p.message))
    // }

    // fn visit_transduce(&self, transduce: &super::ast::Transduce) -> Self::Output {
    //     Span::merge(transduce.location.span, self.visit(&transduce.iterable))
    // }

    fn visit_read(&self, read: &super::ast::Read) -> Self::Output {
        Span::merge(read.location.span, self.visit(&read.expr))
    }

    // fn visit_execute(&self, execute: &super::ast::Execute) -> Self::Output {
    //     let last = if let Some(output_type) = &execute.output_type {
    //         self.visit(output_type)
    //     } else {
    //         self.visit(&execute.collection)
    //     };

    //     Span::merge(execute.location.span, last)
    // }

    fn visit_quote(&self, quote: &super::ast::Quote) -> Self::Output {
        self.visit(&quote.expr)
    }

    fn visit_struct(&self, s: &super::ast::Struct) -> Self::Output {
        s.location.span
    }

    fn visit_macro(&self, _m: &super::ast::Macro) -> Self::Output {
        panic!("Unexpected macro found in span visitor");
    }

    fn visit_eval(&self, e: &super::ast::Eval) -> Self::Output {
        Span::merge(e.location.span, self.visit(&e.expr))
    }

    fn visit_atom(&self, a: &Atom) -> Self::Output {
        a.syn.span
    }

    fn visit_list(&self, l: &super::ast::List) -> Self::Output {
        let span_vec = l.args.iter().map(|x| self.visit(x)).collect::<Vec<_>>();
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

    fn visit_callcc(&self, cc: &super::ast::CallCC) -> Self::Output {
        Span::merge(cc.location.span, self.visit(&cc.expr))
    }
}
