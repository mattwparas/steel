use im_lists::list::List;

use crate::{parser::ast::ExprKind, rvals::Syntax};

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};

use super::{ast::Atom, span::Span, visitors::ConsumingVisitor};

use std::convert::TryFrom;

pub struct TryFromExprKindForSteelVal {
    inside_quote: bool,
}

impl TryFromExprKindForSteelVal {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<SteelVal> {
        TryFromExprKindForSteelVal {
            inside_quote: false,
        }
        .visit(e)
    }
}

impl ConsumingVisitor for TryFromExprKindForSteelVal {
    type Output = Result<SteelVal>;

    fn visit_if(&mut self, f: Box<super::ast::If>) -> Self::Output {
        let expr = [
            SteelVal::try_from(f.location)?,
            self.visit(f.test_expr)?,
            self.visit(f.then_expr)?,
            self.visit(f.else_expr)?,
        ];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_define(&mut self, define: Box<super::ast::Define>) -> Self::Output {
        let expr = [
            SteelVal::try_from(define.location)?,
            self.visit(define.name)?,
            self.visit(define.body)?,
        ];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        let args = lambda_function
            .args
            .into_iter()
            .map(|x| self.visit(x))
            .collect::<Result<List<_>>>()?;

        let expr = [
            SteelVal::try_from(lambda_function.location)?,
            SteelVal::ListV(args),
            self.visit(lambda_function.body)?,
        ];

        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_begin(&mut self, begin: super::ast::Begin) -> Self::Output {
        let mut exprs = vec![SteelVal::try_from(begin.location)?];
        for expr in begin.exprs {
            exprs.push(self.visit(expr)?);
        }
        Ok(SteelVal::ListV(exprs.into()))
    }

    fn visit_return(&mut self, r: Box<super::ast::Return>) -> Self::Output {
        let expr = [SteelVal::try_from(r.location)?, self.visit(r.expr)?];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    // TODO: quotes are handled incorrectly here
    // Interior values should be handled on their own separately - they should evaluate
    // like this: '(a b c) => '(a b c)
    // '(a b 'c) => '(a b 'c) --- currently this ends up as '(a b c)
    fn visit_quote(&mut self, quote: Box<super::ast::Quote>) -> Self::Output {
        if self.inside_quote {
            // self.visit(quote.expr)

            Ok(SteelVal::ListV(im_lists::list![
                SteelVal::SymbolV("quote".into()),
                self.visit(quote.expr)?
            ]))
        } else {
            self.inside_quote = true;
            let res = self.visit(quote.expr);
            self.inside_quote = false;
            res
        }
    }

    fn visit_macro(&mut self, _m: super::ast::Macro) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate macro to steel value")
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        SteelVal::try_from(a.syn)
    }

    fn visit_list(&mut self, l: super::ast::List) -> Self::Output {
        let items: std::result::Result<List<_>, SteelErr> =
            l.args.into_iter().map(|x| self.visit(x)).collect();

        Ok(items?.into())
    }

    fn visit_syntax_rules(&mut self, _l: super::ast::SyntaxRules) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate syntax-rules to steel value")
    }

    fn visit_set(&mut self, s: Box<super::ast::Set>) -> Self::Output {
        let expr = [SteelVal::try_from(s.location)?, self.visit(s.expr)?];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_require(&mut self, _s: super::ast::Require) -> Self::Output {
        stop!(Generic => "internal compiler error - could not translate require to steel value")
    }

    fn visit_let(&mut self, l: Box<super::ast::Let>) -> Self::Output {
        // todo!()

        let pairs = l
            .bindings
            .into_iter()
            .map(|x| {
                Ok(SteelVal::ListV(
                    vec![self.visit(x.0)?, self.visit(x.1)?].into(),
                ))
            })
            .collect::<Result<_>>()?;

        Ok(SteelVal::ListV(
            vec![
                SteelVal::SymbolV("%plain-let".into()),
                SteelVal::ListV(pairs),
                self.visit(l.body_expr)?,
            ]
            .into(),
        ))
    }
}

pub struct SyntaxObjectFromExprKind {
    inside_quote: bool,
}

impl SyntaxObjectFromExprKind {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<SteelVal> {
        SyntaxObjectFromExprKind {
            inside_quote: false,
        }
        .visit(e)
    }
}

impl ConsumingVisitor for SyntaxObjectFromExprKind {
    type Output = Result<SteelVal>;

    fn visit_if(&mut self, f: Box<super::ast::If>) -> Self::Output {
        let raw = SteelVal::try_from(ExprKind::If(f.clone()))?;

        let span = f.location.span;

        let expr = [
            SteelVal::try_from(f.location)?,
            self.visit(f.test_expr)?,
            self.visit(f.then_expr)?,
            self.visit(f.else_expr)?,
        ];
        // Ok(Syntax::new_with_source(SteelVal::ListV(expr.into_iter().collect()), span).into())

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_define(&mut self, define: Box<super::ast::Define>) -> Self::Output {
        let raw: SteelVal = ExprKind::Define(define.clone()).try_into()?;

        let span = define.location.span;

        let expr = [
            SteelVal::try_from(define.location)?,
            self.visit(define.name)?,
            self.visit(define.body)?,
        ];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        let raw = SteelVal::try_from(ExprKind::LambdaFunction(lambda_function.clone()))?;

        let span = lambda_function.location.span;

        let args = lambda_function
            .args
            .into_iter()
            .map(|x| self.visit(x))
            .collect::<Result<List<_>>>()?;

        let expr = [
            SteelVal::try_from(lambda_function.location)?,
            SteelVal::ListV(args),
            self.visit(lambda_function.body)?,
        ];

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_begin(&mut self, begin: super::ast::Begin) -> Self::Output {
        let raw: SteelVal = ExprKind::Begin(b.clone()).into()?;

        let span = begin.location.span;
        let mut exprs = vec![SteelVal::try_from(begin.location)?];
        for expr in begin.exprs {
            exprs.push(self.visit(expr)?);
        }
        Ok(Syntax::raw(raw, SteelVal::ListV(exprs.into()), span).into())
    }

    fn visit_return(&mut self, r: Box<super::ast::Return>) -> Self::Output {
        let span = r.location.span;
        let expr = [SteelVal::try_from(r.location)?, self.visit(r.expr)?];
        Ok(Syntax::new_with_source(SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    // TODO: quotes are handled incorrectly here
    // Interior values should be handled on their own separately - they should evaluate
    // like this: '(a b c) => '(a b c)
    // '(a b 'c) => '(a b 'c) --- currently this ends up as '(a b c)
    fn visit_quote(&mut self, quote: Box<super::ast::Quote>) -> Self::Output {
        let span = quote.location.span;

        if self.inside_quote {
            let raw = SteelVal::try_from(ExprKind::Quote(quote.clone()))?;
            Ok(Syntax::proto(
                raw,
                SteelVal::ListV(im_lists::list![
                    SteelVal::SymbolV("quote".into()),
                    self.visit(quote.expr)?
                ]),
                span,
            )
            .into())
        } else {
            self.inside_quote = true;
            let res = self.visit(quote.expr);
            self.inside_quote = false;
            res
        }
    }

    fn visit_macro(&mut self, _m: super::ast::Macro) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate macro to steel value")
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        let span = a.syn.span;
        Ok(Syntax::new_with_source(SteelVal::try_from(a.syn)?, span).into())
    }

    fn visit_list(&mut self, l: super::ast::List) -> Self::Output {
        let raw = SteelVal::try_from(ExprKind::List(l.clone()))?;

        let items: std::result::Result<List<_>, SteelErr> =
            l.args.into_iter().map(|x| self.visit(x)).collect();

        let items = items?;

        let span_vec = items
            .iter()
            .map(|x| {
                if let SteelVal::SyntaxObject(s) = x {
                    s.syntax_loc()
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        let span = Span::coalesce_span(&span_vec);

        // TODO: we're currently erasing the source here... This isn't what we want to do but we don't have
        // a great model to access the source otherwise
        log::warn!("Erasing the source information during kernel level expansion");
        Ok(Syntax::proto(raw, items.into(), span).into())
    }

    fn visit_syntax_rules(&mut self, _l: super::ast::SyntaxRules) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate syntax-rules to steel value")
    }

    fn visit_set(&mut self, s: Box<super::ast::Set>) -> Self::Output {
        let raw: SteelVal = ExprKind::Set(s.clone()).try_into()?;

        let span = s.location.span;
        let expr = [SteelVal::try_from(s.location)?, self.visit(s.expr)?];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_require(&mut self, _s: super::ast::Require) -> Self::Output {
        stop!(Generic => "internal compiler error - could not translate require to steel value")
    }

    fn visit_let(&mut self, _l: Box<super::ast::Let>) -> Self::Output {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{ast::Quote, parser::SyntaxObject, tokens::TokenType};

    use super::*;

    #[test]
    fn nested_quotes_handled_correctly() {
        let expr = ExprKind::Quote(Box::new(Quote::new(
            ExprKind::List(crate::parser::ast::List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Define))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                    "applesauce".into(),
                )))),
                ExprKind::Quote(Box::new(Quote::new(
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "bananas".into(),
                    )))),
                    SyntaxObject::default(TokenType::Quote),
                ))),
            ])),
            SyntaxObject::default(TokenType::Quote),
        )));

        let result = TryFromExprKindForSteelVal::try_from_expr_kind(expr);

        println!("{result:?}");
    }
}
