use steel_parser::parser::SyntaxObject;

use crate::gc::Gc;
use crate::values::lists::List;

use crate::{parser::ast::ExprKind, rvals::Syntax};

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};

use super::visitors::VisitorMut;
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

    pub fn try_from_expr_kind_quoted(e: ExprKind) -> Result<SteelVal> {
        TryFromExprKindForSteelVal { inside_quote: true }.visit(e)
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
        // dbg!(self.inside_quote);

        if self.inside_quote {
            // self.visit(quote.expr)

            Ok(SteelVal::ListV(
                vec![SteelVal::SymbolV("quote".into()), self.visit(quote.expr)?].into(),
            ))
        } else {
            self.inside_quote = true;
            let res = self.visit(quote.expr);
            self.inside_quote = false;
            res
        }
    }

    fn visit_macro(&mut self, m: Box<super::ast::Macro>) -> Self::Output {
        // TODO

        Ok(SteelVal::ListV(
            vec![
                SteelVal::SymbolV("define-syntax".into()),
                self.visit(*m.name)?,
                self.visit_syntax_rules(m.syntax_rules)?,
            ]
            .into(),
        ))

        // stop!(Generic => "internal compiler error - could not translate macro to steel value")
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        SteelVal::try_from(a.syn)
    }

    fn visit_list(&mut self, l: super::ast::List) -> Self::Output {
        if !l.improper {
            let items: std::result::Result<List<_>, SteelErr> =
                l.args.into_iter().map(|x| self.visit(x)).collect();

            return Ok(items?.into());
        }

        debug_assert!(l.args.len() >= 2);

        if l.args.len() < 2 {
            stop!(Generic => "internal compiler error - unexpected malformed improper list");
        };

        let items: std::result::Result<Vec<_>, SteelErr> =
            l.args.into_iter().map(|x| self.visit(x)).collect();

        let pair = items?
            .into_iter()
            .rev()
            .reduce(|cdr, car| crate::values::lists::Pair::cons(car, cdr).into())
            .unwrap();

        Ok(pair)
    }

    fn visit_syntax_rules(&mut self, s: Box<super::ast::SyntaxRules>) -> Self::Output {
        Ok(SteelVal::ListV(
            vec![
                SteelVal::SymbolV("syntax-rules".into()),
                SteelVal::ListV(
                    s.syntax
                        .into_iter()
                        .map(|x| self.visit(x))
                        .collect::<Result<_>>()?,
                ),
                SteelVal::ListV(
                    s.patterns
                        .into_iter()
                        .map(|x| {
                            Ok(SteelVal::ListV(
                                vec![self.visit(x.pattern)?, self.visit(x.body)?].into(),
                            ))
                        })
                        .collect::<Result<_>>()?,
                ),
            ]
            .into(),
        ))
        // TODO
        // stop!(Generic => "internal compiler error - could not translate syntax-rules to steel value")
    }

    fn visit_set(&mut self, s: Box<super::ast::Set>) -> Self::Output {
        let expr = [
            SteelVal::try_from(s.location)?,
            self.visit(s.variable)?,
            self.visit(s.expr)?,
        ];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_require(&mut self, r: super::ast::Require) -> Self::Output {
        // Just convert it into a list

        // r.modules
        Ok(SteelVal::ListV(List::cons(
            SteelVal::SymbolV("require".into()),
            r.modules
                .into_iter()
                .map(|x| self.visit(x))
                .collect::<Result<_>>()?,
        )))

        // stop!(Generic => "internal compiler error - could not translate require to steel value")
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

// TODO: Have this take a reference, so we don't have to
// clone the whole thing
pub struct SyntaxObjectFromExprKind {
    _inside_quote: bool,
}

impl SyntaxObjectFromExprKind {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<SteelVal> {
        // let now = std::time::Instant::now();

        SyntaxObjectFromExprKind {
            _inside_quote: false,
        }
        .visit(e)

        // log::debug!(target: "pipeline_time", "ExprKind->SyntaxObject time: {:?}", now.elapsed());

        // res
    }
}

pub struct SyntaxObjectFromExprKindRef {
    _inside_quote: bool,
}

impl SyntaxObjectFromExprKindRef {
    pub fn try_from_expr_kind_ref(e: &ExprKind) -> Result<SteelVal> {
        SyntaxObjectFromExprKindRef {
            _inside_quote: false,
        }
        .visit(e)
    }
}

fn convert_location(s: &SyntaxObject) -> Result<SteelVal> {
    let span = s.span;

    let atom = SteelVal::try_from(s.clone())?;

    Ok(Syntax::proto(atom.clone(), atom, span).into())
}

impl VisitorMut for SyntaxObjectFromExprKindRef {
    type Output = Result<SteelVal>;

    fn visit_if(&mut self, f: &steel_parser::ast::If) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::If(Box::new(
            f.clone(),
        )))?;

        let span = f.location.span;

        let expr = [
            convert_location(&f.location)?,
            self.visit(&f.test_expr)?,
            self.visit(&f.then_expr)?,
            self.visit(&f.else_expr)?,
        ];
        // Ok(Syntax::new_with_source(SteelVal::ListV(expr.into_iter().collect()), span).into())

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_define(&mut self, define: &steel_parser::ast::Define) -> Self::Output {
        let raw: SteelVal = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(
            ExprKind::Define(Box::new(define.clone())),
        )?;

        let span = define.location.span;

        let expr = [
            convert_location(&define.location)?,
            self.visit(&define.name)?,
            self.visit(&define.body)?,
        ];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &steel_parser::ast::LambdaFunction,
    ) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::LambdaFunction(
            Box::new(lambda_function.clone()),
        ))?;

        let span = lambda_function.location.span;

        let args = lambda_function
            .args
            .iter()
            .map(|x| self.visit(&x))
            .collect::<Result<List<_>>>()?;

        let span_vec = args
            .iter()
            .map(|x| {
                if let SteelVal::SyntaxObject(s) = x {
                    s.syntax_loc()
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        let args_span = Span::coalesce_span(&span_vec);

        let expr = [
            convert_location(&lambda_function.location)?,
            Syntax::new(SteelVal::ListV(args), args_span).into(),
            self.visit(&lambda_function.body)?,
        ];

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_begin(&mut self, begin: &steel_parser::ast::Begin) -> Self::Output {
        let raw: SteelVal =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Begin(begin.clone()))?;

        let span = begin.location.span;
        let mut exprs = vec![convert_location(&begin.location)?];
        for expr in &begin.exprs {
            exprs.push(self.visit(&expr)?);
        }
        Ok(Syntax::proto(raw, SteelVal::ListV(exprs.into()), span).into())
    }

    fn visit_return(&mut self, r: &steel_parser::ast::Return) -> Self::Output {
        let span = r.location.span;
        let expr = [convert_location(&r.location)?, self.visit(&r.expr)?];
        Ok(Syntax::new_with_source(SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    // TODO: quotes are handled incorrectly here
    // Interior values should be handled on their own separately - they should evaluate
    // like this: '(a b c) => '(a b c)
    // '(a b 'c) => '(a b 'c) --- currently this ends up as '(a b c)
    fn visit_quote(&mut self, quote: &steel_parser::ast::Quote) -> Self::Output {
        let span = quote.location.span;

        // if self.inside_quote {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Quote(
            Box::new(quote.clone()),
        ))?;
        Ok(Syntax::proto(
            raw,
            SteelVal::ListV(
                vec![SteelVal::SymbolV("quote".into()), self.visit(&quote.expr)?].into(),
            ),
            span,
        )
        .into())
        // } else {
        // self.inside_quote = true;
        // let res = self.visit(quote.expr);
        // self.inside_quote = false;
        // res
        // }
    }

    fn visit_macro(&mut self, _m: &steel_parser::ast::Macro) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate macro to steel value")
    }

    fn visit_atom(&mut self, a: &steel_parser::ast::Atom) -> Self::Output {
        let span = a.syn.span;

        let atom = SteelVal::try_from(a.syn.clone())?;

        Ok(Syntax::proto(atom.clone(), atom, span).into())
    }

    fn visit_list(&mut self, l: &steel_parser::ast::List) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::List(l.clone()))?;

        let items: std::result::Result<List<_>, SteelErr> =
            l.args.iter().map(|x| self.visit(&x)).collect();

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
        // log::debug!(
        //     "Erasing the source information during kernel level expansion for: {:?}",
        //     raw
        // );
        Ok(Syntax::proto(raw, items.into(), span).into())
    }

    fn visit_syntax_rules(&mut self, _s: &steel_parser::ast::SyntaxRules) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate syntax-rules to steel value")
    }

    fn visit_set(&mut self, s: &steel_parser::ast::Set) -> Self::Output {
        let raw: SteelVal = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Set(
            Box::new(s.clone()),
        ))?;

        let span = s.location.span;
        let expr = [
            convert_location(&s.location)?,
            self.visit(&s.variable)?,
            self.visit(&s.expr)?,
        ];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_require(&mut self, _s: &steel_parser::ast::Require) -> Self::Output {
        stop!(Generic => "internal compiler error - could not translate require to steel value")
    }

    fn visit_let(&mut self, l: &steel_parser::ast::Let) -> Self::Output {
        let raw: SteelVal = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Let(
            Box::new(l.clone()),
        ))?;

        let span = l.location.span;

        let items = l
            .bindings
            .iter()
            .map(|(x, y)| {
                let items = vec![self.visit(x)?, self.visit(y)?];

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

                let items_span = Span::coalesce_span(&span_vec);

                Ok(SteelVal::SyntaxObject(Gc::new(Syntax::new(
                    SteelVal::ListV(items.into()),
                    items_span,
                ))))
            })
            .collect::<std::result::Result<List<SteelVal>, SteelErr>>()?;
        // .into(),

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

        let items_span = Span::coalesce_span(&span_vec);

        let expr = [
            convert_location(&l.location)?,
            // Bindings
            Syntax::new(items.into(), items_span).into(),
            self.visit(&l.body_expr)?,
        ];

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }
}

impl ConsumingVisitor for SyntaxObjectFromExprKind {
    type Output = Result<SteelVal>;

    fn visit_if(&mut self, f: Box<super::ast::If>) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::If(f.clone()))?;

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
        let raw: SteelVal = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(
            ExprKind::Define(define.clone()),
        )?;

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
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::LambdaFunction(
            lambda_function.clone(),
        ))?;

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
        let raw: SteelVal =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Begin(begin.clone()))?;

        let span = begin.location.span;
        let mut exprs = vec![SteelVal::try_from(begin.location)?];
        for expr in begin.exprs {
            exprs.push(self.visit(expr)?);
        }
        Ok(Syntax::proto(raw, SteelVal::ListV(exprs.into()), span).into())
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

        // if self.inside_quote {
        let raw =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Quote(quote.clone()))?;
        Ok(Syntax::proto(
            raw,
            SteelVal::ListV(
                vec![SteelVal::SymbolV("quote".into()), self.visit(quote.expr)?].into(),
            ),
            span,
        )
        .into())
        // } else {
        // self.inside_quote = true;
        // let res = self.visit(quote.expr);
        // self.inside_quote = false;
        // res
        // }
    }

    fn visit_macro(&mut self, _m: Box<super::ast::Macro>) -> Self::Output {
        // TODO
        stop!(Generic => "internal compiler error - could not translate macro to steel value")
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        let span = a.syn.span;

        let atom = SteelVal::try_from(a.syn)?;

        Ok(Syntax::proto(atom.clone(), atom, span).into())
    }

    fn visit_list(&mut self, l: super::ast::List) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::List(l.clone()))?;

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
        // log::debug!(
        //     "Erasing the source information during kernel level expansion for: {:?}",
        //     raw
        // );
        Ok(Syntax::proto(raw, items.into(), span).into())
    }

    fn visit_syntax_rules(&mut self, _s: Box<super::ast::SyntaxRules>) -> Self::Output {
        // Ok(SteelVal::ListV(
        //     vec![
        //         SteelVal::SymbolV("syntax-rules".into()),
        //         SteelVal::ListV(
        //             s.syntax
        //                 .into_iter()
        //                 .map(|x| self.visit(x))
        //                 .collect::<Result<_>>()?,
        //         ),
        //         SteelVal::ListV(
        //             s.patterns
        //                 .into_iter()
        //                 .map(|x| {
        //                     Ok(SteelVal::ListV(
        //                         vec![self.visit(x.pattern)?, self.visit(x.body)?].into(),
        //                     ))
        //                 })
        //                 .collect::<Result<_>>()?,
        //         ),
        //     ]
        //     .into(),
        // ))

        // TODO
        stop!(Generic => "internal compiler error - could not translate syntax-rules to steel value")
    }

    fn visit_set(&mut self, s: Box<super::ast::Set>) -> Self::Output {
        let raw: SteelVal =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Set(s.clone()))?;

        let span = s.location.span;
        let expr = [
            SteelVal::try_from(s.location)?,
            self.visit(s.variable)?,
            self.visit(s.expr)?,
        ];
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
