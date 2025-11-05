use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use steel_parser::parser::SyntaxObject;

use crate::gc::Gc;
use crate::values::lists::List;

use crate::{parser::ast::ExprKind, rvals::Syntax};

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelByteVector, SteelVal};

use super::visitors::VisitorMut;
use super::{ast::Atom, span::Span, visitors::ConsumingVisitor};

use core::convert::TryFrom;

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

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        // HACK: For some reason, these tokens are incorrect coming in to here
        define.location.ty = steel_parser::tokens::TokenType::Define;

        let expr = [
            // TODO: This needs to get converted into a syntax object,
            // not a symbol?
            SteelVal::try_from(define.location)?,
            self.visit(define.name)?,
            self.visit(define.body)?,
        ];
        Ok(SteelVal::ListV(expr.into_iter().collect()))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        // HACK: For some reason, these tokens are incorrect coming in to here
        lambda_function.location.ty = steel_parser::tokens::TokenType::Lambda;

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

    fn visit_begin(&mut self, mut begin: Box<super::ast::Begin>) -> Self::Output {
        // HACK: For some reason, these tokens are incorrect coming in to here
        begin.location.ty = steel_parser::tokens::TokenType::Begin;

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
            let items: core::result::Result<List<_>, SteelErr> =
                l.args.into_iter().map(|x| self.visit(x)).collect();

            return Ok(items?.into());
        }

        debug_assert!(l.args.len() >= 2);

        if l.args.len() < 2 {
            stop!(Generic => "internal compiler error - unexpected malformed improper list");
        };

        let items: core::result::Result<Vec<_>, SteelErr> =
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

    fn visit_require(&mut self, r: Box<super::ast::Require>) -> Self::Output {
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

    fn visit_vector(&mut self, v: super::ast::Vector) -> Self::Output {
        if v.bytes {
            let bytes: Vec<_> = v
                .args
                .into_iter()
                .flat_map(|exp| {
                    let byte = match exp {
                        ExprKind::Atom(atom) => atom.byte(),
                        _ => None,
                    };

                    debug_assert!(byte.is_some());

                    byte
                })
                .collect();

            Ok(SteelVal::ByteVector(SteelByteVector::new(bytes)))
        } else {
            let args: crate::collections::Vector<_> = v
                .args
                .into_iter()
                .map(|exp| self.visit(exp))
                .collect::<Result<_>>()?;

            Ok(SteelVal::VectorV(Gc::new(args).into()))
        }
    }
}

// TODO: Have this take a reference, so we don't have to
// clone the whole thing
pub struct SyntaxObjectFromExprKind {
    _inside_quote: bool,
}

impl SyntaxObjectFromExprKind {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<SteelVal> {
        // let now = crate::time::Instant::now();

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
            .map(|x| self.visit(x))
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
        let raw: SteelVal = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(
            ExprKind::Begin(Box::new(begin.clone())),
        )?;

        let span = begin.location.span;
        let mut exprs = vec![convert_location(&begin.location)?];
        for expr in &begin.exprs {
            exprs.push(self.visit(expr)?);
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

        // TODO: Don't need to copy the whole syntax object in
        let atom = SteelVal::try_from(a.syn.ty.clone()).map_err(|e| e.with_span(span))?;

        Ok(Syntax::proto(atom.clone(), atom, span).into())
    }

    fn visit_list(&mut self, l: &steel_parser::ast::List) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::List(l.clone()))?;

        let items: core::result::Result<List<_>, SteelErr> =
            l.args.iter().map(|x| self.visit(x)).collect();

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
            .collect::<core::result::Result<List<SteelVal>, SteelErr>>()?;
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

    fn visit_vector(&mut self, v: &steel_parser::ast::Vector) -> Self::Output {
        let raw =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Vector(v.clone()))?;

        if v.bytes {
            let span = v.span;
            let vector =
                TryFromExprKindForSteelVal::try_from_expr_kind(ExprKind::Vector(v.clone()))?;

            return Ok(Syntax::proto(raw, vector, span).into());
        }

        let items: Result<crate::collections::Vector<_>> =
            v.args.iter().map(|x| self.visit(x)).collect();

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

        Ok(Syntax::proto(raw, items.into(), span).into())
    }
}

impl ConsumingVisitor for SyntaxObjectFromExprKind {
    type Output = Result<SteelVal>;

    fn visit_if(&mut self, f: Box<super::ast::If>) -> Self::Output {
        let raw = TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::If(f.clone()))?;

        let span = f.location.span;
        let if_ident = SteelVal::try_from(f.location)?;

        let expr = [
            Syntax::proto(if_ident.clone(), if_ident, span).into(),
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

        let define_ident = SteelVal::try_from(define.location)?;

        let expr = [
            // Make this a proto
            Syntax::proto(define_ident.clone(), define_ident, span).into(),
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

        let lambda_ident = SteelVal::try_from(lambda_function.location)?;

        let args = SteelVal::ListV(args);

        let expr = [
            Syntax::proto(lambda_ident.clone(), lambda_ident, span).into(),
            Syntax::proto(args.clone(), args, span).into(),
            self.visit(lambda_function.body)?,
        ];

        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_begin(&mut self, begin: Box<super::ast::Begin>) -> Self::Output {
        let raw: SteelVal =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Begin(begin.clone()))?;

        let span = begin.location.span;
        let begin_ident = SteelVal::try_from(begin.location)?;

        let mut exprs = vec![Syntax::proto(begin_ident.clone(), begin_ident, span).into()];
        for expr in begin.exprs {
            exprs.push(self.visit(expr)?);
        }
        Ok(Syntax::proto(raw, SteelVal::ListV(exprs.into()), span).into())
    }

    fn visit_return(&mut self, r: Box<super::ast::Return>) -> Self::Output {
        let span = r.location.span;
        let return_ident = SteelVal::try_from(r.location)?;
        let expr = [
            Syntax::proto(return_ident.clone(), return_ident, span).into(),
            self.visit(r.expr)?,
        ];
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

        let items: core::result::Result<List<_>, SteelErr> =
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
        let set_ident = SteelVal::try_from(s.location)?;
        let expr = [
            Syntax::proto(set_ident.clone(), set_ident, span).into(),
            self.visit(s.variable)?,
            self.visit(s.expr)?,
        ];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_require(&mut self, _s: Box<super::ast::Require>) -> Self::Output {
        stop!(Generic => "internal compiler error - could not translate require to steel value")
    }

    fn visit_let(&mut self, l: Box<super::ast::Let>) -> Self::Output {
        let raw: SteelVal =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Let(l.clone()))?;

        let span = l.location.span;
        let let_ident = SteelVal::try_from(l.location)?;

        let bindings = SteelVal::ListV(
            l.bindings
                .into_iter()
                .map(|(binding, expr)| {
                    let binding = self.visit(binding)?;
                    let expr = self.visit(expr)?;

                    Ok(SteelVal::ListV([binding, expr].into_iter().collect()))
                })
                .collect::<Result<List<_>>>()?,
        );

        let expr = [
            Syntax::proto(let_ident.clone(), let_ident, span).into(),
            Syntax::proto(bindings.clone(), bindings, span).into(),
            self.visit(l.body_expr)?,
        ];
        Ok(Syntax::proto(raw, SteelVal::ListV(expr.into_iter().collect()), span).into())
    }

    fn visit_vector(&mut self, v: steel_parser::ast::Vector) -> Self::Output {
        let raw =
            TryFromExprKindForSteelVal::try_from_expr_kind_quoted(ExprKind::Vector(v.clone()))?;

        if v.bytes {
            let span = v.span;
            let vector = TryFromExprKindForSteelVal::try_from_expr_kind(ExprKind::Vector(v))?;

            return Ok(Syntax::proto(raw, vector, span).into());
        }

        let items: Result<crate::collections::Vector<_>> =
            v.args.into_iter().map(|x| self.visit(x)).collect();

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

        Ok(Syntax::proto(raw, items.into(), span).into())
    }
}

#[cfg(all(test, feature = "std"))]
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
