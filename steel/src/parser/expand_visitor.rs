use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::ConsumingVisitor;
// use crate::parser::span::Span;

use crate::rerrs::SteelErr;
use crate::rvals::Result;

use super::ast::Atom;

use std::collections::HashMap;

use crate::parser::expander::SteelMacro;

pub fn extract_macro_defs(
    exprs: Vec<ExprKind>,
    macro_map: &mut HashMap<String, SteelMacro>,
) -> Result<Vec<ExprKind>> {
    let mut non_macros = Vec::new();
    for expr in exprs {
        if let ExprKind::Macro(m) = expr {
            let generated_macro = SteelMacro::parse_from_ast_macro(m)?;
            let name = generated_macro.name().clone();
            macro_map.insert(name.to_string(), generated_macro);
        } else {
            non_macros.push(expr)
        }
    }
    Ok(non_macros)
}

pub fn expand(expr: ExprKind, map: &HashMap<String, SteelMacro>) -> Result<ExprKind> {
    Expander { map }.visit(expr)
}

pub struct Expander<'a> {
    map: &'a HashMap<String, SteelMacro>,
}

impl<'a> ConsumingVisitor for Expander<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, mut f: Box<super::ast::If>) -> Self::Output {
        f.test_expr = self.visit(f.test_expr)?;
        f.then_expr = self.visit(f.then_expr)?;
        f.else_expr = self.visit(f.else_expr)?;
        Ok(ExprKind::If(f))
    }

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        define.body = self.visit(define.body)?;
        Ok(ExprKind::Define(define))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        lambda_function.body = self.visit(lambda_function.body)?;
        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    fn visit_begin(&mut self, mut begin: super::ast::Begin) -> Self::Output {
        begin.exprs = begin
            .exprs
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        Ok(ExprKind::Begin(begin))
    }

    fn visit_return(&mut self, mut r: Box<super::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        Ok(ExprKind::Return(r))
    }

    fn visit_apply(&mut self, mut apply: Box<super::ast::Apply>) -> Self::Output {
        apply.func = self.visit(apply.func)?;
        apply.list = self.visit(apply.list)?;
        Ok(ExprKind::Apply(apply))
    }

    fn visit_panic(&mut self, mut p: Box<super::ast::Panic>) -> Self::Output {
        p.message = self.visit(p.message)?;
        Ok(ExprKind::Panic(p))
    }

    fn visit_transduce(&mut self, mut transduce: Box<super::ast::Transduce>) -> Self::Output {
        transduce.transducer = self.visit(transduce.transducer)?;
        transduce.func = self.visit(transduce.func)?;
        transduce.initial_value = self.visit(transduce.initial_value)?;
        transduce.iterable = self.visit(transduce.iterable)?;
        Ok(ExprKind::Transduce(transduce))
    }

    fn visit_read(&mut self, mut read: Box<super::ast::Read>) -> Self::Output {
        read.expr = self.visit(read.expr)?;
        Ok(ExprKind::Read(read))
    }

    fn visit_execute(&mut self, mut execute: Box<super::ast::Execute>) -> Self::Output {
        execute.transducer = self.visit(execute.transducer)?;
        execute.collection = self.visit(execute.collection)?;
        execute.output_type = execute.output_type.map(|x| self.visit(x)).transpose()?;
        Ok(ExprKind::Execute(execute))
    }

    fn visit_quote(&mut self, mut quote: Box<super::ast::Quote>) -> Self::Output {
        quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_struct(&mut self, s: Box<super::ast::Struct>) -> Self::Output {
        Ok(ExprKind::Struct(s))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(Generic => "unexpected macro definition"; m.location.span)
    }

    fn visit_eval(&mut self, mut e: Box<super::ast::Eval>) -> Self::Output {
        e.expr = self.visit(e.expr)?;
        Ok(ExprKind::Eval(e))
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        Ok(ExprKind::Atom(a))
    }

    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        // todo!()
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    span: sp,
                },
        })) = l.first()
        {
            if let Some(m) = self.map.get(s) {
                let expanded = m.expand(l.clone(), *sp)?;
                return self.visit(expanded);
            }
        }

        l.args = l
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;

        Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }
}

#[cfg(test)]
mod expansion_tests {
    use super::*;

    use crate::parser::expander::MacroCase;
    use crate::parser::expander::MacroPattern;

    use crate::parser::ast::{Begin, If, List};
    use crate::parser::tokens::TokenType;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.to_string(),
        ))))
    }

    fn ellipses() -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Ellipses)))
    }

    #[test]
    fn test_basic_expansion() {
        // (define-syntax when
        //     (syntax-rules ()
        //       [(when a b ...)
        //        (if a (begin b ...) void)]))
        let m = SteelMacro::new(
            "when".to_string(),
            Vec::new(),
            vec![MacroCase::new(
                vec![
                    MacroPattern::Syntax("when".to_string()),
                    MacroPattern::Single("a".to_string()),
                    MacroPattern::Many("b".to_string()),
                ],
                If::new(
                    atom_identifier("a"),
                    Begin::new(
                        vec![atom_identifier("b"), ellipses()],
                        SyntaxObject::default(TokenType::Begin),
                    )
                    .into(),
                    atom_identifier("void"),
                    SyntaxObject::default(TokenType::If),
                )
                .into(),
            )],
        );

        let mut map = HashMap::new();
        map.insert("when".to_string(), m);

        let input: ExprKind = List::new(vec![
            atom_identifier("when"),
            atom_identifier("blagh"),
            atom_identifier("do-thing"),
        ])
        .into();

        let expected: ExprKind = If::new(
            atom_identifier("blagh"),
            Begin::new(
                vec![atom_identifier("do-thing")],
                SyntaxObject::default(TokenType::Begin),
            )
            .into(),
            atom_identifier("void"),
            SyntaxObject::default(TokenType::If),
        )
        .into();

        let output = expand(input, &map).unwrap();

        assert_eq!(expected, output)
    }
}
