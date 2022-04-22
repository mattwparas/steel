use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::ConsumingVisitor;
use crate::rvals::Result;

use super::{ast::Atom, kernel::Kernel};

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
            let name = generated_macro.name();
            macro_map.insert(name.to_string(), generated_macro);
        } else {
            non_macros.push(expr)
        }
    }
    Ok(non_macros)
}

pub fn expand(expr: ExprKind, map: &HashMap<String, SteelMacro>) -> Result<ExprKind> {
    Expander {
        map,
        changed: false,
    }
    .visit(expr)
}

pub struct Expander<'a> {
    map: &'a HashMap<String, SteelMacro>,
    pub(crate) changed: bool,
}

impl<'a> Expander<'a> {
    pub fn new(map: &'a HashMap<String, SteelMacro>) -> Self {
        Self {
            map,
            changed: false,
        }
    }

    pub fn expand(&mut self, expr: ExprKind) -> Result<ExprKind> {
        self.visit(expr)
    }
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

    fn visit_quote(&mut self, mut quote: Box<super::ast::Quote>) -> Self::Output {
        // quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_struct(&mut self, s: Box<super::ast::Struct>) -> Self::Output {
        Ok(ExprKind::Struct(s))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(Generic => "unexpected macro definition"; m.location.span)
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
                    ..
                },
        })) = l.first()
        {
            if let Some(m) = self.map.get(s) {
                let expanded = m.expand(l.clone(), *sp)?;
                self.changed = true;
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

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, s: super::ast::Require) -> Self::Output {
        Ok(ExprKind::Require(s))
        // stop!(Generic => "unexpected require statement during expansion"; s.location.span)
    }

    fn visit_callcc(&mut self, mut cc: Box<super::ast::CallCC>) -> Self::Output {
        cc.expr = self.visit(cc.expr)?;
        Ok(ExprKind::CallCC(cc))
    }

    fn visit_let(&mut self, mut l: Box<super::ast::Let>) -> Self::Output {
        let mut visited_bindings = Vec::new();

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
    }
}

pub fn expand_kernel(expr: ExprKind, kernel: &mut Kernel) -> Result<ExprKind> {
    KernelExpander {
        map: kernel,
        changed: false,
    }
    .visit(expr)
}

pub struct KernelExpander<'a> {
    map: &'a mut Kernel,
    pub(crate) changed: bool,
}

impl<'a> KernelExpander<'a> {
    pub fn new(map: &'a mut Kernel) -> Self {
        Self {
            map,
            changed: false,
        }
    }

    pub fn expand(&mut self, expr: ExprKind) -> Result<ExprKind> {
        self.visit(expr)
    }
}

impl<'a> ConsumingVisitor for KernelExpander<'a> {
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

    fn visit_quote(&mut self, mut quote: Box<super::ast::Quote>) -> Self::Output {
        // quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_struct(&mut self, s: Box<super::ast::Struct>) -> Self::Output {
        Ok(ExprKind::Struct(s))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(Generic => "unexpected macro definition"; m.location.span)
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        Ok(ExprKind::Atom(a))
    }

    // TODO: This is not the best way of doing things, but for now we can accept it for the sake of progress
    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        // todo!()
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                },
        })) = l.first()
        {
            if self.map.contains_macro(s) {
                let expanded = self.map.expand(s, ExprKind::List(l.clone()))?;
                self.changed = true;
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

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, s: super::ast::Require) -> Self::Output {
        Ok(ExprKind::Require(s))
        // stop!(Generic => "unexpected require statement during expansion"; s.location.span)
    }

    fn visit_callcc(&mut self, mut cc: Box<super::ast::CallCC>) -> Self::Output {
        cc.expr = self.visit(cc.expr)?;
        Ok(ExprKind::CallCC(cc))
    }

    fn visit_let(&mut self, mut l: Box<super::ast::Let>) -> Self::Output {
        let mut visited_bindings = Vec::new();

        for (binding, expr) in l.bindings {
            visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
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
