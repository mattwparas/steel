use crate::compiler::passes::VisitorMutControlFlow;
use crate::compiler::program::SYNTAX_SPAN;
use crate::parser::span::Span;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::ConsumingVisitor;
use crate::{compiler::program::DEFINE, parser::parser::SyntaxObject};
use crate::{
    compiler::program::{DATUM_SYNTAX, SYNTAX_CONST_IF},
    parser::ast::ExprKind,
};

use crate::rvals::Result;

use super::expander::BindingKind;
use super::{ast::Atom, interner::InternedString};

use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;

// const DATUM_TO_SYNTAX: &str = "datum->syntax";
// const SYNTAX_CONST_IF: &str = "syntax-const-if";
// TODO: Add level for pure macros to run at compile time... More or less const functions, that still
// have access to the span?
// const CURRENT_FILE: &str = "const-current-file!";

pub fn replace_identifiers(
    expr: ExprKind,
    bindings: &mut HashMap<InternedString, ExprKind>,
    binding_kind: &mut HashMap<InternedString, BindingKind>,
    fallback_bindings: &mut HashMap<InternedString, ExprKind>,
    span: Span,
) -> Result<ExprKind> {
    let rewrite_spans = RewriteSpan::new(span).visit(expr)?;

    ReplaceExpressions::new(bindings, binding_kind, fallback_bindings).visit(rewrite_spans)
}

// struct ConstExprKindTransformers {
//     functions: HashMap<&'static str, fn(&ReplaceExpressions<'_>, ExprKind) -> Result<ExprKind>>,
// }

pub struct ReplaceExpressions<'a> {
    bindings: &'a mut HashMap<InternedString, ExprKind>,
    fallback_bindings: &'a mut HashMap<InternedString, ExprKind>,
    binding_kind: &'a mut HashMap<InternedString, BindingKind>,
}

fn check_ellipses(expr: &ExprKind) -> bool {
    matches!(
        expr,
        ExprKind::Atom(Atom {
            syn: SyntaxObject {
                ty: TokenType::Ellipses,
                ..
            },
        })
    )
}

struct EllipsesExpanderVisitor<'a> {
    bindings: &'a mut HashMap<InternedString, ExprKind>,
    binding_kind: &'a mut HashMap<InternedString, BindingKind>,
    found_length: Option<usize>,
    collected: HashSet<InternedString>,
    error: Option<String>,
}

impl<'a> EllipsesExpanderVisitor<'a> {
    fn find_expansion_width_and_collect_ellipses_expanders(
        bindings: &'a mut HashMap<InternedString, ExprKind>,
        binding_kind: &'a mut HashMap<InternedString, BindingKind>,
        expr: &ExprKind,
    ) -> Self {
        let mut visitor = Self {
            bindings,
            binding_kind,
            found_length: None,
            collected: HashSet::new(),
            error: None,
        };

        VisitorMutControlFlow::visit(&mut visitor, expr);

        visitor
    }
}

impl<'a> VisitorMutControlFlow for EllipsesExpanderVisitor<'a> {
    fn visit_atom(&mut self, a: &Atom) -> ControlFlow<()> {
        let expansion = a.ident().and_then(|x| self.bindings.get(x));

        if let Some(expansion) = expansion {
            if let ExprKind::List(found_list) = expansion {
                if let Some(BindingKind::Many) = a.ident().and_then(|x| self.binding_kind.get(x)) {
                    if let Some(previously_seen_length) = self.found_length {
                        // Check that the length is the same
                        if previously_seen_length != found_list.len() {
                            self.error =
                                Some(format!("Mismatched lengths found in ellipses expansion"));
                            return ControlFlow::Break(());
                        }

                        // Found this one, use it
                        self.collected.insert(*(a.ident().unwrap()));
                    } else {
                        self.found_length = Some(found_list.len());
                        self.collected.insert(*(a.ident().unwrap()));
                    }
                }
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'a> ReplaceExpressions<'a> {
    pub fn new(
        bindings: &'a mut HashMap<InternedString, ExprKind>,
        binding_kind: &'a mut HashMap<InternedString, BindingKind>,
        fallback_bindings: &'a mut HashMap<InternedString, ExprKind>,
    ) -> Self {
        ReplaceExpressions {
            bindings,
            binding_kind,
            fallback_bindings,
        }
    }

    fn expand_atom(&self, expr: Atom) -> ExprKind {
        // Overwrite the span on any atoms
        // expr.syn.set_span(self.span);

        if let TokenType::Identifier(s) = &expr.syn.ty {
            if let Some(body) = self.bindings.get(s) {
                return body.clone();
            }
        }

        ExprKind::Atom(expr)
    }

    fn expand_ellipses(&mut self, vec_exprs: Vec<ExprKind>) -> Result<Vec<ExprKind>> {
        if let Some(ellipses_pos) = vec_exprs.iter().position(check_ellipses) {
            let variable_to_lookup = vec_exprs.get(ellipses_pos - 1).ok_or_else(
                throw!(BadSyntax => "macro expansion failed, could not find variable when expanding ellipses")
            )?;

            match variable_to_lookup {
                ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(var),
                            ..
                        },
                }) => {
                    let rest = self.bindings.get(var).ok_or_else(throw!(BadSyntax => format!("macro expansion failed at finding the variable when expanding ellipses: {var}")))?;

                    let list_of_exprs = if let ExprKind::List(list_of_exprs) = rest {
                        list_of_exprs
                    } else {
                        let res = self.fallback_bindings.get(var).ok_or_else(throw!(BadSyntax => format!("macro expansion failed at finding the variable when expanding ellipses: {var}")))?.list_or_else(
                        throw!(BadSyntax => "macro expansion failed, expected list of expressions, found: {}, within {}", rest, super::ast::List::new(vec_exprs.clone()))
                    )?;

                        res
                    };

                    let mut first_chunk = vec_exprs[0..ellipses_pos - 1].to_vec();
                    first_chunk.extend_from_slice(list_of_exprs);
                    first_chunk.extend_from_slice(&vec_exprs[(ellipses_pos + 1)..]);
                    Ok(first_chunk)
                }

                ExprKind::List(_) => {
                    let visitor = EllipsesExpanderVisitor::find_expansion_width_and_collect_ellipses_expanders(self.bindings, self.binding_kind, variable_to_lookup);

                    if let Some(error) = visitor.error {
                        stop!(BadSyntax => error);
                    }

                    let width = visitor.found_length.ok_or_else(throw!(BadSyntax => "No pattern variables before ellipses in template: at {} in {}", variable_to_lookup, ExprKind::List(super::ast::List::new(vec_exprs.clone()))))?;

                    let mut original_bindings: HashMap<_, _> = visitor
                        .collected
                        .iter()
                        .flat_map(|x| self.bindings.get(x).map(|value| (*x, value.clone())))
                        .collect();

                    std::mem::swap(self.fallback_bindings, &mut original_bindings);

                    let mut expanded_expressions = Vec::with_capacity(width);

                    for i in 0..width {
                        let template = variable_to_lookup.clone();

                        for (key, value) in self.fallback_bindings.iter() {
                            if let ExprKind::List(expansion) = value {
                                let new_binding = expansion
                                    .get(i)
                                    .ok_or_else(throw!(BadSyntax => "Unreachable"))?;

                                self.bindings.insert(*key, new_binding.clone());
                            } else {
                                stop!(BadSyntax => "Unexpected value found in ellipses expansion")
                            }
                        }

                        expanded_expressions.push(self.visit(template)?);
                    }

                    std::mem::swap(self.fallback_bindings, &mut original_bindings);

                    // Move the original bindings back in
                    for (key, value) in original_bindings {
                        self.bindings.insert(key, value);
                    }

                    let mut first_chunk = vec_exprs[0..ellipses_pos - 1].to_vec();
                    first_chunk.extend_from_slice(&expanded_expressions);
                    first_chunk.extend_from_slice(&vec_exprs[(ellipses_pos + 1)..]);

                    Ok(first_chunk)
                }

                _ => {
                    stop!(BadSyntax => "macro expansion failed at lookup!: {}", variable_to_lookup)
                }
            }
        } else {
            Ok(vec_exprs)
        }
    }

    fn vec_expr_syntax_const_if(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        match vec_exprs.get(0) {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *SYNTAX_CONST_IF => {
                if vec_exprs.len() != 4 {
                    stop!(BadSyntax => "syntax-const-if expects a const test condition, a then and an else case");
                }

                let test_expr = vec_exprs.get(1).unwrap();
                let then_expr = vec_exprs.get(2).unwrap();
                let else_expr = vec_exprs.get(3).unwrap();

                if let ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty, .. },
                }) = test_expr
                {
                    // TODO -> what happens if reserved tokens are in here
                    match ty {
                        TokenType::BooleanLiteral(_)
                        | TokenType::IntegerLiteral(_)
                        | TokenType::CharacterLiteral(_)
                        | TokenType::NumberLiteral(_)
                        | TokenType::StringLiteral(_) => return Ok(Some(then_expr.clone())),
                        TokenType::Identifier(s) => {
                            if let Some(ExprKind::Atom(Atom {
                                syn: SyntaxObject { ty, .. },
                            })) = self.bindings.get(s)
                            {
                                log::debug!("Syntax const if resolved to: {:?}", ty);

                                if matches!(
                                    ty,
                                    TokenType::BooleanLiteral(_)
                                        | TokenType::IntegerLiteral(_)
                                        | TokenType::CharacterLiteral(_)
                                        | TokenType::NumberLiteral(_)
                                        | TokenType::StringLiteral(_)
                                ) {
                                    return Ok(Some(then_expr.clone()));
                                }
                            }
                        }
                        _ => {}
                    }
                }

                Ok(Some(else_expr.clone()))
            }
            _ => Ok(None),
        }
    }

    fn vec_expr_datum_to_syntax(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        match vec_exprs.get(0) {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *DATUM_SYNTAX => {
                let mut buffer = String::new();
                if let Some((_, rest)) = vec_exprs.split_first() {
                    for syntax in rest {
                        let transformer = syntax.atom_identifier_or_else(
                            throw!(BadSyntax => "datum->syntax requires an identifier"),
                        )?;

                        let resolved = transformer.resolve();

                        // TODO this is no longer correct
                        // Should actually just visit the variable in the define name part
                        // TODO
                        if resolved.starts_with("##") {
                            if let Some(body) = self.bindings.get(transformer) {
                                buffer.push_str(body.to_string().as_str());
                            } else {
                                let (_, cdr) = resolved.split_at(2);
                                buffer.push_str(cdr);
                            }
                        } else {
                            // Try to get the prepended variable
                            if let Some(body) =
                                self.bindings.get(&("##".to_string() + resolved).into())
                            {
                                // println!("Found datum: {}", transformer);
                                buffer.push_str(body.to_string().as_str());
                            } else {
                                // println!("Unable to find datum: {}", transformer);
                                buffer.push_str(resolved);
                            }
                        }
                    }

                    Ok(Some(ExprKind::Atom(Atom::new(SyntaxObject::default(
                        TokenType::Identifier(buffer.into()),
                    )))))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    // TODO: @Matt - Come back here
    fn vec_syntax_span_object(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        if vec_exprs.len() != 2 {
            return Ok(None);
            // stop!(ArityMismatch => "#%syntax-span requires 2 arguments, found: {}", vec_exprs.len());
        }

        match vec_exprs.get(0) {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *SYNTAX_SPAN => {
                let expr_to_extract_span = vec_exprs.get(1).unwrap();

                // dbg!(&expr_to_extract_span);

                let span = crate::parser::span_visitor::get_span(expr_to_extract_span);

                // dbg!(&span);

                let start = ExprKind::integer_literal(span.start as isize, span);
                let end = ExprKind::integer_literal(span.end as isize, span);

                let source_id = if let Some(source) = span.source_id() {
                    ExprKind::integer_literal(source.0 as isize, span)
                } else {
                    ExprKind::bool_lit(false)
                };

                Ok(Some(ExprKind::Quote(Box::new(super::ast::Quote::new(
                    ExprKind::List(super::ast::List::new(vec![start, end, source_id])),
                    SyntaxObject::default(TokenType::Quote),
                )))))

                // Ok(Some(ExprKind::List(List::new(vec![

                // ]))))
            }
            _ => Ok(None),
        }
    }
}

fn reserved_token_type_to_ident(token: &mut TokenType<InternedString>) {
    if *token == TokenType::Define {
        *token = TokenType::Identifier(*DEFINE);
    }
}

// TODO replace spans on all of the nodes and atoms
impl<'a> ConsumingVisitor for ReplaceExpressions<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, mut f: Box<super::ast::If>) -> Self::Output {
        f.test_expr = self.visit(f.test_expr)?;
        f.then_expr = self.visit(f.then_expr)?;
        f.else_expr = self.visit(f.else_expr)?;
        Ok(ExprKind::If(f))
    }

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        if let ExprKind::List(l) = &define.name {
            if let Some(expanded) = self.vec_expr_datum_to_syntax(&l.args)? {
                define.name = expanded
            }
        }
        define.name = self.visit(define.name)?;
        define.body = self.visit(define.body)?;
        Ok(ExprKind::Define(define))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        lambda_function.args = self.expand_ellipses(lambda_function.args)?;
        lambda_function.args = lambda_function
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        lambda_function.body = self.visit(lambda_function.body)?;

        // TODO: @Matt - 2/28/12 -> clean up this
        // This mangles the values
        lambda_function.args.iter_mut().for_each(|x| {
            if let ExprKind::Atom(Atom {
                syn: SyntaxObject { ty: t, .. },
            }) = x
            {
                log::debug!("Checking if expression needs to be rewritten: {:?}", t);
                reserved_token_type_to_ident(t);
            }

            if let ExprKind::Define(d) = x {
                log::debug!("Found a define to be rewritten: {:?}", d);
            }
        });

        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    fn visit_begin(&mut self, mut begin: super::ast::Begin) -> Self::Output {
        begin.exprs = self.expand_ellipses(begin.exprs)?;
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
        quote.expr = self.visit(quote.expr)?;
        Ok(ExprKind::Quote(quote))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, a: Atom) -> Self::Output {
        Ok(self.expand_atom(a))
    }

    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        if let Some(expanded) = self.vec_expr_datum_to_syntax(&l.args)? {
            return Ok(expanded);
        }

        if let Some(expanded) = self.vec_expr_syntax_const_if(&l.args)? {
            return self.visit(expanded);
        }

        l.args = self.expand_ellipses(l.args)?;
        l.args = l
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;

        if let Some(expanded) = self.vec_syntax_span_object(&l.args)? {
            return Ok(expanded);
        }

        Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: super::ast::SyntaxRules) -> Self::Output {
        dbg!(l.to_string());

        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, mut s: super::ast::Require) -> Self::Output {
        s.modules = s
            .modules
            .into_iter()
            .map(|x| self.visit(x))
            .collect::<Result<_>>()?;

        Ok(ExprKind::Require(s))

        // stop!(Generic => "unexpected require statement in replace idents"; s.location.span)
    }

    fn visit_let(&mut self, mut l: Box<super::ast::Let>) -> Self::Output {
        let mut visited_bindings = Vec::new();

        let (bindings, exprs): (Vec<_>, Vec<_>) = l.bindings.iter().cloned().unzip();

        let bindings = self.expand_ellipses(bindings)?;

        for (binding, expr) in bindings.into_iter().zip(exprs) {
            visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        }

        l.bindings = visited_bindings;
        l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
    }
}

pub struct RewriteSpan {
    span: Span,
}

impl RewriteSpan {
    fn new(span: Span) -> Self {
        Self { span }
    }
}

// TODO replace spans on all of the nodes and atoms
impl ConsumingVisitor for RewriteSpan {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, mut f: Box<super::ast::If>) -> Self::Output {
        f.test_expr = self.visit(f.test_expr)?;
        f.then_expr = self.visit(f.then_expr)?;
        f.else_expr = self.visit(f.else_expr)?;
        f.location.set_span(self.span);
        Ok(ExprKind::If(f))
    }

    fn visit_define(&mut self, mut define: Box<super::ast::Define>) -> Self::Output {
        define.name = self.visit(define.name)?;
        define.body = self.visit(define.body)?;
        define.location.set_span(self.span);
        Ok(ExprKind::Define(define))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        lambda_function.args = lambda_function
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        lambda_function.body = self.visit(lambda_function.body)?;
        lambda_function.location.set_span(self.span);
        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    fn visit_begin(&mut self, mut begin: super::ast::Begin) -> Self::Output {
        begin.exprs = begin
            .exprs
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        begin.location.set_span(self.span);
        Ok(ExprKind::Begin(begin))
    }

    fn visit_return(&mut self, mut r: Box<super::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        r.location.set_span(self.span);
        Ok(ExprKind::Return(r))
    }

    fn visit_quote(&mut self, mut quote: Box<super::ast::Quote>) -> Self::Output {
        quote.expr = self.visit(quote.expr)?;
        quote.location.set_span(self.span);
        Ok(ExprKind::Quote(quote))
    }

    fn visit_macro(&mut self, m: super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition: {}", m); m.location.span)
    }

    fn visit_atom(&mut self, mut a: Atom) -> Self::Output {
        // Overwrite the span on any atoms
        a.syn.set_span(self.span);

        Ok(ExprKind::Atom(a))
    }

    fn visit_list(&mut self, mut l: super::ast::List) -> Self::Output {
        // if let Some(first) = l.first_ident() {
        //     if *first == *QUASISYNTAX || *first == *SYNTAX_QUOTE {
        //         return Ok(ExprKind::List(l));
        //     }
        // }

        l.args = l
            .args
            .into_iter()
            .map(|e| self.visit(e))
            .collect::<Result<Vec<_>>>()?;
        Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: super::ast::SyntaxRules) -> Self::Output {
        dbg!(l.to_string());

        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, mut s: Box<super::ast::Set>) -> Self::Output {
        s.variable = self.visit(s.variable)?;
        s.expr = self.visit(s.expr)?;
        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, mut s: super::ast::Require) -> Self::Output {
        s.modules = s
            .modules
            .into_iter()
            .map(|x| self.visit(x))
            .collect::<Result<_>>()?;

        Ok(ExprKind::Require(s))

        // stop!(Generic => "unexpected require statement in replace idents"; s.location.span)
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
mod replace_expressions_tests {
    use crate::parser::ast::{LambdaFunction, List};

    use super::*;

    macro_rules! map {
        ($ ( $key:expr => $value:expr ), *,) => {{
            let mut hm: HashMap<InternedString, ExprKind> = HashMap::new();
            $ (hm.insert($key.into(), $value); ) *
            hm
        }};
    }

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn ellipses() -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Ellipses)))
    }

    // TODO -> move this to ExprKind
    // fn atom_int(n: isize) -> ExprKind {
    //     ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
    //         n,
    //     ))))
    // }

    // TODO replace this test with something that doesn't use transduce
    // #[test]
    // fn test_expand_atom() {
    //     let bindings = map! {
    //         "apples" => atom_identifier("x"),
    //         "bananas" => atom_identifier("y"),
    //         "number" => atom_int(1),
    //     };

    //     let expr = ExprKind::If(Box::new(If::new(
    //         atom_identifier("test-condition"),
    //         ExprKind::Transduce(Box::new(Transduce::new(
    //             atom_identifier("apples"),
    //             atom_identifier("bananas"),
    //             atom_identifier("number"),
    //             atom_identifier("z"),
    //             SyntaxObject::default(TokenType::Transduce),
    //         ))),
    //         atom_identifier("else-condition"),
    //         SyntaxObject::default(TokenType::If),
    //     )));

    //     let post_condition = ExprKind::If(Box::new(If::new(
    //         atom_identifier("test-condition"),
    //         ExprKind::Transduce(Box::new(Transduce::new(
    //             atom_identifier("x"),
    //             atom_identifier("y"),
    //             atom_int(1),
    //             atom_identifier("z"),
    //             SyntaxObject::default(TokenType::Transduce),
    //         ))),
    //         atom_identifier("else-condition"),
    //         SyntaxObject::default(TokenType::If),
    //     )));

    //     let output = ReplaceExpressions::new(&bindings).visit(expr).unwrap();

    //     assert_eq!(output, post_condition);
    // }

    #[test]
    fn test_expand_datum_syntax() {
        let mut bindings = map! {
            "##struct-name" => atom_identifier("apple"),
        };

        let expr = ExprKind::List(List::new(vec![
            atom_identifier("datum->syntax"),
            atom_identifier("struct-name"),
            atom_identifier("?"),
        ]));

        let post_condition = atom_identifier("apple?");

        let output =
            ReplaceExpressions::new(&mut bindings, &mut HashMap::new(), &mut HashMap::new())
                .visit(expr)
                .unwrap();

        assert_eq!(output, post_condition);
    }

    #[test]
    fn test_expand_ellipses() {
        let mut bindings = map! {
            "apple" => atom_identifier("x"),
            "many" => ExprKind::List(List::new(vec![
                atom_identifier("first"),
                atom_identifier("second")
            ])),
        };

        let expr = ExprKind::List(List::new(vec![
            atom_identifier("apple"),
            atom_identifier("many"),
            ellipses(),
        ]));

        let post_condition = ExprKind::List(List::new(vec![
            atom_identifier("x"),
            atom_identifier("first"),
            atom_identifier("second"),
        ]));

        let output =
            ReplaceExpressions::new(&mut bindings, &mut HashMap::new(), &mut HashMap::new())
                .visit(expr)
                .unwrap();

        assert_eq!(output, post_condition);
    }

    #[test]
    fn test_lambda_expression() {
        let mut bindings = map! {
            "apple" => atom_identifier("x"),
            "many" => ExprKind::List(List::new(vec![
                atom_identifier("first-arg"),
                atom_identifier("second-arg")
            ])),
        };

        let expr: ExprKind = LambdaFunction::new(
            vec![atom_identifier("many"), ellipses()],
            atom_identifier("apple"),
            SyntaxObject::default(TokenType::Lambda),
        )
        .into();

        let post_condition = LambdaFunction::new(
            vec![atom_identifier("first-arg"), atom_identifier("second-arg")],
            atom_identifier("x"),
            SyntaxObject::default(TokenType::Lambda),
        )
        .into();

        let output =
            ReplaceExpressions::new(&mut bindings, &mut HashMap::new(), &mut HashMap::new())
                .visit(expr)
                .unwrap();

        assert_eq!(output, post_condition);
    }
}
