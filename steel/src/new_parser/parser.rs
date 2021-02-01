// use lexer::Tokenizer;
use crate::new_parser::lexer::TokenStream;
use crate::new_parser::tokens::{Token, TokenError, TokenType};

use std::collections::HashMap;
use std::fmt;
// use std::iter::Peekable;
use std::rc::Rc;
use std::result;
use std::str;
use thiserror::Error;

use crate::parser::span::Span;

use crate::new_parser::ast::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxObject {
    pub(crate) ty: TokenType,
    pub(crate) span: Span,
}

impl PartialEq for SyntaxObject {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl SyntaxObject {
    pub fn new(ty: TokenType, span: Span) -> Self {
        SyntaxObject { ty, span }
    }

    pub fn default(ty: TokenType) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0),
        }
    }
}

impl From<&Token<'_>> for SyntaxObject {
    fn from(val: &Token) -> SyntaxObject {
        SyntaxObject::new(val.ty.clone(), val.span)
    }
}

// impl From<Box<dyn CustomType>> for SteelVal {
//     fn from(val: Box<dyn CustomType>) -> SteelVal {
//         val.new_steel_val()
//     }
// }

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Atom(SyntaxObject),
    VectorVal(Vec<Expr>),
}

// pub trait Node {
//     // fn ();
// }

// pub enum TestExpr {
//     Atom(SyntaxObject),
//     Node(Box<dyn Node>),
// }

// struct If {
//     test: TestExpr,
//     then: TestExpr,
//     els: TestExpr
// }

// struct Define {

// }

// impl Node for If {

// }

impl Expr {
    pub fn vector_val_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&[Expr], E> {
        match self {
            Self::VectorVal(v) => Ok(v),
            Self::Atom(_) => Err(err()),
        }
    }

    pub fn atom_identifier_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&str, E> {
        match self {
            Self::Atom(SyntaxObject { ty: t, .. }) => match t {
                TokenType::Identifier(s) => Ok(s),
                _ => Err(err()),
            },
            _ => Err(err()),
        }
    }

    pub fn rewrite_span(expr: Expr, span: Span) -> Self {
        match expr {
            Expr::Atom(SyntaxObject { ty: t, .. }) => {
                Expr::Atom(SyntaxObject::new(t, span.clone()))
            }
            Expr::VectorVal(vec_exprs) => Expr::VectorVal(
                vec_exprs
                    .into_iter()
                    .map(|x| Self::rewrite_span(x, span.clone()))
                    .collect(),
            ),
        }
    }

    pub fn coalesce_span(spans: Vec<Span>) -> Span {
        let span = spans.get(0);
        if let Some(span) = span {
            let mut span = span.clone();
            for s in spans {
                if s.start() < span.start() {
                    span = Span::new(s.start(), span.end());
                }
                if s.end() > span.end() {
                    span = Span::new(s.start(), s.end());
                }
            }
            return span;
        } else {
            Span::new(0, 0)
        }
    }

    pub fn span(&self) -> Span {
        // let mut span = Span::new(0, 0);

        fn collect_span(vec_exprs: Vec<Expr>) -> Vec<Span> {
            let mut spans = Vec::new();
            for exp in vec_exprs {
                match exp {
                    Expr::Atom(SyntaxObject { span: s, .. }) => {
                        spans.push(s);
                    }
                    Expr::VectorVal(vec_exprs2) => {
                        let mut res = collect_span(vec_exprs2);
                        spans.append(&mut res);
                    }
                }
            }
            spans
        }

        match &self {
            Self::Atom(SyntaxObject { span: s, .. }) => {
                return s.clone();
            }
            Self::VectorVal(vec_exprs) => {
                let spans = collect_span(vec_exprs.clone());
                let span = spans.get(0);
                if let Some(span) = span {
                    let mut span = span.clone();
                    for s in spans {
                        if s.start() < span.start() {
                            span = Span::new(s.start(), span.end());
                        }
                        if s.end() > span.end() {
                            span = Span::new(span.start(), s.end());
                        }
                    }
                    return span;
                } else {
                    Span::new(0, 0)
                }
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(t) => write!(f, "{}", t.ty.to_string()),
            Expr::VectorVal(t) => {
                let lst = t
                    .iter()
                    .map(|item| item.to_string() + " ")
                    .collect::<String>();
                write!(f, "({})", lst.trim())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Parse: Error reading tokens: {0}")]
    TokenError(#[from] TokenError),
    #[error("Parse: Unexpected token: {0:?}")]
    Unexpected(TokenType),
    #[error("Parse: Unexpected EOF")]
    UnexpectedEOF,
    #[error("Parse: Unexpected character: {0}")]
    UnexpectedChar(char, Span),
    #[error("Parse: Incomplete String: {0}")]
    IncompleteString(String, Span),
    #[error("Parse: Syntax Error")]
    SyntaxError(String, Span),
    #[error("Parse: Arity mismatch")]
    ArityMismatch(String, Span),
}

// #[derive(Clone, Debug, PartialEq, Error)]
// pub enum TokenError {
//     #[error("Unexpected char, {0} on line: {1}")]
//     UnexpectedChar(char, usize),
//     #[error("Incomplete String on line {0}")]
//     IncompleteString(usize),
//     #[error("Invalid Escape on line {0}")]
//     InvalidEscape(usize),
//     #[error("Invalid Character on line {0}")]
//     InvalidCharacter(usize),
// }

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: TokenStream<'a>,
    intern: &'a mut HashMap<String, Rc<TokenType>>,
    quote_stack: Vec<usize>,
    shorthand_quote_stack: Vec<usize>, // quote_ctx:
                                       // span: String
}

pub type Result<T> = result::Result<T, ParseError>;

fn tokentype_error_to_parse_error(t: &Token) -> ParseError {
    if let TokenType::Error = t.ty {
        if t.source.starts_with("\"") {
            return ParseError::IncompleteString(t.source.to_string(), t.span);
        } else {
            return ParseError::UnexpectedChar(t.source.chars().next().unwrap(), t.span);
        }
    } else {
        return ParseError::UnexpectedEOF;
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, intern: &'a mut HashMap<String, Rc<TokenType>>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, true),
            intern,
            quote_stack: Vec::new(),
            shorthand_quote_stack: Vec::new(),
        }
    }

    // TODO this is definitely wrong
    fn construct_quote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Quote;
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
            // let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            // // self.intern.insert("quote".to_string(), rc_val);
            // val
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn construct_quote_vec(&mut self, val: ExprKind, span: Span) -> Vec<ExprKind> {
        let q = {
            let rc_val = TokenType::Quote;
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
            // let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            // // self.intern.insert("quote".to_string(), rc_val);
            // val
        };

        vec![q, val]
    }

    // Reader macro for `
    fn construct_quasiquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("quasiquote".to_string());
            let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            val
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,
    fn construct_unquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("unquote".to_string());
            let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            val
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,@
    fn construct_unquote_splicing(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("unquote-splicing".to_string());
            let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            val
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for #
    fn construct_lambda_shorthand(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("lambda-hash".to_string());
            let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
            val
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    fn read_from_tokens(&mut self) -> Result<ExprKind> {
        let mut stack: Vec<Vec<ExprKind>> = Vec::new();
        let mut current_frame: Vec<ExprKind> = Vec::new();

        loop {
            match self.tokenizer.next() {
                Some(token) => {
                    match token.ty {
                        TokenType::Error => return Err(tokentype_error_to_parse_error(&token)), // TODO
                        TokenType::QuoteTick => {
                            // quote_count += 1;
                            self.quote_stack.push(current_frame.len());
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_quote(x, token.span));

                            current_frame.push(quote_inner?);
                        }
                        TokenType::Unquote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_unquote(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::QuasiQuote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_quasiquote(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::UnquoteSplice => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_unquote_splicing(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::Hash => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_lambda_shorthand(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::OpenParen => {
                            stack.push(current_frame);
                            current_frame = Vec::new();
                        }
                        TokenType::CloseParen => {
                            if let Some(mut prev_frame) = stack.pop() {
                                match self.quote_stack.last() {
                                    Some(last_quote_index)
                                        if stack.len() == *last_quote_index
                                            && self.quote_stack.len() > 1 =>
                                    {
                                        self.quote_stack.pop();
                                        prev_frame.push(ExprKind::List(List::new(current_frame)))
                                    }
                                    Some(_) => {
                                        prev_frame.push(ExprKind::List(List::new(current_frame)))
                                    }
                                    _ => match self.shorthand_quote_stack.last() {
                                        Some(_) => prev_frame
                                            .push(ExprKind::List(List::new(current_frame))),
                                        _ => {
                                            prev_frame.push(ExprKind::try_from(current_frame)?);
                                        }
                                    },
                                }
                                current_frame = prev_frame;
                            } else {
                                match self.shorthand_quote_stack.last() {
                                    Some(last_quote_index) if stack.len() == *last_quote_index => {
                                        self.shorthand_quote_stack.pop();
                                        return Ok(ExprKind::List(List::new(current_frame)));
                                    }

                                    _ => return ExprKind::try_from(current_frame),
                                }
                            }
                        }

                        _ => {
                            if let TokenType::Quote = &token.ty {
                                self.quote_stack.push(current_frame.len());
                            }

                            current_frame
                                .push(ExprKind::Atom(Atom::new(SyntaxObject::from(&token))))
                        }
                    }
                }

                None => return Err(ParseError::UnexpectedEOF),
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ExprKind>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokenizer.next().map(|res| match res.ty {
            // Err(e) => Err(ParseError::TokenError(e)),
            TokenType::QuoteTick => {
                // See if this does the job
                self.shorthand_quote_stack.push(0);

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF))
                    .map(|x| self.construct_quote_vec(x, res.span));

                match value {
                    Ok(v) => ExprKind::try_from(v),
                    Err(e) => Err(e),
                }
            }
            TokenType::Unquote => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF))
                .map(|x| self.construct_unquote(x, res.span)),
            TokenType::UnquoteSplice => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF))
                .map(|x| self.construct_unquote_splicing(x, res.span)),
            TokenType::QuasiQuote => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF))
                .map(|x| self.construct_quasiquote(x, res.span)),
            TokenType::Hash => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF))
                .map(|x| self.construct_lambda_shorthand(x, res.span)),
            TokenType::OpenParen => self.read_from_tokens(),
            TokenType::CloseParen => Err(ParseError::Unexpected(TokenType::CloseParen)),
            TokenType::Error => Err(tokentype_error_to_parse_error(&res)),
            _ => Ok(ExprKind::Atom(Atom::new(SyntaxObject::from(&res)))),
        })
    }
}

#[cfg(test)]
mod parser_tests {
    use super::TokenType::*;
    use super::*;
    use crate::new_parser::ast::ExprKind;
    use crate::new_parser::ast::{
        Begin, Define, Execute, If, LambdaFunction, Panic, Quote, Return, Transduce,
    };

    fn assert_parse(s: &str, result: &[ExprKind]) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<ExprKind>> = Parser::new(s, &mut cache).collect();
        let a = a.unwrap();
        assert_eq!(a.as_slice(), result);
    }

    fn assert_parse_err(s: &str, err: ParseError) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<ExprKind>> = Parser::new(s, &mut cache).collect();
        assert_eq!(a, Err(err));
    }

    fn assert_parse_is_err(s: &str) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<ExprKind>> = Parser::new(s, &mut cache).collect();
        assert!(a.is_err());
    }

    #[test]
    fn test_error() {
        assert_parse_err("(", ParseError::UnexpectedEOF);
        assert_parse_err("(abc", ParseError::UnexpectedEOF);
        assert_parse_err("(ab 1 2", ParseError::UnexpectedEOF);
        assert_parse_err("((((ab 1 2) (", ParseError::UnexpectedEOF);
        assert_parse_err("())", ParseError::Unexpected(TokenType::CloseParen));
        assert_parse_err("() ((((", ParseError::UnexpectedEOF);
        assert_parse_err("')", ParseError::Unexpected(TokenType::CloseParen));
        assert_parse_err("(')", ParseError::Unexpected(TokenType::CloseParen));
        assert_parse_err("('", ParseError::UnexpectedEOF);
    }

    #[test]
    fn test_should_err() {
        assert_parse_is_err("(lambda (1 2) (+ 1 2 3))");
        assert_parse_is_err("(define (1 2 3) 10)");
        assert_parse_is_err("(execute)");
        assert_parse_is_err("(panic)");
    }

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[ExprKind::List(List::new(vec![]))]);
    }

    #[test]
    fn test_multi_parse_simple() {
        assert_parse(
            "a b +",
            &[
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
            ],
        );
    }

    #[test]
    fn test_multi_parse_complicated() {
        assert_parse(
            "a b (funcall  1 (+ 2 3.5))",
            &[
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "funcall".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(NumberLiteral(3.5)))),
                    ])),
                ])),
            ],
        )
    }
    #[test]
    fn test_parse_simple() {
        assert_parse(
            "(+ 1 2 3) (- 4 3)",
            &[
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "-".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(4)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ])),
            ],
        );
    }

    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "foo".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "bar".to_owned(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                    ])),
                ])),
            ]))],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "foo".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "bar".to_owned(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                    ])),
                ])),
            ]))],
        );
    }

    #[test]
    fn test_if() {
        assert_parse(
            "(+ 1 (if 2 3 4) (foo (+ (bar 1 1) 3) 5))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                ExprKind::If(Box::new(If::new(
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(4)))),
                    SyntaxObject::default(If),
                ))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "foo".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "bar".to_string(),
                            )))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                        ])),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                    ])),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(5)))),
                ])),
            ]))],
        );
    }

    #[test]
    fn test_quote() {
        assert_parse(
            "(quote (if 1 2))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand() {
        assert_parse(
            "'(if 1 2)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_nested() {
        assert_parse(
            "(quote (if (if 1 2) 3))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ])),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand_nested() {
        assert_parse(
            "'(if (if 1 2) 3)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ])),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_shorthand_multiple_exprs() {
        assert_parse(
            "'(if (if 1 2) 3) (+ 1 (if 2 3 4) (foo (+ (bar 1 1) 3) 5))",
            &[
                ExprKind::Quote(Box::new(Quote::new(
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::If))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                        ])),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                    ])),
                    SyntaxObject::default(TokenType::Quote),
                ))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::If(Box::new(If::new(
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(4)))),
                        SyntaxObject::default(If),
                    ))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "foo".to_string(),
                        )))),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "+".to_string(),
                            )))),
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "bar".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                            ])),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                        ])),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(5)))),
                    ])),
                ])),
            ],
        )
    }

    #[test]
    fn test_quote_inner() {
        assert_parse(
            "'(applesauce 'one)",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "applesauce".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Quote))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "one".to_string(),
                        )))),
                    ])),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quote_inner_without_shorthand() {
        assert_parse(
            "(quote (applesauce 'one))",
            &[ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "applesauce".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Quote))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "one".to_string(),
                        )))),
                    ])),
                ])),
                SyntaxObject::default(TokenType::Quote),
            )))],
        )
    }

    #[test]
    fn test_quasiquote_shorthand() {
        assert_parse(
            "`(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "quasiquote".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_quasiquote_normal() {
        assert_parse(
            "(quasiquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "quasiquote".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_shorthand() {
        assert_parse(
            ",(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "unquote".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_normal() {
        assert_parse(
            "(unquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "unquote".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_shorthand() {
        assert_parse(
            ",@(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "unquote-splicing".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_normal() {
        assert_parse(
            "(unquote-splicing (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "unquote-splicing".to_string(),
                )))),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "+".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                ])),
            ]))],
        )
    }

    #[test]
    fn test_transduce() {
        assert_parse(
            "(transduce a b c d)",
            &[ExprKind::Transduce(Box::new(Transduce::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "c".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "d".to_string(),
                )))),
                SyntaxObject::default(TokenType::Transduce),
            )))],
        )
    }

    #[test]
    fn test_transduce_complex() {
        assert_parse(
            "(if #t (transduce a b c d) (if #f 10 20))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
                ExprKind::Transduce(Box::new(Transduce::new(
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "a".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "b".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "c".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "d".to_string(),
                    )))),
                    SyntaxObject::default(TokenType::Transduce),
                ))),
                ExprKind::If(Box::new(If::new(
                    ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(false)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                    SyntaxObject::default(If),
                ))),
                SyntaxObject::default(If),
            )))],
        )
    }

    #[test]
    fn test_define_simple() {
        assert_parse(
            "(define a 10)",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_simple() {
        assert_parse(
            "(define (foo x) (+ x 10))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "foo".to_string(),
                )))),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                        Identifier("x".to_string()),
                    )))],
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                    ])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_multiple_args() {
        assert_parse(
            "(define (foo x y z) (+ x 10))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "foo".to_string(),
                )))),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "y".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "z".to_string(),
                        )))),
                    ],
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                    ])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_multiple_args_multiple_body_exprs() {
        assert_parse(
            "(define (foo x y z) (+ x 10) (+ y 20) (+ z 30))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "foo".to_string(),
                )))),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "x".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "y".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "z".to_string(),
                        )))),
                    ],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "+".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "x".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(
                                    10,
                                )))),
                            ])),
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "+".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "y".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(
                                    20,
                                )))),
                            ])),
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "+".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "z".to_string(),
                                )))),
                                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(
                                    30,
                                )))),
                            ])),
                        ],
                        SyntaxObject::default(TokenType::Begin),
                    )),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_recursive_function() {
        assert_parse(
            "(define (test) (define (foo) (bar)) (define (bar) (foo)))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "test".to_string(),
                )))),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::Define(Box::new(Define::new(
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "foo".to_string(),
                                )))),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![ExprKind::Atom(Atom::new(
                                        SyntaxObject::default(Identifier("bar".to_string())),
                                    ))])),
                                    SyntaxObject::default(TokenType::Lambda),
                                ))),
                                SyntaxObject::default(TokenType::Define),
                            ))),
                            ExprKind::Define(Box::new(Define::new(
                                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                    "bar".to_string(),
                                )))),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![ExprKind::Atom(Atom::new(
                                        SyntaxObject::default(Identifier("foo".to_string())),
                                    ))])),
                                    SyntaxObject::default(TokenType::Lambda),
                                ))),
                                SyntaxObject::default(TokenType::Define),
                            ))),
                        ],
                        SyntaxObject::default(TokenType::Begin),
                    )),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_execute_two_arguments() {
        assert_parse(
            "(execute a b)",
            &[ExprKind::Execute(Box::new(Execute::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                None,
                SyntaxObject::default(TokenType::Execute),
            )))],
        )
    }

    #[test]
    fn test_execute_three_arguments() {
        assert_parse(
            "(execute a b c)",
            &[ExprKind::Execute(Box::new(Execute::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                Some(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    Identifier("c".to_string()),
                )))),
                SyntaxObject::default(TokenType::Execute),
            )))],
        )
    }

    #[test]
    fn test_return_normal() {
        assert_parse(
            "(return 10)",
            &[ExprKind::Return(Box::new(Return::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                SyntaxObject::default(TokenType::Return),
            )))],
        )
    }

    #[test]
    fn test_begin() {
        assert_parse(
            "(begin 1 2 3)",
            &[ExprKind::Begin(Begin::new(
                vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(1)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(2)))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(3)))),
                ],
                SyntaxObject::default(TokenType::Begin),
            ))],
        )
    }

    #[test]
    fn test_panic_normal() {
        assert_parse(
            "(panic 10)",
            &[ExprKind::Panic(Box::new(Panic::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                SyntaxObject::default(TokenType::Panic),
            )))],
        )
    }

    #[test]
    fn test_lambda_function() {
        assert_parse(
            "(lambda (x) 10)",
            &[ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                    Identifier("x".to_string()),
                )))],
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                SyntaxObject::default(TokenType::Lambda),
            )))],
        )
    }

    #[test]
    fn test_lambda_matches_let() {
        assert_parse(
            "((lambda (a) (+ a 20)) 10)",
            &[ExprKind::List(List::new(vec![
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                        Identifier("a".to_string()),
                    )))],
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "a".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                    ])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
            ]))],
        );
    }

    #[test]
    fn test_let() {
        assert_parse(
            "(let ([a 10]) (+ a 20))",
            &[ExprKind::List(List::new(vec![
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                        Identifier("a".to_string()),
                    )))],
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "+".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "a".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                    ])),
                    SyntaxObject::default(TokenType::Let),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
            ]))],
        )
    }
}
