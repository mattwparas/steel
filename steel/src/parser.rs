pub mod lexer;
pub mod span;
pub mod tokens;

// use lexer::Tokenizer;
use lexer::TokenStream;
use tokens::{Token, TokenError, TokenType};

use std::collections::HashMap;
use std::fmt;
// use std::iter::Peekable;
use std::rc::Rc;
use std::result;
use std::str;
use thiserror::Error;

use crate::parser::span::Span;

#[derive(Debug, Clone)]
pub struct SyntaxObject {
    pub(crate)ty: TokenType,
    pub(crate)span: Span
}

impl PartialEq for SyntaxObject {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl SyntaxObject {
    pub fn new(ty: TokenType, span: Span) -> Self {
        SyntaxObject {
            ty,
            span
        }
    }

    pub fn default(ty: TokenType) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0)
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(SyntaxObject),
    VectorVal(Vec<Expr>),
}

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
            Self::Atom(SyntaxObject { ty: t, .. })  => match t {
                TokenType::Identifier(s) => Ok(s),
                _ => Err(err())
            },
            _ => Err(err()),
        }
    }

    pub fn rewrite_span(expr: Expr, span: Span) -> Self {
        match expr {
            Expr::Atom(SyntaxObject { ty: t, ..}) => {
                Expr::Atom(SyntaxObject::new(t, span.clone()))
            }
            Expr::VectorVal(vec_exprs) => {
                Expr::VectorVal(
                    vec_exprs.into_iter().map(|x| Self::rewrite_span(x, span.clone())).collect()
                )
            }
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
            return span
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
                    Expr::Atom(SyntaxObject { span: s, ..}) => {
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
            Self::Atom(SyntaxObject{ span: s, ..}) => {
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
                    return span
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
    // span: String,
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
        }
    }

    // TODO
    fn construct_quote(&mut self, val: Expr, span: Span) -> Expr {

        let q = {
            let rc_val = TokenType::Identifier("quote".to_string());
            let val = Expr::Atom(SyntaxObject::new(rc_val, span));
            // self.intern.insert("quote".to_string(), rc_val);
            val
        };

        // let q = match self.intern.get("quote") {
        //     Some(rc) => Expr::Atom(SyntaxObject::new(rc, span)),
        //     None => {
        //         let rc_val = TokenType::Identifier("quote".to_string());
        //         let val = Expr::Atom(SyntaxObject::new(rc_val, span));
        //         self.intern.insert("quote".to_string(), rc_val);
        //         val
        //     }
        // };

        Expr::VectorVal(vec![q, val])
    }

    // Reader macro for `
    fn construct_quasiquote(&mut self, val: Expr, span: Span) -> Expr {
        let q = {
            let rc_val = TokenType::Identifier("quasiquote".to_string());
            let val = Expr::Atom(SyntaxObject::new(rc_val, span));
            val
        };

        Expr::VectorVal(vec![q, val])
    }

    // Reader macro for ,
    fn construct_unquote(&mut self, val: Expr, span: Span) -> Expr {
        let q = {
            let rc_val = TokenType::Identifier("unquote".to_string());
            let val = Expr::Atom(SyntaxObject::new(rc_val, span));
            val
        };

        Expr::VectorVal(vec![q, val])
    }

    // Reader macro for ,@
    fn construct_unquote_splicing(&mut self, val: Expr, span: Span) -> Expr {
        let q = {
            let rc_val = TokenType::Identifier("unquote-splicing".to_string());
            let val = Expr::Atom(SyntaxObject::new(rc_val, span));
            val
        };

        Expr::VectorVal(vec![q, val])
    }

    // Reader macro for #
    fn construct_lambda_shorthand(&mut self, val: Expr, span: Span) -> Expr {
        let q = {
            let rc_val = TokenType::Identifier("lambda-hash".to_string());
            let val = Expr::Atom(SyntaxObject::new(rc_val, span));
            val
        };

        Expr::VectorVal(vec![q, val])
    }

    // Jason's attempt
    fn read_from_tokens(&mut self) -> Result<Expr> {
        let mut stack: Vec<Vec<Expr>> = Vec::new();
        let mut current_frame: Vec<Expr> = Vec::new();

        loop {
            match self.tokenizer.next() {
                Some(token) => {
                    match token.ty {
                        TokenType::Error => return Err(tokentype_error_to_parse_error(&token)), // TODO
                        TokenType::QuoteTick => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_quote(x, token.span));
                            match quote_inner {
                                Ok(expr) => current_frame.push(expr),
                                Err(e) => return Err(e),
                            }
                        }
                        TokenType::Unquote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_unquote(x, token.span));
                            match quote_inner {
                                Ok(expr) => current_frame.push(expr),
                                Err(e) => return Err(e),
                            }
                        }
                        TokenType::QuasiQuote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_quasiquote(x, token.span));
                            match quote_inner {
                                Ok(expr) => current_frame.push(expr),
                                Err(e) => return Err(e),
                            }
                        }
                        TokenType::UnquoteSplice => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_unquote_splicing(x, token.span));
                            match quote_inner {
                                Ok(expr) => current_frame.push(expr),
                                Err(e) => return Err(e),
                            }
                        }
                        TokenType::Hash => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF))
                                .map(|x| self.construct_lambda_shorthand(x, token.span));
                            match quote_inner {
                                Ok(expr) => current_frame.push(expr),
                                Err(e) => return Err(e),
                            } 
                        }
                        TokenType::OpenParen => {
                            stack.push(current_frame);
                            current_frame = Vec::new();
                        }
                        TokenType::CloseParen => {
                            if let Some(mut prev_frame) = stack.pop() {
                                prev_frame.push(Expr::VectorVal(current_frame));
                                current_frame = prev_frame;
                            } else {
                                return Ok(Expr::VectorVal(current_frame));
                            }
                        }
                        // tok => match &tok {
                        // TODO take out all of the Rc's and redo the intern cache so that its not garbo
                        // TokenType::Identifier(ref s) => match self.intern.get(s) {
                        //     Some(rc) => current_frame.push(Rc::clone(rc)),
                        //     None => {
                        //         // could convert the variable.methodname into methodname variable here
                        //         let val = Rc::new(Expr::Atom(SyntaxObject::from(&token))); // TODO
                        //         current_frame.push(val.clone());
                        //         self.intern.insert(s.to_string(), val);
                        //     }
                        // },
                        _ => current_frame.push(Expr::Atom(SyntaxObject::from(&token))), // TODO
                        // },
                    }
                }
                

                None => return Err(ParseError::UnexpectedEOF)



                // Some(Token { ty: t, .. }) => match t {
                //     TokenType::Error => return Err(ParseError::Unexpected(TokenType::Error)), // TODO
                //     TokenType::QuoteTick => {
                //         let quote_inner = self
                //             .next()
                //             .unwrap_or(Err(ParseError::UnexpectedEOF))
                //             .map(|x| self.construct_quote(x));
                //         match quote_inner {
                //             Ok(expr) => current_frame.push(Rc::new(expr)),
                //             Err(e) => return Err(e),
                //         }
                //     }
                //     TokenType::OpenParen => {
                //         stack.push(current_frame);
                //         current_frame = Vec::new();
                //     }
                //     TokenType::CloseParen => {
                //         if let Some(mut prev_frame) = stack.pop() {
                //             prev_frame.push(Rc::new(Expr::VectorVal(current_frame)));
                //             current_frame = prev_frame;
                //         } else {
                //             return Ok(Expr::VectorVal(current_frame));
                //         }
                //     }
                //     tok => match &tok {
                //         TokenType::Identifier(s) => match self.intern.get(s) {
                //             Some(rc) => current_frame.push(Rc::clone(rc)),
                //             None => {
                //                 // could convert the variable.methodname into methodname variable here
                //                 let val = Rc::new(Expr::Atom(tok.clone()));
                //                 current_frame.push(val.clone());
                //                 self.intern.insert(s.to_string(), val);
                //             }
                //         },
                //         _ => current_frame.push(Rc::new(Expr::Atom(tok))),
                //     },
                // },
                // // Some(Err(e)) => return Err(ParseError::TokenError(e)),
                // None => return Err(ParseError::UnexpectedEOF),
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokenizer.next().map(|res| match res.ty {
            // Err(e) => Err(ParseError::TokenError(e)),
            TokenType::QuoteTick => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF))
                .map(|x| self.construct_quote(x, res.span)),
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
            _ => Ok(Expr::Atom(SyntaxObject::from(&res))),
        })
    }
}

#[cfg(test)]
mod parser_tests {
    use super::Expr::*;
    use super::TokenType::*;
    use super::*;

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[VectorVal(vec![])]);
    }

    #[test]
    fn test_multi_parse() {
        assert_parse(
            "a b +",
            &[
                Atom(SyntaxObject::default(Identifier("a".to_string()))),
                Atom(SyntaxObject::default(Identifier("b".to_string()))),
                Atom(SyntaxObject::default(Identifier("+".to_string()))),
            ],
        );
        assert_parse(
            "a b (lambda  1 (+ 2 3.5))",
            &[
                Atom(SyntaxObject::default(Identifier("a".to_string()))),
                Atom(SyntaxObject::default(Identifier("b".to_string()))),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("lambda".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(1))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("+".to_string()))),
                        Atom(SyntaxObject::default(IntegerLiteral(2))),
                        Atom(SyntaxObject::default(NumberLiteral(3.5))),
                    ]),
                ]),
            ],
        )
    }
    #[test]
    fn test_parse_simple() {
        assert_parse(
            "(+ 1 2 3) (- 4 3)",
            &[
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("+".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(1))),
                    Atom(SyntaxObject::default(IntegerLiteral(2))),
                    Atom(SyntaxObject::default(IntegerLiteral(3))),
                ]),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("-".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(4))),
                    Atom(SyntaxObject::default(IntegerLiteral(3))),
                ]),
            ],
        );
    }
    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[VectorVal(vec![
                Atom(SyntaxObject::default(Identifier("+".to_string()))),
                Atom(SyntaxObject::default(IntegerLiteral(1))),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("foo".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("bar".to_owned()))),
                        Atom(SyntaxObject::default(IntegerLiteral(2))),
                        Atom(SyntaxObject::default(IntegerLiteral(3))),
                    ]),
                ]),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[VectorVal(vec![
                Atom(SyntaxObject::default(Identifier("+".to_string()))),
                Atom(SyntaxObject::default(IntegerLiteral(1))),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("+".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(2))),
                    Atom(SyntaxObject::default(IntegerLiteral(3))),
                ]),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("foo".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("bar".to_owned()))),
                        Atom(SyntaxObject::default(IntegerLiteral(2))),
                        Atom(SyntaxObject::default(IntegerLiteral(3))),
                    ]),
                ]),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (+ (bar 1 1) 3) 5))",
            &[VectorVal(vec![
                Atom(SyntaxObject::default(Identifier("+".to_string()))),
                Atom(SyntaxObject::default(IntegerLiteral(1))),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("+".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(2))),
                    Atom(SyntaxObject::default(IntegerLiteral(3))),
                ]),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("foo".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("+".to_string()))),
                        VectorVal(vec![
                            Atom(SyntaxObject::default(Identifier("bar".to_string()))),
                            Atom(SyntaxObject::default(IntegerLiteral(1))),
                            Atom(SyntaxObject::default(IntegerLiteral(1))),
                        ]),
                        Atom(SyntaxObject::default(IntegerLiteral(3))),
                    ]),
                    Atom(SyntaxObject::default(IntegerLiteral(5))),
                ]),
            ])],
        );
    }
    #[test]
    fn test_parse_specials() {
        assert_parse(
            "(define (foo a b) (+ (- a 1) b))",
            &[VectorVal(vec![
                Atom(SyntaxObject::default(Identifier("define".to_string()))),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("foo".to_string()))),
                    Atom(SyntaxObject::default(Identifier("a".to_string()))),
                    Atom(SyntaxObject::default(Identifier("b".to_string()))),
                ]),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("+".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("-".to_string()))),
                        Atom(SyntaxObject::default(Identifier("a".to_string()))),
                        Atom(SyntaxObject::default(IntegerLiteral(1))),
                    ]),
                    Atom(SyntaxObject::default(Identifier("b".to_string()))),
                ]),
            ])],
        );

        assert_parse(
            "(if   #t     1 2)",
            &[VectorVal(vec![
                Atom(SyntaxObject::default(Identifier("if".to_string()))),
                Atom(SyntaxObject::default(BooleanLiteral(true))),
                Atom(SyntaxObject::default(IntegerLiteral(1))),
                Atom(SyntaxObject::default(IntegerLiteral(2))),
            ])],
        );
        assert_parse(
            "(lambda (a b) (+ a b)) (- 1 2) (\"dumpsterfire\")",
            &[
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("lambda".to_string()))),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("a".to_string()))),
                        Atom(SyntaxObject::default(Identifier("b".to_string()))),
                    ]),
                    VectorVal(vec![
                        Atom(SyntaxObject::default(Identifier("+".to_string()))),
                        Atom(SyntaxObject::default(Identifier("a".to_string()))),
                        Atom(SyntaxObject::default(Identifier("b".to_string()))),
                    ]),
                ]),
                VectorVal(vec![
                    Atom(SyntaxObject::default(Identifier("-".to_string()))),
                    Atom(SyntaxObject::default(IntegerLiteral(1))),
                    Atom(SyntaxObject::default(IntegerLiteral(2))),
                ]),
                VectorVal(vec![Atom(SyntaxObject::default(StringLiteral(
                    "dumpsterfire".to_string(),
                )))]),
            ],
        );
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

    fn assert_parse_err(s: &str, err: ParseError) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<Expr>> = Parser::new(s, &mut cache).collect();
        assert_eq!(a, Err(err));
    }

    fn assert_parse(s: &str, result: &[Expr]) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<Expr>> = Parser::new(s, &mut cache).collect();
        let a = a.unwrap();
        assert_eq!(a, result);
    }
}
