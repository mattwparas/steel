use crate::parser::lexer::TokenStream;
use crate::parser::tokens::{Token, TokenError, TokenType, TokenType::*};

use std::collections::HashMap;
use std::rc::Rc;
use std::result;
use std::str;
use thiserror::Error;

use crate::parser::span::Span;

use crate::parser::ast::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

use super::ast;

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

    pub fn set_span(&mut self, span: Span) {
        self.span = span
    }
}

impl From<&Token<'_>> for SyntaxObject {
    fn from(val: &Token) -> SyntaxObject {
        SyntaxObject::new(val.ty.clone(), val.span)
    }
}

impl TryFrom<SyntaxObject> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: SyntaxObject) -> std::result::Result<Self, Self::Error> {
        let span = e.span;
        match e.ty {
            OpenParen => Err(SteelErr::UnexpectedToken("(".to_string(), Some(span))),
            CloseParen => Err(SteelErr::UnexpectedToken(")".to_string(), Some(span))),
            CharacterLiteral(x) => Ok(CharV(x)),
            BooleanLiteral(x) => Ok(BoolV(x)),
            Identifier(x) => Ok(SymbolV(x.clone())),
            NumberLiteral(x) => Ok(NumV(x)),
            IntegerLiteral(x) => Ok(IntV(x)),
            StringLiteral(x) => Ok(StringV(x.clone())),
            QuoteTick => Err(SteelErr::UnexpectedToken("'".to_string(), Some(span))),
            Unquote => Err(SteelErr::UnexpectedToken(",".to_string(), Some(span))),
            QuasiQuote => Err(SteelErr::UnexpectedToken("`".to_string(), Some(span))),
            UnquoteSplice => Err(SteelErr::UnexpectedToken(",@".to_string(), Some(span))),
            Error => Err(SteelErr::UnexpectedToken("error".to_string(), Some(span))),
            Comment => Err(SteelErr::UnexpectedToken("comment".to_string(), Some(span))),
            Hash => Err(SteelErr::UnexpectedToken("#".to_string(), Some(span))),
            If => Ok(SymbolV("if".to_string())),
            Define => Ok(SymbolV("define".to_string())),
            Let => Ok(SymbolV("let".to_string())),
            Transduce => Ok(SymbolV("transduce".to_string())),
            Execute => Ok(SymbolV("execute".to_string())),
            Return => Ok(SymbolV("return".to_string())),
            Begin => Ok(SymbolV("begin".to_string())),
            Panic => Ok(SymbolV("panic".to_string())),
            Lambda => Ok(SymbolV("lambda".to_string())),
            Quote => Ok(SymbolV("quote".to_string())),
            DefineSyntax => Ok(SymbolV("define-syntax".to_string())),
            SyntaxRules => Ok(SymbolV("syntax-rules".to_string())),
            Ellipses => Ok(SymbolV("...".to_string())),
            Struct => Ok(SymbolV("struct".to_string())),
            Apply => Ok(SymbolV("apply".to_string())),
            Set => Ok(SymbolV("set!".to_string())),
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
    #[error("Parse: Syntax Error: {0}")]
    SyntaxError(String, Span),
    #[error("Parse: Arity mismatch: {0}")]
    ArityMismatch(String, Span),
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::TokenError(_) => None,
            ParseError::Unexpected(_) => None,
            ParseError::UnexpectedEOF => None,
            ParseError::UnexpectedChar(_, s) => Some(*s),
            ParseError::IncompleteString(_, s) => Some(*s),
            ParseError::SyntaxError(_, s) => Some(*s),
            ParseError::ArityMismatch(_, s) => Some(*s),
        }
    }
}

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
        // let q = {
        //     let rc_val = TokenType::Quote;
        //     ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        //     // let val = ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)));
        //     // // self.intern.insert("quote".to_string(), rc_val);
        //     // val
        // };

        // ExprKind::List(List::new(vec![q, val]))

        ExprKind::Quote(Box::new(ast::Quote::new(
            val,
            SyntaxObject::new(TokenType::Quote, span),
        )))
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

        self.quote_stack = Vec::new();

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
                            self.quote_stack.pop();

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
                                        // println!("Stack length: {}", stack.len());
                                        // println!("last_quote_index: {}", last_quote_index);
                                        // println!("{:?}", current_frame);
                                        prev_frame.push(ExprKind::List(List::new(current_frame)))
                                    }
                                    Some(_) => {
                                        prev_frame.push(ExprKind::List(List::new(current_frame)))
                                    }
                                    _ => match self.shorthand_quote_stack.last() {
                                        Some(_) => {
                                            // self.shorthand_quote_stack.pop();
                                            // println!("{:?}", current_frame);
                                            prev_frame
                                                .push(ExprKind::List(List::new(current_frame)))
                                        }
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
        // self.shorthand_quote_stack = Vec::new();
        // self.quote_stack = Vec::new();

        self.tokenizer.next().map(|res| match res.ty {
            // Err(e) => Err(ParseError::TokenError(e)),
            TokenType::QuoteTick => {
                // See if this does the job
                self.shorthand_quote_stack.push(0);

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF))
                    .map(|x| self.construct_quote_vec(x, res.span));

                self.shorthand_quote_stack.pop();

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
    // use super::TokenType::*;
    use super::*;
    use crate::parser::ast::ExprKind;
    use crate::parser::ast::{
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
        assert_parse_is_err("(execute)");
        assert_parse_is_err("(panic!)");
    }

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[ExprKind::List(List::new(vec![]))]);
    }

    #[test]
    fn test_empty_quote() {
        assert_parse(
            "'()",
            &[ExprKind::Quote(
                Quote::new(
                    List::new(vec![]).into(),
                    SyntaxObject::default(TokenType::Quote),
                )
                .into(),
            )],
        )
    }

    #[test]
    fn test_empty_quote_nested() {
        assert_parse(
            "(list '())",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                    "list".to_string(),
                )))),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
            ]))],
        )
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
                    ExprKind::Quote(Box::new(Quote::new(
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "one".to_string(),
                        )))),
                        SyntaxObject::default(TokenType::Quote)
                    )))
                    // ])),
                    // ExprKind::List(List::new(vec![
                    //     ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Quote))),
                    //     ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    //         "one".to_string(),
                    //     )))),
                    // ])),
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
                    ExprKind::Quote(Box::new(Quote::new(
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "one".to_string(),
                        )))),
                        SyntaxObject::default(TokenType::Quote)
                    )))
                    // ExprKind::List(List::new(vec![
                    //     ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Quote))),
                    //     ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    //         "one".to_string(),
                    //     )))),
                    // ])),
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
            "(return! 10)",
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
            "(panic! 10)",
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

    #[test]
    fn test_execute() {
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
    fn test_execute_nested() {
        assert_parse(
            "(if (empty? lst) '() (execute a b))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "empty?".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "lst".to_string(),
                    )))),
                ])),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::Execute(Box::new(Execute::new(
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "a".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "b".to_string(),
                    )))),
                    None,
                    SyntaxObject::default(TokenType::Execute),
                ))),
                SyntaxObject::default(TokenType::If),
            )))],
        )
    }

    #[test]
    fn test_quote_with_inner_nested() {
        assert_parse(
            "'(#f '())",
            &[ExprKind::Quote(
                Quote::new(
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(
                            TokenType::BooleanLiteral(false),
                        ))),
                        ExprKind::Quote(
                            Quote::new(
                                List::new(vec![]).into(),
                                SyntaxObject::default(TokenType::Quote),
                            )
                            .into(),
                        ),
                    ])),
                    SyntaxObject::default(TokenType::Quote),
                )
                .into(),
            )],
        )
    }

    #[test]
    fn test_quote_with_inner_nested_sub_expr() {
        assert_parse(
            "(if (null? contents)
                '(#f '())
                (list (car contents) (cdr contents)))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "null?".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "contents".to_string(),
                    )))),
                ])),
                ExprKind::Quote(
                    Quote::new(
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(
                                TokenType::BooleanLiteral(false),
                            ))),
                            ExprKind::Quote(
                                Quote::new(
                                    List::new(vec![]).into(),
                                    SyntaxObject::default(TokenType::Quote),
                                )
                                .into(),
                            ),
                        ])),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "list".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "car".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "contents".to_string(),
                        )))),
                    ])),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "cdr".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "contents".to_string(),
                        )))),
                    ])),
                ])),
                SyntaxObject::default(TokenType::If),
            )))],
        );
    }

    #[test]
    fn test_quote_normal_with_inner_nested_sub_expr() {
        assert_parse(
            "(if (null? contents)
                (quote (#f '()))
                (list (car contents) (cdr contents)))",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "null?".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "contents".to_string(),
                    )))),
                ])),
                ExprKind::Quote(
                    Quote::new(
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(
                                TokenType::BooleanLiteral(false),
                            ))),
                            ExprKind::Quote(
                                Quote::new(
                                    List::new(vec![]).into(),
                                    SyntaxObject::default(TokenType::Quote),
                                )
                                .into(),
                            ),
                        ])),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                        "list".to_string(),
                    )))),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "car".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "contents".to_string(),
                        )))),
                    ])),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "cdr".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "contents".to_string(),
                        )))),
                    ])),
                ])),
                SyntaxObject::default(TokenType::If),
            )))],
        );
    }

    #[test]
    fn test_quote_with_inner_sub_expr_even_more_nested() {
        assert_parse(
            "(list 
                (if (null? contents)
                '(#f '())
                (list (car contents) (cdr contents))))",
            &[ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                    "list".to_string(),
                )))),
                ExprKind::If(Box::new(If::new(
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "null?".to_string(),
                        )))),
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "contents".to_string(),
                        )))),
                    ])),
                    ExprKind::Quote(
                        Quote::new(
                            ExprKind::List(List::new(vec![
                                ExprKind::Atom(Atom::new(SyntaxObject::default(
                                    TokenType::BooleanLiteral(false),
                                ))),
                                ExprKind::Quote(
                                    Quote::new(
                                        List::new(vec![]).into(),
                                        SyntaxObject::default(TokenType::Quote),
                                    )
                                    .into(),
                                ),
                            ])),
                            SyntaxObject::default(TokenType::Quote),
                        )
                        .into(),
                    ),
                    ExprKind::List(List::new(vec![
                        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                            "list".to_string(),
                        )))),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "car".to_string(),
                            )))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "contents".to_string(),
                            )))),
                        ])),
                        ExprKind::List(List::new(vec![
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "cdr".to_string(),
                            )))),
                            ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                                "contents".to_string(),
                            )))),
                        ])),
                    ])),
                    SyntaxObject::default(TokenType::If),
                ))),
            ]))],
        );
    }

    #[test]
    fn test_define_with_datum_syntax_name() {
        assert_parse(
            "(define (datum->syntax var) (car ret-value))",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "datum->syntax".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "var".to_string(),
                    )))),
                ])),
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "car".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "ret-value".to_string(),
                    )))),
                ])),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_with_datum_syntax_function_name() {
        assert_parse(
            "(define ((datum->syntax var) arg) 10)",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::List(List::new(vec![
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "datum->syntax".to_string(),
                    )))),
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                        "var".to_string(),
                    )))),
                ])),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                        TokenType::Identifier("arg".to_string()),
                    )))],
                    ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
                        10,
                    )))),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }
}
