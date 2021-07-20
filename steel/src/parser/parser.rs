use crate::parser::lexer::TokenStream;
use crate::parser::tokens::{Token, TokenType, TokenType::*};

use std::rc::Rc;
use std::result;
use std::str;
use std::{collections::HashMap, path::PathBuf};
use thiserror::Error;

use crate::parser::span::Span;

use crate::parser::ast::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

use super::ast;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyntaxObject {
    pub(crate) ty: TokenType,
    pub(crate) span: Span,
    pub(crate) source: Option<Rc<PathBuf>>,
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
            span,
            source: None,
        }
    }

    pub fn default(ty: TokenType) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0),
            source: None,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span
    }

    pub fn from_token_with_source(val: &Token, source: &Option<Rc<PathBuf>>) -> Self {
        SyntaxObject {
            ty: val.ty.clone(),
            span: val.span,
            source: source.as_ref().map(Rc::clone),
        }
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
            OpenParen => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "(".to_string()).with_span(span))
            }
            CloseParen => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ")".to_string()).with_span(span))
            }
            CharacterLiteral(x) => Ok(CharV(x)),
            BooleanLiteral(x) => Ok(BoolV(x)),
            Identifier(x) => Ok(SymbolV(x.into())),
            NumberLiteral(x) => Ok(NumV(x)),
            IntegerLiteral(x) => Ok(IntV(x)),
            StringLiteral(x) => Ok(StringV(x.into())),
            QuoteTick => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "'".to_string()).with_span(span))
            }
            Unquote => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ",".to_string()).with_span(span))
            }
            QuasiQuote => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "`".to_string()).with_span(span))
            }
            UnquoteSplice => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, ",@".to_string()).with_span(span))
            }
            Error => {
                Err(SteelErr::new(ErrorKind::UnexpectedToken, "error".to_string()).with_span(span))
            }
            Comment => Err(
                SteelErr::new(ErrorKind::UnexpectedToken, "comment".to_string()).with_span(span),
            ),
            Hash => Err(SteelErr::new(ErrorKind::UnexpectedToken, "#".to_string()).with_span(span)),
            If => Ok(SymbolV("if".into())),
            Define => Ok(SymbolV("define".into())),
            Let => Ok(SymbolV("let".into())),
            Transduce => Ok(SymbolV("transduce".into())),
            Execute => Ok(SymbolV("execute".into())),
            Return => Ok(SymbolV("return!".into())),
            Begin => Ok(SymbolV("begin".into())),
            Panic => Ok(SymbolV("panic!".into())),
            Lambda => Ok(SymbolV("lambda".into())),
            Quote => Ok(SymbolV("quote".into())),
            DefineSyntax => Ok(SymbolV("define-syntax".into())),
            SyntaxRules => Ok(SymbolV("syntax-rules".into())),
            Ellipses => Ok(SymbolV("...".into())),
            Struct => Ok(SymbolV("struct".into())),
            Apply => Ok(SymbolV("apply".into())),
            Set => Ok(SymbolV("set!".into())),
            Read => Ok(SymbolV("read".into())),
            Eval => Ok(SymbolV("eval".into())),
            Require => Ok(SymbolV("require".into())),
            CallCC => Ok(SymbolV("call/cc".into())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Error)]
pub enum ParseError {
    // #[error("Parse: Error reading tokens: {0}")]
    // TokenError(#[from] TokenError),
    #[error("Parse: Unexpected token: {0:?}")]
    Unexpected(TokenType, Option<Rc<PathBuf>>),
    #[error("Parse: Unexpected EOF")]
    UnexpectedEOF(Option<Rc<PathBuf>>),
    #[error("Parse: Unexpected character: {0:?}")]
    UnexpectedChar(char, Span, Option<Rc<PathBuf>>),
    #[error("Parse: Incomplete String: {0}")]
    IncompleteString(String, Span, Option<Rc<PathBuf>>),
    #[error("Parse: Syntax Error: {0}")]
    SyntaxError(String, Span, Option<Rc<PathBuf>>),
    #[error("Parse: Arity mismatch: {0}")]
    ArityMismatch(String, Span, Option<Rc<PathBuf>>),
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            // ParseError::TokenError(_) => None,
            ParseError::Unexpected(_, _) => None,
            ParseError::UnexpectedEOF(_) => None,
            ParseError::UnexpectedChar(_, s, _) => Some(*s),
            ParseError::IncompleteString(_, s, _) => Some(*s),
            ParseError::SyntaxError(_, s, _) => Some(*s),
            ParseError::ArityMismatch(_, s, _) => Some(*s),
        }
    }

    pub fn set_source(self, source: Option<Rc<PathBuf>>) -> Self {
        use ParseError::*;
        match self {
            ParseError::Unexpected(l, _) => Unexpected(l, source),
            ParseError::UnexpectedEOF(_) => UnexpectedEOF(source),
            ParseError::UnexpectedChar(l, s, _) => UnexpectedChar(l, s, source),
            ParseError::IncompleteString(l, s, _) => IncompleteString(l, s, source),
            ParseError::SyntaxError(l, s, _) => SyntaxError(l, s, source),
            ParseError::ArityMismatch(l, s, _) => ArityMismatch(l, s, source),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: TokenStream<'a>,
    intern: &'a mut HashMap<String, Rc<TokenType>>,
    quote_stack: Vec<usize>,
    shorthand_quote_stack: Vec<usize>,
    source_name: Option<Rc<PathBuf>>,
}

impl<'a> Parser<'a> {
    #[cfg(test)]
    pub fn parse(expr: &str) -> Result<Vec<ExprKind>> {
        let mut intern = HashMap::new();
        Parser::new(expr, &mut intern).collect()
    }
}

pub type Result<T> = result::Result<T, ParseError>;

fn tokentype_error_to_parse_error(t: &Token) -> ParseError {
    if let TokenType::Error = t.ty {
        // println!("Found an error: {}", t);

        if t.source.starts_with('\"') {
            ParseError::IncompleteString(t.source.to_string(), t.span, None)
        } else {
            ParseError::UnexpectedChar(t.source.chars().next().unwrap(), t.span, None)
        }
    } else {
        ParseError::UnexpectedEOF(None)
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, intern: &'a mut HashMap<String, Rc<TokenType>>) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, true),
            intern,
            quote_stack: Vec::new(),
            shorthand_quote_stack: Vec::new(),
            source_name: None,
        }
    }

    pub fn new_from_source(
        input: &'a str,
        intern: &'a mut HashMap<String, Rc<TokenType>>,
        source_name: PathBuf,
    ) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, true),
            intern,
            quote_stack: Vec::new(),
            shorthand_quote_stack: Vec::new(),
            source_name: Some(Rc::from(source_name)),
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
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,
    fn construct_unquote(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("unquote".to_string());
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for ,@
    fn construct_unquote_splicing(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("unquote-splicing".to_string());
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
        };

        ExprKind::List(List::new(vec![q, val]))
    }

    // Reader macro for #
    fn construct_lambda_shorthand(&mut self, val: ExprKind, span: Span) -> ExprKind {
        let q = {
            let rc_val = TokenType::Identifier("lambda-hash".to_string());
            ExprKind::Atom(Atom::new(SyntaxObject::new(rc_val, span)))
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
                            // self.quote_stack.push(current_frame.len());
                            self.shorthand_quote_stack.push(current_frame.len());
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_quote(x, token.span));
                            // self.quote_stack.pop();
                            self.shorthand_quote_stack.pop();

                            current_frame.push(quote_inner?);
                        }
                        TokenType::Unquote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_unquote(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::QuasiQuote => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_quasiquote(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::UnquoteSplice => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_unquote_splicing(x, token.span));
                            current_frame.push(quote_inner?);
                        }
                        TokenType::Hash => {
                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
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
                                    Some(last_quote_index)
                                        if stack.len() == *last_quote_index
                                            && self.quote_stack.len() == 1 =>
                                    {
                                        self.quote_stack.pop();

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                prev_frame.push(
                                                    ExprKind::try_from(current_frame).map_err(
                                                        |x| x.set_source(self.source_name.clone()),
                                                    )?,
                                                );
                                            }
                                            _ => prev_frame
                                                .push(ExprKind::List(List::new(current_frame))),
                                        }
                                    }
                                    Some(_) if self.quote_stack.len() == 1 => {
                                        // self.quote_stack.pop();

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                prev_frame.push(
                                                    ExprKind::try_from(current_frame).map_err(
                                                        |x| x.set_source(self.source_name.clone()),
                                                    )?,
                                                );
                                            }
                                            _ => prev_frame
                                                .push(ExprKind::List(List::new(current_frame))),
                                        }
                                    }
                                    Some(_) => {
                                        // println!("Inside here");
                                        // println!("quote stack: {:?}", self.quote_stack);
                                        prev_frame.push(ExprKind::List(List::new(current_frame)))
                                    }
                                    _ => {
                                        // println!(
                                        //     "Shorthand quote stack: {:?}",
                                        //     self.shorthand_quote_stack
                                        // );

                                        // println!("Getting here!");

                                        match self.shorthand_quote_stack.last() {
                                            Some(_) => prev_frame
                                                .push(ExprKind::List(List::new(current_frame))),
                                            _ => {
                                                prev_frame.push(
                                                    ExprKind::try_from(current_frame).map_err(
                                                        |x| x.set_source(self.source_name.clone()),
                                                    )?,
                                                );
                                            }
                                        }
                                    }
                                }
                                current_frame = prev_frame;
                            } else {
                                // println!("Shorthand quote stack: {:?}", self.shorthand_quote_stack);
                                // println!("Current frame: {:?}", current_frame);
                                // println!("Quote stack: {:?}", self.quote_stack);

                                match self.shorthand_quote_stack.last() {
                                    Some(last_quote_index) if stack.len() == *last_quote_index => {
                                        // self.shorthand_quote_stack.pop();

                                        // println!("Inside here!");

                                        return Ok(ExprKind::List(List::new(current_frame)));
                                    }
                                    Some(_) => {
                                        // println!("Inside second one");

                                        // self.shorthand_quote_stack.pop();
                                        return Ok(ExprKind::List(List::new(current_frame)));
                                    }

                                    _ => {
                                        // if !self.quote_stack.is_empty() {
                                        //     unimplemented!()
                                        // }

                                        // println!("Inside third here");

                                        return ExprKind::try_from(current_frame)
                                            .map_err(|x| x.set_source(self.source_name.clone()));
                                    }
                                }
                            }
                        }

                        _ => {
                            if let TokenType::Quote = &token.ty {
                                // self.quote_stack.push(current_frame.len());
                                self.quote_stack.push(stack.len());
                            }

                            // println!("{}", token);

                            current_frame.push(ExprKind::Atom(Atom::new(
                                SyntaxObject::from_token_with_source(
                                    &token,
                                    &self.source_name.clone(),
                                ),
                            )))
                        }
                    }
                }

                None => return Err(ParseError::UnexpectedEOF(self.source_name.clone())),
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
                    .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                    .map(|x| self.construct_quote_vec(x, res.span));

                self.shorthand_quote_stack.pop();

                match value {
                    Ok(v) => ExprKind::try_from(v),
                    Err(e) => Err(e),
                }
            }
            TokenType::Unquote => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                .map(|x| self.construct_unquote(x, res.span)),
            TokenType::UnquoteSplice => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                .map(|x| self.construct_unquote_splicing(x, res.span)),
            TokenType::QuasiQuote => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                .map(|x| self.construct_quasiquote(x, res.span)),
            TokenType::Hash => self
                .next()
                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                .map(|x| self.construct_lambda_shorthand(x, res.span)),
            TokenType::OpenParen => self.read_from_tokens(),
            TokenType::CloseParen => Err(ParseError::Unexpected(
                TokenType::CloseParen,
                self.source_name.clone().clone(),
            )),
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

    fn parses(s: &str) {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<_>> = Parser::new(s, &mut cache).collect();
        a.unwrap();
    }

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
    fn parse_quote() {
        // parses("(displayln (match (quote (lambda y z)) '(x y z)))")
        parses("(displayln (match '(lambda y z) '(x y z)))")
    }

    #[test]
    fn parse_unicode() {
        assert_parse(
            "#\\¡",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral('¡'),
            )))],
        );
        assert_parse(
            "#\\\\u{b}",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral('\u{b}'),
            )))],
        );
    }

    #[test]
    fn parse_more_unicode() {
        assert_parse(
            "#\\\\u{a0}",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral('\u{a0}'),
            )))],
        );
    }

    #[test]
    fn parse_strange_characters() {
        assert_parse(
            "#\\^",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral('^'),
            )))],
        );
    }

    #[test]
    fn parse_character_sequence() {
        assert_parse(
            "#\\¡ #\\SPACE #\\g",
            &[
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('¡'),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral(' '),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('g'),
                ))),
            ],
        )
    }

    #[test]
    fn parse_character_sequence_inside_if() {
        assert_parse(
            "(if #\\¡ #\\SPACE #\\g)",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('¡'),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral(' '),
                ))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('g'),
                ))),
                SyntaxObject::default(TokenType::If),
            )))],
        )
    }

    #[test]
    fn parse_close_paren_character() {
        assert_parse(
            "#\\)",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral(')'),
            )))],
        );
        assert_parse(
            "#\\]",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral(']'),
            )))],
        )
    }

    #[test]
    fn parse_open_paren_character() {
        assert_parse(
            "#\\(",
            &[ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral('('),
            )))],
        )
    }

    #[test]
    fn test_error() {
        assert_parse_err("(", ParseError::UnexpectedEOF(None));
        assert_parse_err("(abc", ParseError::UnexpectedEOF(None));
        assert_parse_err("(ab 1 2", ParseError::UnexpectedEOF(None));
        assert_parse_err("((((ab 1 2) (", ParseError::UnexpectedEOF(None));
        assert_parse_err("())", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("() ((((", ParseError::UnexpectedEOF(None));
        assert_parse_err("')", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("(')", ParseError::Unexpected(TokenType::CloseParen, None));
        assert_parse_err("('", ParseError::UnexpectedEOF(None));
    }

    #[test]
    fn test_panic_should_err() {
        assert_parse_is_err("(panic!)");
        assert_parse_is_err("(panic! 1 2)")
    }

    #[test]
    fn quote_multiple_args_should_err() {
        assert_parse_is_err("(quote a b c)");
    }

    #[test]
    fn test_eval_should_err() {
        assert_parse_is_err("(eval)");
        assert_parse_is_err("(eval 1 2)");
    }

    #[test]
    fn test_read_should_err() {
        assert_parse_is_err("(read)");
        assert_parse_is_err("(read 1 2)");
    }

    #[test]
    fn test_let_should_err() {
        assert_parse_is_err("(let)");
        assert_parse_is_err("(let (a) 10)");
    }

    #[test]
    fn test_execute_should_err() {
        assert_parse_is_err("(execute)");
        assert_parse_is_err("(execute 1)");
        assert_parse_is_err("(execute 1 2 3 4)");
    }

    #[test]
    fn test_if_should_err() {
        assert_parse_is_err("(if)");
        assert_parse_is_err("(if 1)");
        assert_parse_is_err("(if 1 2)");
        assert_parse_is_err("(if 1 2 3 4)");
    }

    #[test]
    fn test_transduce_should_err() {
        assert_parse_is_err("(transduce)");
        assert_parse_is_err("(transduce 1)");
        assert_parse_is_err("(transduce 1 2)");
        assert_parse_is_err("(transduce 1 2 3)");
        assert_parse_is_err("(transduce 1 2 3 4 5)");
    }

    #[test]
    fn test_define_should_err() {
        assert_parse_is_err("(define)");
        assert_parse_is_err("(define blagh)");
        assert_parse_is_err("(define test 1 2)");
        assert_parse_is_err("(define () test");
    }

    #[test]
    fn test_lambda_should_err() {
        assert_parse_is_err("(lambda)");
        assert_parse_is_err("(lambda (x))");
    }

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[ExprKind::List(List::new(vec![]))]);
    }

    #[test]
    fn test_empty_quote_inside_if() {
        assert_parse(
            "(if #\\¡ (quote ()) #\\g)",
            &[ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('¡'),
                ))),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                ExprKind::Atom(Atom::new(SyntaxObject::default(
                    TokenType::CharacterLiteral('g'),
                ))),
                SyntaxObject::default(TokenType::If),
            )))],
        )
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
                        SyntaxObject::default(TokenType::Quote),
                    ))),
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
                        SyntaxObject::default(TokenType::Quote),
                    ))),
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
