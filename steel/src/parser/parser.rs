use crate::parser::lexer::TokenStream;
use crate::parser::tokens::{Token, TokenType, TokenType::*};

use std::result;
use std::str;
use std::{collections::HashMap, path::PathBuf};
use std::{fmt::write, rc::Rc};
use thiserror::Error;

use crate::parser::span::Span;

use crate::parser::ast::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

use super::ast;

use std::sync::atomic::{AtomicUsize, Ordering};

pub(crate) static SYNTAX_OBJECT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug)]
pub struct SyntaxObjectId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug)]
pub struct ListId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default, Debug)]
pub struct FunctionId(usize);

impl std::fmt::Display for SyntaxObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A syntax object that can hold anything as the syntax
/// In this case, we're using the token type emitted by logos
///
/// This should open the door to interning our strings to make
/// parsing (and optimizations later) faster

#[derive(Serialize, Deserialize)]
pub struct RawSyntaxObject<T> {
    pub(crate) ty: T,
    pub(crate) span: Span,
    pub(crate) source: Option<Rc<PathBuf>>,
    pub(crate) metadata: Option<IdentifierMetadata>,
    pub(crate) syntax_object_id: SyntaxObjectId,
}

impl<T: Clone> Clone for RawSyntaxObject<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            span: self.span.clone(),
            source: self.source.clone(),
            metadata: self.metadata.clone(),
            syntax_object_id: self.syntax_object_id.clone(),
        }
    }
}

/// Denotes what kind of identifier we actually have
#[derive(Clone, Serialize, Deserialize)]
pub enum IdentifierType {
    Free,
    Global,
    Local,
    Macro,
    Module,
}

#[derive(Clone, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord, PartialEq, Default)]
pub struct IdentifierMetadata {
    // kind: IdentifierType,
    // built_in: bool,
    pub(crate) depth: usize,
}

impl<T: std::fmt::Debug> std::fmt::Debug for RawSyntaxObject<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RawSyntaxObject")
            .field("ty", &self.ty)
            .field("span", &self.span)
            .field("source", &self.source)
            .finish()
    }
}

// Implementing hash here just on the token type - we dont want the span included
// For determining the hash here
impl<T: std::hash::Hash> std::hash::Hash for RawSyntaxObject<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.span.hash(state);
        self.metadata.hash(state);
    }
}

pub type SyntaxObject = RawSyntaxObject<TokenType>;

// #[derive(Debug, Clone, Serialize, Deserialize)]
// pub struct SyntaxObject {
//     pub(crate) ty: TokenType,
//     pub(crate) span: Span,
//     pub(crate) source: Option<Rc<PathBuf>>,
// }

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
            metadata: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::SeqCst)),
        }
    }

    pub fn new_with_source(ty: TokenType, span: Span, source: Option<Rc<PathBuf>>) -> Self {
        SyntaxObject {
            ty,
            span,
            source,
            metadata: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::SeqCst)),
        }
    }

    pub fn default(ty: TokenType) -> Self {
        SyntaxObject {
            ty,
            span: Span::new(0, 0),
            source: None,
            metadata: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::SeqCst)),
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
            metadata: None,
            syntax_object_id: SyntaxObjectId(SYNTAX_OBJECT_ID.fetch_add(1, Ordering::SeqCst)),
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
            Keyword(x) => Ok(SymbolV(x.into())),
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
            If => Ok(SymbolV("if".into())),
            Define => Ok(SymbolV("define".into())),
            Let => Ok(SymbolV("let".into())),
            TestLet => Ok(SymbolV("test-let".into())),
            // Transduce => Ok(SymbolV("transduce".into())),
            // Execute => Ok(SymbolV("execute".into())),
            Return => Ok(SymbolV("return!".into())),
            Begin => Ok(SymbolV("begin".into())),
            // Panic => Ok(SymbolV("panic!".into())),
            Lambda => Ok(SymbolV("lambda".into())),
            Quote => Ok(SymbolV("quote".into())),
            DefineSyntax => Ok(SymbolV("define-syntax".into())),
            SyntaxRules => Ok(SymbolV("syntax-rules".into())),
            Ellipses => Ok(SymbolV("...".into())),
            Struct => Ok(SymbolV("struct".into())),
            // Apply => Ok(SymbolV("apply".into())),
            Set => Ok(SymbolV("set!".into())),
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
    _intern: &'a mut HashMap<String, Rc<TokenType>>,
    quote_stack: Vec<usize>,
    shorthand_quote_stack: Vec<usize>,
    source_name: Option<Rc<PathBuf>>,
    context: Vec<ParsingContext>,
}

#[derive(Debug)]
enum ParsingContext {
    // Inside of a quote. Expressions should be parsed without being coerced into a typed variant of the AST
    Quote(usize),
    // Shortened version of a quote
    QuoteTick(usize),
    // Inside of an unquote - expressions should actually be parsed as usual
    Unquote(usize),
    // Shortened version of unquote
    UnquoteTick(usize),
    // Treat this like a normal quote
    Quasiquote(usize),
    // Shortened version of quasiquote
    QuasiquoteTick(usize),
    // expressions should parsed as normal
    UnquoteSplicing(usize),
    // Shorted version of Unquote Splicing
    UnquoteSplicingTick(usize),
}

impl<'a> Parser<'a> {
    // #[cfg(test)]
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
            _intern: intern,
            quote_stack: Vec::new(),
            shorthand_quote_stack: Vec::new(),
            source_name: None,
            context: Vec::new(),
        }
    }

    pub fn new_from_source(
        input: &'a str,
        intern: &'a mut HashMap<String, Rc<TokenType>>,
        source_name: PathBuf,
    ) -> Self {
        Parser {
            tokenizer: TokenStream::new(input, true),
            _intern: intern,
            quote_stack: Vec::new(),
            shorthand_quote_stack: Vec::new(),
            source_name: Some(Rc::from(source_name)),
            context: Vec::new(),
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
        // println!("Inside construct quote vec with: {:?}", val);

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

                            // println!("Entering context: Quote Tick in read from tokens");

                            self.context
                                .push(ParsingContext::QuoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_quote(x, token.span));
                            // self.quote_stack.pop();
                            self.shorthand_quote_stack.pop();

                            // println!(
                            //     "Exiting Context: {:?} in read from tokens",
                            //     self.context.pop()
                            // );

                            // self.context.pop();

                            let popped_value = self.context.pop();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                            }

                            current_frame.push(quote_inner?);
                        }
                        TokenType::Unquote => {
                            // println!("Entering context: Unquote");

                            self.context
                                .push(ParsingContext::UnquoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_unquote(x, token.span));

                            let popped_value = self.context.pop();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(popped, ParsingContext::UnquoteTick(_)))
                            }
                            // println!("Exiting Context: {:?}", self.context.pop());
                            current_frame.push(quote_inner?);
                        }
                        TokenType::QuasiQuote => {
                            // println!("Entering context: Quasiquote");

                            self.context
                                .push(ParsingContext::QuasiquoteTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_quasiquote(x, token.span));

                            // self.context.pop();
                            // println!(
                            //     ">>>>>>>>>>>>>>>>>>> Exiting Context: {:?}",
                            //     self.context.pop()
                            // );

                            let popped_value = self.context.pop();

                            if let Some(popped) = popped_value {
                                // println!("Popped: {:?}", popped);
                                debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                            }

                            current_frame.push(quote_inner?);
                        }
                        TokenType::UnquoteSplice => {
                            // println!("Entering context: UnquoteSplicing");

                            self.context
                                .push(ParsingContext::UnquoteSplicingTick(current_frame.len()));

                            let quote_inner = self
                                .next()
                                .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                                .map(|x| self.construct_unquote_splicing(x, token.span));

                            // self.context.pop();

                            let popped_value = self.context.pop();

                            if let Some(popped) = popped_value {
                                debug_assert!(matches!(
                                    popped,
                                    ParsingContext::UnquoteSplicingTick(_)
                                ))
                            }

                            // println!("Exiting Context: {:?}", self.context.pop());
                            current_frame.push(quote_inner?);
                        }
                        TokenType::OpenParen => {
                            stack.push(current_frame);
                            current_frame = Vec::new();
                        }
                        TokenType::CloseParen => {
                            // This is the match that we'll want to move inside the below stack.pop() match statement
                            // As we close the current context, we check what our current state is -

                            if let Some(mut prev_frame) = stack.pop() {
                                match self.context.last() {
                                    // TODO: Change this -> This should really be just Some(ParsingContext::Quote)
                                    // If we have _anything_ then we should check if we need to parse it differently. If we're at the last_quote_index,
                                    // then we can pop it off inside there.
                                    Some(ParsingContext::Quote(last_quote_index))
                                    | Some(ParsingContext::Quasiquote(last_quote_index)) => {
                                        // println!(
                                        //     "Q/QQ: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        // let last_quote_index = *last_quote_index;

                                        if stack.len() <= *last_quote_index {
                                            self.context.pop();
                                            // println!("Exiting Context: {:?}", self.context.pop());
                                        }

                                        // println!("Inside here!");
                                        // println!("Frame: {:?}", current_frame);

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                match self.context.last() {
                                                    Some(
                                                        ParsingContext::Quasiquote(_)
                                                        | ParsingContext::QuasiquoteTick(_)
                                                        | ParsingContext::Quote(_)
                                                        | ParsingContext::QuoteTick(_),
                                                    ) => prev_frame.push(ExprKind::List(
                                                        List::new(current_frame),
                                                    )),
                                                    _ => {
                                                        prev_frame.push(
                                                            ExprKind::try_from(current_frame)
                                                                .map_err(|x| {
                                                                    x.set_source(
                                                                        self.source_name.clone(),
                                                                    )
                                                                })?,
                                                        );
                                                    }
                                                }

                                                // if  {
                                                //     // println!("Exiting Context: {:?}", self.context.pop());

                                                //     println!("Converting to quote");

                                                //     prev_frame.push(
                                                //         ExprKind::try_from(current_frame).map_err(
                                                //             |x| {
                                                //                 x.set_source(
                                                //                     self.source_name.clone(),
                                                //                 )
                                                //             },
                                                //         )?,
                                                //     );
                                                // } else {
                                                //     println!("Not converting to quote");

                                                //     prev_frame.push(ExprKind::List(List::new(
                                                //         current_frame,
                                                //     )))
                                                // }
                                            }
                                            _ => {
                                                // println!("Converting to list");
                                                // println!("Context here: {:?}", self.context);
                                                prev_frame
                                                    .push(ExprKind::List(List::new(current_frame)))
                                            }
                                        }
                                    }

                                    Some(ParsingContext::QuoteTick(_))
                                    | Some(ParsingContext::QuasiquoteTick(_)) => {
                                        // if stack.len() == *last_quote_index && *last_quote_index > 1
                                        // {
                                        //     self.context.pop();
                                        // }

                                        // if stack.len() <= *last_quote_index {
                                        //     self.context.pop();
                                        // }

                                        // println!("QuoteTick: Inside here!");
                                        // println!("QuoteTick: Frame: {:?}", current_frame);

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                // println!("Converting to quote inside quote tick");
                                                prev_frame.push(
                                                    ExprKind::try_from(current_frame).map_err(
                                                        |x| x.set_source(self.source_name.clone()),
                                                    )?,
                                                );
                                            }
                                            _ => {
                                                // println!("Converting to list inside quote tick");
                                                prev_frame
                                                    .push(ExprKind::List(List::new(current_frame)))
                                            }
                                        }
                                    }

                                    // If we're in the short hand reader world, just ignore popping off the stack
                                    // but still treat it as a normal expression
                                    Some(ParsingContext::UnquoteTick(_))
                                    | Some(ParsingContext::UnquoteSplicingTick(_)) => {
                                        // println!(
                                        //     "UQ/UQS: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        // if stack.len() <= *last_quote_index {
                                        //     // println!("Exiting Context: {:?}", self.context.pop());
                                        //     self.context.pop();
                                        // }

                                        prev_frame.push(
                                            ExprKind::try_from(current_frame).map_err(|x| {
                                                x.set_source(self.source_name.clone())
                                            })?,
                                        );
                                    }

                                    Some(ParsingContext::Unquote(last_quote_index))
                                    | Some(ParsingContext::UnquoteSplicing(last_quote_index)) => {
                                        // println!(
                                        //     "UQ/UQS: Stack length: {:?}, last_quote_index: {:?}",
                                        //     stack.len(),
                                        //     last_quote_index
                                        // );

                                        if stack.len() <= *last_quote_index {
                                            // println!("Exiting Context: {:?}", self.context.pop());
                                            self.context.pop();
                                        }

                                        prev_frame.push(
                                            ExprKind::try_from(current_frame).map_err(|x| {
                                                x.set_source(self.source_name.clone())
                                            })?,
                                        );
                                    }

                                    // Else case, just go ahead and assume it is a normal frame
                                    _ => prev_frame.push(
                                        ExprKind::try_from(current_frame)
                                            .map_err(|x| x.set_source(self.source_name.clone()))?,
                                    ),
                                }

                                // Reinitialize current frame here
                                current_frame = prev_frame;
                            } else {
                                // println!("Else case: {:?}", current_frame);
                                // println!("Context: {:?}", self.context);

                                match self.context.last() {
                                    Some(ParsingContext::QuoteTick(_))
                                    | Some(ParsingContext::QuasiquoteTick(_)) => {
                                        return Ok(ExprKind::List(List::new(current_frame)));
                                    }
                                    _ => {
                                        return ExprKind::try_from(current_frame)
                                            .map_err(|x| x.set_source(self.source_name.clone()))
                                    }
                                }

                                // if self.context.last().is_some() {
                                //     println!(
                                //         "Context at this point before dumping to list: {:?}",
                                //         self.context
                                //     );
                                //     return Ok(ExprKind::List(List::new(current_frame)));
                                // } else {
                                //     println!(
                                //         "Context at this point before turning into typed ast: {:?}",
                                //         self.context
                                //     );
                                //     return ExprKind::try_from(current_frame)
                                //         .map_err(|x| x.set_source(self.source_name.clone()));
                                // }
                            }

                            /*

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

                                        // println!("Inside here");

                                        match current_frame.first() {
                                            Some(ExprKind::Atom(Atom {
                                                syn:
                                                    SyntaxObject {
                                                        ty: TokenType::Quote,
                                                        ..
                                                    },
                                            })) => {
                                                self.quote_stack.pop();

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

                            */
                        }

                        _ => {
                            if let TokenType::Quote = &token.ty {
                                // self.quote_stack.push(current_frame.len());
                                self.quote_stack.push(stack.len());
                            }

                            // Mark what context we're inside with the context stack:
                            // This only works when its the first argument - check the function call in open paren?
                            if current_frame.is_empty() {
                                match &token.ty {
                                    TokenType::Quote => {
                                        self.context.push(ParsingContext::Quote(stack.len()))
                                    }
                                    TokenType::Identifier(ident) if ident.as_str() == "unquote" => {
                                        self.context.push(ParsingContext::Unquote(stack.len()))
                                    }
                                    TokenType::Identifier(ident)
                                        if ident.as_str() == "quasiquote" =>
                                    {
                                        self.context.push(ParsingContext::Quasiquote(stack.len()))
                                    }
                                    TokenType::Identifier(ident)
                                        if ident.as_str() == "unquote-splicing" =>
                                    {
                                        self.context
                                            .push(ParsingContext::UnquoteSplicing(stack.len()))
                                    }
                                    _ => {}
                                }

                                // println!("Context on application: {:?}", self.context);
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

    // TODO -> put the
    fn next(&mut self) -> Option<Self::Item> {
        // self.shorthand_quote_stack = Vec::new();
        // self.quote_stack = Vec::new();

        self.tokenizer.next().map(|res| match res.ty {
            // Err(e) => Err(ParseError::TokenError(e)),
            TokenType::QuoteTick => {
                // See if this does the job
                self.shorthand_quote_stack.push(0);

                // println!("Entering Context: Quote Tick");
                self.context.push(ParsingContext::QuoteTick(0));

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                    .map(|x| self.construct_quote_vec(x, res.span));

                self.shorthand_quote_stack.pop();

                let popped_value = self.context.pop();

                if let Some(popped) = popped_value {
                    debug_assert!(matches!(popped, ParsingContext::QuoteTick(_)))
                }

                // println!("Exiting context: {:?}", self.context.pop());
                // println!("Result: {:?}", value);

                match value {
                    Ok(v) => ExprKind::try_from(v),
                    Err(e) => Err(e),
                }
            }
            TokenType::Unquote => {
                // println!("Entering Context: Unquote");
                self.context.push(ParsingContext::UnquoteTick(0));

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                    .map(|x| self.construct_unquote(x, res.span));

                let popped_value = self.context.pop();

                if let Some(popped) = popped_value {
                    debug_assert!(matches!(popped, ParsingContext::UnquoteTick(_)))
                }
                // println!("Exiting context: {:?}", self.context.pop());

                value
            }
            TokenType::UnquoteSplice => {
                // println!("Entering Context: Unquotesplicing");
                self.context.push(ParsingContext::UnquoteSplicingTick(0));

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                    .map(|x| self.construct_unquote_splicing(x, res.span));

                let popped_value = self.context.pop();

                if let Some(popped) = popped_value {
                    debug_assert!(matches!(popped, ParsingContext::UnquoteSplicingTick(_)))
                }

                // println!("Exiting context: {:?}", self.context.pop());

                value
            }
            TokenType::QuasiQuote => {
                // println!("Entering Context: Quasiquote");
                self.context.push(ParsingContext::QuasiquoteTick(0));

                let value = self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF(self.source_name.clone())))
                    .map(|x| self.construct_quasiquote(x, res.span));

                // println!("{:?}", self.context.pop());

                // println!("Top level Context: {:?}", self.context);

                let popped_value = self.context.pop();

                if let Some(popped) = popped_value {
                    debug_assert!(matches!(popped, ParsingContext::QuasiquoteTick(_)))
                }

                // println!("Exiting context: {:?}", self.context.pop());

                value
            }
            TokenType::OpenParen => self.read_from_tokens(),
            TokenType::CloseParen => Err(ParseError::Unexpected(
                TokenType::CloseParen,
                self.source_name.clone(),
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
    use crate::parser::ast::{Begin, Define, If, LambdaFunction, Quote, Return};

    fn atom(ident: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
            ident.to_string(),
        ))))
    }

    fn int(num: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(num))))
    }

    fn character(c: char) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            TokenType::CharacterLiteral(c),
        )))
    }

    #[test]
    fn check_quote_parsing() {
        println!("{:?}", Parser::parse("'(a b 'c)"));
    }

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
    fn parses_make_struct() {
        parses("(define make-struct (lambda (struct-name fields) (map (lambda (field) (list (quote define) (concat-symbols struct-name field) (quote (lambda (this) (vector-ref this 0))))) fields)))")
    }

    #[test]
    fn parses_quasiquote() {
        parses(r#"(quasiquote ((unquote x) xs ...)) "#);
    }

    #[test]
    fn parse_syntax_rules() {
        parses(
            r#"
            (syntax-rules (unquote unquote-splicing)
              ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
              ((quasiquote ((unquote-splicing x)))        (append (list x) '()))
              ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
              ((quasiquote (unquote x))                 x)
              ((quasiquote (x))                          '(x))
              ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
              ((quasiquote x)                           'x))
            "#,
        );
    }

    #[test]
    fn parse_define_syntax() {
        parses(
            r#"
        (define-syntax quasiquote
            (syntax-rules (unquote unquote-splicing)
              ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
              ((quasiquote ((unquote-splicing x)))        (append (list x) '()))
              ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
              ((quasiquote (unquote x))                 x)
              ((quasiquote (x))                          '(x))
              ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
              ((quasiquote x)                           'x)))
        "#,
        );
    }

    #[test]
    fn parse_quote() {
        // parses("(displayln (match (quote (lambda y z)) '(x y z)))")
        parses("(displayln (match '(lambda y z) '(x y z)))")
    }

    #[test]
    fn parse_unicode() {
        assert_parse("#\\¡", &[character('¡')]);
        assert_parse("#\\\\u{b}", &[character('\u{b}')]);
    }

    #[test]
    fn parse_more_unicode() {
        assert_parse("#\\\\u{a0}", &[character('\u{a0}')]);
    }

    #[test]
    fn parse_strange_characters() {
        assert_parse("#\\^", &[character('^')]);
    }

    #[test]
    fn parse_character_sequence() {
        assert_parse(
            "#\\¡ #\\SPACE #\\g",
            &[character('¡'), character(' '), character('g')],
        )
    }

    #[test]
    fn parse_character_sequence_inside_if() {
        assert_parse(
            "(if #\\¡ #\\SPACE #\\g)",
            &[ExprKind::If(Box::new(If::new(
                character('¡'),
                character(' '),
                character('g'),
                SyntaxObject::default(TokenType::If),
            )))],
        )
    }

    #[test]
    fn parse_close_paren_character() {
        assert_parse("#\\)", &[character(')')]);
        assert_parse("#\\]", &[character(']')])
    }

    #[test]
    fn parse_open_paren_character() {
        assert_parse("#\\(", &[character('(')])
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
    fn quote_multiple_args_should_err() {
        assert_parse_is_err("(quote a b c)");
    }

    #[test]
    fn test_let_should_err() {
        assert_parse_is_err("(let)");
        assert_parse_is_err("(let (a) 10)");
    }

    #[test]
    fn test_if_should_err() {
        assert_parse_is_err("(if)");
        assert_parse_is_err("(if 1)");
        assert_parse_is_err("(if 1 2)");
        assert_parse_is_err("(if 1 2 3 4)");
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
                character('¡'),
                ExprKind::Quote(
                    Quote::new(
                        List::new(vec![]).into(),
                        SyntaxObject::default(TokenType::Quote),
                    )
                    .into(),
                ),
                character('g'),
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
                atom("list"),
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
        assert_parse("a b +", &[atom("a"), atom("b"), atom("+")]);
    }

    #[test]
    fn test_multi_parse_complicated() {
        assert_parse(
            "a b (funcall  1 (+ 2 3.5))",
            &[
                atom("a"),
                atom("b"),
                ExprKind::List(List::new(vec![
                    atom("funcall"),
                    int(1),
                    ExprKind::List(List::new(vec![
                        atom("+"),
                        int(2),
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
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2), int(3)])),
                ExprKind::List(List::new(vec![atom("-"), int(4), int(3)])),
            ],
        );
    }

    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![atom("bar"), int(2), int(3)])),
                ])),
            ]))],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::List(List::new(vec![atom("+"), int(2), int(3)])),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![atom("bar"), int(2), int(3)])),
                ])),
            ]))],
        );
    }

    #[test]
    fn test_if() {
        assert_parse(
            "(+ 1 (if 2 3 4) (foo (+ (bar 1 1) 3) 5))",
            &[ExprKind::List(List::new(vec![
                atom("+"),
                int(1),
                ExprKind::If(Box::new(If::new(
                    int(2),
                    int(3),
                    int(4),
                    SyntaxObject::default(If),
                ))),
                ExprKind::List(List::new(vec![
                    atom("foo"),
                    ExprKind::List(List::new(vec![
                        atom("+"),
                        ExprKind::List(List::new(vec![atom("bar"), int(1), int(1)])),
                        int(3),
                    ])),
                    int(5),
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
                    int(1),
                    int(2),
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
                    int(1),
                    int(2),
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
                        int(1),
                        int(2),
                    ])),
                    int(3),
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
                        int(1),
                        int(2),
                    ])),
                    int(3),
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
                            int(1),
                            int(2),
                        ])),
                        int(3),
                    ])),
                    SyntaxObject::default(TokenType::Quote),
                ))),
                ExprKind::List(List::new(vec![
                    atom("+"),
                    int(1),
                    ExprKind::If(Box::new(If::new(
                        int(2),
                        int(3),
                        int(4),
                        SyntaxObject::default(If),
                    ))),
                    ExprKind::List(List::new(vec![
                        atom("foo"),
                        ExprKind::List(List::new(vec![
                            atom("+"),
                            ExprKind::List(List::new(vec![atom("bar"), int(1), int(1)])),
                            int(3),
                        ])),
                        int(5),
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
                    atom("applesauce"),
                    ExprKind::Quote(Box::new(Quote::new(
                        atom("one"),
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
                    atom("applesauce"),
                    ExprKind::Quote(Box::new(Quote::new(
                        atom("one"),
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
                atom("quasiquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_quasiquote_normal() {
        assert_parse(
            "(quasiquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("quasiquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_shorthand() {
        assert_parse(
            ",(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                atom("unquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_normal() {
        assert_parse(
            "(unquote (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("unquote"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_shorthand() {
        assert_parse(
            ",@(+ 1 2)",
            &[ExprKind::List(List::new(vec![
                atom("unquote-splicing"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_unquote_splicing_normal() {
        assert_parse(
            "(unquote-splicing (+ 1 2))",
            &[ExprKind::List(List::new(vec![
                atom("unquote-splicing"),
                ExprKind::List(List::new(vec![atom("+"), int(1), int(2)])),
            ]))],
        )
    }

    #[test]
    fn test_define_simple() {
        assert_parse(
            "(define a 10)",
            &[ExprKind::Define(Box::new(Define::new(
                atom("a"),
                int(10),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_func_simple() {
        assert_parse(
            "(define (foo x) (+ x 10))",
            &[ExprKind::Define(Box::new(Define::new(
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x")],
                    ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
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
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x"), atom("y"), atom("z")],
                    ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
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
                atom("foo"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("x"), atom("y"), atom("z")],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::List(List::new(vec![atom("+"), atom("x"), int(10)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("y"), int(20)])),
                            ExprKind::List(List::new(vec![atom("+"), atom("z"), int(30)])),
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
                atom("test"),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![],
                    ExprKind::Begin(Begin::new(
                        vec![
                            ExprKind::Define(Box::new(Define::new(
                                atom("foo"),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![atom("bar")])),
                                    SyntaxObject::default(TokenType::Lambda),
                                ))),
                                SyntaxObject::default(TokenType::Define),
                            ))),
                            ExprKind::Define(Box::new(Define::new(
                                atom("bar"),
                                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    vec![],
                                    ExprKind::List(List::new(vec![atom("foo")])),
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
    fn test_return_normal() {
        assert_parse(
            "(return! 10)",
            &[ExprKind::Return(Box::new(Return::new(
                int(10),
                SyntaxObject::default(TokenType::Return),
            )))],
        )
    }

    #[test]
    fn test_begin() {
        assert_parse(
            "(begin 1 2 3)",
            &[ExprKind::Begin(Begin::new(
                vec![int(1), int(2), int(3)],
                SyntaxObject::default(TokenType::Begin),
            ))],
        )
    }

    #[test]
    fn test_lambda_function() {
        assert_parse(
            "(lambda (x) 10)",
            &[ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                vec![atom("x")],
                int(10),
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
                    vec![atom("a")],
                    ExprKind::List(List::new(vec![atom("+"), atom("a"), int(20)])),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                int(10),
            ]))],
        );
    }

    #[test]
    fn test_let() {
        assert_parse(
            "(let ([a 10]) (+ a 20))",
            &[ExprKind::List(List::new(vec![
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("a")],
                    ExprKind::List(List::new(vec![atom("+"), atom("a"), int(20)])),
                    SyntaxObject::default(TokenType::Let),
                ))),
                int(10),
            ]))],
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
                ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
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
                    atom("list"),
                    ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                    ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
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
                ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
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
                    atom("list"),
                    ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                    ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
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
                atom("list"),
                ExprKind::If(Box::new(If::new(
                    ExprKind::List(List::new(vec![atom("null?"), atom("contents")])),
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
                        atom("list"),
                        ExprKind::List(List::new(vec![atom("car"), atom("contents")])),
                        ExprKind::List(List::new(vec![atom("cdr"), atom("contents")])),
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
                ExprKind::List(List::new(vec![atom("datum->syntax"), atom("var")])),
                ExprKind::List(List::new(vec![atom("car"), atom("ret-value")])),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }

    #[test]
    fn test_define_with_datum_syntax_function_name() {
        assert_parse(
            "(define ((datum->syntax var) arg) 10)",
            &[ExprKind::Define(Box::new(Define::new(
                ExprKind::List(List::new(vec![atom("datum->syntax"), atom("var")])),
                ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                    vec![atom("arg")],
                    int(10),
                    SyntaxObject::default(TokenType::Lambda),
                ))),
                SyntaxObject::default(TokenType::Define),
            )))],
        )
    }
}
