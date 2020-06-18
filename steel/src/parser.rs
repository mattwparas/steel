pub mod lexer;
pub mod tokens;
use lexer::Tokenizer;
use tokens::{Token, TokenError};

use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::rc::Rc;
use std::result;
use std::str;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Token),
    VectorVal(Vec<Rc<Expr>>),
}

impl Expr {
    pub fn vector_val_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&[Rc<Expr>], E> {
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
            Self::Atom(Token::Identifier(s)) => Ok(s),
            _ => Err(err()),
        }
    }

    // pub fn
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(t) => write!(f, "{}", t.to_string()),
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
    Unexpected(Token),
    #[error("Parse: Unexpected EOF")]
    UnexpectedEOF,
}

// pub struct PeekTokenizer<'a> {
//     tokenizer:
// }

// pub trait LineNumber {
//     fn line_number(&self) -> usize;
// }

// pub struct PeekTokenizer<'a> {
//     line_number:
// }

// impl<'a> LineNumber for Peekable<Tokenizer<'a>> {
//     fn line_number(&self) -> usize {
//         unimplemented!()
//     }
// }

// impl Peekable<Tokenizer<'a>> {
//     pub fn line_number(&self) -> usize {
//         unimplemented!()
//     }
// }

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
    intern: &'a mut HashMap<String, Rc<Expr>>,
    // span: String,
}

pub type Result<T> = result::Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, intern: &'a mut HashMap<String, Rc<Expr>>) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input).peekable(),
            intern,
        }
    }

    fn construct_quote(&mut self, val: Expr) -> Expr {
        let q = match self.intern.get("quote") {
            Some(rc) => Rc::clone(rc),
            None => {
                let val = Rc::new(Expr::Atom(Token::Identifier("quote".to_string())));
                self.intern.insert("quote".to_string(), Rc::clone(&val));
                val
            }
        };

        Expr::VectorVal(vec![q, Rc::new(val)])
    }

    // Jason's attempt
    fn read_from_tokens(&mut self) -> Result<Expr> {
        let mut stack: Vec<Vec<Rc<Expr>>> = Vec::new();
        let mut current_frame: Vec<Rc<Expr>> = Vec::new();

        loop {
            match self.tokenizer.next() {
                Some(Ok(t)) => match t {
                    Token::QuoteTick => {
                        let quote_inner = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF))
                            .map(|x| self.construct_quote(x));
                        match quote_inner {
                            Ok(expr) => current_frame.push(Rc::new(expr)),
                            Err(e) => return Err(e),
                        }
                    }
                    Token::OpenParen => {
                        stack.push(current_frame);
                        current_frame = Vec::new();
                    }
                    Token::CloseParen => {
                        if let Some(mut prev_frame) = stack.pop() {
                            prev_frame.push(Rc::new(Expr::VectorVal(current_frame)));
                            current_frame = prev_frame;
                        } else {
                            return Ok(Expr::VectorVal(current_frame));
                        }
                    }
                    tok => match &tok {
                        Token::Identifier(s) => match self.intern.get(s) {
                            Some(rc) => current_frame.push(Rc::clone(rc)),
                            None => {
                                let val = Rc::new(Expr::Atom(tok.clone()));
                                current_frame.push(val.clone());
                                self.intern.insert(s.to_string(), val);
                            }
                        },
                        _ => current_frame.push(Rc::new(Expr::Atom(tok))),
                    },
                },
                Some(Err(e)) => return Err(ParseError::TokenError(e)),
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokenizer.next().map(|res| match res {
            Err(e) => Err(ParseError::TokenError(e)),
            Ok(tok) => match tok {
                Token::QuoteTick => self
                    .next()
                    .unwrap_or(Err(ParseError::UnexpectedEOF))
                    .map(|x| self.construct_quote(x)),
                Token::OpenParen => self.read_from_tokens(),
                Token::CloseParen => Err(ParseError::Unexpected(Token::CloseParen)),
                tok => Ok(Expr::Atom(tok)),
            },
        })
    }
}

#[cfg(test)]
mod parser_tests {
    use super::Expr::*;
    use super::Token::*;
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
                Atom(Identifier("a".to_string())),
                Atom(Identifier("b".to_string())),
                Atom(Identifier("+".to_string())),
            ],
        );
        assert_parse(
            "a b (lambda  1 (+ 2 3.5))",
            &[
                Atom(Identifier("a".to_string())),
                Atom(Identifier("b".to_string())),
                VectorVal(vec![
                    Rc::new(Atom(Identifier("lambda".to_string()))),
                    Rc::new(Atom(IntegerLiteral(1))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("+".to_string()))),
                        Rc::new(Atom(IntegerLiteral(2))),
                        Rc::new(Atom(NumberLiteral(3.5))),
                    ])),
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
                    Rc::new(Atom(Identifier("+".to_string()))),
                    Rc::new(Atom(IntegerLiteral(1))),
                    Rc::new(Atom(IntegerLiteral(2))),
                    Rc::new(Atom(IntegerLiteral(3))),
                ]),
                VectorVal(vec![
                    Rc::new(Atom(Identifier("-".to_string()))),
                    Rc::new(Atom(IntegerLiteral(4))),
                    Rc::new(Atom(IntegerLiteral(3))),
                ]),
            ],
        );
    }
    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[VectorVal(vec![
                Rc::new(Atom(Identifier("+".to_string()))),
                Rc::new(Atom(IntegerLiteral(1))),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("foo".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("bar".to_owned()))),
                        Rc::new(Atom(IntegerLiteral(2))),
                        Rc::new(Atom(IntegerLiteral(3))),
                    ])),
                ])),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[VectorVal(vec![
                Rc::new(Atom(Identifier("+".to_string()))),
                Rc::new(Atom(IntegerLiteral(1))),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("+".to_string()))),
                    Rc::new(Atom(IntegerLiteral(2))),
                    Rc::new(Atom(IntegerLiteral(3))),
                ])),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("foo".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("bar".to_owned()))),
                        Rc::new(Atom(IntegerLiteral(2))),
                        Rc::new(Atom(IntegerLiteral(3))),
                    ])),
                ])),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (+ (bar 1 1) 3) 5))",
            &[VectorVal(vec![
                Rc::new(Atom(Identifier("+".to_string()))),
                Rc::new(Atom(IntegerLiteral(1))),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("+".to_string()))),
                    Rc::new(Atom(IntegerLiteral(2))),
                    Rc::new(Atom(IntegerLiteral(3))),
                ])),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("foo".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("+".to_string()))),
                        Rc::new(VectorVal(vec![
                            Rc::new(Atom(Identifier("bar".to_string()))),
                            Rc::new(Atom(IntegerLiteral(1))),
                            Rc::new(Atom(IntegerLiteral(1))),
                        ])),
                        Rc::new(Atom(IntegerLiteral(3))),
                    ])),
                    Rc::new(Atom(IntegerLiteral(5))),
                ])),
            ])],
        );
    }
    #[test]
    fn test_parse_specials() {
        assert_parse(
            "(define (foo a b) (+ (- a 1) b))",
            &[VectorVal(vec![
                Rc::new(Atom(Identifier("define".to_string()))),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("foo".to_string()))),
                    Rc::new(Atom(Identifier("a".to_string()))),
                    Rc::new(Atom(Identifier("b".to_string()))),
                ])),
                Rc::new(VectorVal(vec![
                    Rc::new(Atom(Identifier("+".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("-".to_string()))),
                        Rc::new(Atom(Identifier("a".to_string()))),
                        Rc::new(Atom(IntegerLiteral(1))),
                    ])),
                    Rc::new(Atom(Identifier("b".to_string()))),
                ])),
            ])],
        );

        assert_parse(
            "(if   #t     1 2)",
            &[VectorVal(vec![
                Rc::new(Atom(Identifier("if".to_string()))),
                Rc::new(Atom(BooleanLiteral(true))),
                Rc::new(Atom(IntegerLiteral(1))),
                Rc::new(Atom(IntegerLiteral(2))),
            ])],
        );
        assert_parse(
            "(lambda (a b) (+ a b)) (- 1 2) (\"dumpsterfire\")",
            &[
                VectorVal(vec![
                    Rc::new(Atom(Identifier("lambda".to_string()))),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("a".to_string()))),
                        Rc::new(Atom(Identifier("b".to_string()))),
                    ])),
                    Rc::new(VectorVal(vec![
                        Rc::new(Atom(Identifier("+".to_string()))),
                        Rc::new(Atom(Identifier("a".to_string()))),
                        Rc::new(Atom(Identifier("b".to_string()))),
                    ])),
                ]),
                VectorVal(vec![
                    Rc::new(Atom(Identifier("-".to_string()))),
                    Rc::new(Atom(IntegerLiteral(1))),
                    Rc::new(Atom(IntegerLiteral(2))),
                ]),
                VectorVal(vec![Rc::new(Atom(StringLiteral(
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
        assert_parse_err("())", ParseError::Unexpected(Token::CloseParen));
        assert_parse_err("() ((((", ParseError::UnexpectedEOF);
        assert_parse_err("')", ParseError::Unexpected(Token::CloseParen));
        assert_parse_err("(')", ParseError::Unexpected(Token::CloseParen));
        assert_parse_err("('", ParseError::UnexpectedEOF);
    }

    fn assert_parse_err(s: &str, err: ParseError) {
        let mut cache: HashMap<String, Rc<Expr>> = HashMap::new();
        let a: Result<Vec<Expr>> = Parser::new(s, &mut cache).collect();
        assert_eq!(a, Err(err));
    }

    fn assert_parse(s: &str, result: &[Expr]) {
        let mut cache: HashMap<String, Rc<Expr>> = HashMap::new();
        let a: Result<Vec<Expr>> = Parser::new(s, &mut cache).collect();
        let a = a.unwrap();
        assert_eq!(a, result);
    }
}
