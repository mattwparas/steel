pub mod lexer;
pub mod tokens;
use lexer::Tokenizer;
use tokens::{Token, TokenError};

use std::fmt;
use std::iter::Peekable;
use std::result;
use std::str;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Token),
    ListVal(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(t) => write!(f, "{}", t.to_string()),
            Expr::ListVal(t) => {
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
    #[error("Parse: Error reading tokens")]
    TokenError(#[from] TokenError),
    #[error("Parse: Unexpected token, {0:?}")]
    Unexpected(Token),
    #[error("Parse: Unexpected EOF")]
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

pub type Result<T> = result::Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input).peekable(),
        }
    }

    // Jason's attempt
    fn read_from_tokens(&mut self) -> Result<Expr> {
        let mut stack: Vec<Vec<Expr>> = Vec::new();
        let mut current_frame: Vec<Expr> = Vec::new();

        loop {
            match self.tokenizer.next() {
                Some(Ok(t)) => match t {
                    Token::QuoteTick => {
                        let quote_inner = self
                            .next()
                            .unwrap_or(Err(ParseError::UnexpectedEOF))
                            .map(construct_quote);
                        match quote_inner {
                            Ok(expr) => current_frame.push(expr),
                            Err(e) => return Err(e),
                        }
                    }
                    Token::OpenParen => {
                        stack.push(current_frame);
                        current_frame = Vec::new();
                    }
                    Token::CloseParen => {
                        if let Some(mut prev_frame) = stack.pop() {
                            prev_frame.push(Expr::ListVal(current_frame));
                            current_frame = prev_frame;
                        } else {
                            return Ok(Expr::ListVal(current_frame));
                        }
                    }
                    tok => {
                        current_frame.push(Expr::Atom(tok));
                    }
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
                    .map(construct_quote),
                Token::OpenParen => self.read_from_tokens(),
                Token::CloseParen => Err(ParseError::Unexpected(Token::CloseParen)),
                tok => Ok(Expr::Atom(tok)),
            },
        })
    }
}

fn construct_quote(val: Expr) -> Expr {
    Expr::ListVal(vec![
        Expr::Atom(Token::Identifier("quote".to_string())),
        val,
    ])
}

#[cfg(test)]
mod parser_tests {
    use super::Expr::*;
    use super::Token::*;
    use super::*;

    #[test]
    fn test_empty() {
        assert_parse("", &[]);
        assert_parse("()", &[ListVal(vec![])]);
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
                ListVal(vec![
                    Atom(Identifier("lambda".to_string())),
                    Atom(NumberLiteral(1.0)),
                    ListVal(vec![
                        Atom(Identifier("+".to_string())),
                        Atom(NumberLiteral(2.0)),
                        Atom(NumberLiteral(3.5)),
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
                ListVal(vec![
                    Atom(Identifier("+".to_string())),
                    Atom(NumberLiteral(1.0)),
                    Atom(NumberLiteral(2.0)),
                    Atom(NumberLiteral(3.0)),
                ]),
                ListVal(vec![
                    Atom(Identifier("-".to_string())),
                    Atom(NumberLiteral(4.0)),
                    Atom(NumberLiteral(3.0)),
                ]),
            ],
        );
    }
    #[test]
    fn test_parse_nested() {
        assert_parse(
            "(+ 1 (foo (bar 2 3)))",
            &[ListVal(vec![
                Atom(Identifier("+".to_string())),
                Atom(NumberLiteral(1.0)),
                ListVal(vec![
                    Atom(Identifier("foo".to_string())),
                    ListVal(vec![
                        Atom(Identifier("bar".to_owned())),
                        Atom(NumberLiteral(2.0)),
                        Atom(NumberLiteral(3.0)),
                    ]),
                ]),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (bar 2 3)))",
            &[ListVal(vec![
                Atom(Identifier("+".to_string())),
                Atom(NumberLiteral(1.0)),
                ListVal(vec![
                    Atom(Identifier("+".to_string())),
                    Atom(NumberLiteral(2.0)),
                    Atom(NumberLiteral(3.0)),
                ]),
                ListVal(vec![
                    Atom(Identifier("foo".to_string())),
                    ListVal(vec![
                        Atom(Identifier("bar".to_owned())),
                        Atom(NumberLiteral(2.0)),
                        Atom(NumberLiteral(3.0)),
                    ]),
                ]),
            ])],
        );
        assert_parse(
            "(+ 1 (+ 2 3) (foo (+ (bar 1 1) 3) 5))",
            &[ListVal(vec![
                Atom(Identifier("+".to_string())),
                Atom(NumberLiteral(1.0)),
                ListVal(vec![
                    Atom(Identifier("+".to_string())),
                    Atom(NumberLiteral(2.0)),
                    Atom(NumberLiteral(3.0)),
                ]),
                ListVal(vec![
                    Atom(Identifier("foo".to_string())),
                    ListVal(vec![
                        Atom(Identifier("+".to_string())),
                        ListVal(vec![
                            Atom(Identifier("bar".to_string())),
                            Atom(NumberLiteral(1.0)),
                            Atom(NumberLiteral(1.0)),
                        ]),
                        Atom(NumberLiteral(3.0)),
                    ]),
                    Atom(NumberLiteral(5.0)),
                ]),
            ])],
        );
    }
    #[test]
    fn test_parse_specials() {
        assert_parse(
            "(define (foo a b) (+ (- a 1) b))",
            &[ListVal(vec![
                Atom(Identifier("define".to_string())),
                ListVal(vec![
                    Atom(Identifier("foo".to_string())),
                    Atom(Identifier("a".to_string())),
                    Atom(Identifier("b".to_string())),
                ]),
                ListVal(vec![
                    Atom(Identifier("+".to_string())),
                    ListVal(vec![
                        Atom(Identifier("-".to_string())),
                        Atom(Identifier("a".to_string())),
                        Atom(NumberLiteral(1.0)),
                    ]),
                    Atom(Identifier("b".to_string())),
                ]),
            ])],
        );

        assert_parse(
            "(if   #t     1 2)",
            &[ListVal(vec![
                Atom(Identifier("if".to_string())),
                Atom(BooleanLiteral(true)),
                Atom(NumberLiteral(1.0)),
                Atom(NumberLiteral(2.0)),
            ])],
        );
        assert_parse(
            "(lambda (a b) (+ a b)) (- 1 2) (\"dumpsterfire\")",
            &[
                ListVal(vec![
                    Atom(Identifier("lambda".to_string())),
                    ListVal(vec![
                        Atom(Identifier("a".to_string())),
                        Atom(Identifier("b".to_string())),
                    ]),
                    ListVal(vec![
                        Atom(Identifier("+".to_string())),
                        Atom(Identifier("a".to_string())),
                        Atom(Identifier("b".to_string())),
                    ]),
                ]),
                ListVal(vec![
                    Atom(Identifier("-".to_string())),
                    Atom(NumberLiteral(1.0)),
                    Atom(NumberLiteral(2.0)),
                ]),
                ListVal(vec![Atom(StringLiteral("dumpsterfire".to_string()))]),
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
        let a: Result<Vec<Expr>> = Parser::new(s).collect();
        assert_eq!(a, Err(err));
    }

    fn assert_parse(s: &str, result: &[Expr]) {
        let a: Result<Vec<Expr>> = Parser::new(s).collect();
        let a = a.unwrap();
        assert_eq!(a, result);
    }
}
