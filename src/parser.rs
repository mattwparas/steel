use std::iter::Peekable;
use std::result;
use std::str;

use crate::lexer::{Token, TokenError, Tokenizer};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Token),
    ListVal(Vec<Expr>),
    // Cond(Vec<Expr>),
}

impl Expr {
    fn push_on_end(&mut self, expr: Expr) -> Result<()> {
        match self {
            Expr::Atom(_) => Err(ParseError::ExtendingAtom(expr)),
            Expr::ListVal(v) => {
                v.push(expr);
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    ScanError(TokenError),
    Unexpected(Token),
    UnexpectedEOF,
    ExtendingAtom(Expr),
}

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

    fn read_from_tokens(&mut self) -> Result<Expr> {
        if let None = self.tokenizer.peek() {
            return Err(ParseError::UnexpectedEOF);
        }

        let mut exprs: Vec<Expr> = Vec::new();
        let mut open_paren_count = 1; // implicit open paren here
        let mut close_paren_count = 0;

        while let Some(Ok(t)) = self.tokenizer.next() {
            match t {
                Token::OpenParen => {
                    let list_val = Expr::ListVal(Vec::new());
                    exprs.push(list_val);
                    open_paren_count += 1;
                }
                Token::CloseParen => {
                    close_paren_count += 1;
                    if open_paren_count == close_paren_count {
                        break;
                    }
                    continue;
                }
                t => {
                    let last_val = exprs.last_mut();
                    let atom = Expr::Atom(t);
                    match last_val {
                        None => {
                            exprs.push(atom);
                        }
                        Some(v) => {
                            v.push_on_end(atom)?;
                        }
                    }
                }
            }
        }

        // TODO fix this here
        if open_paren_count != close_paren_count {
            return Err(ParseError::Unexpected(Token::OpenParen));
        } else {
            Ok(Expr::ListVal(exprs))
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        return match &self.tokenizer.peek() {
            None => return None,
            Some(Err(_)) => match self.tokenizer.next() {
                Some(Err(e2)) => return Some(Err(ParseError::ScanError(e2))),
                _ => return None,
            },
            Some(Ok(_)) => match self.tokenizer.next() {
                Some(Ok(Token::BooleanLiteral(b))) => {
                    Some(Ok(Expr::Atom(Token::BooleanLiteral(b))))
                }
                Some(Ok(Token::NumberLiteral(n))) => Some(Ok(Expr::Atom(Token::NumberLiteral(n)))),
                Some(Ok(Token::StringLiteral(s))) => Some(Ok(Expr::Atom(Token::StringLiteral(s)))),
                Some(Ok(Token::Identifier(s))) => Some(Ok(Expr::Atom(Token::Identifier(s)))),
                Some(Ok(Token::OpenParen)) => Some(self.read_from_tokens()),
                Some(Ok(t)) => Some(Err(ParseError::Unexpected(t))),
                _ => None,
            },
        };
    }
}
