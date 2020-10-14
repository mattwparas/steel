// pub mod parser;
// pub mod tokens;

use crate::parser::tokens::{Token, TokenError, TokenType};
use std::iter::Iterator;
use std::result;
// use std::str::Chars;

// use core::ops;
use std::fmt;
// use thiserror::Error;
// use TokenType::*;

use logos::{Lexer, Logos};

// use crate::parser::span::Span;

// use crate::parser::tokens::TokenType;

pub type Result<T> = result::Result<T, TokenError>;

#[derive(Clone)]
pub struct TokenStream<'a> {
    lexer: Lexer<'a, TokenType>,
    skip_comments: bool,
    // skip_doc_comments: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool) -> Self {
        Self {
            lexer: TokenType::lexer(input),
            skip_comments,
            // skip_doc_comments,
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().and_then(|token| {
            let token = Token::new(token, self.lexer.slice(), self.lexer.span());
            match token.ty {
                // TokenType::Space => self.next(),
                TokenType::Comment if self.skip_comments => self.next(),
                // TokenType::DocComment if self.skip_doc_comments => self.next(),
                _ => Some(token),
            }
        })
    }
}

impl fmt::Debug for TokenStream<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tokens = self.clone().collect::<Vec<Token<'_>>>();

        f.debug_struct("TokenStream")
            .field("lexer", &tokens)
            .field("skip_comments", &self.skip_comments)
            // .field("skip_doc_comments", &self.skip_doc_comments)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span::Span;
    use crate::parser::tokens::TokenType::*;

    #[test]
    fn test_chars() {
        let mut s = TokenStream::new("#\\a #\\b #\\λ", true);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('a'),
                source: "#\\a",
                span: Span::new(0, 3)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('b'),
                source: "#\\b",
                span: Span::new(4, 7)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('λ'),
                source: "#\\λ",
                span: Span::new(8, 12)
            })
        );
    }

    #[test]
    fn test_unexpected_char() {
        let mut s = TokenStream::new("($)", true);
        assert_eq!(
            s.next(),
            Some(Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(0, 1)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: Error,
                source: "$",
                span: Span::new(1, 2)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(2, 3)
            })
        );
    }

    #[test]
    fn test_words() {
        let mut s = TokenStream::new("foo FOO _123_ Nil #f #t", true);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("foo".to_owned()),
                source: "foo",
                span: Span::new(0, 3)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("FOO".to_owned()),
                source: "FOO",
                span: Span::new(4, 7)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("_123_".to_owned()),
                source: "_123_",
                span: Span::new(8, 13)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("Nil".to_owned()),
                source: "Nil",
                span: Span::new(14, 17)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(false),
                source: "#f",
                span: Span::new(18, 20)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(true),
                source: "#t",
                span: Span::new(21, 23)
            })
        );

        assert_eq!(s.next(), None);
    }

    #[test]
    fn test_number() {
        let mut s = TokenStream::new("0 -0 -1.2 +2.3 999 1.", true);
        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(0),
                source: "0",
                span: Span::new(0, 1),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(0),
                source: "-0",
                span: Span::new(2, 4),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(-1.2),
                source: "-1.2",
                span: Span::new(5, 9),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(2.3),
                source: "+2.3",
                span: Span::new(10, 14),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(999),
                source: "999",
                span: Span::new(15, 18),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(1.0),
                source: "1.",
                span: Span::new(19, 21),
            })
        );

        assert_eq!(s.next(), None);
    }

    #[test]
    fn test_string() {
        let mut s = TokenStream::new(r#" "" "Foo bar" "\"\\" "#, true);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: StringLiteral("".to_owned()),
                source: "\"\"",
                span: Span::new(1, 3),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: StringLiteral("Foo bar".to_owned()),
                source: "\"Foo bar\"",
                span: Span::new(4, 13),
            })
        );

        // TODO come check out this test
        // assert_eq!(
        //     s.next(),
        //     Some(Token {
        //         ty: StringLiteral(r#""\"\\""#.to_owned()),
        //         source: r#""\"\\""#,
        //         span: Span::new(14, 20),
        //     })
        // );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: Error,
                source: "\"\\\"",
                span: Span::new(14, 17),
            })
        );

        // assert_eq!(s.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut s = TokenStream::new(";!/usr/bin/gate\n   ; foo\n", true);
        assert_eq!(s.next(), None);
    }

    #[test]
    fn scheme_statement() {
        let s = TokenStream::new("(apples (function a b) (+ a b))", true);
        let res: Vec<Token> = s.collect();

        let expected: Vec<Token> = vec![
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(0, 1),
            },
            Token {
                ty: Identifier("apples".to_string()),
                source: "apples",
                span: Span::new(1, 7),
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(8, 9),
            },
            Token {
                ty: Identifier("function".to_string()),
                source: "function",
                span: Span::new(9, 17),
            },
            Token {
                ty: Identifier("a".to_string()),
                source: "a",
                span: Span::new(18, 19),
            },
            Token {
                ty: Identifier("b".to_string()),
                source: "b",
                span: Span::new(20, 21),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(21, 22),
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(23, 24),
            },
            Token {
                ty: Identifier("+".to_string()),
                source: "+",
                span: Span::new(24, 25),
            },
            Token {
                ty: Identifier("a".to_string()),
                source: "a",
                span: Span::new(26, 27),
            },
            Token {
                ty: Identifier("b".to_string()),
                source: "b",
                span: Span::new(28, 29),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(29, 30),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(30, 31),
            },
        ];

        assert_eq!(res, expected);
    }
}
