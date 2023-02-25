use crate::tokens::{Token, TokenType};
use logos::{Lexer, Logos};
use std::fmt;
use std::iter::Iterator;

use super::parser::SourceId;

#[derive(Clone)]
pub struct TokenStream<'a> {
    lexer: Lexer<'a, TokenType<&'a str>>,
    skip_comments: bool,
    source_id: Option<SourceId>, // skip_doc_comments: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, source_id: Option<SourceId>) -> Self {
        Self {
            lexer: TokenType::lexer(input),
            skip_comments,
            source_id, // skip_doc_comments,
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().and_then(|token| {
            let token = Token::new(token, self.lexer.slice(), self.lexer.span(), self.source_id);
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
    use crate::span::Span;
    use crate::tokens::TokenType::*;

    #[test]
    fn test_chars() {
        let mut s = TokenStream::new("#\\a #\\b #\\λ", true, None);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('a'),
                source: "#\\a",
                span: Span::new(0, 3, None)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('b'),
                source: "#\\b",
                span: Span::new(4, 7, None)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('λ'),
                source: "#\\λ",
                span: Span::new(8, 12, None)
            })
        );
    }

    #[test]
    fn test_unexpected_char() {
        let mut s = TokenStream::new("($)", true, None);
        assert_eq!(
            s.next(),
            Some(Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(0, 1, None)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: Error,
                source: "$",
                span: Span::new(1, 2, None)
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(2, 3, None)
            })
        );
    }

    #[test]
    fn test_words() {
        let mut s = TokenStream::new("foo FOO _123_ Nil #f #t", true, None);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("foo"),
                source: "foo",
                span: Span::new(0, 3, None)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("FOO"),
                source: "FOO",
                span: Span::new(4, 7, None)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("_123_"),
                source: "_123_",
                span: Span::new(8, 13, None)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: Identifier("Nil"),
                source: "Nil",
                span: Span::new(14, 17, None)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(false),
                source: "#f",
                span: Span::new(18, 20, None)
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(true),
                source: "#t",
                span: Span::new(21, 23, None)
            })
        );

        assert_eq!(s.next(), None);
    }

    #[test]
    fn test_number() {
        let mut s = TokenStream::new("0 -0 -1.2 +2.3 999 1.", true, None);
        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(0),
                source: "0",
                span: Span::new(0, 1, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(0),
                source: "-0",
                span: Span::new(2, 4, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(-1.2),
                source: "-1.2",
                span: Span::new(5, 9, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(2.3),
                source: "+2.3",
                span: Span::new(10, 14, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(999),
                source: "999",
                span: Span::new(15, 18, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: NumberLiteral(1.0),
                source: "1.",
                span: Span::new(19, 21, None),
            })
        );

        assert_eq!(s.next(), None);
    }

    #[test]
    fn test_string() {
        let mut s = TokenStream::new(r#" "" "Foo bar" "\"\\" "#, true, None);

        assert_eq!(
            s.next(),
            Some(Token {
                ty: StringLiteral("".to_owned()),
                source: "\"\"",
                span: Span::new(1, 3, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: StringLiteral("Foo bar".to_owned()),
                source: "\"Foo bar\"",
                span: Span::new(4, 13, None),
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
                span: Span::new(14, 17, None),
            })
        );

        // assert_eq!(s.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut s = TokenStream::new(";!/usr/bin/gate\n   ; foo\n", true, None);
        assert_eq!(s.next(), None);
    }

    #[test]
    fn scheme_statement() {
        let s = TokenStream::new("(apples (function a b) (+ a b))", true, None);
        let res: Vec<Token> = s.collect();

        let expected: Vec<Token> = vec![
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(0, 1, None),
            },
            Token {
                ty: Identifier("apples"),
                source: "apples",
                span: Span::new(1, 7, None),
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(8, 9, None),
            },
            Token {
                ty: Identifier("function"),
                source: "function",
                span: Span::new(9, 17, None),
            },
            Token {
                ty: Identifier("a"),
                source: "a",
                span: Span::new(18, 19, None),
            },
            Token {
                ty: Identifier("b"),
                source: "b",
                span: Span::new(20, 21, None),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(21, 22, None),
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(23, 24, None),
            },
            Token {
                ty: Identifier("+"),
                source: "+",
                span: Span::new(24, 25, None),
            },
            Token {
                ty: Identifier("a"),
                source: "a",
                span: Span::new(26, 27, None),
            },
            Token {
                ty: Identifier("b"),
                source: "b",
                span: Span::new(28, 29, None),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(29, 30, None),
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(30, 31, None),
            },
        ];

        assert_eq!(res, expected);
    }
}
