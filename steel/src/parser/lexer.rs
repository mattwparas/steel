// pub mod parser;
// pub mod tokens;

use crate::parser::tokens::{Token, TokenError, TokenType};
use std::iter::{Iterator, Peekable};
use std::result;
use std::str::Chars;

use core::ops;
use std::fmt;
use thiserror::Error;
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

// #[derive(Debug)]
// pub struct Tokenizer<'a> {
//     input: Peekable<Chars<'a>>,
//     line_number: usize,
// }

// enum Sign {
//     Pos,
//     Neg,
// }

// // pub trait LineNumber {
// //     fn line_number(&self) -> usize;
// // }

// impl<'a> Tokenizer<'a> {
//     pub fn new(input: &'a str) -> Self {
//         Tokenizer {
//             input: input.chars().peekable(),
//             line_number: 1,
//         }
//     }

//     pub fn line_number(&self) -> usize {
//         self.line_number
//     }

//     fn consume_whitespace(&mut self) {
//         while let Some(&c) = self.input.peek() {
//             if c.is_whitespace() {
//                 self.input.next();
//             } else {
//                 break;
//             }
//         }
//     }

//     fn consume_whitespace_and_comments_until_next_input(&mut self) {
//         while let Some(&c) = self.input.peek() {
//             match c {
//                 c if c.is_whitespace() => {
//                     if c == '\n' {
//                         self.line_number += 1;
//                     }
//                     self.input.next();
//                 }
//                 ';' => self.read_rest_of_line(),
//                 _ => break,
//             };
//         }

//         self.consume_whitespace()
//     }

//     fn read_rest_of_line(&mut self) {
//         while let Some(c) = self.input.next() {
//             if c == '\n' {
//                 self.line_number += 1;
//                 break;
//             }
//         }
//     }

//     fn read_word(&mut self) -> Token {
//         let mut word = String::new();
//         while let Some(&c) = self.input.peek() {
//             match c {
//                 '(' | '[' | '{' | ')' | ']' | '}' => break,
//                 c if c.is_whitespace() => break,
//                 _ => {
//                     self.input.next();
//                     word.push(c);
//                 }
//             };
//         }

//         Token::Identifier(word)
//     }

//     fn read_word_with_starting_hyphen(&mut self) -> Token {
//         let mut word = "-".to_string();
//         while let Some(&c) = self.input.peek() {
//             match c {
//                 '(' | '[' | '{' | ')' | ']' | '}' => break,
//                 c if c.is_whitespace() => break,
//                 _ => {
//                     self.input.next();
//                     word.push(c);
//                 }
//             };
//         }

//         Token::Identifier(word)
//     }

//     fn read_hash_value(&mut self) -> Result<Token> {
//         let mut word = String::new();
//         while let Some(&c) = self.input.peek() {
//             match c {
//                 '(' | '[' | '{' | ')' | ']' | '}' => break,
//                 c if c.is_whitespace() => break,
//                 c if c == '#' => return Err(TokenError::UnexpectedChar('#', self.line_number)),
//                 _ => {
//                     self.input.next();
//                     word.push(c);
//                 }
//             };
//         }

//         match word.as_ref() {
//             "t" | "true" => Ok(Token::BooleanLiteral(true)),
//             "f" | "false" => Ok(Token::BooleanLiteral(false)),
//             character if character.starts_with('\\') => match word.len() {
//                 2 | 3 | 4 => {
//                     let c = word
//                         .chars()
//                         .last()
//                         .ok_or(TokenError::InvalidCharacter(self.line_number))?;
//                     Ok(Token::CharacterLiteral(c))
//                 }
//                 _ => Err(TokenError::InvalidCharacter(self.line_number)),
//             },
//             _ => Ok(Token::Identifier(word)), // TODO
//                                               // _ => Err(TokenError::UnexpectedChar(#))
//         }
//     }

//     fn read_num_or_int(&mut self, sign: Sign) -> Result<Token> {
//         // unimplemented!()
//         let mut num = String::new();
//         while let Some(&c) = self.input.peek() {
//             if !c.is_numeric() {
//                 break;
//             }

//             self.input.next();
//             num.push(c);
//         }

//         if let Some(&'.') = self.input.peek() {
//             self.input.next();
//             num.push('.');

//             while let Some(&c) = self.input.peek() {
//                 if !c.is_numeric() {
//                     break;
//                 }

//                 self.input.next();
//                 num.push(c);
//             }
//             let num: f64 = match sign {
//                 Sign::Pos => num.parse().unwrap(),
//                 Sign::Neg => num.parse::<f64>().unwrap() * -1.0,
//             };
//             Ok(Token::NumberLiteral(num))
//         } else {
//             // We've found an integer
//             let num: isize = match sign {
//                 Sign::Pos => num.parse().unwrap(),
//                 Sign::Neg => num.parse::<isize>().unwrap() * -1,
//             };
//             Ok(Token::IntegerLiteral(num))
//         }
//     }

//     // fn read_number(&mut self) -> f64 {
//     //     let mut num = String::new();
//     //     while let Some(&c) = self.input.peek() {
//     //         if !c.is_numeric() {
//     //             break;
//     //         }

//     //         self.input.next();
//     //         num.push(c);
//     //     }

//     //     if let Some(&'.') = self.input.peek() {
//     //         self.input.next();
//     //         num.push('.');

//     //         while let Some(&c) = self.input.peek() {
//     //             if !c.is_numeric() {
//     //                 break;
//     //             }

//     //             self.input.next();
//     //             num.push(c);
//     //         }
//     //     }

//     //     num.parse().unwrap()
//     // }

//     fn read_string(&mut self) -> Result<Token> {
//         // Skip the opening quote.
//         self.input.next();

//         let mut buf = String::new();
//         while let Some(&c) = self.input.peek() {
//             self.input.next();
//             match c {
//                 '"' => return Ok(Token::StringLiteral(buf)),
//                 '\\' => match self.input.peek() {
//                     Some(&c) if c == '"' || c == '\\' => {
//                         self.input.next();
//                         buf.push(c);
//                     }
//                     _ => return Err(TokenError::InvalidEscape(self.line_number)),
//                 },
//                 _ => buf.push(c),
//             }
//         }

//         buf.insert(0, '"');
//         Err(TokenError::IncompleteString(self.line_number))
//     }
// }

// // pub trait LineNumber {

// // }

// // impl LineNumber for std::iter::Peekable<Tokenizer<'a>> {
// //     // self.line_number
// // }

// impl<'a> Iterator for Tokenizer<'a> {
//     type Item = Result<Token>;

//     // fn count()

//     fn next(&mut self) -> Option<Self::Item> {
//         self.consume_whitespace_and_comments_until_next_input();

//         match self.input.peek() {
//             None => None,
//             Some('(') | Some('[') | Some('{') => {
//                 self.input.next();
//                 Some(Ok(Token::OpenParen))
//             }
//             Some(')') | Some(']') | Some('}') => {
//                 self.input.next();
//                 Some(Ok(Token::CloseParen))
//             }
//             Some('\'') => {
//                 self.input.next();
//                 Some(Ok(Token::QuoteTick))
//             }
//             Some('+') => {
//                 self.input.next();
//                 match self.input.peek() {
//                     Some(&c) if c.is_numeric() => {
//                         Some(self.read_num_or_int(Sign::Pos))
//                         // Some(Ok(Token::NumberLiteral(self.read_number())))
//                     }
//                     _ => Some(Ok(Token::Identifier("+".to_string()))),
//                 }
//             }
//             Some('-') => {
//                 self.input.next();
//                 match self.input.peek() {
//                     Some(&c) if c.is_numeric() => {
//                         Some(self.read_num_or_int(Sign::Neg))
//                         // Some(Ok(Token::NumberLiteral(self.read_number() * -1.0)))
//                     }
//                     Some(&c) if c == ' ' => Some(Ok(Token::Identifier("-".to_string()))),
//                     _ => Some(Ok(self.read_word_with_starting_hyphen())),
//                 }
//             }
//             Some('*') => {
//                 self.input.next();
//                 Some(Ok(Token::Identifier("*".to_string())))
//             }
//             Some('/') => {
//                 self.input.next();
//                 Some(Ok(Token::Identifier("/".to_string())))
//             }
//             Some('#') => {
//                 self.input.next();
//                 Some(self.read_hash_value())
//             }
//             Some('"') => Some(self.read_string()),
//             Some(c)
//                 if !c.is_whitespace()
//                     && (c.is_alphabetic() && !c.is_numeric() || *c == '?' || *c == '!')
//                     || *c == '_'
//                     || *c == '.' =>
//             {
//                 Some(Ok(self.read_word()))
//             }
//             Some('=') | Some('<') | Some('>') => Some(Ok(self.read_word())),
//             Some(c) if c.is_numeric() => Some(self.read_num_or_int(Sign::Pos)),
//             Some(_) => match self.input.next() {
//                 Some(e) => Some(Err(TokenError::UnexpectedChar(e, self.line_number))),
//                 _ => None,
//             },
//         }
//     }
// }
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span::Span;
    // use crate::parser::tokens::TokenError;
    // use crate::parser::tokens::TokenType;
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

        assert_eq!(
            s.next(),
            Some(Token {
                ty: StringLiteral(r#"\"\\"#.to_owned()),
                source: r#""\"\\""#,
                span: Span::new(14, 20),
            })
        );

        // assert_eq!(s.next(), Some(Ok(StringLiteral("".to_owned()))));
        // assert_eq!(s.next(), Some(Ok(StringLiteral("Foo bar".to_owned()))));
        // assert_eq!(s.next(), Some(Ok(StringLiteral(r#""\"#.to_owned()))));
        assert_eq!(s.next(), None);
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
                span: Span::new(0, 1)
            },
            Token {
                ty: Identifier("apples".to_string()),
                source: "apples",
                span: Span::new(1, 7)
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(8, 9)
            },
            Token {
                ty: Identifier("function".to_string()),
                source: "function",
                span: Span::new(9, 17)
            },
            Token {
                ty: Identifier("a".to_string()),
                source: "a",
                span: Span::new(18, 19)
            },
            Token {
                ty: Identifier("b".to_string()),
                source: "b",
                span: Span::new(20, 21)
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(21, 22)
            },
            Token {
                ty: OpenParen,
                source: "(",
                span: Span::new(23, 24)
            },
            Token {
                ty: Identifier("+".to_string()),
                source: "+",
                span: Span::new(24, 25)
            },
            Token {
                ty: Identifier("a".to_string()),
                source: "a",
                span: Span::new(26, 27)
            },
            Token {
                ty: Identifier("b".to_string()),
                source: "b",
                span: Span::new(28, 29)
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(29, 30)
            },
            Token {
                ty: CloseParen,
                source: ")",
                span: Span::new(30, 31)
            }

            // Identifier("apples".to_string()),
            // OpenParen,
            // Identifier("function".to_string()),
            // Identifier("a".to_string()),
            // Identifier("b".to_string()),
            // CloseParen,
            // OpenParen,
            // Identifier("+".to_string()),
            // Identifier("a".to_string()),
            // Identifier("b".to_string()),
            // CloseParen,
            // CloseParen,
        ];

        assert_eq!(res, expected);
    }
}
