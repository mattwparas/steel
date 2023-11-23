use std::{iter::Peekable, marker::PhantomData, str::Chars};

use crate::{
    lexer::ToOwnedString,
    parser::SourceId,
    tokens::{parse_unicode_str, MaybeBigInt, Token, TokenType},
};

pub type Span = core::ops::Range<usize>;

pub struct Lexer<'a> {
    source: &'a str,

    chars: Peekable<Chars<'a>>,

    token_start: usize,
    token_end: usize,
    // skip_comments: bool,
    // source_id: Option<SourceId>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            token_start: 0,
            token_end: 0,
            // skip_comments,
            // source_id,
        }
    }

    fn eat(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.token_end += c.len_utf8();

            Some(c)
        } else {
            None
        }
    }

    // Consume characters until the next non whitespace input
    fn consume_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() {
                self.eat();

                self.token_start = self.token_end;
            } else {
                break;
            }
        }
    }

    fn read_string(&mut self) -> Result<TokenType<&'a str>> {
        // Skip the opening quote.
        self.eat();

        let mut buf = String::new();
        while let Some(&c) = self.chars.peek() {
            self.eat();
            // println!("{}", c);
            // println!("{:?}", buf);
            match c {
                '"' => return Ok(TokenType::StringLiteral(buf)),
                '\\' => match self.chars.peek() {
                    Some('"') => {
                        self.eat();
                        buf.push('"');
                    }

                    Some('\\') => {
                        self.eat();
                        buf.push('\\')
                    }

                    Some('t') => {
                        self.eat();
                        buf.push('\t');
                    }

                    Some('n') => {
                        self.eat();
                        buf.push('\n');
                    }

                    Some('r') => {
                        self.eat();
                        buf.push('\r');
                    }

                    _ => return Err(TokenError::InvalidEscape),
                },
                _ => buf.push(c),
            }
        }

        buf.insert(0, '"');
        Err(TokenError::IncompleteString)
    }

    fn read_hash_value(&mut self) -> Result<TokenType<&'a str>> {
        fn parse_char(slice: &str) -> Option<char> {
            use std::str::FromStr;

            match slice {
                "#\\SPACE" => Some(' '),
                "#\\space" => Some(' '),
                "#\\\\" => Some('\\'),
                "#\\tab" => Some('\t'),
                "#\\TAB" => Some('\t'),
                "#\\NEWLINE" => Some('\n'),
                "#\\newline" => Some('\n'),
                "#\\return" => Some('\r'),
                "#\\RETURN" => Some('\r'),
                "#\\)" => Some(')'),
                "#\\]" => Some(']'),
                "#\\[" => Some('['),
                "#\\(" => Some('('),
                "#\\^" => Some('^'),

                character if character.starts_with("#\\") => {
                    let parsed_unicode = parse_unicode_str(character);

                    if parsed_unicode.is_some() {
                        return parsed_unicode;
                    }
                    char::from_str(character.trim_start_matches("#\\")).ok()
                }
                _ => None,
            }
        }

        while let Some(&c) = self.chars.peek() {
            match c {
                '\\' => {
                    self.eat();
                    self.eat();
                }
                '(' | '[' | ')' | ']' => break,
                c if c.is_whitespace() => break,
                _ => {
                    self.eat();
                }
            };
        }

        match self.slice() {
            "#true" | "#t" => Ok(TokenType::BooleanLiteral(true)),
            "#false" | "#f" => Ok(TokenType::BooleanLiteral(false)),

            "#'" => Ok(TokenType::QuoteSyntax),
            "#`" => Ok(TokenType::QuasiQuoteSyntax),
            "#," => Ok(TokenType::UnquoteSyntax),
            "#,@" => Ok(TokenType::UnquoteSpliceSyntax),

            hex if hex.starts_with("#x") => {
                todo!()
            }

            octal if octal.starts_with("#o") => {
                todo!()
            }

            binary if binary.starts_with("#b") => {
                todo!()
            }

            keyword if keyword.starts_with("#:") => Ok(TokenType::Keyword(self.slice())),

            character if character.starts_with("#\\") => {
                if let Some(parsed_character) = parse_char(character) {
                    Ok(TokenType::CharacterLiteral(parsed_character))
                } else {
                    Err(TokenError::InvalidCharacter)
                }
            }

            _ => Ok(self.read_word()),
        }
    }

    fn read_number(&mut self) -> TokenType<&'a str> {
        while let Some(&c) = self.chars.peek() {
            if matches!(c, '(' | '[' | ')' | ']') {
                break;
            }

            if c.is_whitespace() {
                break;
            }

            if c == '.' {
                break;
            }

            if !c.is_numeric() {
                self.eat();
                return self.read_word();
            }

            self.eat();
        }

        if let Some(&'.') = self.chars.peek() {
            self.eat();

            while let Some(&c) = self.chars.peek() {
                if matches!(c, '(' | '[' | ')' | ']') {
                    break;
                }
                if c.is_whitespace() {
                    break;
                }

                if !c.is_numeric() {
                    self.eat();
                    return self.read_word();
                }

                self.eat();
                // num.push(c);
            }

            return TokenType::NumberLiteral(self.slice().parse().unwrap());
        }

        TokenType::IntegerLiteral(self.slice().parse().unwrap())
    }

    fn read_rest_of_line(&mut self) {
        while let Some(c) = self.eat() {
            if c == '\n' {
                break;
            }
        }
    }

    fn read_word(&mut self) -> TokenType<&'a str> {
        while let Some(&c) = self.chars.peek() {
            match c {
                '(' | '[' | ')' | ']' => break,
                c if c.is_whitespace() => break,
                '\'' => {
                    break;
                }
                // Could be a quote within a word, we should handle escaping it accordingly
                // (even though its a bit odd)
                '\\' => {
                    self.eat();
                    self.eat();
                }

                _ => {
                    self.eat();
                }
            };
        }

        match self.slice() {
            "define" | "defn" | "#%define" => TokenType::Define,
            "let" => TokenType::Let,
            "%plain-let" => TokenType::TestLet,
            "return!" => TokenType::Return,
            "begin" => TokenType::Begin,
            "lambda" | "fn" | "#%plain-lambda" | "λ" => TokenType::Lambda,
            "quote" => TokenType::Quote,
            "syntax-rules" => TokenType::SyntaxRules,
            "define-syntax" => TokenType::DefineSyntax,
            "..." => TokenType::Ellipses,
            "set!" => TokenType::Set,
            "require" => TokenType::Require,
            "if" => TokenType::If,

            identifier => TokenType::Identifier(identifier),
        }
    }
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        self.token_start..self.token_end
    }

    #[inline]
    pub fn slice(&self) -> &'a str {
        self.source.get(self.span()).unwrap()
    }
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    skip_comments: bool,
    source_id: Option<SourceId>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, source_id: Option<SourceId>) -> Self {
        Self {
            lexer: Lexer::new(input),
            skip_comments,
            source_id, // skip_doc_comments,
        }
    }

    pub fn into_owned<T, F: ToOwnedString<T>>(self, adapter: F) -> OwnedTokenStream<'a, T, F> {
        OwnedTokenStream {
            stream: self,
            adapter,
            _token_type: PhantomData,
        }
    }
}

pub struct OwnedTokenStream<'a, T, F> {
    stream: TokenStream<'a>,
    adapter: F,
    _token_type: PhantomData<T>,
}

impl<'a, T, F: ToOwnedString<T>> Iterator for OwnedTokenStream<'a, T, F> {
    type Item = Token<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.next().map(|x| Token {
            ty: x.ty.map(|x| self.adapter.own(x)),
            source: x.source,
            span: x.span,
        })
    }
}

impl<'a, T, F: ToOwnedString<T>> OwnedTokenStream<'a, T, F> {
    pub fn offset(&self) -> usize {
        self.stream.lexer.span().end
    }
}
impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a, &'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().and_then(|token| {
            let token = match token {
                Ok(token) => token,
                Err(_) => TokenType::Error,
            };

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

pub type Result<T> = std::result::Result<T, TokenError>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    // #[error("Unexpected char, {0}")]
    UnexpectedChar(char),
    // #[error("Incomplete String")]
    IncompleteString,
    // #[error("Invalid Escape")]
    InvalidEscape,
    // #[error("Invalid Character")]
    InvalidCharacter,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenType<&'a str>>;

    fn next(&mut self) -> Option<Self::Item> {
        // self.consume_whitespace_and_comments_until_next_input();

        // Crunch until the next input
        self.consume_whitespace();

        self.token_start = self.token_end;

        match self.chars.peek() {
            Some(';') => {
                self.eat();
                self.read_rest_of_line();
                Some(Ok(TokenType::Comment))
            }

            Some('"') => Some(self.read_string()),

            Some('(') | Some('[') | Some('{') => {
                self.eat();
                Some(Ok(TokenType::OpenParen))
            }
            Some(')') | Some(']') | Some('}') => {
                self.eat();
                Some(Ok(TokenType::CloseParen))
            }

            // Handle Quotes
            Some('\'') => {
                self.eat();
                Some(Ok(TokenType::QuoteTick))
            }

            Some('`') => {
                self.eat();
                Some(Ok(TokenType::QuasiQuote))
            }
            Some(',') => {
                self.eat();

                if let Some('@') = self.chars.peek() {
                    self.eat();

                    Some(Ok(TokenType::UnquoteSplice))
                } else {
                    Some(Ok(TokenType::Unquote))
                }
            }

            Some('+') => {
                self.eat();
                match self.chars.peek() {
                    Some(&c) if c.is_numeric() => Some(Ok(self.read_number())),
                    _ => Some(Ok(TokenType::Identifier(self.slice()))),
                }
            }
            Some('-') => {
                self.eat();
                match self.chars.peek() {
                    Some(&c) if c.is_numeric() => Some(Ok(self.read_number())),
                    _ => Some(Ok(self.read_word())),
                }
            }
            Some('#') => {
                self.eat();
                Some(self.read_hash_value())
            }

            Some(c) if !c.is_whitespace() && !c.is_numeric() || *c == '_' => {
                Some(Ok(self.read_word()))
            }
            Some(c) if c.is_numeric() => Some(Ok(self.read_number())),
            Some(_) => match self.eat() {
                Some(e) => Some(Err(TokenError::UnexpectedChar(e))),
                _ => None,
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use crate::span::Span;
    use crate::tokens::{MaybeBigInt, TokenType::*};

    // TODO: Figure out why this just cause an infinite loop when parsing it?
    #[test]
    fn test_identifier_with_quote_end() {
        let s = TokenStream::new(
            "        (define (stream-cdr stream)
            ((stream-cdr' stream)))
",
            true,
            None,
        );

        for token in s {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_bracket_characters() {
        let s = TokenStream::new(
            "[(equal? #\\[ (car chars)) (b (cdr chars) (+ sum 1))]",
            true,
            None,
        );

        for token in s {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_escape_in_string() {
        let s = TokenStream::new(r#"(display "}\n")"#, true, None);

        for token in s {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_quote_within_word() {
        let s = TokenStream::new("'foo\\'a", true, None);

        // println!("{:?}", s.next());
        // println!("{:?}", s.next());
        // println!("{:?}", s.next());
    }

    #[test]
    fn test_single_period() {
        let mut s = TokenStream::new(".", true, None);

        println!("{:?}", s.next());
    }

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
                ty: Identifier("$"),
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
                ty: IntegerLiteral(MaybeBigInt::Small(0)),
                source: "0",
                span: Span::new(0, 1, None),
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: IntegerLiteral(MaybeBigInt::Small(0)),
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
                ty: IntegerLiteral(MaybeBigInt::Small(999)),
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
        //         span: Span::new(14, 20, None),
        //     })
        // );
        // assert_eq!(
        //     s.next(),
        //     Some(Token {
        //         ty: Error,
        //         source: "\"\\\"",
        //         span: Span::new(14, 17, None),
        //     })
        // );

        // assert_eq!(s.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut s = TokenStream::new(";!/usr/bin/gate\n   ; foo\n", true, None);
        assert_eq!(s.next(), None);
    }

    #[test]
    fn function_definition() {
        let s = TokenStream::new(
            "(define odd-rec? (lambda (x) (if (= x 0) #f (even-rec? (- x 1)))))",
            true,
            None,
        );
        let res: Vec<Token<&str>> = s.collect();

        println!("{:#?}", res);
    }

    #[test]
    fn scheme_statement() {
        let s = TokenStream::new("(apples (function a b) (+ a b))", true, None);
        let res: Vec<Token<&str>> = s.collect();

        let expected: Vec<Token<&str>> = vec![
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

    #[test]
    fn test_bigint() {
        let s = TokenStream::new("9223372036854775808", true, None); // isize::MAX + 1
        let res: Vec<Token<&str>> = s.collect();

        let expected_bigint = "9223372036854775808".parse().unwrap();

        let expected: Vec<Token<&str>> = vec![Token {
            ty: IntegerLiteral(MaybeBigInt::Big(expected_bigint)),
            source: "9223372036854775808",
            span: Span::new(0, 19, None),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn negative_test_bigint() {
        let s = TokenStream::new("-9223372036854775809", true, None); // isize::MIN - 1
        let res: Vec<Token<&str>> = s.collect();

        let expected_bigint = "-9223372036854775809".parse().unwrap();

        let expected: Vec<Token<&str>> = vec![Token {
            ty: IntegerLiteral(MaybeBigInt::Big(expected_bigint)),
            source: "-9223372036854775809",
            span: Span::new(0, 20, None),
        }];

        assert_eq!(res, expected);
    }
}
