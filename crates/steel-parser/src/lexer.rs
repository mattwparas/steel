use super::parser::SourceId;
use crate::tokens::{parse_unicode_str, NumberLiteral, RealLiteral};
use crate::tokens::{IntLiteral, Token, TokenType};
use smallvec::SmallVec;
use std::iter::Iterator;
use std::marker::PhantomData;
use std::{iter::Peekable, str::Chars};

pub const INFINITY: &str = "+inf.0";
pub const NEG_INFINITY: &str = "-inf.0";
pub const NAN: &str = "+nan.0";
pub const NEG_NAN: &str = "-nan.0";

pub struct OwnedString;

impl ToOwnedString<String> for OwnedString {
    fn own(&self, s: &str) -> String {
        s.to_string()
    }
}

pub trait ToOwnedString<T> {
    fn own(&self, s: &str) -> T;
}

pub type Span = core::ops::Range<usize>;

pub struct Lexer<'a> {
    /// The source of the lexer.
    source: &'a str,
    /// An iterator over the characters.
    chars: Peekable<Chars<'a>>,
    /// The  next token to return or `None` if it should be parsed.
    queued: Option<TokenType<&'a str>>,
    token_start: usize,
    token_end: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            queued: None,
            token_start: 0,
            token_end: 0,
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

                    Some('0') => {
                        self.eat();
                        buf.push('\0');
                    }

                    Some('x') => {
                        self.eat();

                        let digit1 = self.eat().ok_or_else(|| TokenError::MalformedByteEscape)?;
                        let digit2 = self.eat().ok_or_else(|| TokenError::MalformedByteEscape)?;

                        let mut chars = String::new();
                        chars.push(digit1);
                        chars.push(digit2);

                        let byte = u8::from_str_radix(&chars, 16)
                            .map_err(|_| TokenError::MalformedByteEscape)?;

                        let char = char::from_u32(byte as u32)
                            .ok_or_else(|| TokenError::MalformedByteEscape)?;

                        buf.push(char);
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
                let hex = isize::from_str_radix(hex.strip_prefix("#x").unwrap(), 16)
                    .map_err(|_| TokenError::MalformedHexInteger)?;

                Ok(IntLiteral::Small(hex).into())
            }

            octal if octal.starts_with("#o") => {
                let hex = isize::from_str_radix(octal.strip_prefix("#o").unwrap(), 8)
                    .map_err(|_| TokenError::MalformedOctalInteger)?;

                Ok(IntLiteral::Small(hex).into())
            }

            binary if binary.starts_with("#b") => {
                let hex = isize::from_str_radix(binary.strip_prefix("#b").unwrap(), 2)
                    .map_err(|_| TokenError::MalformedBinaryInteger)?;

                Ok(IntLiteral::Small(hex).into())
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
            match c {
                c if c.is_numeric() => {
                    self.eat();
                }
                '+' | '-' | '.' | '/' | 'e' | 'E' | 'i' => {
                    self.eat();
                }
                '(' | ')' | '[' | ']' => {
                    return if let Some(t) = parse_number(self.slice()) {
                        t.into()
                    } else {
                        self.read_word()
                    }
                }
                c if c.is_whitespace() => {
                    return if let Some(t) = parse_number(self.slice()) {
                        t.into()
                    } else {
                        self.read_word()
                    }
                }
                _ => return self.read_word(),
            }
        }
        match parse_number(self.slice()) {
            Some(n) => n.into(),
            None => self.read_word(),
        }
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
                '(' | '[' | ')' | ']' | '{' | '}' => break,
                c if c.is_whitespace() => break,
                '\'' | '"' | '`' | ';' | ',' => {
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
            "." => TokenType::Dot,
            "define" | "defn" | "#%define" => TokenType::Define,
            "let" => TokenType::Let,
            "%plain-let" => TokenType::TestLet,
            "return!" => TokenType::Return,
            "begin" => TokenType::Begin,
            "lambda" | "fn" | "#%plain-lambda" | "λ" => TokenType::Lambda,
            "quote" => TokenType::Quote,
            // "unquote" => TokenType::Unquote,
            "syntax-rules" => TokenType::SyntaxRules,
            "define-syntax" => TokenType::DefineSyntax,
            "..." => TokenType::Ellipses,
            "set!" => TokenType::Set,
            "require" => TokenType::Require,
            "if" => TokenType::If,
            INFINITY => TokenType::Number(RealLiteral::Float(f64::INFINITY).into()),
            NEG_INFINITY => TokenType::Number(RealLiteral::Float(f64::NEG_INFINITY).into()),
            NAN => TokenType::Number(RealLiteral::Float(f64::NAN).into()),
            NEG_NAN => TokenType::Number(RealLiteral::Float(f64::NAN).into()),
            "|.|" => TokenType::Identifier("."),
            identifier => {
                if identifier.len() > 1 && identifier.starts_with('+') && self.queued.is_none() {
                    self.queued = Some(TokenType::Identifier(&identifier[1..]));
                    TokenType::Identifier("+")
                } else {
                    TokenType::Identifier(identifier)
                }
            }
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
    UnexpectedChar(char),
    IncompleteString,
    InvalidEscape,
    InvalidCharacter,
    MalformedHexInteger,
    MalformedOctalInteger,
    MalformedBinaryInteger,
    MalformedByteEscape,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenType<&'a str>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(t) = self.queued.take() {
            return Some(Ok(t));
        }
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
            Some('+') | Some('-') => {
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
            Some(_) => self.eat().map(|e| Err(TokenError::UnexpectedChar(e))),
            None => None,
        }
    }
}

// Split the string by + and -. Returns at most 2 elements or `None` if there were more than 2.
fn split_into_complex<'a>(s: &'a str) -> Option<SmallVec<[NumPart<'a>; 2]>> {
    let classify_num_part = |s: &'a str| -> NumPart<'a> {
        match s.chars().last() {
            Some('i') => NumPart::Imaginary(&s[..s.len() - 1]),
            _ => NumPart::Real(s),
        }
    };
    let idxs: SmallVec<[usize; 3]> = s
        .char_indices()
        .filter(|(_, ch)| *ch == '+' || *ch == '-')
        .map(|(idx, _)| idx)
        .take(3)
        .collect();
    let parts = match idxs.as_slice() {
        [] | [0] => SmallVec::from_iter(std::iter::once(s).map(classify_num_part)),
        [idx] | [0, idx] => {
            SmallVec::from_iter([&s[0..*idx], &s[*idx..]].into_iter().map(classify_num_part))
        }
        _ => return None,
    };
    Some(parts)
}

#[derive(Debug)]
enum NumPart<'a> {
    Real(&'a str),
    Imaginary(&'a str),
}

fn parse_real(s: &str) -> Option<RealLiteral> {
    let mut has_e = false;
    let mut has_dot = false;
    let mut frac_position = None;
    for (idx, ch) in s.chars().enumerate() {
        match ch {
            '+' => {
                if idx != 0 {
                    return None;
                }
            }
            '-' => {
                if idx != 0 {
                    return None;
                }
            }
            'e' | 'E' => {
                if has_e {
                    return None;
                };
                has_e = true;
            }
            '/' => {
                frac_position = match frac_position {
                    Some(_) => return None,
                    None => Some(idx),
                }
            }
            '.' => {
                if has_dot {
                    return None;
                }
                has_dot = true
            }
            _ => {}
        }
    }
    if has_e || has_dot {
        s.parse().map(|f| RealLiteral::Float(f)).ok()
    } else if let Some(p) = frac_position {
        let (n_str, d_str) = s.split_at(p);
        let d_str = &d_str[1..];
        let n: IntLiteral = n_str.parse().ok()?;
        let d: IntLiteral = d_str.parse().ok()?;
        Some(RealLiteral::Rational(n, d))
    } else {
        let int: IntLiteral = s.parse().ok()?;
        Some(RealLiteral::Int(int))
    }
}

fn parse_number(s: &str) -> Option<NumberLiteral> {
    match split_into_complex(s)?.as_slice() {
        [NumPart::Real(x)] => parse_real(x).map(NumberLiteral::from),
        [NumPart::Imaginary(x)] => {
            if !matches!(x.chars().next(), Some('+') | Some('-')) {
                return None;
            };
            Some(NumberLiteral::Complex(IntLiteral::Small(0).into(), parse_real(x)?).into())
        }
        [NumPart::Real(re), NumPart::Imaginary(im)]
        | [NumPart::Imaginary(im), NumPart::Real(re)] => {
            Some(NumberLiteral::Complex(parse_real(re)?, parse_real(im)?))
        }
        _ => None,
    }
}

#[cfg(test)]
mod lexer_tests {
    use std::str::FromStr;

    use super::*;
    use crate::span::Span;
    use crate::tokens::{IntLiteral, TokenType::*};
    use pretty_assertions::assert_eq;

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
        let mut s = TokenStream::new("'foo\\'a", true, None);

        println!("{:?}", s.next());
        println!("{:?}", s.next());
        println!("{:?}", s.next());
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
    fn test_almost_literals() {
        let got: Vec<_> =
            TokenStream::new("1e 1ee 1.2e5.4 1E10/4 1.45# 3- e10", true, None).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: Identifier("1e"),
                    source: "1e",
                    span: Span::new(0, 2, None),
                },
                Token {
                    ty: Identifier("1ee"),
                    source: "1ee",
                    span: Span::new(3, 6, None),
                },
                Token {
                    ty: Identifier("1.2e5.4"),
                    source: "1.2e5.4",
                    span: Span::new(7, 14, None),
                },
                Token {
                    ty: Identifier("1E10/4"),
                    source: "1E10/4",
                    span: Span::new(15, 21, None),
                },
                Token {
                    ty: Identifier("1.45#"),
                    source: "1.45#",
                    span: Span::new(22, 27, None),
                },
                Token {
                    ty: Identifier("3-"),
                    source: "3-",
                    span: Span::new(28, 30, None),
                },
                Token {
                    ty: Identifier("e10"),
                    source: "e10",
                    span: Span::new(31, 34, None),
                },
            ]
        );
    }

    #[test]
    fn test_real_numbers() {
        let got: Vec<_> = TokenStream::new(
            "0 -0 -1.2 +2.3 999 1. 1e2 1E2 1.2e2 1.2E2 +inf.0 -inf.0",
            true,
            None,
        )
        .collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: IntLiteral::Small(0).into(),
                    source: "0",
                    span: Span::new(0, 1, None),
                },
                Token {
                    ty: IntLiteral::Small(0).into(),
                    source: "-0",
                    span: Span::new(2, 4, None),
                },
                Token {
                    ty: RealLiteral::Float(-1.2).into(),
                    source: "-1.2",
                    span: Span::new(5, 9, None),
                },
                Token {
                    ty: RealLiteral::Float(2.3).into(),
                    source: "+2.3",
                    span: Span::new(10, 14, None),
                },
                Token {
                    ty: IntLiteral::Small(999).into(),
                    source: "999",
                    span: Span::new(15, 18, None),
                },
                Token {
                    ty: RealLiteral::Float(1.0).into(),
                    source: "1.",
                    span: Span::new(19, 21, None),
                },
                Token {
                    ty: RealLiteral::Float(100.0).into(),
                    source: "1e2",
                    span: Span::new(22, 25, None),
                },
                Token {
                    ty: RealLiteral::Float(100.0).into(),
                    source: "1E2",
                    span: Span::new(26, 29, None),
                },
                Token {
                    ty: RealLiteral::Float(120.0).into(),
                    source: "1.2e2",
                    span: Span::new(30, 35, None),
                },
                Token {
                    ty: RealLiteral::Float(120.0).into(),
                    source: "1.2E2",
                    span: Span::new(36, 41, None),
                },
                Token {
                    ty: RealLiteral::Float(f64::INFINITY).into(),
                    source: "+inf.0",
                    span: Span::new(42, 48, None),
                },
                Token {
                    ty: RealLiteral::Float(f64::NEG_INFINITY).into(),
                    source: "-inf.0",
                    span: Span::new(49, 55, None),
                },
            ]
        );
    }

    #[test]
    fn test_nan() {
        // nan does not equal nan so we have to run the is_nan predicate.
        let got = TokenStream::new("+nan.0", true, None).next().unwrap();
        assert!(
            matches!(got.ty, TokenType::Number(NumberLiteral::Real(RealLiteral::Float(x))) if x.is_nan())
        );
        let got = TokenStream::new("-nan.0", true, None).next().unwrap();
        assert!(
            matches!(got.ty, TokenType::Number(NumberLiteral::Real(RealLiteral::Float(x))) if x.is_nan())
        );
    }

    #[test]
    fn test_rationals() {
        let got: Vec<_> = TokenStream::new(
            r#"
                1/4
                (1/4 1/3)
                11111111111111111111/22222222222222222222
                /
                1/
                1/4.0
                1//4
                1 / 4
"#,
            true,
            None,
        )
        .collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(4)).into(),
                    source: "1/4",
                    span: Span::new(17, 20, None),
                },
                Token {
                    ty: OpenParen,
                    source: "(",
                    span: Span::new(37, 38, None),
                },
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(4)).into(),
                    source: "1/4",
                    span: Span::new(38, 41, None),
                },
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(3)).into(),
                    source: "1/3",
                    span: Span::new(42, 45, None),
                },
                Token {
                    ty: CloseParen,
                    source: ")",
                    span: Span::new(45, 46, None),
                },
                Token {
                    ty: RealLiteral::Rational(
                        IntLiteral::from_str("11111111111111111111").unwrap(),
                        IntLiteral::from_str("22222222222222222222").unwrap(),
                    )
                    .into(),
                    source: "11111111111111111111/22222222222222222222",
                    span: Span::new(63, 104, None),
                },
                Token {
                    ty: Identifier("/"),
                    source: "/",
                    span: Span::new(121, 122, None),
                },
                Token {
                    ty: Identifier("1/"),
                    source: "1/",
                    span: Span::new(139, 141, None),
                },
                Token {
                    ty: Identifier("1/4.0"),
                    source: "1/4.0",
                    span: Span::new(158, 163, None),
                },
                Token {
                    ty: Identifier("1//4"),
                    source: "1//4",
                    span: Span::new(180, 184, None),
                },
                Token {
                    ty: IntLiteral::Small(1).into(),
                    source: "1",
                    span: Span::new(201, 202, None),
                },
                Token {
                    ty: Identifier("/"),
                    source: "/",
                    span: Span::new(203, 204, None),
                },
                Token {
                    ty: IntLiteral::Small(4).into(),
                    source: "4",
                    span: Span::new(205, 206, None),
                },
            ]
        );
    }

    #[test]
    fn test_complex_numbers() {
        let got: Vec<_> =
            TokenStream::new("1+2i 3-4i +5+6i +1i 1.0+2.0i 3-4.0i +1.0i", true, None).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(1).into(),
                        IntLiteral::Small(2).into()
                    )
                    .into(),
                    source: "1+2i",
                    span: Span::new(0, 4, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(3).into(),
                        IntLiteral::Small(-4).into()
                    )
                    .into(),
                    source: "3-4i",
                    span: Span::new(5, 9, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(5).into(),
                        IntLiteral::Small(6).into()
                    )
                    .into(),
                    source: "+5+6i",
                    span: Span::new(10, 15, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        IntLiteral::Small(1).into()
                    )
                    .into(),
                    source: "+1i",
                    span: Span::new(16, 19, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        RealLiteral::Float(1.0).into(),
                        RealLiteral::Float(2.0).into()
                    )
                    .into(),
                    source: "1.0+2.0i",
                    span: Span::new(20, 28, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(3).into(),
                        RealLiteral::Float(-4.0).into()
                    )
                    .into(),
                    source: "3-4.0i",
                    span: Span::new(29, 35, None),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        RealLiteral::Float(1.0).into()
                    )
                    .into(),
                    source: "+1.0i",
                    span: Span::new(36, 41, None),
                },
            ]
        );
    }

    #[test]
    fn test_malformed_complex_numbers_are_identifiers() {
        let got: Vec<_> = TokenStream::new("i -i 1i+1i 4+i -4+-2i", true, None).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: Identifier("i"),
                    source: "i",
                    span: Span::new(0, 1, None),
                },
                Token {
                    ty: Identifier("-i"),
                    source: "-i",
                    span: Span::new(2, 4, None),
                },
                Token {
                    ty: Identifier("1i+1i"),
                    source: "1i+1i",
                    span: Span::new(5, 10, None),
                },
                Token {
                    ty: Identifier("4+i"),
                    source: "4+i",
                    span: Span::new(11, 14, None),
                },
                Token {
                    ty: Identifier("-4+-2i"),
                    source: "-4+-2i",
                    span: Span::new(15, 21, None),
                },
            ]
        );
    }

    #[test]
    fn test_string() {
        let got: Vec<_> = TokenStream::new(r#" "" "Foo bar" "\"\\" "#, true, None).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: StringLiteral(r#""#.to_string()),
                    source: r#""""#,
                    span: Span::new(1, 3, None),
                },
                Token {
                    ty: StringLiteral(r#"Foo bar"#.to_string()),
                    source: r#""Foo bar""#,
                    span: Span::new(4, 13, None),
                },
                Token {
                    ty: StringLiteral(r#""\"#.to_string()),
                    source: r#""\"\\""#,
                    span: Span::new(14, 20, None),
                },
            ]
        );
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
    fn lex_string_with_escape_chars() {
        let s = TokenStream::new("\"\0\0\0\"", true, None);
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

        let expected_bigint = Box::new("9223372036854775808".parse().unwrap());

        let expected: Vec<Token<&str>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "9223372036854775808",
            span: Span::new(0, 19, None),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn negative_test_bigint() {
        let s = TokenStream::new("-9223372036854775809", true, None); // isize::MIN - 1
        let res: Vec<Token<&str>> = s.collect();

        let expected_bigint = Box::new("-9223372036854775809".parse().unwrap());

        let expected: Vec<Token<&str>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "-9223372036854775809",
            span: Span::new(0, 20, None),
        }];

        assert_eq!(res, expected);
    }
}
