use super::parser::SourceId;
use crate::tokens::{IntLiteral, Token, TokenType};
use crate::tokens::{NumberLiteral, Paren, ParenMod, RealLiteral};
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use std::char;
use std::iter::Iterator;
use std::marker::PhantomData;
use std::sync::Arc;
use std::{iter::Peekable, str::Chars};

pub const INFINITY: &str = "+inf.0";
pub const NEG_INFINITY: &str = "-inf.0";
pub const NAN: &str = "+nan.0";
pub const NEG_NAN: &str = "-nan.0";

pub struct OwnedString;

impl ToOwnedString<String> for OwnedString {
    fn own(&self, s: Cow<str>) -> String {
        s.to_string()
    }
}

pub trait ToOwnedString<T> {
    fn own(&self, s: Cow<str>) -> T;
}

pub type Span = core::ops::Range<usize>;

pub struct Lexer<'a> {
    /// The source of the lexer.
    source: &'a str,
    /// An iterator over the characters.
    chars: Peekable<Chars<'a>>,
    /// The  next token to return or `None` if it should be parsed.
    queued: Option<TokenType<Cow<'a, str>>>,
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

    fn read_string(&mut self) -> Result<TokenType<Cow<'a, str>>> {
        // Skip the opening quote.
        self.eat();

        let mut buf = String::new();

        while let Some(&c) = self.chars.peek() {
            self.eat();
            match c {
                '"' => return Ok(TokenType::StringLiteral(Arc::new(buf))),
                '\\' => {
                    if let Some(c) = self.read_string_escape()? {
                        buf.push(c);
                    }
                }
                _ => buf.push(c),
            }
        }

        Err(TokenError::IncompleteString)
    }

    fn read_string_escape(&mut self) -> Result<Option<char>> {
        let c = match self.chars.peek() {
            Some('"') => {
                self.eat();
                '"'
            }

            Some('a') => {
                self.eat();
                '\x07'
            }

            Some('b') => {
                self.eat();
                '\x08'
            }

            Some('\\') => {
                self.eat();
                '\\'
            }

            Some('|') => {
                self.eat();
                '|'
            }

            Some('t') => {
                self.eat();
                '\t'
            }

            Some('n') => {
                self.eat();
                '\n'
            }

            Some('r') => {
                self.eat();
                '\r'
            }

            Some('0') => {
                self.eat();
                '\0'
            }

            Some(&code @ ('x' | 'u')) => {
                self.eat();

                let mut digits = String::new();

                let braces = match self.chars.peek().copied() {
                    Some('{') if code == 'u' => {
                        self.eat();
                        true
                    }
                    _ => false,
                };

                loop {
                    let Some(c) = self.eat() else {
                        return Err(TokenError::MalformedByteEscape);
                    };

                    match c {
                        ';' if !braces => break,
                        '}' if braces => break,
                        c if c.is_ascii_digit() => {
                            digits.push(c);
                        }
                        'a'..='f' | 'A'..='F' => {
                            digits.push(c);
                        }
                        _ => return Err(TokenError::MalformedByteEscape),
                    }
                }

                let codepoint = u32::from_str_radix(&digits, 16)
                    .map_err(|_| TokenError::MalformedByteEscape)?;
                let char = char::from_u32(codepoint).ok_or(TokenError::MalformedByteEscape)?;

                char
            }

            Some(&start @ (' ' | '\t' | '\n')) => {
                self.eat();

                let mut trimming = start == '\n';

                loop {
                    let Some(c) = self.chars.peek() else {
                        return Err(TokenError::IncompleteString);
                    };

                    match c {
                        ' ' | '\t' => {
                            self.eat();
                        }
                        '\n' if !trimming => {
                            self.eat();
                            trimming = true;
                        }
                        _ if trimming => return Ok(None),

                        _ => return Err(TokenError::InvalidEscape),
                    }
                }
            }

            Some(_) => return Err(TokenError::InvalidEscape),

            None => return Err(TokenError::IncompleteString),
        };

        Ok(Some(c))
    }

    fn read_hash_value(&mut self) -> Result<TokenType<Cow<'a, str>>> {
        fn parse_char(slice: &str) -> Option<char> {
            use std::str::FromStr;

            debug_assert!(slice.len() > 2);

            match &slice[2..] {
                s if s.eq_ignore_ascii_case("alarm") => Some('\x07'),
                s if s.eq_ignore_ascii_case("backspace") => Some('\x08'),
                s if s.eq_ignore_ascii_case("delete") => Some('\x7F'),
                s if s.eq_ignore_ascii_case("escape") => Some('\x1B'),
                s if s.eq_ignore_ascii_case("newline") => Some('\n'),
                s if s.eq_ignore_ascii_case("null") => Some('\0'),
                s if s.eq_ignore_ascii_case("return") => Some('\r'),
                s if s.eq_ignore_ascii_case("space") => Some(' '),
                s if s.eq_ignore_ascii_case("tab") => Some('\t'),
                "\\" => Some('\\'),
                ")" => Some(')'),
                "]" => Some(']'),
                "[" => Some('['),
                "(" => Some('('),
                "^" => Some('^'),

                character => {
                    let first = character.as_bytes()[0];

                    let escape = (first == b'u' || first == b'x') && slice.len() > 3;

                    if !escape {
                        return char::from_str(character).ok();
                    }

                    let payload = if first == b'u' && character.as_bytes().get(1) == Some(&b'{') {
                        if character.as_bytes().last() != Some(&b'}') {
                            return None;
                        }

                        &character[2..(character.len() - 1)]
                    } else {
                        &character[1..]
                    };

                    let code = u32::from_str_radix(payload, 16).ok()?;

                    char::from_u32(code)
                }
            }
        }

        while let Some(&c) = self.chars.peek() {
            match c {
                '\\' => {
                    self.eat();
                    self.eat();
                }
                '\'' | '`' => {
                    self.eat();
                    break;
                }

                ',' => {
                    self.eat();
                    if Some('@') == self.chars.peek().copied() {
                        self.eat();
                        break;
                    } else {
                        break;
                    }
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

            keyword if keyword.starts_with("#:") => Ok(TokenType::Keyword(self.slice().into())),

            character if character.starts_with("#\\") => {
                if character.len() <= 2 {
                    return Err(TokenError::InvalidCharacter);
                }

                if let Some(parsed_character) = parse_char(character) {
                    Ok(TokenType::CharacterLiteral(parsed_character))
                } else {
                    Err(TokenError::InvalidCharacter)
                }
            }

            "#" if self.chars.peek() == Some(&'(') => {
                self.eat();
                Ok(TokenType::OpenParen(Paren::Round, Some(ParenMod::Vector)))
            }

            "#u8" if self.chars.peek() == Some(&'(') => {
                self.eat();
                Ok(TokenType::OpenParen(Paren::Round, Some(ParenMod::Bytes)))
            }

            _ => self.read_word(),
        }
    }

    fn read_number(&mut self) -> Result<TokenType<Cow<'a, str>>> {
        while let Some(&c) = self.chars.peek() {
            match c {
                c if c.is_ascii_digit() => {
                    self.eat();
                }
                '+' | '-' | '.' | '/' | 'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e'
                | 'E' | 'f' | 'F' | 'i' | 'n' => {
                    self.eat();
                }
                '(' | ')' | '[' | ']' => {
                    return if let Some(t) = parse_number(self.slice()) {
                        Ok(t.into())
                    } else {
                        self.read_word()
                    }
                }
                c if c.is_whitespace() => {
                    return if let Some(t) = parse_number(self.slice()) {
                        Ok(t.into())
                    } else {
                        self.read_word()
                    }
                }
                _ => return self.read_word(),
            }
        }
        match parse_number(self.slice()) {
            Some(n) => Ok(n.into()),
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

    fn read_word(&mut self) -> Result<TokenType<Cow<'a, str>>> {
        let escaped_identifier = self.chars.peek().copied() == Some('|');

        if escaped_identifier {
            self.eat();
        }

        let mut ident_buffer = IdentBuffer::new(self.chars.clone());

        while let Some(&c) = self.chars.peek() {
            match c {
                '|' if escaped_identifier => {
                    self.eat();

                    break;
                }
                '\\' if escaped_identifier => {
                    self.eat();

                    let escaped = self.read_string_escape().map_err(|err| match err {
                        TokenError::IncompleteString => TokenError::IncompleteIdentifier,
                        err => err,
                    })?;

                    ident_buffer.push_escape(escaped);
                }
                c if escaped_identifier => {
                    ident_buffer.push(c);
                    self.eat();
                }
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

        let token = match self.slice() {
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
            identifier => {
                debug_assert!(!identifier.is_empty());

                match identifier.as_bytes() {
                    [b'+', _, ..] if self.queued.is_none() => {
                        self.queued = Some(TokenType::Identifier((&identifier[1..]).into()));
                        TokenType::Identifier("+".into())
                    }
                    [b'|', .., b'|'] if escaped_identifier => {
                        if ident_buffer.ident.is_empty() {
                            TokenType::Identifier((&identifier[1..identifier.len() - 1]).into())
                        } else {
                            TokenType::Identifier(ident_buffer.ident.into())
                        }
                    }
                    _ if escaped_identifier => {
                        return Err(TokenError::IncompleteIdentifier);
                    }
                    _ => TokenType::Identifier(identifier.into()),
                }
            }
        };

        Ok(token)
    }

    fn read_nestable_comment(&mut self) -> Result<TokenType<Cow<'a, str>>> {
        self.eat();

        let mut depth = 1;

        while let Some(c) = self.eat() {
            match c {
                '|' => {
                    if self.chars.peek().copied() == Some('#') {
                        self.eat();
                        depth -= 1;

                        if depth == 0 {
                            return Ok(TokenType::Comment);
                        }
                    }
                }
                '#' => {
                    if self.chars.peek().copied() == Some('|') {
                        self.eat();
                        depth += 1;
                    }
                }
                _ => {}
            }
        }

        Err(TokenError::IncompleteComment)
    }
}

struct IdentBuffer<'a> {
    chars: Peekable<Chars<'a>>,
    ident: String,
    // works as Either:
    //  - Ok: saw a non-trivial escape, buffering into ident
    //  - Err: "trivial" string, keeping count of its len
    mode: std::result::Result<(), usize>,
}

impl<'a> IdentBuffer<'a> {
    fn new(chars: Peekable<Chars<'a>>) -> Self {
        Self {
            chars,
            ident: Default::default(),
            mode: Err(0),
        }
    }

    fn push(&mut self, c: char) {
        if let Err(len) = self.mode.as_mut() {
            *len += 1;
        } else {
            self.ident.push(c);
        }
    }

    fn push_escape(&mut self, c: Option<char>) {
        if let Err(len) = self.mode {
            self.ident.extend(self.chars.clone().take(len));
            self.mode = Ok(());
        }

        if let Some(c) = c {
            self.ident.push(c);
        }
    }
}

fn strip_shebang_line(input: &str) -> (&str, usize, usize) {
    if input.starts_with("#!") {
        let stripped = input.trim_start_matches("#!");
        let result = match stripped.char_indices().skip_while(|x| x.1 != '\n').next() {
            Some((pos, _)) => &stripped[pos..],
            None => "",
        };

        let original = input.len();
        let new = result.len();

        (
            result,
            original - new,
            input.as_bytes().len() - result.as_bytes().len(),
        )
    } else {
        (input, 0, 0)
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
    pub(crate) lexer: Lexer<'a>,
    skip_comments: bool,
    source_id: Option<SourceId>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, source_id: Option<SourceId>) -> Self {
        let (_, char_offset, bytes_offset) = strip_shebang_line(input);

        let mut res = Self {
            lexer: Lexer::new(input),
            skip_comments,
            source_id, // skip_doc_comments,
        };

        res.lexer.token_start += bytes_offset;
        res.lexer.token_end += bytes_offset;

        for _ in 0..char_offset {
            res.lexer.chars.next();
        }

        res
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
    pub(crate) stream: TokenStream<'a>,
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
    type Item = Token<'a, Cow<'a, str>>;

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
    IncompleteIdentifier,
    IncompleteComment,
    InvalidEscape,
    InvalidCharacter,
    MalformedHexInteger,
    MalformedOctalInteger,
    MalformedBinaryInteger,
    MalformedByteEscape,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenType<Cow<'a, str>>>;

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

            Some(&paren @ ('(' | '[' | '{')) => {
                self.eat();
                let kind = match paren {
                    '[' => Paren::Square,
                    '{' => Paren::Curly,
                    _ => Paren::Round,
                };
                Some(Ok(TokenType::OpenParen(kind, None)))
            }

            Some(&paren @ (')' | ']' | '}')) => {
                self.eat();
                let kind = match paren {
                    ']' => Paren::Square,
                    '}' => Paren::Curly,
                    _ => Paren::Round,
                };
                Some(Ok(TokenType::CloseParen(kind)))
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
                Some(self.read_number())
            }
            Some('#') => {
                self.eat();
                let next = self.chars.peek().copied();

                let token = match next {
                    Some('x' | 'X' | 'd' | 'D' | 'o' | 'O' | 'b' | 'B') => {
                        self.eat();
                        self.read_number()
                    }
                    Some('|') => self.read_nestable_comment(),
                    Some(';') => {
                        self.eat();
                        Ok(TokenType::DatumComment)
                    }
                    _ => self.read_hash_value(),
                };

                Some(token)
            }

            Some(c) if !c.is_whitespace() && !c.is_ascii_digit() || *c == '_' => {
                Some(self.read_word())
            }
            Some(c) if c.is_ascii_digit() => Some(self.read_number()),
            Some(_) => self.eat().map(|e| Err(TokenError::UnexpectedChar(e))),
            None => None,
        }
    }
}

// Split the string by + and -. Returns at most 2 elements or `None` if there were more than 2.
fn split_into_complex<'a>(s: &'a str) -> Option<SmallVec<[NumPart<'a>; 2]>> {
    let classify_num_part = |s: &'a str| -> NumPart<'a> {
        match s.as_bytes().last() {
            Some(b'i') => NumPart::Imaginary(&s[..s.len() - 1]),
            _ => NumPart::Real(s),
        }
    };

    let mut idxs = SmallVec::<[usize; 3]>::new();

    let mut chars = s.char_indices();
    while let Some((idx, ch)) = chars.next() {
        if ch == '+' || ch == '-' {
            if idxs.len() == 2 {
                return None;
            } else {
                idxs.push(idx);
            }
        } else if ch == 'e' || ch == 'E' {
            // ignore any + or - after an e
            let _ = chars.next();
        }
    }

    let parts = match idxs.as_slice() {
        [] | [0] => smallvec![classify_num_part(s)],
        [idx] | [0, idx] => smallvec![
            classify_num_part(&s[0..*idx]),
            classify_num_part(&s[*idx..])
        ],
        _ => return None,
    };
    Some(parts)
}

#[derive(Debug)]
enum NumPart<'a> {
    Real(&'a str),
    Imaginary(&'a str),
}

fn parse_real(s: &str, radix: u32) -> Option<RealLiteral> {
    if s == NEG_INFINITY {
        return Some(RealLiteral::Float(f64::NEG_INFINITY));
    } else if s == INFINITY {
        return Some(RealLiteral::Float(f64::INFINITY));
    } else if s == NAN || s == NEG_NAN {
        return Some(RealLiteral::Float(f64::NAN));
    }

    let mut has_e = false;
    let mut has_dot = false;
    let mut frac_position = None;
    for (idx, ch) in s.chars().enumerate() {
        match ch {
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
        if radix != 10 {
            // radix for floating points not yet supported
            return None;
        }

        s.parse().map(RealLiteral::Float).ok()
    } else if let Some(p) = frac_position {
        let (n_str, d_str) = s.split_at(p);
        let d_str = &d_str[1..];
        let n = IntLiteral::from_str_radix(n_str, radix).ok()?;
        let d = IntLiteral::from_str_radix(d_str, radix).ok()?;
        Some(RealLiteral::Rational(n, d))
    } else {
        let int = IntLiteral::from_str_radix(s, radix).ok()?;
        Some(RealLiteral::Int(int))
    }
}

fn parse_number(s: &str) -> Option<NumberLiteral> {
    let (s, radix) = match s.get(0..2) {
        Some("#x" | "#X") => (&s[2..], 16),
        Some("#d" | "#D") => (&s[2..], 10),
        Some("#o" | "#O") => (&s[2..], 8),
        Some("#b" | "#B") => (&s[2..], 2),
        _ => (s, 10),
    };

    match split_into_complex(s)?.as_slice() {
        [NumPart::Real(x)] => parse_real(x, radix).map(NumberLiteral::from),
        [NumPart::Imaginary(x)] => {
            if !matches!(x.as_bytes().first(), Some(b'+') | Some(b'-')) {
                return None;
            };
            Some(NumberLiteral::Complex(IntLiteral::Small(0).into(), parse_real(x, radix)?).into())
        }
        [NumPart::Real(re), NumPart::Imaginary(im)] => Some(NumberLiteral::Complex(
            parse_real(re, radix)?,
            parse_real(im, radix)?,
        )),
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

    fn identifier(ident: &str) -> TokenType<Cow<str>> {
        Identifier(ident.into())
    }

    // TODO: Figure out why this just cause an infinite loop when parsing it?
    #[test]
    fn test_identifier_with_quote_end() {
        let s = TokenStream::new(
            "        (define (stream-cdr stream)
            ((stream-cdr' stream)))
",
            true,
            SourceId::none(),
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
            SourceId::none(),
        );

        for token in s {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_escape_in_string() {
        let s = TokenStream::new(r#"(display "}\n")"#, true, SourceId::none());

        for token in s {
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_quote_within_word() {
        let mut s = TokenStream::new("'foo\\'a", true, SourceId::none());

        println!("{:?}", s.next());
        println!("{:?}", s.next());
        println!("{:?}", s.next());
    }

    #[test]
    fn test_single_period() {
        let mut s = TokenStream::new(".", true, SourceId::none());

        println!("{:?}", s.next());
    }

    #[test]
    fn test_chars() {
        let mut s = TokenStream::new("#\\a #\\b #\\λ", true, SourceId::none());

        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('a'),
                source: "#\\a",
                span: Span::new(0, 3, SourceId::none())
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('b'),
                source: "#\\b",
                span: Span::new(4, 7, SourceId::none())
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CharacterLiteral('λ'),
                source: "#\\λ",
                span: Span::new(8, 12, SourceId::none())
            })
        );
    }

    #[test]
    fn test_unicode_escapes() {
        let mut s = TokenStream::new(
            r#"  #\xAb #\u{0D300} #\u0540 "\x00D;" "\u1044;" "\u{045}"  "#,
            true,
            SourceId::none(),
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: CharacterLiteral('«'),
                source: r#"#\xAb"#,
                span: Span::new(2, 7, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: CharacterLiteral('팀'),
                source: r#"#\u{0D300}"#,
                span: Span::new(8, 18, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: CharacterLiteral('Հ'),
                source: r#"#\u0540"#,
                span: Span::new(19, 26, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: StringLiteral(Arc::from("\r".to_string())),
                source: r#""\x00D;""#,
                span: Span::new(27, 35, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: StringLiteral(Arc::from("၄".to_string())),
                source: r#""\u1044;""#,
                span: Span::new(36, 45, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: StringLiteral(Arc::from("E".to_string())),
                source: r#""\u{045}""#,
                span: Span::new(46, 55, SourceId::none())
            }
        );
    }

    #[test]
    fn test_invalid_unicode_escapes() {
        let tokens = [
            r#" #\xd820 "#,
            r#" #\u{1 "#,
            r#" "\xabx" "#,
            r#" "\u0045" "#,
            r#" #\xaaaaaaaa " "#,
            r#" "\u{ffffffff}" "#,
            r#" #\u{} "#,
        ];

        for token in tokens {
            let mut s = TokenStream::new(token, true, SourceId::none());

            assert_eq!(s.next().unwrap().ty, Error, "{:?} should be invalid", token);
        }
    }

    #[test]
    fn test_string_newlines() {
        let mut s = TokenStream::new(
            " \"foo\nbar\" \"foo \\  \n   bar\" ",
            true,
            SourceId::none(),
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: StringLiteral(Arc::from("foo\nbar".to_string())),
                source: "\"foo\nbar\"",
                span: Span::new(1, 10, SourceId::none())
            }
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: StringLiteral(Arc::from("foo bar".to_string())),
                source: "\"foo \\  \n   bar\"",
                span: Span::new(11, 27, SourceId::none())
            }
        );
    }

    #[test]
    fn test_unexpected_char() {
        let mut s = TokenStream::new("($)", true, SourceId::none());
        assert_eq!(
            s.next(),
            Some(Token {
                ty: OpenParen(Paren::Round, None),
                source: "(",
                span: Span::new(0, 1, SourceId::none())
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: identifier("$"),
                source: "$",
                span: Span::new(1, 2, SourceId::none())
            })
        );
        assert_eq!(
            s.next(),
            Some(Token {
                ty: CloseParen(Paren::Round),
                source: ")",
                span: Span::new(2, 3, SourceId::none())
            })
        );
    }

    #[test]
    fn test_words() {
        let mut s = TokenStream::new("foo FOO _123_ Nil #f #t", true, SourceId::none());

        assert_eq!(
            s.next(),
            Some(Token {
                ty: identifier("foo"),
                source: "foo",
                span: Span::new(0, 3, SourceId::none())
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: identifier("FOO"),
                source: "FOO",
                span: Span::new(4, 7, SourceId::none())
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: identifier("_123_"),
                source: "_123_",
                span: Span::new(8, 13, SourceId::none())
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: identifier("Nil"),
                source: "Nil",
                span: Span::new(14, 17, SourceId::none())
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(false),
                source: "#f",
                span: Span::new(18, 20, SourceId::none())
            })
        );

        assert_eq!(
            s.next(),
            Some(Token {
                ty: BooleanLiteral(true),
                source: "#t",
                span: Span::new(21, 23, SourceId::none())
            })
        );

        assert_eq!(s.next(), None);
    }

    #[test]
    fn test_almost_literals() {
        let got: Vec<_> =
            TokenStream::new("1e 1ee 1.2e5.4 1E10/4 1.45# 3- e10", true, SourceId::none())
                .collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: identifier("1e"),
                    source: "1e",
                    span: Span::new(0, 2, SourceId::none()),
                },
                Token {
                    ty: identifier("1ee"),
                    source: "1ee",
                    span: Span::new(3, 6, SourceId::none()),
                },
                Token {
                    ty: identifier("1.2e5.4"),
                    source: "1.2e5.4",
                    span: Span::new(7, 14, SourceId::none()),
                },
                Token {
                    ty: identifier("1E10/4"),
                    source: "1E10/4",
                    span: Span::new(15, 21, SourceId::none()),
                },
                Token {
                    ty: identifier("1.45#"),
                    source: "1.45#",
                    span: Span::new(22, 27, SourceId::none()),
                },
                Token {
                    ty: identifier("3-"),
                    source: "3-",
                    span: Span::new(28, 30, SourceId::none()),
                },
                Token {
                    ty: identifier("e10"),
                    source: "e10",
                    span: Span::new(31, 34, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_real_numbers() {
        let got: Vec<_> = TokenStream::new(
            "0 -0 -1.2 +2.3 999 1. 1e2 1E2 1.2e2 1.2E2 +inf.0 -inf.0 2e-4 2e+10",
            true,
            SourceId::none(),
        )
        .collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: IntLiteral::Small(0).into(),
                    source: "0",
                    span: Span::new(0, 1, SourceId::none()),
                },
                Token {
                    ty: IntLiteral::Small(0).into(),
                    source: "-0",
                    span: Span::new(2, 4, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(-1.2).into(),
                    source: "-1.2",
                    span: Span::new(5, 9, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(2.3).into(),
                    source: "+2.3",
                    span: Span::new(10, 14, SourceId::none()),
                },
                Token {
                    ty: IntLiteral::Small(999).into(),
                    source: "999",
                    span: Span::new(15, 18, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(1.0).into(),
                    source: "1.",
                    span: Span::new(19, 21, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(100.0).into(),
                    source: "1e2",
                    span: Span::new(22, 25, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(100.0).into(),
                    source: "1E2",
                    span: Span::new(26, 29, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(120.0).into(),
                    source: "1.2e2",
                    span: Span::new(30, 35, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(120.0).into(),
                    source: "1.2E2",
                    span: Span::new(36, 41, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(f64::INFINITY).into(),
                    source: "+inf.0",
                    span: Span::new(42, 48, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(f64::NEG_INFINITY).into(),
                    source: "-inf.0",
                    span: Span::new(49, 55, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(2e-4).into(),
                    source: "2e-4",
                    span: Span::new(56, 60, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Float(2e+10).into(),
                    source: "2e+10",
                    span: Span::new(61, 66, SourceId::none())
                }
            ]
        );
    }

    #[test]
    fn test_nan() {
        // nan does not equal nan so we have to run the is_nan predicate.
        let got = TokenStream::new("+nan.0", true, SourceId::none())
            .next()
            .unwrap();

        match got.ty {
            TokenType::Number(n) => {
                assert!(matches!(*n, NumberLiteral::Real(RealLiteral::Float(x)) if x.is_nan()))
            }

            _ => panic!("Didn't match"),
        }

        let got = TokenStream::new("-nan.0", true, None).next().unwrap();

        match got.ty {
            TokenType::Number(n) => {
                assert!(matches!(*n, NumberLiteral::Real(RealLiteral::Float(x)) if x.is_nan()))
            }

            _ => panic!("Didn't match"),
        }
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
            SourceId::none(),
        )
        .collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(4)).into(),
                    source: "1/4",
                    span: Span::new(17, 20, SourceId::none()),
                },
                Token {
                    ty: OpenParen(Paren::Round, None),
                    source: "(",
                    span: Span::new(37, 38, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(4)).into(),
                    source: "1/4",
                    span: Span::new(38, 41, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(3)).into(),
                    source: "1/3",
                    span: Span::new(42, 45, SourceId::none()),
                },
                Token {
                    ty: CloseParen(Paren::Round),
                    source: ")",
                    span: Span::new(45, 46, SourceId::none()),
                },
                Token {
                    ty: RealLiteral::Rational(
                        IntLiteral::from_str("11111111111111111111").unwrap(),
                        IntLiteral::from_str("22222222222222222222").unwrap(),
                    )
                    .into(),
                    source: "11111111111111111111/22222222222222222222",
                    span: Span::new(63, 104, SourceId::none()),
                },
                Token {
                    ty: identifier("/"),
                    source: "/",
                    span: Span::new(121, 122, SourceId::none()),
                },
                Token {
                    ty: identifier("1/"),
                    source: "1/",
                    span: Span::new(139, 141, SourceId::none()),
                },
                Token {
                    ty: identifier("1/4.0"),
                    source: "1/4.0",
                    span: Span::new(158, 163, SourceId::none()),
                },
                Token {
                    ty: identifier("1//4"),
                    source: "1//4",
                    span: Span::new(180, 184, SourceId::none()),
                },
                Token {
                    ty: IntLiteral::Small(1).into(),
                    source: "1",
                    span: Span::new(201, 202, SourceId::none()),
                },
                Token {
                    ty: identifier("/"),
                    source: "/",
                    span: Span::new(203, 204, SourceId::none()),
                },
                Token {
                    ty: IntLiteral::Small(4).into(),
                    source: "4",
                    span: Span::new(205, 206, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_complex_numbers() {
        let got: Vec<_> = TokenStream::new(
            "1+2i 3-4i +5+6i +1i 1.0+2.0i 3-4.0i +1.0i 2e+4+inf.0i -inf.0-2e-4i",
            true,
            SourceId::none(),
        )
        .collect();
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
                    span: Span::new(0, 4, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(3).into(),
                        IntLiteral::Small(-4).into()
                    )
                    .into(),
                    source: "3-4i",
                    span: Span::new(5, 9, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(5).into(),
                        IntLiteral::Small(6).into()
                    )
                    .into(),
                    source: "+5+6i",
                    span: Span::new(10, 15, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        IntLiteral::Small(1).into()
                    )
                    .into(),
                    source: "+1i",
                    span: Span::new(16, 19, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        RealLiteral::Float(1.0).into(),
                        RealLiteral::Float(2.0).into()
                    )
                    .into(),
                    source: "1.0+2.0i",
                    span: Span::new(20, 28, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(3).into(),
                        RealLiteral::Float(-4.0).into()
                    )
                    .into(),
                    source: "3-4.0i",
                    span: Span::new(29, 35, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        RealLiteral::Float(1.0).into()
                    )
                    .into(),
                    source: "+1.0i",
                    span: Span::new(36, 41, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        RealLiteral::Float(2e+4),
                        RealLiteral::Float(f64::INFINITY),
                    )
                    .into(),
                    source: "2e+4+inf.0i",
                    span: Span::new(42, 53, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        RealLiteral::Float(f64::NEG_INFINITY),
                        RealLiteral::Float(-2e-4),
                    )
                    .into(),
                    source: "-inf.0-2e-4i",
                    span: Span::new(54, 66, SourceId::none()),
                }
            ]
        );
    }

    #[test]
    fn test_malformed_complex_numbers_are_identifiers() {
        let got: Vec<_> =
            TokenStream::new("i -i 1i+1i 4+i -4+-2i", true, SourceId::none()).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: identifier("i"),
                    source: "i",
                    span: Span::new(0, 1, SourceId::none()),
                },
                Token {
                    ty: identifier("-i"),
                    source: "-i",
                    span: Span::new(2, 4, SourceId::none()),
                },
                Token {
                    ty: identifier("1i+1i"),
                    source: "1i+1i",
                    span: Span::new(5, 10, SourceId::none()),
                },
                Token {
                    ty: identifier("4+i"),
                    source: "4+i",
                    span: Span::new(11, 14, SourceId::none()),
                },
                Token {
                    ty: identifier("-4+-2i"),
                    source: "-4+-2i",
                    span: Span::new(15, 21, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_string() {
        let got: Vec<_> =
            TokenStream::new(r#" "" "Foo bar" "\"\\" "#, true, SourceId::none()).collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: StringLiteral(Arc::new(r#""#.to_string())),
                    source: r#""""#,
                    span: Span::new(1, 3, SourceId::none()),
                },
                Token {
                    ty: StringLiteral(Arc::new(r#"Foo bar"#.to_string())),
                    source: r#""Foo bar""#,
                    span: Span::new(4, 13, SourceId::none()),
                },
                Token {
                    ty: StringLiteral(Arc::new(r#""\"#.to_string())),
                    source: r#""\"\\""#,
                    span: Span::new(14, 20, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_comment() {
        let mut s = TokenStream::new(";!/usr/bin/gate\n   ; foo\n", true, SourceId::none());
        assert_eq!(s.next(), None);
    }

    #[test]
    fn function_definition() {
        let s = TokenStream::new(
            "(define odd-rec? (lambda (x) (if (= x 0) #f (even-rec? (- x 1)))))",
            true,
            SourceId::none(),
        );
        let res: Vec<Token<Cow<str>>> = s.collect();

        println!("{:#?}", res);
    }

    #[test]
    fn lex_string_with_escape_chars() {
        let s = TokenStream::new("\"\0\0\0\"", true, SourceId::none());
        let res: Vec<Token<Cow<str>>> = s.collect();
        println!("{:#?}", res);
    }

    #[test]
    fn scheme_statement() {
        let s = TokenStream::new("(apples (function a b) (+ a b))", true, SourceId::none());
        let res: Vec<Token<Cow<str>>> = s.collect();

        let expected: Vec<Token<Cow<str>>> = vec![
            Token {
                ty: OpenParen(Paren::Round, None),
                source: "(",
                span: Span::new(0, 1, SourceId::none()),
            },
            Token {
                ty: identifier("apples"),
                source: "apples",
                span: Span::new(1, 7, SourceId::none()),
            },
            Token {
                ty: OpenParen(Paren::Round, None),
                source: "(",
                span: Span::new(8, 9, SourceId::none()),
            },
            Token {
                ty: identifier("function"),
                source: "function",
                span: Span::new(9, 17, SourceId::none()),
            },
            Token {
                ty: identifier("a"),
                source: "a",
                span: Span::new(18, 19, SourceId::none()),
            },
            Token {
                ty: identifier("b"),
                source: "b",
                span: Span::new(20, 21, SourceId::none()),
            },
            Token {
                ty: CloseParen(Paren::Round),
                source: ")",
                span: Span::new(21, 22, SourceId::none()),
            },
            Token {
                ty: OpenParen(Paren::Round, None),
                source: "(",
                span: Span::new(23, 24, SourceId::none()),
            },
            Token {
                ty: identifier("+"),
                source: "+",
                span: Span::new(24, 25, SourceId::none()),
            },
            Token {
                ty: identifier("a"),
                source: "a",
                span: Span::new(26, 27, SourceId::none()),
            },
            Token {
                ty: identifier("b"),
                source: "b",
                span: Span::new(28, 29, SourceId::none()),
            },
            Token {
                ty: CloseParen(Paren::Round),
                source: ")",
                span: Span::new(29, 30, SourceId::none()),
            },
            Token {
                ty: CloseParen(Paren::Round),
                source: ")",
                span: Span::new(30, 31, SourceId::none()),
            },
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_bigint() {
        let s = TokenStream::new("9223372036854775808", true, SourceId::none()); // isize::MAX + 1
        let res: Vec<Token<Cow<str>>> = s.collect();

        let expected_bigint = Box::new("9223372036854775808".parse().unwrap());

        let expected: Vec<Token<Cow<str>>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "9223372036854775808",
            span: Span::new(0, 19, SourceId::none()),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn negative_test_bigint() {
        let s = TokenStream::new("-9223372036854775809", true, SourceId::none()); // isize::MIN - 1
        let res: Vec<Token<Cow<str>>> = s.collect();

        let expected_bigint = Box::new("-9223372036854775809".parse().unwrap());

        let expected: Vec<Token<Cow<str>>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "-9223372036854775809",
            span: Span::new(0, 20, SourceId::none()),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn identifier_test() {
        let s = TokenStream::new("a b(c`d'e\"www\"f,g;", true, SourceId::none());

        let tokens: Vec<(TokenType<Cow<str>>, &str)> =
            s.map(|token| (token.ty, token.source)).collect();

        assert_eq!(tokens[0], (identifier("a"), "a"));
        assert_eq!(tokens[1], (identifier("b"), "b"));
        assert_eq!(tokens[3], (identifier("c"), "c"));
        assert_eq!(tokens[5], (identifier("d"), "d"));
        assert_eq!(tokens[7], (identifier("e"), "e"));
        assert_eq!(tokens[9], (identifier("f"), "f"));
        assert_eq!(tokens[11], (identifier("g"), "g"));
    }

    #[test]
    fn vector_test() {
        let s = TokenStream::new("a b #(c d)", true, None);

        let tokens: Vec<(TokenType<Cow<str>>, &str)> =
            s.map(|token| (token.ty, token.source)).collect();

        assert_eq!(tokens[0], (identifier("a"), "a"));
        assert_eq!(tokens[1], (identifier("b"), "b"));
        assert_eq!(
            tokens[2],
            (
                TokenType::OpenParen(Paren::Round, Some(ParenMod::Vector)),
                "#("
            )
        );
        assert_eq!(tokens[3], (identifier("c"), "c"));
        assert_eq!(tokens[4], (identifier("d"), "d"));
    }

    #[test]
    fn bytevector_test() {
        let s = TokenStream::new("a b #u8(1 2)", true, None);

        let tokens: Vec<(TokenType<Cow<str>>, &str)> =
            s.map(|token| (token.ty, token.source)).collect();

        assert_eq!(tokens[0], (identifier("a"), "a"));
        assert_eq!(tokens[1], (identifier("b"), "b"));
        assert_eq!(
            tokens[2],
            (
                TokenType::OpenParen(Paren::Round, Some(ParenMod::Bytes)),
                "#u8("
            )
        );
        assert_eq!(tokens[5], (TokenType::CloseParen(Paren::Round), ")"));
    }

    #[test]
    fn special_comments_test() {
        let mut lexer = Lexer::new("#| f(\n [ |#");
        assert_eq!(lexer.next(), Some(Ok(TokenType::Comment)));

        let mut lexer = Lexer::new("#| a #| ( |# |#");
        assert_eq!(lexer.next(), Some(Ok(TokenType::Comment)));

        let mut lexer = Lexer::new("#;(a b)");
        assert_eq!(lexer.next(), Some(Ok(TokenType::DatumComment)));

        let mut lexer = Lexer::new("#; #(#true 3)");
        assert_eq!(lexer.next(), Some(Ok(TokenType::DatumComment)));

        let mut lexer = Lexer::new("#; #; 3 5");
        assert_eq!(lexer.next(), Some(Ok(TokenType::DatumComment)));
    }

    #[test]
    fn comment_error_test() {
        let mut lexer = Lexer::new("#|");

        assert_eq!(lexer.next().unwrap(), Err(TokenError::IncompleteComment));
    }

    #[test]
    fn escaped_identifier_test() {
        let mut s = TokenStream::new(r#"|a| |a b| |\x61;| |.|"#, true, SourceId::none());

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: identifier("a"),
                source: "|a|",
                span: Span::new(0, 3, None),
            },
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: identifier("a b"),
                source: "|a b|",
                span: Span::new(4, 9, None),
            },
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: identifier("a"),
                source: r#"|\x61;|"#,
                span: Span::new(10, 17, None),
            },
        );

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: identifier("."),
                source: "|.|",
                span: Span::new(18, 21, None),
            },
        );

        let mut s = TokenStream::new("|a\\\nb|", true, SourceId::none());

        assert_eq!(
            s.next().unwrap(),
            Token {
                ty: identifier("ab"),
                source: "|a\\\nb|",
                span: Span::new(0, 6, None),
            },
        );
    }
}
