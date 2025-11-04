use super::parser::SourceId;
use crate::interner::InternedString;
use crate::tokens::{IntLiteral, Token, TokenLike, TokenType};
use crate::tokens::{NumberLiteral, Paren, ParenMod, RealLiteral};
use alloc::{borrow::Cow, string::String};
use alloc::sync::Arc;
use core::char;
use core::iter::Iterator;
use core::ops::Range;
use core::{iter::Peekable, str::Chars};
use num_bigint::BigInt;
use smallvec::{smallvec, SmallVec};

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
    queued: Option<TokenType<InternedString>>,
    token_start: u32,
    token_end: u32,
    error: Range<u32>,

    ident_buffer: String,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            queued: None,
            token_start: 0,
            token_end: 0,
            error: Default::default(),
            ident_buffer: String::new(),
        }
    }

    fn eat(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.token_end += c.len_utf8() as u32;
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

    fn read_string(&mut self) -> Result<TokenType<InternedString>> {
        // Skip the opening quote.
        self.eat();

        let mut buf = String::new();

        while let Some(&c) = self.chars.peek() {
            self.eat();
            match c {
                '"' => return Ok(TokenType::StringLiteral(Arc::new(buf))),
                '\\' => {
                    if let Some(c) = self.read_string_escape(TokenError::IncompleteString, '"')? {
                        buf.push(c);
                    }
                }
                _ => buf.push(c),
            }
        }

        Err(TokenError::IncompleteString)
    }

    fn read_string_escape(&mut self, incomplete: TokenError, delim: char) -> Result<Option<char>> {
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
                let start = self.token_end - 2;

                let mut digits = String::new();

                let escape_end = match self.chars.peek().copied() {
                    Some('{') if code == 'u' => {
                        self.eat();
                        '}'
                    }
                    _ => ';',
                };

                let valid = loop {
                    let Some(c) = self.eat() else {
                        return Err(incomplete);
                    };

                    match c {
                        c if c == escape_end => break true,
                        // note that this overlaps partially with `escapeEnd`
                        ';' | '\\' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' => break false,
                        c if c == delim => break false,
                        _ => digits.push(c),
                    }
                };

                if !valid {
                    self.error = start..self.token_end - 1;

                    return Err(TokenError::UnclosedHexEscape(escape_end));
                }

                let error = start..self.token_end;

                let codepoint = u32::from_str_radix(&digits, 16)
                    .map_err(TokenError::InvalidHexEscapeLiteral)
                    .inspect_err(|_| self.error = error.clone())?;

                char::from_u32(codepoint)
                    .ok_or(TokenError::InvalidHexCodePoint(codepoint))
                    .inspect_err(|_| self.error = error)?
            }

            Some(&start @ (' ' | '\t' | '\n')) => {
                self.eat();

                let mut trimming = start == '\n';

                loop {
                    let Some(c) = self.chars.peek() else {
                        return Err(incomplete);
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

                        c => {
                            self.error = self.token_end..(self.token_end + c.len_utf8() as u32);
                            return Err(TokenError::InvalidWhitespace);
                        }
                    }
                }
            }

            Some(c) => {
                self.error = (self.token_end - 1)..(self.token_end + c.len_utf8() as u32);
                return Err(TokenError::InvalidStringEscape(*c));
            }

            None => return Err(incomplete),
        };

        Ok(Some(c))
    }

    fn read_hash_value(&mut self) -> Result<TokenType<InternedString>> {
        fn parse_char(slice: &str) -> Result<char> {
            use core::str::FromStr;

            debug_assert!(slice.len() > 2);

            match &slice[2..] {
                s if s.eq_ignore_ascii_case("alarm") => Ok('\x07'),
                s if s.eq_ignore_ascii_case("backspace") => Ok('\x08'),
                s if s.eq_ignore_ascii_case("delete") => Ok('\x7F'),
                s if s.eq_ignore_ascii_case("escape") => Ok('\x1B'),
                s if s.eq_ignore_ascii_case("newline") => Ok('\n'),
                s if s.eq_ignore_ascii_case("null") => Ok('\0'),
                s if s.eq_ignore_ascii_case("return") => Ok('\r'),
                s if s.eq_ignore_ascii_case("space") => Ok(' '),
                s if s.eq_ignore_ascii_case("tab") => Ok('\t'),

                character => {
                    let first = character.as_bytes()[0];

                    let escape = (first == b'u' || first == b'x') && slice.len() > 3;

                    if !escape {
                        return char::from_str(character).map_err(|_| TokenError::InvalidCharName);
                    }

                    let payload = if first == b'u' && character.as_bytes().get(1) == Some(&b'{') {
                        if character.as_bytes().last() != Some(&b'}') {
                            return Err(TokenError::UnclosedHexEscape('}'));
                        }

                        &character[2..(character.len() - 1)]
                    } else {
                        &character[1..]
                    };

                    let code = u32::from_str_radix(payload, 16)
                        .map_err(TokenError::InvalidHexEscapeLiteral)?;

                    char::from_u32(code).ok_or(TokenError::InvalidHexCodePoint(code))
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

                let parsed = match parse_char(character) {
                    Ok(it) => it,
                    Err(err) => {
                        self.error = self.token_start..self.token_end;
                        return Err(err);
                    }
                };

                Ok(TokenType::CharacterLiteral(parsed))
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

    fn read_number(&mut self) -> Result<TokenType<InternedString>> {
        while let Some(&c) = self.chars.peek() {
            match c {
                c if c.is_ascii_digit() => {
                    self.eat();
                }
                '+' | '-' | '.' | '/' | '@' | 'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D'
                | 'e' | 'E' | 'f' | 'F' | 'i' | 'n' => {
                    self.eat();
                }
                '(' | ')' | '[' | ']' => {
                    return if let Some(t) = try_parse_number(self.slice(), None)? {
                        Ok(t.into())
                    } else {
                        self.read_word()
                    }
                }
                c if c.is_whitespace() => {
                    return if let Some(t) = try_parse_number(self.slice(), None)? {
                        Ok(t.into())
                    } else {
                        self.read_word()
                    }
                }
                _ => return self.read_word(),
            }
        }
        match try_parse_number(self.slice(), None)? {
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

    fn read_word(&mut self) -> Result<TokenType<InternedString>> {
        let escaped_identifier = self.chars.peek().copied() == Some('|');

        if escaped_identifier {
            self.eat();
        }

        let mut buffer = core::mem::take(&mut self.ident_buffer);
        buffer.clear();

        let mut ident_buffer = IdentBuffer::new(self.chars.clone(), &mut buffer);

        while let Some(&c) = self.chars.peek() {
            match c {
                '|' if escaped_identifier => {
                    self.eat();

                    break;
                }
                '\\' if escaped_identifier => {
                    self.eat();

                    let escaped = self.read_string_escape(TokenError::IncompleteIdentifier, '|')?;

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
            "if" => TokenType::If,
            "let" => TokenType::Let,
            "define" | "defn" | "#%define" => TokenType::Define,
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
                            TokenType::Identifier(ident_buffer.ident.as_str().into())
                        }
                    }
                    _ if escaped_identifier => {
                        ident_buffer.ident.clear();
                        return Err(TokenError::IncompleteIdentifier);
                    }
                    _ => TokenType::Identifier(identifier.into()),
                }
            }
        };

        ident_buffer.ident.clear();
        self.ident_buffer = buffer;

        Ok(token)
    }

    fn read_nestable_comment(&mut self) -> Result<TokenType<InternedString>> {
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

struct IdentBuffer<'b, 'a: 'b> {
    chars: Peekable<Chars<'a>>,
    ident: &'b mut String,
    // works as Either:
    //  - Ok: saw a non-trivial escape, buffering into ident
    //  - Err: "trivial" string, keeping count of its len
    mode: core::result::Result<(), usize>,
}

impl<'b, 'a: 'b> IdentBuffer<'b, 'a> {
    fn new(chars: Peekable<Chars<'a>>, buffer: &'b mut String) -> Self {
        Self {
            chars,
            ident: buffer,
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

fn strip_shebang_line(input: &str) -> (usize, usize) {
    if input.starts_with("#!") {
        // split is guaranteed to yield at least one element
        let shebang = input.split('\n').next().unwrap();
        (shebang.chars().count(), shebang.len())
    } else {
        (0, 0)
    }
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        self.token_start as _..self.token_end as _
    }

    pub fn small_span(&self) -> core::ops::Range<u32> {
        self.token_start..self.token_end
    }

    #[inline]
    pub fn slice(&self) -> &'a str {
        self.source.get(self.span()).unwrap()
    }
}

pub struct TokenStream<'a> {
    pub(crate) lexer: Lexer<'a>,
    pub(crate) skip_comments: bool,
    source_id: Option<SourceId>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str, skip_comments: bool, source_id: Option<SourceId>) -> Self {
        let (char_offset, bytes_offset) = strip_shebang_line(input);

        let mut res = Self {
            lexer: Lexer::new(input),
            skip_comments,
            source_id, // skip_doc_comments,
        };

        res.lexer.token_start += bytes_offset as u32;
        res.lexer.token_end += bytes_offset as u32;

        for _ in 0..char_offset {
            res.lexer.chars.next();
        }

        res
    }

    pub fn into_owned(self) -> OwnedTokenStream<'a> {
        OwnedTokenStream { stream: self }
    }
}

pub struct OwnedTokenStream<'a> {
    pub(crate) stream: TokenStream<'a>,
}

impl<'a> Iterator for OwnedTokenStream<'a> {
    type Item = core::result::Result<Token<'a, InternedString>, TokenLike<'a, TokenError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.next()
    }
}

impl<'a> OwnedTokenStream<'a> {
    pub fn offset(&self) -> usize {
        self.stream.lexer.span().end
    }
}
impl<'a> Iterator for TokenStream<'a> {
    type Item = core::result::Result<Token<'a, InternedString>, TokenLike<'a, TokenError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().and_then(|token| {
            let token = match token {
                Ok(token) => token,
                Err(err) => {
                    return Some(Err(TokenLike::new(
                        err,
                        self.lexer.slice(),
                        if self.lexer.error.is_empty() {
                            self.lexer.small_span()
                        } else {
                            self.lexer.error.clone()
                        },
                        self.source_id,
                    )))
                }
            };

            let token = Token::new(
                token,
                self.lexer.slice(),
                self.lexer.small_span(),
                self.source_id,
            );
            match token.ty {
                // TokenType::Space => self.next(),
                TokenType::Comment if self.skip_comments => self.next(),
                // TokenType::DocComment if self.skip_doc_comments => self.next(),
                _ => Some(Ok(token)),
            }
        })
    }
}

pub type Result<T> = core::result::Result<T, TokenError>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    UnexpectedChar(char),
    IncompleteString,
    IncompleteIdentifier,
    IncompleteComment,
    InvalidWhitespace,
    InvalidStringEscape(char),
    InvalidCharacter,
    ZeroDenominator,
    UnclosedHexEscape(char),
    InvalidCharName,
    InvalidHexEscapeLiteral(core::num::ParseIntError),
    InvalidHexCodePoint(u32),
}

impl core::fmt::Display for TokenError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TokenError::UnexpectedChar(c) => write!(f, "unexpected char {c:?}"),
            TokenError::IncompleteString => write!(f, "incomplete string"),
            TokenError::IncompleteIdentifier => write!(f, "incomplete identifier"),
            TokenError::IncompleteComment => write!(f, "incomplete comment"),
            TokenError::InvalidWhitespace => {
                write!(f, "unexpected character, expected whitespace or newline")
            }
            TokenError::InvalidStringEscape(c) => write!(f, "invalid escape {c:?}"),
            TokenError::InvalidCharacter => write!(f, "invalid character"),
            TokenError::ZeroDenominator => {
                write!(f, "division by zero is not allowed in rational literals")
            }
            TokenError::UnclosedHexEscape(close) => {
                write!(f, "unclosed hex escape, expected {close:?}")
            }
            TokenError::InvalidCharName => write!(f, "invalid character name"),
            TokenError::InvalidHexEscapeLiteral(error) => {
                write!(f, "invalid hex escape literal, {error}")
            }
            TokenError::InvalidHexCodePoint(code) => write!(f, "invalid code point {code:x}"),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenType<InternedString>>;

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
            Some('+') | Some('-') | Some('.') => {
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
                    Some('#') => {
                        self.eat();
                        Err(TokenError::UnexpectedChar('#'))
                    }
                    _ => self.read_hash_value(),
                };

                Some(token)
            }

            Some(c) if !c.is_whitespace() && !c.is_ascii_digit() || *c == '_' => {
                Some(self.read_word())
            }
            Some(c) if c.is_ascii_digit() => Some(self.read_number()),
            Some(_) => {
                // this is very much unexpected
                debug_assert!(false);

                self.eat().map(|e| Err(TokenError::UnexpectedChar(e)))
            }
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

    let mut has_dot = false;
    let mut has_exponent = false;
    let mut frac_position = None;
    for (idx, ch) in s.chars().enumerate() {
        match ch {
            'e' | 'E' if radix < 15 => {
                if has_exponent {
                    return None;
                };
                has_exponent = true;
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

    if has_exponent || has_dot {
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

fn try_parse_number(s: &str, radix: Option<u32>) -> Result<Option<NumberLiteral>> {
    let Some(n) = parse_number(s, radix) else {
        return Ok(None);
    };

    fn validate_real_literal(lit: &RealLiteral) -> Result<()> {
        let RealLiteral::Rational(_, int) = lit else {
            return Ok(());
        };

        match int {
            IntLiteral::Small(n) if *n == 0 => Err(TokenError::ZeroDenominator),
            IntLiteral::Big(big_int) if **big_int == BigInt::ZERO => {
                Err(TokenError::ZeroDenominator)
            }
            _ => Ok(()),
        }
    }

    match &n {
        NumberLiteral::Real(real) => validate_real_literal(real)?,
        NumberLiteral::Complex(r, i) => {
            validate_real_literal(r)?;
            validate_real_literal(i)?;
        }
        NumberLiteral::Polar(r, theta) => {
            validate_real_literal(r)?;
            validate_real_literal(theta)?;
        }
    }

    Ok(Some(n))
}

pub fn parse_number(s: &str, radix: Option<u32>) -> Option<NumberLiteral> {
    let (s, radix) = match s.get(0..2) {
        Some("#x" | "#X") => (&s[2..], 16),
        Some("#d" | "#D") => (&s[2..], 10),
        Some("#o" | "#O") => (&s[2..], 8),
        Some("#b" | "#B") => (&s[2..], 2),
        _ => (s, radix.unwrap_or(10)),
    };

    if let Some((r, theta)) = s.split_once('@') {
        let r = parse_real(r, radix)?;
        let theta = parse_real(theta, radix)?;
        return Some(NumberLiteral::Polar(r, theta));
    }

    match split_into_complex(s)?.as_slice() {
        [NumPart::Real(x)] => parse_real(x, radix).map(NumberLiteral::from),
        [NumPart::Imaginary(x)] => {
            if !matches!(x.as_bytes().first(), Some(b'+') | Some(b'-')) {
                return None;
            };

            let imaginary = if *x == "+" {
                IntLiteral::Small(1).into()
            } else if *x == "-" {
                IntLiteral::Small(-1).into()
            } else {
                parse_real(x, radix)?
            };
            Some(NumberLiteral::Complex(
                IntLiteral::Small(0).into(),
                imaginary,
            ))
        }
        [NumPart::Real(re), NumPart::Imaginary(im)] => Some(NumberLiteral::Complex(
            parse_real(re, radix)?,
            if *im == "+" {
                IntLiteral::Small(1).into()
            } else if *im == "-" {
                IntLiteral::Small(-1).into()
            } else {
                parse_real(im, radix)?
            },
        )),
        _ => None,
    }
}

#[cfg(test)]
mod lexer_tests {
    use core::str::FromStr;

    use super::*;
    use crate::span::Span;
    use crate::tokens::{IntLiteral, TokenType::*};
    use pretty_assertions::assert_eq;

    fn identifier(ident: &str) -> TokenType<InternedString> {
        Identifier(ident.into())
    }

    fn token_stream(source: &str) -> impl Iterator<Item = Token<'_, InternedString>> {
        TokenStream::new(source, true, None).map(|t| t.expect("unexpected parsing error"))
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
        let mut s = token_stream("#\\a #\\b #\\λ");

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
        let mut s = token_stream(r#"  #\xAb #\u{0D300} #\u0540 "\x00D;" "\u1044;" "\u{045}"  "#);

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
            let mut s = TokenStream::new(token, true, None);

            // FIXME: concrete errors
            assert!(s.next().unwrap().is_err(), "{token:?} should be invalid");
        }
    }

    #[test]
    fn test_string_newlines() {
        let mut s = token_stream(" \"foo\nbar\" \"foo \\  \n   bar\" ");

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
        let mut s = token_stream("($)");
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
        let mut s = token_stream("foo FOO _123_ Nil #f #t");

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
        let got: Vec<_> = token_stream("1e 1ee 1.2e5.4 1E10/4 1.45# 3- e10").collect();
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
        let got: Vec<_> =
            token_stream("0 -0 -1.2 +2.3 999 1. 1e2 1E2 1.2e2 1.2E2 +inf.0 -inf.0 2e-4 2e+10")
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
        let got = token_stream("+nan.0").next().unwrap();

        match got.ty {
            TokenType::Number(n) => {
                assert!(matches!(*n, NumberLiteral::Real(RealLiteral::Float(x)) if x.is_nan()))
            }

            _ => panic!("Didn't match"),
        }

        let got = token_stream("-nan.0").next().unwrap();

        match got.ty {
            TokenType::Number(n) => {
                assert!(matches!(*n, NumberLiteral::Real(RealLiteral::Float(x)) if x.is_nan()))
            }

            _ => panic!("Didn't match"),
        }
    }

    #[test]
    fn test_rationals() {
        let got: Vec<_> = token_stream(
            r#"
                1/4
                (1/4 1/3)
                11111111111111111111/22222222222222222222
                /
                1/
                1/4.0
                1//4
                1 / 4
                .2
"#,
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
                Token {
                    ty: RealLiteral::Float(0.2).into(),
                    source: ".2",
                    span: Span::new(223, 225, SourceId::none())
                }
            ]
        );
    }

    #[test]
    fn test_complex_numbers() {
        let got: Vec<_> = token_stream(
            "1+2i 3-4i +5+6i +1i 1.0+2.0i 3-4.0i +1.0i 2e+4+inf.0i -inf.0-2e-4i 1/2@0 -3/2@1 +i -i 4+i",
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
                },
                Token {
                    ty: NumberLiteral::Polar(
                        RealLiteral::Rational(IntLiteral::Small(1), IntLiteral::Small(2)),
                        IntLiteral::Small(0).into()
                    )
                    .into(),
                    source: "1/2@0",
                    span: Span::new(67, 72, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Polar(
                        RealLiteral::Rational(IntLiteral::Small(-3), IntLiteral::Small(2)),
                        IntLiteral::Small(1).into()
                    )
                    .into(),
                    source: "-3/2@1",
                    span: Span::new(73, 79, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        IntLiteral::Small(1).into(),
                    )
                    .into(),
                    source: "+i",
                    span: Span::new(80, 82, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(0).into(),
                        IntLiteral::Small(-1).into()
                    )
                    .into(),
                    source: "-i",
                    span: Span::new(83, 85, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(4).into(),
                        IntLiteral::Small(1).into()
                    )
                    .into(),
                    source: "4+i",
                    span: Span::new(86, 89, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_numbers_with_radix() {
        let got = token_stream("#xff #xce #o777 #o1/20 #b1/10 #x10+ffi #d1.0").collect::<Vec<_>>();

        assert_eq!(
            &*got,
            &[
                Token {
                    ty: NumberLiteral::Real(IntLiteral::Small(255).into()).into(),
                    source: "#xff",
                    span: Span::new(0, 4, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Real(IntLiteral::Small(206).into()).into(),
                    source: "#xce",
                    span: Span::new(5, 9, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Real(IntLiteral::Small(511).into()).into(),
                    source: "#o777",
                    span: Span::new(10, 15, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Real(RealLiteral::Rational(
                        IntLiteral::Small(1),
                        IntLiteral::Small(16)
                    ))
                    .into(),
                    source: "#o1/20",
                    span: Span::new(16, 22, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Real(RealLiteral::Rational(
                        IntLiteral::Small(1),
                        IntLiteral::Small(2)
                    ))
                    .into(),
                    source: "#b1/10",
                    span: Span::new(23, 29, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Complex(
                        IntLiteral::Small(16).into(),
                        IntLiteral::Small(255).into(),
                    )
                    .into(),
                    source: "#x10+ffi",
                    span: Span::new(30, 38, SourceId::none()),
                },
                Token {
                    ty: NumberLiteral::Real(RealLiteral::Float(1.0)).into(),
                    source: "#d1.0",
                    span: Span::new(39, 44, SourceId::none()),
                }
            ]
        );
    }

    #[test]
    fn test_malformed_complex_numbers_are_identifiers() {
        let got: Vec<_> = token_stream("i 1i+1i -4+-2i").collect();
        assert_eq!(
            got.as_slice(),
            &[
                Token {
                    ty: identifier("i"),
                    source: "i",
                    span: Span::new(0, 1, SourceId::none()),
                },
                Token {
                    ty: identifier("1i+1i"),
                    source: "1i+1i",
                    span: Span::new(2, 7, SourceId::none()),
                },
                Token {
                    ty: identifier("-4+-2i"),
                    source: "-4+-2i",
                    span: Span::new(8, 14, SourceId::none()),
                },
            ]
        );
    }

    #[test]
    fn test_string() {
        let got: Vec<_> = token_stream(r#" "" "Foo bar" "\"\\" "#).collect();
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
        let mut s = token_stream(";!/usr/bin/gate\n   ; foo\n");
        assert_eq!(s.next(), None);
    }

    #[test]
    fn function_definition() {
        let s = token_stream("(define odd-rec? (lambda (x) (if (= x 0) #f (even-rec? (- x 1)))))");
        let res: Vec<_> = s.collect();

        println!("{:#?}", res);
    }

    #[test]
    fn lex_string_with_escape_chars() {
        let s = token_stream("\"\0\0\0\"");
        let res: Vec<_> = s.collect();
        println!("{:#?}", res);
    }

    #[test]
    fn scheme_statement() {
        let s = token_stream("(apples (function a b) (+ a b))");
        let res: Vec<_> = s.collect();

        let expected: Vec<Token<InternedString>> = vec![
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
        let s = token_stream("9223372036854775808"); // isize::MAX + 1
        let res: Vec<_> = s.collect();

        let expected_bigint = Box::new("9223372036854775808".parse().unwrap());

        let expected: Vec<Token<InternedString>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "9223372036854775808",
            span: Span::new(0, 19, SourceId::none()),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn negative_test_bigint() {
        let s = token_stream("-9223372036854775809"); // isize::MIN - 1
        let res: Vec<_> = s.collect();

        let expected_bigint = Box::new("-9223372036854775809".parse().unwrap());

        let expected: Vec<Token<InternedString>> = vec![Token {
            ty: IntLiteral::Big(expected_bigint).into(),
            source: "-9223372036854775809",
            span: Span::new(0, 20, SourceId::none()),
        }];

        assert_eq!(res, expected);
    }

    #[test]
    fn identifier_test() {
        let s = token_stream("a b(c`d'e\"www\"f,g;");

        let tokens: Vec<(TokenType<InternedString>, &str)> =
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
        let tokens: Vec<_> = token_stream("a b #(c d)")
            .map(|token| (token.ty, token.source))
            .collect();

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
        let tokens: Vec<_> = token_stream("a b #u8(1 2)")
            .map(|token| (token.ty, token.source))
            .collect();

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
        let mut s = token_stream(r#"|a| |a b| |\x61;| |.|"#);

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

        let mut s = token_stream("|a\\\nb|");

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
