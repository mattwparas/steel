extern crate rustyline;
use colored::Colorize;
use steel_parser::interner::InternedString;
use steel_parser::parser::SourceId;
use steel_parser::tokens::TokenType;

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use rustyline::highlight::Highlighter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};

use rustyline::Helper;
use rustyline::{hint::Hinter, Context};

use steel_parser::lexer::{TokenError, TokenStream};

use rustyline::completion::Completer;
use rustyline::completion::Pair;

use std::borrow::Cow;

#[derive(Helper)]
pub struct RustylineHelper {
    globals: Arc<Mutex<HashSet<InternedString>>>,
    bracket: crossbeam_utils::atomic::AtomicCell<Option<(u8, usize)>>, // keywords: HashSet<&'static str>,
}

impl RustylineHelper {
    pub fn new(globals: Arc<Mutex<HashSet<InternedString>>>) -> Self {
        Self {
            globals,
            bracket: crossbeam_utils::atomic::AtomicCell::new(None),
        }
    }
}

impl Completer for RustylineHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        // Only do completions if we're in an identifier
        let Some((span, symbol)) = TokenStream::new(line, true, SourceId::none())
            .filter_map(Result::ok)
            .find_map(|token| match token.ty {
                TokenType::Identifier(ref symbol) => (
                    // Inclusive range as curser usually placed directly after the symbol
                    token.span().start()..=token.span().end()
                )
                    .contains(&(pos as u32))
                    .then_some((token.span(), symbol.clone())),
                _ => None,
            })
        else {
            return Ok((0, Vec::new()));
        };

        // Find all globals containing the identifier the user is currently typing
        let mut starting = Vec::new();
        let mut containing = Vec::new();
        for interned in self.globals.lock().unwrap().iter() {
            let str = interned.resolve();
            if str.starts_with(symbol.resolve()) {
                starting.push(str.to_owned());
            } else if str.contains(symbol.resolve()) {
                containing.push(str.to_owned())
            }
        }

        // Sort identifiers, present the ones that only require completing the end first
        let compare = |a: &String, b: &String| {
            a.contains("builtin")
                .cmp(&b.contains("builtin"))
                .then(a.starts_with('#').cmp(&b.starts_with('#')))
                .then(a.cmp(b))
        };
        starting.sort_by(compare);
        containing.sort_by(compare);
        let candidates = starting.into_iter().chain(containing);

        // Apply colors to distinguish completion from typed text
        let completions = candidates
            .map(|ident| Pair {
                display: format!("{}", ident.white()),
                replacement: ident,
            })
            .collect();

        Ok((span.start() as usize, completions))
    }

    fn update(
        &self,
        line: &mut rustyline::line_buffer::LineBuffer,
        start: usize,
        elected: &str,
        cl: &mut rustyline::Changeset,
    ) {
        // Cursor can be anywhere in the identifier, so find its span again.
        if let Some(end) = TokenStream::new(line, true, SourceId::none())
            .filter_map(Result::ok)
            .find_map(|token| {
                (token.span().start() as usize == start).then_some(token.span().end())
            })
        {
            line.replace(start..end as usize, elected, cl);
        }
    }
}

impl Validator for RustylineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        use steel_parser::tokens::TokenType;

        let token_stream = TokenStream::new(ctx.input(), true, SourceId::none());

        let mut balance = 0;

        let mut unfinished = false;

        for token in token_stream {
            let token = match token {
                Ok(token) => token,
                Err(err) => {
                    unfinished = std::matches!(
                        err.ty,
                        TokenError::IncompleteString
                            | TokenError::IncompleteIdentifier
                            | TokenError::IncompleteComment
                    );

                    break;
                }
            };

            match token.ty {
                TokenType::OpenParen(..) => {
                    balance += 1;
                }
                TokenType::CloseParen(_) => {
                    balance -= 1;
                }
                _ => {}
            }
        }

        if balance > 0 || unfinished {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

impl Hinter for RustylineHelper {
    type Hint = String;
    fn hint(&self, _line: &str, _pos: usize, _context: &Context) -> Option<String> {
        None
    }
}

impl Highlighter for RustylineHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        use steel_parser::tokens::TokenType;

        // let line

        // Borrowed(line)

        // let matching_parens = self.highlighter.highlight(line, pos);

        // let tokenizer = steel::parser

        let mut line_to_highlight = line.to_owned();

        let mut token_stream = TokenStream::new(line, true, SourceId::none())
            .filter_map(Result::ok)
            .peekable();

        let mut ranges_to_replace: Vec<(std::ops::Range<usize>, String)> = Vec::new();

        let mut stack = vec![];
        let mut cursor = None;

        let mut paren_to_highlight = None;

        while let Some(token) = token_stream.next() {
            match token.typ() {
                TokenType::OpenParen(paren, paren_mod) if paren_to_highlight.is_none() => {
                    let open_span = TokenType::open_span(token.span, *paren_mod);

                    if open_span.start as usize == pos
                        || (open_span.start as usize == pos + 1 && cursor.is_none())
                    {
                        cursor = Some((*paren, open_span));
                    }

                    stack.push((*paren, open_span));
                }

                TokenType::CloseParen(paren) if paren_to_highlight.is_none() => {
                    let mut matches = token.span.start as usize == pos;

                    if token.span.end as usize == pos {
                        let next_span = match token_stream.peek() {
                            Some(steel_parser::tokens::Token {
                                ty: TokenType::CloseParen(_),
                                span,
                                ..
                            }) => Some(*span),

                            Some(steel_parser::tokens::Token {
                                ty: TokenType::OpenParen(_, paren_mod),
                                span,
                                ..
                            }) => Some(TokenType::open_span(*span, *paren_mod)),

                            _ => None,
                        };

                        matches = match next_span {
                            Some(span) => span.start as usize > pos,

                            _ => true,
                        }
                    }

                    if matches {
                        cursor = Some((*paren, token.span));
                    }

                    match (stack.pop(), cursor) {
                        (Some((open, span)), Some((_, cursor_span))) if open == *paren => {
                            if cursor_span == span {
                                paren_to_highlight = Some(token.span.start);
                            } else if cursor_span == token.span {
                                paren_to_highlight = Some(span.start);
                            }
                        }
                        _ => {}
                    }
                }
                // steel::parser::tokens::TokenType::QuoteTick => todo!(),
                // steel::parser::tokens::TokenType::QuasiQuote => todo!(),
                // steel::parser::tokens::TokenType::Unquote => todo!(),
                // steel::parser::tokens::TokenType::UnquoteSplice => todo!(),
                // steel::parser::tokens::TokenType::TestLet => todo!(),
                // steel::parser::tokens::TokenType::Return => todo!(),
                // steel::parser::tokens::TokenType::Begin => todo!(),
                TokenType::Lambda
                | TokenType::If
                | TokenType::Define
                | TokenType::Let
                | TokenType::Require => {
                    let highlighted = format!("{}", token.source().bright_purple());

                    ranges_to_replace.push((token.span().usize_range(), highlighted));

                    // line_to_highlight.replace_range(token.span().range(), &highlighted);
                }
                // steel::parser::tokens::TokenType::Quote => todo!(),
                // steel::parser::tokens::TokenType::SyntaxRules => todo!(),
                // steel::parser::tokens::TokenType::DefineSyntax => todo!(),
                // steel::parser::tokens::TokenType::Ellipses => todo!(),
                // steel::parser::tokens::TokenType::Set => todo!(),
                // steel::parser::tokens::TokenType::Require => todo!(),
                // steel::parser::tokens::TokenType::CharacterLiteral(_) => todo!(),
                // steel::parser::tokens::TokenType::Comment => todo!(),
                TokenType::BooleanLiteral(_) => {
                    let highlighted = format!("{}", token.source().bright_magenta());
                    ranges_to_replace.push((token.span().usize_range(), highlighted));
                }
                TokenType::Identifier(ident) => {
                    // If its a free identifier, nix it?

                    if self.globals.lock().unwrap().contains(ident) {
                        let highlighted = format!("{}", token.source().bright_blue());
                        ranges_to_replace.push((token.span().usize_range(), highlighted));
                    }

                    // TODO:
                    // if self.engine.lock().unwrap().global_exists(ident) {
                    //     // println!("before length: {}", token.source().as_bytes().len());
                    //     let highlighted = format!("{}", token.source().bright_blue());
                    //     ranges_to_replace.push((token.span().range(), highlighted));
                    // }
                }
                // steel::parser::tokens::TokenType::Keyword(_) => todo!(),
                TokenType::Number(_) => {
                    let highlighted = format!("{}", token.source().bright_yellow());
                    ranges_to_replace.push((token.span().usize_range(), highlighted));
                }
                TokenType::StringLiteral(_) => {
                    let highlighted = format!("{}", token.source().bright_green());
                    ranges_to_replace.push((token.span().usize_range(), highlighted));
                }
                // steel::parser::tokens::TokenType::Error => todo!(),
                _ => {}
            }
        }

        let mut offset = 0;

        for (range, highlighted) in ranges_to_replace.into_iter().rev() {
            // self.bracket.set(check_bracket(&line_to_highlight, pos));

            // println!("pos: {}")

            let old_length = line_to_highlight.as_bytes().len();

            // self.bracket.set(check_bracket(&line_to_highlight, start));

            let start = range.start;

            line_to_highlight.replace_range(range, &highlighted);

            let new_length = line_to_highlight.as_bytes().len();

            // TODO just store the updated location back in
            if let Some(pos) = paren_to_highlight {
                if start <= pos as _ {
                    offset += new_length - old_length;
                }
            }
        }

        // Cow::Owned(
        //     self.highlighter
        //         .highlight(&line_to_highlight, pos)
        //         .to_string(),
        // )

        // if line.len() <= 1 {
        //     return Borrowed(line);
        // }

        // highlight matching brace/bracket/parenthesis if it exists
        if let Some(pos) = paren_to_highlight {
            let idx = if pos == 0 { 0 } else { pos as usize + offset };

            line_to_highlight.replace_range(
                idx..=idx,
                &format!(
                    "\x1b[1;34m{}\x1b[0m",
                    line_to_highlight.as_bytes()[idx] as char
                ),
            );
        }

        Cow::Owned(line_to_highlight)
    }
    // fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
    //     &'s self,
    //     prompt: &'p str,
    //     default: bool,
    // ) -> Cow<'b, str> {}

    // fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {}

    // fn highlight_candidate<'c>(
    //     &self,
    //     candidate: &'c str,
    //     completion: CompletionType,
    // ) -> Cow<'c, str> {
    //     self.highlighter.highlight_candidate(candidate, completion)
    // }

    // fn highlight_char(&self, line: &str, pos: usize) -> bool {
    //     self.highlighter.highlight_char(line, pos)
    // }

    fn highlight_char(&self, line: &str, mut pos: usize, _: bool) -> bool {
        // will highlight matching brace/bracket/parenthesis if it exists
        self.bracket.store(check_bracket(line, pos));
        if self.bracket.load().is_some() {
            return true;
        }

        if line.is_empty() {
            return false;
        }

        if pos >= line.len() {
            pos = line.len() - 1; // before cursor
            let b = line.as_bytes()[pos]; // previous byte
            match b {
                b'"' | b' ' => true,
                x if x.is_ascii_digit() => true,
                _ => false,
            }
        } else {
            self.bracket.load().is_some()
        }
    }
}

// impl Highlighter for MatchingBracketHighlighter {
//     fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
//         use Cow::*;

//         if line.len() <= 1 {
//             return Borrowed(line);
//         }
//         // highlight matching brace/bracket/parenthesis if it exists
//         if let Some((bracket, pos)) = self.bracket.get() {
//             if let Some((matching, idx)) = find_matching_bracket(line, pos, bracket) {
//                 let mut copy = line.to_owned();
//                 copy.replace_range(idx..=idx, &format!("\x1b[1;34m{}\x1b[0m", matching as char));
//                 return Owned(copy);
//             }
//         }
//         Borrowed(line)
//     }

//     // fn highlight_char(&self, line: &str, pos: usize) -> bool {
//     //     // will highlight matching brace/bracket/parenthesis if it exists
//     //     self.bracket.set(check_bracket(line, pos));
//     //     self.bracket.get().is_some()
//     // }
// }

// check under or before the cursor
fn check_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if line.is_empty() {
        return None;
    }

    let bytes = line.as_bytes();

    let on_bracket = |pos: usize| {
        let b = bytes.get(pos).copied()?;
        let open = is_open_bracket(b);
        let close = is_close_bracket(b);

        if (open && (pos + 1 < bytes.len())) || (close && pos > 0) {
            Some((b, open))
        } else {
            None
        }
    };

    if let Some((current, _)) = on_bracket(pos) {
        return Some((current, pos));
    }

    if pos > 0 {
        if let Some((current, open)) = on_bracket(pos - 1) {
            if !open {
                return Some((current, pos - 1));
            }
        }
    }

    if let Some((current, open)) = on_bracket(pos + 1) {
        if open {
            return Some((current, pos + 1));
        }
    }

    None
}

fn is_open_bracket(bracket: u8) -> bool {
    matches!(bracket, b'{' | b'[' | b'(')
}

fn is_close_bracket(bracket: u8) -> bool {
    matches!(bracket, b'}' | b']' | b')')
}
