extern crate rustyline;
use colored::*;

use std::{cell::RefCell, rc::Rc};

use rustyline::highlight::Highlighter;
use rustyline::validate::{
    MatchingBracketValidator, ValidationContext, ValidationResult, Validator,
};

use rustyline::{hint::Hinter, Context};
use rustyline_derive::Helper;

use steel_parser::new_lexer::TokenStream;

use rustyline::completion::Completer;
use rustyline::completion::Pair;

use std::borrow::Cow;

use steel::steel_vm::engine::Engine;

impl Completer for RustylineHelper {
    type Candidate = Pair;
}

#[derive(Helper)]
pub struct RustylineHelper {
    // highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    engine: Rc<RefCell<Engine>>,
    bracket: std::cell::Cell<Option<(u8, usize)>>, // keywords: HashSet<&'static str>,
}

impl RustylineHelper {
    pub fn new(validator: MatchingBracketValidator, engine: Rc<RefCell<Engine>>) -> Self {
        Self {
            validator,
            engine,
            bracket: std::cell::Cell::new(None),
        }
    }
}

impl Validator for RustylineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

impl Hinter for RustylineHelper {
    type Hint = String;
    fn hint(&self, _line: &str, _pos: usize, _context: &Context) -> Option<String> {
        None
    }
}

impl Highlighter for RustylineHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        use steel_parser::tokens::TokenType;

        use Cow::*;

        // let line

        // Borrowed(line)

        // let matching_parens = self.highlighter.highlight(line, pos);

        // let tokenizer = steel::parser

        let mut line_to_highlight = line.to_owned();

        let token_stream = TokenStream::new(line, true, None);

        let mut ranges_to_replace: Vec<(std::ops::Range<usize>, String)> = Vec::new();

        for token in token_stream {
            // todo!()

            // if token.span().end() > pos {
            //     return self.highlighter.highlight(line, pos);
            // }

            match token.typ() {
                // steel::parser::tokens::TokenType::OpenParen => todo!(),
                // steel::parser::tokens::TokenType::CloseParen => todo!(),
                // steel::parser::tokens::TokenType::QuoteTick => todo!(),
                // steel::parser::tokens::TokenType::QuasiQuote => todo!(),
                // steel::parser::tokens::TokenType::Unquote => todo!(),
                // steel::parser::tokens::TokenType::UnquoteSplice => todo!(),
                // steel::parser::tokens::TokenType::If => todo!(),
                // steel::parser::tokens::TokenType::Define => todo!(),
                // steel::parser::tokens::TokenType::Let => todo!(),
                // steel::parser::tokens::TokenType::TestLet => todo!(),
                // steel::parser::tokens::TokenType::Return => todo!(),
                // steel::parser::tokens::TokenType::Begin => todo!(),
                TokenType::Lambda
                | TokenType::If
                | TokenType::Define
                | TokenType::Let
                | TokenType::Require => {
                    let highlighted = format!("{}", token.source().bright_purple());

                    ranges_to_replace.push((token.span().range(), highlighted));

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
                    ranges_to_replace.push((token.span().range(), highlighted));
                }
                TokenType::Identifier(ident) => {
                    // If its a free identifier, nix it?
                    if self.engine.borrow().global_exists(ident) {
                        // println!("before length: {}", token.source().as_bytes().len());
                        let highlighted = format!("{}", token.source().bright_blue());
                        // println!("After length: {}", highlighted.as_bytes().len());

                        // println!("paren pos: {:?}", self.bracket.get());

                        ranges_to_replace.push((token.span().range(), highlighted));
                    }

                    // else if self.engine.borrow().in_scope_macros().contains_key(*ident) {
                    //     let highlighted = format!("{}", token.source().bright_cyan());
                    //     ranges_to_replace.push((token.span().range(), highlighted));
                    // }
                }
                // steel::parser::tokens::TokenType::Keyword(_) => todo!(),
                TokenType::NumberLiteral(_) | TokenType::IntegerLiteral(_) => {
                    // println!("Found something to replace! @ {:?}", token.span().range());

                    let highlighted = format!("{}", token.source().bright_yellow());
                    ranges_to_replace.push((token.span().range(), highlighted));

                    // line_to_highlight.replace_range(token.span().range(), &highlighted);
                }
                TokenType::StringLiteral(_) => {
                    let highlighted = format!("{}", token.source().bright_green());
                    ranges_to_replace.push((token.span().range(), highlighted));
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
            if let Some((_bracket, pos)) = self.bracket.get() {
                if start <= pos {
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
        if let Some((bracket, mut pos)) = self.bracket.get() {
            if pos > 1 {
                // println!("position!: {}", pos);
                pos += offset;
            }

            // This seems its finding the matching bracket within the escaped sequences - this will need
            // to _not_ check escaped
            if let Some((matching, idx)) = find_matching_bracket(&line_to_highlight, pos, bracket) {
                // let mut copy = line.to_owned();
                line_to_highlight
                    .replace_range(idx..=idx, &format!("\x1b[1;34m{}\x1b[0m", matching as char));
                return Owned(line_to_highlight);
            }
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

    fn highlight_char(&self, line: &str, mut pos: usize) -> bool {
        // will highlight matching brace/bracket/parenthesis if it exists
        self.bracket.set(check_bracket(line, pos));
        if self.bracket.get().is_some() {
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
            self.bracket.get().is_some()
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

fn find_matching_bracket(line: &str, pos: usize, bracket: u8) -> Option<(u8, usize)> {
    let matching = matching_bracket(bracket);
    let mut idx;
    let mut unmatched = 1;
    if is_open_bracket(bracket) {
        // forward search
        idx = pos + 1;
        let bytes = &line.as_bytes()[idx..];
        for b in bytes {
            if *b == matching {
                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx]);
                    return Some((matching, idx));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx += 1;
        }
        debug_assert_eq!(idx, line.len());
    } else {
        // backward search
        idx = pos;
        let bytes = &line.as_bytes()[..idx];
        for b in bytes.iter().rev() {
            if *b == matching {
                // if idx > 3 {
                //     println!("{:?}", &line.as_bytes().get(idx));
                //     println!("{:?}", b"\x1b");
                // }

                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx - 1]);
                    return Some((matching, idx - 1));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx -= 1;
        }
        debug_assert_eq!(idx, 0);
    }
    None
}

// check under or before the cursor
fn check_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if line.is_empty() {
        return None;
    }
    let mut pos = pos;
    if pos >= line.len() {
        pos = line.len() - 1; // before cursor
        let b = line.as_bytes()[pos]; // previous byte
        if is_close_bracket(b) {
            Some((b, pos))
        } else {
            None
        }
    } else {
        let mut under_cursor = true;
        loop {
            let b = line.as_bytes()[pos];
            if is_close_bracket(b) {
                return if pos == 0 { None } else { Some((b, pos)) };
            } else if is_open_bracket(b) {
                return if pos + 1 == line.len() {
                    None
                } else {
                    Some((b, pos))
                };
            } else if under_cursor && pos > 0 {
                under_cursor = false;
                pos -= 1; // or before cursor
            } else {
                return None;
            }
        }
    }
}

const fn matching_bracket(bracket: u8) -> u8 {
    match bracket {
        b'{' => b'}',
        b'}' => b'{',
        // b'[' => b']',
        // b']' => b'[',
        b'(' => b')',
        b')' => b'(',
        b => b,
    }
}
fn is_open_bracket(bracket: u8) -> bool {
    // matches!(bracket, b'{' | b'[' | b'(')
    matches!(bracket, b'{' | b'(')
}
fn is_close_bracket(bracket: u8) -> bool {
    // matches!(bracket, b'}' | b']' | b')')
    matches!(bracket, b'}' | b')')
}
