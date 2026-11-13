//! Plain line input, for when the Steel line editor can't or shouldn't run:
//! stdin isn't a terminal, `STEEL_REPL_PLAIN` is set, or the editor failed.
//! Needs nothing from the platform beyond stdin and stdout.

use std::io::{BufRead, Write};

use steel_parser::lexer::{TokenError, TokenStream};
use steel_parser::parser::SourceId;
use steel_parser::tokens::TokenType;

pub(crate) enum Input {
    Line(String),
    /// Ctrl-C while editing
    Interrupted,
    /// Ctrl-D, or the end of stdin
    Eof,
}

/// Whether `text` is a whole form, or the user is still typing one: an open
/// paren, or an unfinished string, identifier or block comment.
pub(crate) fn input_complete(text: &str) -> bool {
    let mut balance = 0;

    for token in TokenStream::new(text, true, SourceId::none()) {
        match token {
            Ok(token) => match token.ty {
                TokenType::OpenParen(..) => balance += 1,
                TokenType::CloseParen(_) => balance -= 1,
                _ => {}
            },
            Err(err) => {
                return !matches!(
                    err.ty,
                    TokenError::IncompleteString
                        | TokenError::IncompleteIdentifier
                        | TokenError::IncompleteComment
                );
            }
        }
    }

    balance <= 0
}

/// `STEEL_REPL_PLAIN` set to anything but `0` asks for plain lines even on a
/// terminal
pub(crate) fn plain_requested() -> bool {
    std::env::var_os("STEEL_REPL_PLAIN").is_some_and(|value| value != "0")
}

/// Reads whole lines from stdin. No editing and no history, but multi-line
/// forms still work: lines are read until the form is complete.
pub(crate) struct PlainEditor;

impl PlainEditor {
    pub(crate) fn read_line(&mut self, prompt: &str) -> std::io::Result<Input> {
        let mut text = String::new();
        let mut stdout = std::io::stdout();
        let mut stdin = std::io::stdin().lock();

        write!(stdout, "{prompt}")?;
        stdout.flush()?;

        loop {
            if stdin.read_line(&mut text)? == 0 {
                // Hand back a half typed form so its error shows up
                return Ok(if text.trim().is_empty() {
                    Input::Eof
                } else {
                    Input::Line(text)
                });
            }

            if input_complete(&text) {
                let len = text.trim_end_matches(['\n', '\r']).len();
                text.truncate(len);
                return Ok(Input::Line(text));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::input_complete;

    #[test]
    fn complete_forms() {
        assert!(input_complete(""));
        assert!(input_complete("(+ 1 2)"));
        assert!(input_complete("(define (f x)\n  (* x 2))"));
        assert!(input_complete("x"));
        assert!(input_complete("\"a (string\""));
        // Too many closers is the reader's error to report, not a reason to wait
        assert!(input_complete("(+ 1 2))"));
    }

    #[test]
    fn incomplete_forms() {
        assert!(!input_complete("(+ 1"));
        assert!(!input_complete("(define (f x)\n"));
        assert!(!input_complete("\"a string\nover lines"));
        assert!(!input_complete("#| block comment"));
        assert!(!input_complete("[1 2"));
    }
}
