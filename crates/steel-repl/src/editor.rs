//! Where the repl reads its input from. Rustyline when it's built in and
//! stdin is a terminal, otherwise a plain line reader that needs nothing
//! from the platform beyond stdin and stdout.

use std::collections::HashSet;
use std::io::{BufRead, Write};
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

use steel_parser::interner::InternedString;
use steel_parser::lexer::{TokenError, TokenStream};
use steel_parser::parser::SourceId;
use steel_parser::tokens::TokenType;

pub(crate) enum Input {
    Line(String),
    /// Ctrl-C while editing
    #[cfg_attr(not(feature = "rustyline"), allow(dead_code))]
    Interrupted,
    /// Ctrl-D, or the end of stdin
    Eof,
}

pub(crate) trait LineEditor: Send + Sync {
    fn read_line(&mut self, prompt: &str) -> std::io::Result<Input>;
    fn add_history_entry(&mut self, line: &str);
    fn save_history(&mut self, path: &Path) -> std::io::Result<()>;
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

/// Rustyline needs a terminal on stdin. `STEEL_REPL_PLAIN` set to anything
/// but `0` asks for plain input anyway.
#[cfg(feature = "rustyline")]
fn use_rustyline() -> bool {
    use std::io::IsTerminal;

    std::io::stdin().is_terminal()
        && !std::env::var_os("STEEL_REPL_PLAIN").is_some_and(|value| value != "0")
}

/// The editor to use: rustyline if it's compiled in and [`use_rustyline`]
/// agrees, otherwise [`PlainEditor`].
#[cfg_attr(not(feature = "rustyline"), allow(unused_variables))]
pub(crate) fn new_editor(
    history_path: &Path,
    globals: Arc<Mutex<HashSet<InternedString>>>,
    close_on_interrupt: Arc<AtomicBool>,
) -> Box<dyn LineEditor> {
    #[cfg(feature = "rustyline")]
    if use_rustyline() {
        return Box::new(rusty::RustylineEditor::new(
            history_path,
            Some(globals),
            Some(close_on_interrupt),
        ));
    }

    Box::new(PlainEditor)
}

/// The editor behind `#%private/steel/readline`, which has no globals to
/// complete against and no Ctrl-C binding
#[cfg_attr(not(feature = "rustyline"), allow(unused_variables))]
pub(crate) fn new_module_editor(history_path: &Path) -> Box<dyn LineEditor> {
    #[cfg(feature = "rustyline")]
    if use_rustyline() {
        return Box::new(rusty::RustylineEditor::new(history_path, None, None));
    }

    Box::new(PlainEditor)
}

/// Reads whole lines from stdin. No editing and no history, but multi-line
/// forms still work: lines are read until the form is complete.
pub(crate) struct PlainEditor;

impl LineEditor for PlainEditor {
    fn read_line(&mut self, prompt: &str) -> std::io::Result<Input> {
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

    fn add_history_entry(&mut self, _line: &str) {}

    fn save_history(&mut self, _path: &Path) -> std::io::Result<()> {
        Ok(())
    }
}

#[cfg(feature = "rustyline")]
mod rusty {
    use super::{Input, LineEditor};
    use crate::highlight::RustylineHelper;

    use std::collections::HashSet;
    use std::fs::File;
    use std::path::Path;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Mutex};

    use rustyline::config::Configurer;
    use rustyline::error::ReadlineError;
    use rustyline::history::DefaultHistory;
    use rustyline::{
        Cmd, ConditionalEventHandler, Editor, Event, EventContext, EventHandler, KeyEvent,
        RepeatCount,
    };
    use steel_parser::interner::InternedString;

    pub(crate) struct RustylineEditor(Editor<RustylineHelper, DefaultHistory>);

    impl RustylineEditor {
        pub(crate) fn new(
            history_path: &Path,
            globals: Option<Arc<Mutex<HashSet<InternedString>>>>,
            close_on_interrupt: Option<Arc<AtomicBool>>,
        ) -> Self {
            let mut rl = Editor::<RustylineHelper, DefaultHistory>::new()
                .expect("Unable to instantiate the repl!");
            rl.set_check_cursor_position(true);

            if rl.load_history(history_path).is_err() && File::create(history_path).is_err() {
                eprintln!("Unable to create repl history file {:?}", history_path)
            };

            if let Some(globals) = globals {
                rl.set_helper(Some(RustylineHelper::new(globals)));
            }

            if let Some(close_on_interrupt) = close_on_interrupt {
                let ctrlc = Box::new(CtrlCHandler::new(close_on_interrupt));
                rl.bind_sequence(KeyEvent::ctrl('c'), EventHandler::Conditional(ctrlc));
            }

            Self(rl)
        }
    }

    impl LineEditor for RustylineEditor {
        fn read_line(&mut self, prompt: &str) -> std::io::Result<Input> {
            match self.0.readline(prompt) {
                Ok(line) => Ok(Input::Line(line)),
                Err(ReadlineError::Interrupted) => Ok(Input::Interrupted),
                Err(ReadlineError::Eof) => Ok(Input::Eof),
                Err(ReadlineError::Io(err)) => Err(err),
                Err(err) => Err(std::io::Error::other(format!("{err:?}"))),
            }
        }

        fn add_history_entry(&mut self, line: &str) {
            self.0.add_history_entry(line).ok();
        }

        fn save_history(&mut self, path: &Path) -> std::io::Result<()> {
            self.0
                .save_history(path)
                .map_err(|err| std::io::Error::other(err.to_string()))
        }
    }

    /// Ctrl-C clears the line, twice in a row on an empty line exits
    struct CtrlCHandler {
        close_on_interrupt: Arc<AtomicBool>,
        empty_line_cancelled: AtomicBool,
    }

    impl CtrlCHandler {
        fn new(close_on_interrupt: Arc<AtomicBool>) -> Self {
            CtrlCHandler {
                close_on_interrupt,
                empty_line_cancelled: AtomicBool::new(false),
            }
        }
    }

    impl ConditionalEventHandler for CtrlCHandler {
        fn handle(&self, _: &Event, _: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
            if !ctx.line().is_empty() {
                // if the line is not empty, reset the PREVIOUS_LINE_CANCELLED state
                self.empty_line_cancelled.store(false, Ordering::Release);
            } else if self.empty_line_cancelled.swap(true, Ordering::Release) {
                self.close_on_interrupt.store(true, Ordering::Release);
            }

            Some(Cmd::Interrupt)
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
