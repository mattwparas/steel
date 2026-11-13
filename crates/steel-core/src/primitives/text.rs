//! `#%private/steel/text`: what Steel code needs to lay text out on a
//! terminal. steel/markdown renders docs with it, and the REPL's line editor
//! colours code with `tokenize`.

use std::io::IsTerminal;

use steel_parser::lexer::TokenStream;
use steel_parser::parser::SourceId;
use steel_parser::tokens::TokenType;
use unicode_width::UnicodeWidthStr;

use crate::rvals::{SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

/// `(kind start end)` per token of `text`, in chars (Steel strings index by
/// char, the lexer's spans are bytes). kind is open, close, form (define,
/// lambda and the like), identifier, keyword, string, number, char, boolean,
/// comment or other. Whatever the lexer rejects is left out.
pub fn tokenize(text: &str) -> Vec<SteelVal> {
    let mut chars_before = vec![0usize; text.len() + 1];
    let mut count = 0;
    for (byte, c) in text.char_indices() {
        for slot in &mut chars_before[byte..byte + c.len_utf8()] {
            *slot = count;
        }
        count += 1;
    }
    chars_before[text.len()] = count;

    TokenStream::new(text, false, SourceId::none())
        .filter_map(Result::ok)
        .map(|token| {
            let (kind, span) = match &token.ty {
                TokenType::OpenParen(_, paren_mod) => {
                    ("open", TokenType::open_span(token.span, *paren_mod))
                }
                TokenType::CloseParen(_) => ("close", token.span),
                TokenType::If
                | TokenType::Define
                | TokenType::Let
                | TokenType::Lambda
                | TokenType::Require
                | TokenType::Begin
                | TokenType::Set
                | TokenType::Quote
                | TokenType::DefineSyntax
                | TokenType::SyntaxRules => ("form", token.span),
                TokenType::Identifier(_) => ("identifier", token.span),
                TokenType::Keyword(_) => ("keyword", token.span),
                TokenType::StringLiteral(_) => ("string", token.span),
                TokenType::Number(_) => ("number", token.span),
                TokenType::CharacterLiteral(_) => ("char", token.span),
                TokenType::BooleanLiteral(_) => ("boolean", token.span),
                TokenType::Comment => ("comment", token.span),
                _ => ("other", token.span),
            };
            let start = chars_before[(span.start as usize).min(text.len())];
            let end = chars_before[(span.end as usize).min(text.len())];
            SteelVal::ListV(
                vec![
                    SteelVal::SymbolV(kind.into()),
                    SteelVal::IntV(start as isize),
                    SteelVal::IntV(end as isize),
                ]
                .into(),
            )
        })
        .collect()
}

/// Columns `text` takes up on a terminal. `text` must not contain escape
/// sequences.
pub fn display_width(text: &str) -> usize {
    text.width()
}

/// How wide the terminal stdout writes to is, else `COLUMNS`, else unknown
pub fn terminal_columns() -> Option<usize> {
    #[cfg(unix)]
    {
        let mut size = core::mem::MaybeUninit::<libc::winsize>::zeroed();
        if unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, size.as_mut_ptr()) } == 0 {
            let columns = unsafe { size.assume_init() }.ws_col as usize;
            if columns > 0 {
                return Some(columns);
            }
        }
    }

    std::env::var("COLUMNS")
        .ok()
        .and_then(|columns| columns.trim().parse().ok())
        .filter(|&columns| columns > 0)
}

pub fn text_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("#%private/steel/text");
    module
        .register_fn("tokenize", |text: SteelString| tokenize(text.as_str()))
        .register_fn("string-display-width", |text: SteelString| {
            display_width(text.as_str())
        })
        // Columns, or #f when that can't be found out
        .register_fn("terminal-columns", terminal_columns)
        .register_fn("stdout-terminal?", || std::io::stdout().is_terminal());
    module
}
