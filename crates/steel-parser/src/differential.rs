//! Helpers for checking that feeding the parser in chunks agrees with parsing
//! the whole input at once. Used by the integration tests and the fuzz targets,
//! which is why it lives here rather than in `tests/`.

use crate::ast::ExprKind;
use crate::parser::{Parser, ParserState, SourceId};

use alloc::format;
use alloc::string::{String, ToString};

/// `syntax_object_id` and the interner ids come from global counters, so they
/// vary between runs. Values get compared through `Display`, which resolves them.
fn strip(input: String, prefix: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut rest = input.as_str();

    while let Some(at) = rest.find(prefix) {
        out.push_str(&rest[..at]);
        out.push_str(prefix);
        out.push('_');

        rest = &rest[at + prefix.len()..];

        let digits = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        rest = &rest[digits..];
    }

    out.push_str(rest);
    out
}

fn normalize(input: String) -> String {
    strip(strip(input, "syntax_object_id: "), "InternedNumber(")
}

// Display carries the values, Debug the spans
fn render(expr: &ExprKind) -> String {
    normalize(format!("{} | {:?}", expr, expr))
}

fn show(result: Option<crate::parser::Result<ExprKind>>) -> String {
    match result {
        Some(Ok(expr)) => render(&expr),
        Some(Err(err)) => normalize(format!("ERR({:?})", err)),
        None => "NONE".to_string(),
    }
}

/// The first datum, parsing `input` in one go.
pub fn one_shot(input: &str) -> String {
    show(Parser::new_flat(input, SourceId::none()).next())
}

/// The first datum, feeding `input` `chunk` bytes at a time and suspending at
/// every boundary. Nothing is consumed, so offsets stay absolute and the result
/// is directly comparable to [`one_shot`].
pub fn chunked(input: &str, chunk: usize) -> String {
    let chunk = chunk.max(1);
    let mut fed = 0;
    let mut state: Option<ParserState> = None;

    loop {
        fed = usize::min(fed + chunk, input.len());
        while fed < input.len() && !input.is_char_boundary(fed) {
            fed += 1;
        }

        let visible = &input[..fed];
        let more = fed < input.len();

        let mut parser = match state.take() {
            Some(state) => Parser::resume(visible, SourceId::none(), state, more),
            None => Parser::new_incremental(visible, SourceId::none(), more),
        };

        match parser.next() {
            Some(Ok(expr)) => return render(&expr),
            // A hard error only comes from a token that is already complete, so
            // more input cannot change it. Stopping here is what the reader does.
            Some(Err(err)) if !parser.needs_more_input(&err) => return show(Some(Err(err))),
            other => {
                if !more {
                    return show(other);
                }

                // None means this parse cannot be resumed and has to be redone
                state = parser.take_state();
            }
        }
    }
}

/// `Err` with a description when chunking changes the result.
pub fn check(input: &str, chunk: usize) -> Result<(), String> {
    let expected = one_shot(input);
    let got = chunked(input, chunk);

    if expected == got {
        return Ok(());
    }

    Err(format!(
        "input {:?} chunk {}\n  one-shot:    {}\n  chunked:     {}",
        input, chunk, expected, got
    ))
}

/// Tokens handed to the parse when reading `input` `chunk` bytes at a time, over
/// the count a single pass needs. Stays near 1 if the parse resumes, grows with
/// the input if it has to start over on every chunk.
///
/// There is a floor above 1: the token at each boundary gets held back and lexed
/// again, which costs about one token per chunk.
pub fn rework(input: &str, chunk: usize) -> f64 {
    let chunk = chunk.max(1);

    let mut once = Parser::new_flat(input, SourceId::none());
    while once.next().is_some() {}

    let baseline = once.tokens_lexed().max(1);

    let mut fed = 0;
    let mut state: Option<ParserState> = None;
    let mut total = 0;

    loop {
        fed = usize::min(fed + chunk, input.len());
        while fed < input.len() && !input.is_char_boundary(fed) {
            fed += 1;
        }

        let visible = &input[..fed];
        let more = fed < input.len();

        let mut parser = match state.take() {
            Some(state) => Parser::resume(visible, SourceId::none(), state, more),
            None => Parser::new_incremental(visible, SourceId::none(), more),
        };

        // Stop where `chunked` would: a datum, a hard error, or the end of input
        let done = match parser.next() {
            Some(Ok(_)) => true,
            Some(Err(err)) => !parser.needs_more_input(&err) || !more,
            None => !more,
        };

        if !done {
            state = parser.take_state();
        }

        total += parser.tokens_lexed();

        if done {
            return total as f64 / baseline as f64;
        }
    }
}
