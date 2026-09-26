//! Parsing arbitrary text should produce a value or an error, never a panic.

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Ok(text) = core::str::from_utf8(data) else {
            return;
        };

        for expr in steel_parser::parser::Parser::new_flat(
            text,
            steel_parser::parser::SourceId::none(),
        ) {
            // Stop at the first error, the way a caller would
            if expr.is_err() {
                break;
            }
        }
    });
}
