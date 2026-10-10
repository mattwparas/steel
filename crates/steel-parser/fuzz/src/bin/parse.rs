fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Ok(text) = core::str::from_utf8(data) else {
            return;
        };

        for expr in steel_parser::parser::Parser::new_flat(
            text,
            steel_parser::parser::SourceId::none(),
        ) {
            if expr.is_err() {
                break;
            }
        }
    });
}
