fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Ok(text) = core::str::from_utf8(data) else {
            return;
        };

        for token in steel_parser::lexer::TokenStream::new(
            text,
            true,
            steel_parser::parser::SourceId::none(),
        ) {
            let _ = token;
        }
    });
}
