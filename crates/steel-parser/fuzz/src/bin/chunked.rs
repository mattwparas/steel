//! Feeding the parser in chunks has to produce what parsing the whole input at
//! once does. This is the property the suspend and resume machinery exists to
//! satisfy, and the one most likely to break on unusual input.
//!
//! The first input byte picks the chunk size so afl can steer it; the rest is the
//! text to parse.

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let Some((first, rest)) = data.split_first() else {
            return;
        };

        let Ok(text) = core::str::from_utf8(rest) else {
            return;
        };

        let chunk = (*first as usize % 64) + 1;

        if let Err(err) = steel_parser::differential::check(text, chunk) {
            panic!("{}", err);
        }
    });
}
