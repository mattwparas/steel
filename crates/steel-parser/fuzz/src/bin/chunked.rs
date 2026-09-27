fn main() {
    // First byte is the chunk size, so afl can steer where the boundaries land
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
