use ropey::Rope;
use steel::parser::span::Span;
use steel_language_server::backend::{Config, OffsetEncoding};
use tower_lsp::lsp_types::{Position, Range};

const SOURCE: &str = "(define x 1)\n(display \"héllo 😀\")\n(+ x 1)\n";

const CLOSE_PAREN: u32 = 35;

fn config(encoding: OffsetEncoding) -> Config {
    let config = Config::new();
    config.encoding.store(encoding);
    config
}

#[test]
fn source_fixture_is_what_the_tests_assume() {
    assert_eq!(SOURCE.as_bytes()[CLOSE_PAREN as usize], b')');
    assert_eq!(SOURCE.lines().nth(1), Some("(display \"héllo 😀\")"));
}

#[test]
fn offset_to_position_counts_bytes_in_utf8() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf8);

    assert_eq!(
        config.offset_to_position(CLOSE_PAREN as usize, &rope),
        Some(Position::new(1, 22))
    );
}

#[test]
fn offset_to_position_counts_code_units_in_utf16() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf16);

    assert_eq!(
        config.offset_to_position(CLOSE_PAREN as usize, &rope),
        Some(Position::new(1, 19))
    );
}

#[test]
fn offset_to_position_counts_characters_in_utf32() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf32);

    assert_eq!(
        config.offset_to_position(CLOSE_PAREN as usize, &rope),
        Some(Position::new(1, 18))
    );
}

#[test]
fn position_and_offset_round_trip_in_every_encoding() {
    let rope = Rope::from_str(SOURCE);

    for encoding in [
        OffsetEncoding::Utf8,
        OffsetEncoding::Utf16,
        OffsetEncoding::Utf32,
    ] {
        let config = config(encoding);

        for (offset, _) in SOURCE.char_indices() {
            let position = config
                .offset_to_position(offset, &rope)
                .unwrap_or_else(|| panic!("{:?}: no position for offset {}", encoding, offset));

            assert_eq!(
                config.position_to_offset(position, &rope),
                Some(offset),
                "{:?}: {:?} did not round trip from offset {}",
                encoding,
                position,
                offset
            );
        }
    }
}

#[test]
fn the_first_line_needs_no_adjustment() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf16);

    assert_eq!(
        config.offset_to_position(0, &rope),
        Some(Position::new(0, 0))
    );
    assert_eq!(
        config.offset_to_position(8, &rope),
        Some(Position::new(0, 8))
    );
    assert_eq!(
        config.position_to_offset(Position::new(0, 8), &rope),
        Some(8)
    );
}

#[test]
fn span_to_range_covers_the_whole_span() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf8);

    // x in (define x 1)
    assert_eq!(
        config.span_to_range(&Span::new(8, 9, None), &rope),
        Some(Range::new(Position::new(0, 8), Position::new(0, 9)))
    );

    assert_eq!(
        config.span_to_range(&Span::new(8, CLOSE_PAREN, None), &rope),
        Some(Range::new(Position::new(0, 8), Position::new(1, 22)))
    );
}

#[test]
fn out_of_range_offsets_and_positions_are_rejected() {
    let rope = Rope::from_str(SOURCE);
    let config = config(OffsetEncoding::Utf16);

    assert_eq!(config.offset_to_position(SOURCE.len() + 1, &rope), None);
    assert_eq!(config.position_to_offset(Position::new(99, 0), &rope), None);
    assert_eq!(
        config.span_to_range(&Span::new(0, SOURCE.len() as u32 + 1, None), &rope),
        None
    );
}
