use steel_parser::parser::{ParseError, Parser, SourceId, MAX_PREFIX_DEPTH};

// Nested lists are read iteratively, so depth is cheap here.
#[test]
fn nested_lists_are_read_iteratively() {
    let text = "(".repeat(10_000) + &")".repeat(10_000);
    let mut parser = Parser::new_flat(&text, SourceId::none());

    assert!(parser.next().map(|res| res.is_ok()).unwrap_or(false));
}

// Quote prefixes are read by recursing, one frame per level. This pins the
// headroom there is, to catch it shrinking.
#[test]
fn stacked_prefixes_have_headroom() {
    for prefix in ["'", "`", ",", ",@", "#'", "#`", "#,", "#,@"] {
        let text = prefix.repeat(400) + "a";
        let mut parser = Parser::new_flat(&text, SourceId::none());

        assert!(
            parser.next().map(|res| res.is_ok()).unwrap_or(false),
            "400 stacked {:?} did not parse",
            prefix
        );
    }
}

// Past the cap it reports an error the caller can catch, rather than running the
// stack out. The thread here gets the 2MiB a thread gets by default, which is the
// budget the cap has to fit inside.
#[test]
fn nesting_past_the_cap_is_an_error() {
    std::thread::Builder::new()
        .stack_size(2 * 1024 * 1024)
        .spawn(|| {
            let text = "'".repeat(MAX_PREFIX_DEPTH as usize + 8) + "a";
            let mut parser = Parser::new_flat(&text, SourceId::none());

            let err = parser.next().unwrap().unwrap_err();
            assert!(
                matches!(err, ParseError::SyntaxError(..)),
                "expected a syntax error, got {:?}",
                err
            );
        })
        .unwrap()
        .join()
        .unwrap();
}

// A `#;` marker belongs to the read that saw it. An error used to leave it behind,
// and the next read then discarded a good datum with it.
#[test]
fn a_datum_comment_does_not_outlive_a_failed_read() {
    let parsed: Vec<_> = Parser::new_flat("#;(a] 1 2", SourceId::none())
        .map(|res| res.map(|expr| expr.to_string()).map_err(|_| ()))
        .collect();

    assert_eq!(
        parsed,
        vec![Err(()), Ok("1".to_string()), Ok("2".to_string())]
    );
}
