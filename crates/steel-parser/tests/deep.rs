use steel_parser::parser::{ParseError, Parser, SourceId, MAX_PREFIX_DEPTH};

#[test]
fn nested_lists_are_read_iteratively() {
    let text = "(".repeat(10_000) + &")".repeat(10_000);
    let mut parser = Parser::new_flat(&text, SourceId::none());

    assert!(parser.next().map(|res| res.is_ok()).unwrap_or(false));
}

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
