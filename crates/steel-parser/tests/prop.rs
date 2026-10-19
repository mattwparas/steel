use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use steel_parser::differential::{check, one_shot, rework};
use steel_parser::parser::{Parser, SourceId};

fn atom_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("a".to_string()),
        Just("b".to_string()),
        Just("define".to_string()),
        Just("1".to_string()),
        Just("-2.5".to_string()),
        Just("1/2".to_string()),
        Just("#t".to_string()),
        Just("#\\a".to_string()),
        Just("\"s\"".to_string()),
        Just("#u8(1 2)".to_string()),
        Just("|odd ident|".to_string()),
        Just(".".to_string()),
        Just("...".to_string()),
        Just("; comment\n".to_string()),
        Just("#| block |#".to_string()),
    ]
}

fn prefix_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("'".to_string()),
        Just("`".to_string()),
        Just(",".to_string()),
        Just(",@".to_string()),
        Just("#'".to_string()),
        Just("#`".to_string()),
        Just("#,".to_string()),
        Just("#,@".to_string()),
        Just("#;".to_string()),
    ]
}

fn open_strategy() -> impl Strategy<Value = (String, String)> {
    prop_oneof![
        Just(("(".to_string(), ")".to_string())),
        Just(("[".to_string(), "]".to_string())),
        Just(("{".to_string(), "}".to_string())),
        Just(("#(".to_string(), ")".to_string())),
        Just(("#u8(".to_string(), ")".to_string())),
    ]
}

fn source_strategy() -> impl Strategy<Value = String> {
    atom_strategy().prop_recursive(5, 48, 5, |inner| {
        prop_oneof![
            (open_strategy(), prop::collection::vec(inner.clone(), 0..5))
                .prop_map(|((open, close), parts)| format!("{}{}{}", open, parts.join(" "), close)),
            (prefix_strategy(), inner.clone())
                .prop_map(|(prefix, inner)| format!("{}{}", prefix, inner)),
            (inner.clone(), inner).prop_map(|(left, right)| format!("{} {}", left, right)),
        ]
    })
}

fn long_source_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(source_strategy(), 8..20)
        .prop_map(|parts| format!("({})", parts.join(" ")))
}

fn truncated_source_strategy() -> impl Strategy<Value = String> {
    (source_strategy(), any::<prop::sample::Index>()).prop_map(|(text, index)| {
        let mut at = index.index(text.len() + 1);
        while at < text.len() && !text.is_char_boundary(at) {
            at += 1;
        }
        text[..at].to_string()
    })
}

proptest! {
    #![proptest_config(ProptestConfig {
        failure_persistence: Some(Box::new(FileFailurePersistence::Direct(
            "tests/prop-regressions.txt",
        ))),
        ..ProptestConfig::default()
    })]

    #[test]
    fn chunking_does_not_change_the_result(
        input in truncated_source_strategy(),
        chunk in 1usize..40,
    ) {
        if let Err(err) = check(&input, chunk) {
            prop_assert!(false, "{}", err);
        }
    }

    #[test]
    fn chunking_does_not_change_the_result_for_arbitrary_text(
        input in ".{0,200}",
        chunk in 1usize..40,
    ) {
        if let Err(err) = check(&input, chunk) {
            prop_assert!(false, "{}", err);
        }
    }

    #[test]
    fn parsing_arbitrary_text_does_not_panic(input in ".{0,400}") {
        let _ = one_shot(&input);
    }

    #[test]
    fn spans_stay_inside_the_input(input in source_strategy()) {
        for expr in Parser::new_flat(&input, SourceId::none()) {
            let Ok(expr) = expr else { break };

            let span = expr.span();
            prop_assert!(span.start <= span.end, "backwards span in {:?}", input);
            prop_assert!(
                span.end as usize <= input.len(),
                "span past the end in {:?}",
                input
            );
        }
    }

    #[test]
    fn offsets_advance_within_the_input(input in source_strategy()) {
        let mut parser = Parser::new_flat(&input, SourceId::none());
        let mut previous = 0;

        while let Some(result) = parser.next() {
            let offset = parser.offset();

            prop_assert!(offset <= input.len(), "offset past the end in {:?}", input);

            if result.is_ok() {
                prop_assert!(offset > previous, "made no progress in {:?}", input);
            }

            previous = offset;
        }
    }

    #[test]
    fn chunked_reading_does_not_reparse(input in long_source_strategy()) {
        let factor = rework(&input, 8);
        prop_assert!(factor < 1.5, "reparsed {:.1}x for {:?}", factor, input);
    }
}
