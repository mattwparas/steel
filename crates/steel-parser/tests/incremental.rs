use steel_parser::differential::{check, rework};

const CORPUS: &[&str] = &[
    "1",
    "(a b c)",
    "(a (b (c (d))))",
    "(define (f x) (+ x 1))",
    "'(1 2 3)",
    "`(1 ,x ,@ys)",
    "(quote (a . b))",
    "(a . b)",
    "#(1 2 3)",
    "#u8(1 2 3)",
    "\"a string\"",
    "\"multi\\nline\"",
    "(list \"a\" #\\b 'c)",
    "(λ (x) x)",
    "(a \"λμν\" b)",
    "(define-syntax foo (syntax-rules () ((_ x) x)))",
    "(let ([x 1] [y 2]) (+ x y))",
    "``(a ,,b)",
    "(a ; comment\n b)",
    "(vector 1 2)",
    "(deeply (nested (list (of (things (here (we (go))))))))",
    "(a #'b #`c #,d)",
    "(1/2 3.14 #t #f)",
    "#| block comment |# (a b)",
    "(a #| inner |# b)",
    "(a \"paren ) inside\" b)",
    "(a \"escaped \\\" quote\" b)",
    "#;(ignored) (kept)",
    "(#;(ignored) kept)",
    "(a . (b . (c . ())))",
    "(|weird ident| b)",
    "(a\nb\nc)",
    "((((1))))",
    "(set! x (lambda () (begin 1 2 3)))",
    "' (1 2)",
    "(a ' b)",
    "#; (ignored) (kept)",
    "(define data '(1 2 3))",
    "#;#;(a) (b) (kept)",
    "#;(a b c) (kept)",
    // A `#;` and a prefix interleaved. Which applies first depends on the order
    // they appear in, so these pin that down.
    "(,@#;a b)",
    "(#;'1 2)",
    "#u8(#;'1 2)",
    "(#;a b)",
    "('#;a b)",
    "(#;,@a b)",
    "(a ,@#;b c)",
    "#;'a b ",
    "'#;a b ",
    "(a ')",
    "(#;)",
    "(,@#;)",
    // A quote applied to a list that starts with `quote`, which the quote context
    // bookkeeping treats specially. The kernel relies on this shape.
    "(a '(quote ()))",
    "(equal? x '(quote ()))",
    "'(quote ())",
    // Turned up by the fuzzer
    "#!,e)",                  // shebang line split across a chunk
    "#! x\n(a b)",            // shebang line that is complete
    ",@#;a b ",               // a prefix whose datum is behind a `#;`
    ",@#;(a b) c ",
    ".||mn",                  // malformed identifier the lexer carries on past
    "(list \"a\" #\\b 'c)",     // `#\` split from its character
    // Turned up by the property tests
    "#u8(-2.5 (#u8(1 2)",
    "\"\\\u{1}\u{0}\u{b} a",
    "('(a .) a (...) ; comment\n a a (,@|odd ident|) '#| block |# '; comment\n a)",
];

// A long datum, where rework shows up clearly
fn long(prefix: &str, suffix: &str) -> String {
    let mut out = String::from(prefix);
    out.push('(');

    for i in 0..60 {
        out.push_str(&format!("(item {}) ", i));
    }

    out.push(')');
    out.push_str(suffix);
    out
}

#[test]
fn chunked_matches_one_shot() {
    let mut failures = Vec::new();

    for input in CORPUS {
        for chunk in [1usize, 2, 3, 5, 7, 13, 64] {
            if let Err(err) = check(input, chunk) {
                failures.push(err);
            }
        }
    }

    assert!(
        failures.is_empty(),
        "{} mismatches\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn chunked_matches_one_shot_on_a_large_datum() {
    let input = long("", "");

    for chunk in [1usize, 3, 17, 256] {
        check(&input, chunk).unwrap();
    }
}

// Boundaries that do not divide evenly, to shake out off by one handling
#[test]
fn chunked_matches_one_shot_at_pseudorandom_boundaries() {
    let mut seed = 0x243f_6a88_85a3_08d3u64;
    let mut next = move || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        seed
    };

    for input in CORPUS {
        for _ in 0..25 {
            check(input, (next() % 9) as usize + 1).unwrap();
        }
    }
}

#[test]
fn a_long_datum_is_not_reparsed() {
    for shape in [
        long("", ""),                 // plain
        long("'", ""),                // prefix outside any list
        long("`", ""),                // quasiquote outside any list
        long("''", ""),               // stacked prefixes
        long("(define data '", ")"),  // prefix inside a list
        long("#;(skipped) ", ""),     // after a datum comment
    ] {
        let factor = rework(&shape, 1);

        assert!(
            factor < 1.5,
            "reparsed {:.1}x for {:?}...",
            factor,
            &shape[..shape.len().min(24)]
        );
    }
}

#[test]
fn short_datums_resume_too() {
    for input in CORPUS {
        assert!(rework(input, 1) < 1.5, "reparsed {:?}", input);
    }
}
