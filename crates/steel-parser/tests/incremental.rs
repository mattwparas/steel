use steel_parser::parser::{Parser, ParserState, SourceId};

mod common;

use common::compare::{check, relex_ratio};

fn programs() -> impl Iterator<Item = &'static str> {
    static PROGRAMS: &[&str] = &[
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
        "(a '(quote ()))",
        "(equal? x '(quote ()))",
        "'(quote ())",
        "(a)#!eof\n(b)",
        "#!,e)",
        "#! x\n(a b)",
        ",@#;a b ",
        ",@#;(a b) c ",
        ".||mn",
        "(list \"a\" #\\b 'c)",
        "#u8(-2.5 (#u8(1 2)",
        "\"\\\u{1}\u{0}\u{b} a",
        "('(a .) a (...) ; comment\n a a (,@|odd ident|) '#| block |# '; comment\n a)",
        "; line\n#| block |# (a #;(b) c)",
        "#;(skipped) (kept)",
        "((((((((((((x))))))))))))",
        "(1 -2.5 1/2 #x1f +inf.0)",
        "`(a ,b ,@c (d ,(e f)))",
        "''(1 2)",
        ")",
        "(\"a\\nb\" #\\a #\\space |odd ident|)",
        "(a #'b #`c #,d #,@e)",
        "(a (b",
        "#(1 2 #u8(3 4))",
    ];

    PROGRAMS.iter().copied()
}

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

fn shapes() -> [String; 6] {
    [
        long("", ""),
        long("'", ""),
        long("`", ""),
        long("''", ""),
        long("(define data '", ")"),
        long("#;(skipped) ", ""),
    ]
}

#[test]
fn chunked_matches_one_shot() {
    let mut failures = Vec::new();

    for input in programs() {
        for chunk in [1usize, 2, 3, 4, 5, 6, 7, 8, 9, 13, 64] {
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
fn a_long_datum_matches_one_shot() {
    for input in shapes() {
        for chunk in [1usize, 3, 17, 256] {
            if let Err(err) = check(&input, chunk) {
                panic!("{}", err);
            }
        }
    }
}

#[test]
fn a_long_datum_is_not_reparsed() {
    for shape in shapes() {
        let factor = relex_ratio(&shape, 1);

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
    for input in programs() {
        assert!(relex_ratio(input, 1) < 1.5, "reparsed {:?}", input);
    }
}

static DOC_COMMENTED: &str =
    ";;@doc a\n;; b\n(define-syntax m (syntax-rules () ((_) 1)))\n(tail)\n";

fn drain(input: &str, chunk: usize) -> Vec<String> {
    let mut out = Vec::new();
    let mut base = 0;
    let mut fed = 0;
    let mut state: Option<ParserState> = None;

    loop {
        if fed < input.len() {
            fed = usize::min(fed + usize::max(chunk, 1), input.len());

            while fed < input.len() && !input.is_char_boundary(fed) {
                fed += 1;
            }
        }

        let visible = &input[base..fed];
        let more = fed < input.len();

        let mut parser = match state.take() {
            Some(state) => Parser::resume(visible, SourceId::none(), state, more, base == 0),
            None => Parser::new_incremental(visible, SourceId::none(), more, base == 0),
        };

        let result = parser.next();
        let offset = parser.offset();

        match result {
            Some(Ok(expr)) => {
                out.push(expr.to_string());
                base += offset;
            }
            Some(Err(err)) if more && parser.needs_more_input(&err) => state = parser.take_state(),
            Some(Err(_)) => {
                out.push("ERR".to_string());
                return out;
            }
            None if more => state = parser.take_state(),
            None => return out,
        }
    }
}

#[test]
fn the_compiler_desugars_a_doc_comment() {
    let expressions = Parser::new_flat(DOC_COMMENTED, SourceId::none())
        .map(|expr| expr.unwrap().to_string())
        .collect::<Vec<_>>();

    assert_eq!(
        expressions,
        [
            "(@doc \"b\" #%macro m)",
            "(define-syntax m (syntax-rules () ((_) 1)))",
            "(tail)"
        ]
    );
}

#[test]
fn the_reader_hands_back_the_datum_instead() {
    let expected = [
        "(define-syntax m (syntax-rules () ((_) 1)))".to_string(),
        "(tail)".to_string(),
    ];

    for chunk in 1..=DOC_COMMENTED.len() {
        assert_eq!(drain(DOC_COMMENTED, chunk), expected, "chunk {}", chunk);
    }
}

fn suspended(input: &str) -> ParserState {
    let mut parser = Parser::new_incremental(input, SourceId::none(), true, true);

    assert!(parser.next().is_none_or(|result| result.is_err()));

    parser.take_state().unwrap()
}

#[test]
fn a_string_cut_short_waits_for_its_closing_quote() {
    static ARRIVALS: [(&str, &str, bool); 7] = [
        ("(a \"bc", "de (f) ; g", false),
        ("(a \"bc", "de\" f)", true),
        ("(a \"bc", "de\\\" f", false),
        ("(a \"bc\\", "\" f", false),
        ("(a \"bc\\\\", "\" f", true),
        ("(a \"bc", "\\\\\\\" \\\\\" f", true),
        ("(a bc", "de", true),
    ];

    for (first, second, advances) in ARRIVALS {
        let whole = format!("{first}{second}");

        assert_eq!(
            suspended(first).could_advance(whole.as_bytes(), first.len()),
            advances,
            "{first:?} then {second:?}"
        );
    }
}
