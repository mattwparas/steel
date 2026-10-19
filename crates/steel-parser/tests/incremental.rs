use steel_parser::differential::{check, rework};

const PROGRAMS: &[&str] = &[
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

fn programs() -> impl Iterator<Item = &'static str> {
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
fn chunked_matches_one_shot_on_a_large_datum() {
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
    for input in programs() {
        assert!(rework(input, 1) < 1.5, "reparsed {:?}", input);
    }
}
