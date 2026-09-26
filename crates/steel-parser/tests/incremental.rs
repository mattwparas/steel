use steel_parser::parser::{Parser, ParserState, SourceId};

// `syntax_object_id` and the interner ids come from global counters, so they vary
// between runs. Values get compared through `Display`, which resolves them.
fn strip(s: String, prefix: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut rest = s.as_str();
    while let Some(i) = rest.find(prefix) {
        out.push_str(&rest[..i]);
        out.push_str(prefix);
        out.push('_');
        rest = &rest[i + prefix.len()..];
        let digits = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        rest = &rest[digits..];
    }
    out.push_str(rest);
    out
}

fn normalize(s: String) -> String {
    strip(strip(s, "syntax_object_id: "), "InternedNumber(")
}

// Display carries the values, Debug the spans
fn render(expr: &steel_parser::ast::ExprKind) -> String {
    format!("{} | {}", expr, normalize(format!("{:?}", expr)))
}

fn show(r: Option<steel_parser::parser::Result<steel_parser::ast::ExprKind>>) -> String {
    match r {
        Some(Ok(e)) => render(&e),
        Some(Err(e)) => normalize(format!("ERR({:?})", e)),
        None => "NONE".to_string(),
    }
}

fn one_shot(input: &str) -> String {
    show(Parser::new_flat(input, SourceId::none()).next())
}

// Feed `input` `chunk` bytes at a time. The corpus is one datum per entry, so
// nothing is ever consumed and offsets stay absolute - exactly comparable to the
// one shot parse.
fn incremental(input: &str, chunk: usize) -> String {
    let mut fed = 0usize;
    let mut state: Option<ParserState> = None;

    loop {
        fed = usize::min(fed + chunk, input.len());
        while fed < input.len() && !input.is_char_boundary(fed) {
            fed += 1;
        }

        let visible = &input[..fed];
        let more = fed < input.len();

        let mut parser = match state.take() {
            Some(st) => Parser::resume(visible, SourceId::none(), st, more),
            None => Parser::new_incremental(visible, SourceId::none(), more),
        };

        match parser.next() {
            Some(Ok(expr)) => return render(&expr),
            other => {
                if !more {
                    return show(other);
                }
                // Carry what we can; None means start over on the next chunk.
                state = parser.take_state();
            }
        }
    }
}

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
];

// A bigger synthetic datum, to exercise many suspends in one parse.
fn deep_input() -> String {
    let mut s = String::from("(");
    for i in 0..200 {
        s.push_str(&format!("(item {} \"s{}\") ", i, i));
    }
    s.push(')');
    s
}

// Total tokens handed to the parse when reading `input` a byte at a time. Stays
// close to the token count if the parse resumes, grows quadratically if it has to
// start over on every chunk.
fn tokens_lexed(input: &str) -> usize {
    let mut fed = 0usize;
    let mut state: Option<ParserState> = None;
    let mut total = 0;

    loop {
        fed = usize::min(fed + 1, input.len());
        while fed < input.len() && !input.is_char_boundary(fed) {
            fed += 1;
        }

        let visible = &input[..fed];
        let more = fed < input.len();

        let mut parser = match state.take() {
            Some(st) => Parser::resume(visible, SourceId::none(), st, more),
            None => Parser::new_incremental(visible, SourceId::none(), more),
        };

        let done = matches!(parser.next(), Some(Ok(_))) || !more;
        if !done {
            state = parser.take_state();
        }
        total += parser.tokens_lexed();

        if done {
            return total;
        }
    }
}

// One shot lexes each token once, so this is the rework factor
fn rework(input: &str) -> f64 {
    let mut once = Parser::new_flat(input, SourceId::none());
    while once.next().is_some() {}
    let baseline = once.tokens_lexed().max(1);

    tokens_lexed(input) as f64 / baseline as f64
}

// Rework shows up clearly on a long datum
fn long(prefix: &str, suffix: &str) -> String {
    let mut s = String::from(prefix);
    s.push('(');
    for i in 0..60 {
        s.push_str(&format!("(item {}) ", i));
    }
    s.push(')');
    s.push_str(suffix);
    s
}

#[test]
fn a_long_datum_is_not_reparsed() {
    for shape in [
        long("", ""),                   // plain
        long("'", ""),                  // prefix outside any list
        long("`", ""),                  // quasiquote outside any list
        long("''", ""),                 // stacked prefixes
        long("(define data '", ")"),    // prefix inside a list
        long("#;(skipped) ", ""),       // after a datum comment
    ] {
        let factor = rework(&shape);
        assert!(
            factor < 3.0,
            "reparsed {:.1}x for {:?}...",
            factor,
            &shape[..shape.len().min(24)]
        );
    }
}

#[test]
fn short_datums_resume_too() {
    for input in [
        "(a b c)",
        "(a (b (c (d))))",
        "#(1 2 3)",
        "(define data '(1 2 3))",
        "(a `(b ,c ,@d) e)",
        "(a #,@b)",
        "(a . 'b)",
        "'(1 2 3)",
        "(a ''b)",
        "#;(ignored) (kept)",
        "#;#;(a) (b) (kept)",
    ] {
        assert!(rework(input) < 3.0, "reparsed {:?}", input);
    }
}

#[test]
fn incremental_matches_one_shot() {
    let mut failures = Vec::new();

    for input in CORPUS {
        let expected = one_shot(input);
        for chunk in [1usize, 2, 3, 5, 7, 13, 64] {
            let got = incremental(input, chunk);
            if got != expected {
                failures.push(format!(
                    "input {:?} chunk {}\n  one-shot:    {}\n  incremental: {}",
                    input, chunk, expected, got
                ));
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
fn incremental_matches_one_shot_on_a_large_datum() {
    let input = deep_input();
    let expected = one_shot(&input);

    for chunk in [1usize, 3, 17, 256] {
        assert_eq!(incremental(&input, chunk), expected, "chunk {}", chunk);
    }
}

// Chunk boundaries that do not divide evenly, to shake out off by one handling.
#[test]
fn incremental_matches_one_shot_at_pseudorandom_boundaries() {
    let mut seed = 0x243f_6a88_85a3_08d3u64;
    let mut next = move || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        seed
    };

    for input in CORPUS {
        let expected = one_shot(input);
        for _ in 0..25 {
            let chunk = (next() % 9) as usize + 1;
            assert_eq!(incremental(input, chunk), expected, "input {:?}", input);
        }
    }
}
