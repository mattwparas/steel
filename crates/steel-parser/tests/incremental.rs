use steel_parser::parser::{Parser, ParserState, SourceId};

// `syntax_object_id` comes from a global counter, so it differs run to run and
// says nothing about whether the parse matched.
fn normalize(s: String) -> String {
    let mut out = String::with_capacity(s.len());
    let mut rest = s.as_str();
    while let Some(i) = rest.find("syntax_object_id: ") {
        out.push_str(&rest[..i]);
        out.push_str("syntax_object_id: _");
        rest = &rest[i + "syntax_object_id: ".len()..];
        let digits = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        rest = &rest[digits..];
    }
    out.push_str(rest);
    out
}

fn show(r: Option<steel_parser::parser::Result<steel_parser::ast::ExprKind>>) -> String {
    match r {
        Some(Ok(e)) => normalize(format!("{:?}", e)),
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
            Some(Ok(expr)) => return normalize(format!("{:?}", expr)),
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
