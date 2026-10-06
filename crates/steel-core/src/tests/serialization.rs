//! Values serialized by one engine and read by another, which is what the
//! bytes are for, and random values sent both ways.
//!
//! The round trips within one engine are in
//! `success/serialization_round_trips.scm`.

use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;

/// Definitions both ends have, as two programs that share a struct would.
const SHARED: &str = r#"
(struct point (x y) #:transparent)
(struct counter (n) #:mutable)
(struct record (name items extra) #:mutable #:transparent)
"#;

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.run(SHARED).expect("the shared definitions load");
    engine
}

fn last(results: Vec<SteelVal>) -> SteelVal {
    results.last().cloned().expect("the program has a value")
}

/// Serializes what `expression` makes in `from` and binds what `to` reads of
/// it to `received` there.
fn send(from: &mut Engine, to: &mut Engine, expression: &str) -> Result<(), String> {
    let bytes = from
        .run(format!(
            "(serialized->bytes (serialize-value {}))",
            expression
        ))
        .map(last)
        .map_err(|e| e.to_string())?;

    to.register_value("incoming-bytes", bytes);
    to.run("(define received (deserialize-value (bytes->serialized incoming-bytes)))")
        .map(|_| ())
        .map_err(|e| e.to_string())
}

fn holds(engine: &mut Engine, check: &str) -> bool {
    match engine.run(check.to_string()) {
        Ok(results) => matches!(results.last(), Some(SteelVal::BoolV(true))),
        Err(e) => panic!("`{}` raised: {}", check, e),
    }
}

/// Sends `expression` from one new engine to another and checks it there.
fn arrives(expression: &str, check: &str) {
    let mut from = engine();
    let mut to = engine();
    send(&mut from, &mut to, expression)
        .unwrap_or_else(|e| panic!("sending `{}` failed: {}", expression, e));
    assert!(
        holds(&mut to, check),
        "`{}` arrived as something `{}` doesn't hold of",
        expression,
        check
    );
}

/// The same value made at both ends is `equal?`.
fn arrives_equal(expression: &str) {
    arrives(expression, &format!("(equal? received {})", expression));
}

#[test]
fn plain_data_crosses_engines() {
    for expression in [
        "0",
        "-17",
        "4611686018427387903",
        "1.5",
        "1/3",
        "12345678901234567890",
        "(/ 12345678901234567890 7)",
        "(make-rectangular 1 2)",
        "#t",
        "#f",
        "#\\a",
        "#\\λ",
        "\"\"",
        "\"a string with λ and \\\"quotes\\\"\"",
        "'a-symbol",
        "'()",
        "(list 1 2 3)",
        "(list 1 \"two\" 'three 4.5 (list 5 (list 6)))",
        "(cons 1 2)",
        "(bytes 0 1 2 255)",
        "(immutable-vector 1 \"two\" (list 3))",
        "(flat-vector 1 \"two\" (flat-vector 3))",
        "(hash 'a 1 \"b\" (list 2) 3 (hash 'c 4))",
        "(hashset 1 2 \"three\")",
        "(range 0 2000)",
    ] {
        arrives_equal(expression);
    }
}

#[test]
fn symbols_arrive_as_the_symbols_they_are_there() {
    arrives("'a-symbol", "(eq? received 'a-symbol)");
    arrives(
        "(list 'one (list 'two) (hash 'key 'three))",
        "(and (eq? (car received) 'one)
              (eq? (car (cadr received)) 'two)
              (eq? (hash-ref (caddr received) 'key) 'three))",
    );
    // One the other end has never seen.
    arrives(
        "'a-symbol-made-up-for-this-test",
        "(equal? (symbol->string received) \"a-symbol-made-up-for-this-test\")",
    );
}

#[test]
fn cells_cross_engines() {
    arrives(
        "(box (list 1 2))",
        "(and (equal? (unbox received) (list 1 2))
              (begin (set-box! received 'changed) (eq? (unbox received) 'changed)))",
    );
    arrives(
        "(mutable-vector 1 2 3)",
        "(begin (vector-push! received 4) (vector-set! received 0 'changed)
                (and (= (vector-length received) 4) (eq? (vector-ref received 0) 'changed)))",
    );
    arrives(
        "(let ([shared (box 1)]) (list shared shared))",
        "(begin (set-box! (car received) 'changed)
                (and (eq? (unbox (cadr received)) 'changed) (eq? (car received) (cadr received))))",
    );
    arrives(
        "(list (box 1) (box 1))",
        "(not (eq? (car received) (cadr received)))",
    );
    arrives(
        "(let ([b (box #f)]) (set-box! b (list b 1)) b)",
        "(eq? (car (unbox received)) received)",
    );
    arrives(
        "(let ([v (mutable-vector #f 1)]) (vector-set! v 0 v) v)",
        "(and (eq? (vector-ref received 0) received) (= (vector-length received) 2))",
    );
}

#[test]
fn structs_cross_engines() {
    arrives_equal("(point 1 2)");
    arrives_equal("(list (point 1 (point 2 3)) (point \"a\" 'b))");
    arrives(
        "(point 1 2)",
        "(and (point? received) (= (point-x received) 1) (not (counter? received)))",
    );
    arrives(
        "(counter 5)",
        "(and (counter? received)
              (= (counter-n received) 5)
              (begin (set-counter-n! received 6) (= (counter-n received) 6)))",
    );
    arrives_equal("(record \"name\" (list 1 2) (point 3 4))");
    arrives(
        "(record \"name\" (list 1 2) #f)",
        "(begin (set-record-extra! received 'set) (eq? (record-extra received) 'set))",
    );
    arrives(
        "(let ([c (counter 1)]) (list c (box c) (mutable-vector c)))",
        "(begin (set-counter-n! (car received) 'changed)
                (and (eq? (counter-n (unbox (cadr received))) 'changed)
                     (eq? (counter-n (vector-ref (caddr received) 0)) 'changed)))",
    );
    arrives(
        "(let* ([a (counter #f)] [b (counter a)]) (set-counter-n! a b) a)",
        "(eq? (counter-n (counter-n received)) received)",
    );
    arrives(
        "(let* ([cell (box #f)] [p (point cell 'cyclic)]) (set-box! cell p) p)",
        "(eq? (point-y (unbox (point-x received))) 'cyclic)",
    );
}

#[test]
fn functions_cross_engines() {
    arrives("car", "(= (received (list 1 2)) 1)");
    arrives("(lambda (x) (* x 2))", "(= (received 21) 42)");
    arrives("(lambda args (length args))", "(= (received 1 2 3) 3)");
    arrives(
        "(lambda () (list \"a constant\" 'quoted 1/3 12345678901234567890))",
        "(equal? (received) (list \"a constant\" 'quoted 1/3 12345678901234567890))",
    );
    arrives("(let ([n 5]) (lambda (m) (+ n m)))", "(= (received 6) 11)");
    arrives(
        "(let ([data (list 1 2 3)]) (lambda () (apply + data)))",
        "(= (received) 6)",
    );
    arrives(
        "(let ([count (box 0)]) (lambda () (set-box! count (+ 1 (unbox count))) (unbox count)))",
        "(and (= (received) 1) (= (received) 2))",
    );
    arrives(
        "(let ([state (counter 10)]) (lambda () (set-counter-n! state (+ 1 (counter-n state))) (counter-n state)))",
        "(and (= (received) 11) (= (received) 12))",
    );
    arrives(
        "(lambda (xs) (map (lambda (x) (+ x 1)) xs))",
        "(equal? (received (list 1 2 3)) (list 2 3 4))",
    );
    arrives("(lambda (x) (point x x))", "(= (point-y (received 3)) 3)");
    arrives("point-x", "(= (received (point 8 9)) 8)");
    arrives("point", "(= (point-y (received 8 9)) 9)");
    arrives(
        "counter?",
        "(and (received (counter 1)) (not (received 1)))",
    );
    arrives(
        "(let ([cell (box 0)])
           (list (lambda () (set-box! cell (+ 1 (unbox cell)))) (lambda () (unbox cell))))",
        "(begin ((car received)) ((car received)) (= ((cadr received)) 2))",
    );
}

#[test]
fn functions_bring_the_globals_they_use() {
    let mut from = engine();
    from.run(
        "(define offset 100)
         (define (add-offset n) (+ n offset))
         (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
         (define (even-n? n) (if (= n 0) #t (odd-n? (- n 1))))
         (define (odd-n? n) (if (= n 0) #f (even-n? (- n 1))))
         (define (make-adder n) (lambda (m) (+ n m)))
         (define (twice f) (lambda (x) (f (f x))))",
    )
    .unwrap();

    for (expression, check) in [
        ("add-offset", "(= (received 1) 101)"),
        ("fact", "(= (received 10) 3628800)"),
        ("even-n?", "(and (received 10) (not (received 7)))"),
        ("(make-adder 5)", "(= (received 6) 11)"),
        ("make-adder", "(= ((received 1) 2) 3)"),
        ("(twice add-offset)", "(= (received 1) 201)"),
        (
            "(list fact add-offset)",
            "(= ((cadr received) ((car received) 3)) 106)",
        ),
    ] {
        let mut to = engine();
        send(&mut from, &mut to, expression)
            .unwrap_or_else(|e| panic!("sending `{}` failed: {}", expression, e));
        assert!(holds(&mut to, check), "`{}` and `{}`", expression, check);
    }
}

#[test]
fn many_values_cross_between_the_same_two_engines() {
    let mut from = engine();
    let mut to = engine();

    for n in 0..200 {
        let expression = format!(
            "(list {n} (number->string {n}) (box {n}) (point {n} (counter {n})) (lambda (m) (+ m {n})))"
        );
        send(&mut from, &mut to, &expression).unwrap();
        assert!(holds(
            &mut to,
            &format!(
                "(and (= (car received) {n})
                      (= (unbox (caddr received)) {n})
                      (= (counter-n (point-y (list-ref received 3))) {n})
                      (= ((list-ref received 4) 1) (+ 1 {n})))"
            )
        ));
    }

    // And back the other way.
    for n in 0..50 {
        send(
            &mut to,
            &mut from,
            &format!("(mutable-vector {n} (list {n}))"),
        )
        .unwrap();
        assert!(holds(
            &mut from,
            &format!("(= (car (vector-ref received 1)) {n})")
        ));
    }
}

/// The generator from the history tests, here so the two don't depend on each
/// other.
struct Random(u64);

impl Random {
    fn below(&mut self, n: u64) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0 % n
    }
}

/// An expression that makes some value, the same one each time it is run.
/// Nothing in it is compared by identity, so two of them are `equal?`.
fn random_value(random: &mut Random, depth: usize) -> String {
    let leaf = depth == 0 || random.below(4) == 0;
    if leaf {
        return match random.below(13) {
            0 => format!("{}", random.below(1000) as i64 - 500),
            1 => format!("{}.5", random.below(1000)),
            2 => format!("{}/{}", random.below(50) + 1, random.below(50) + 51),
            3 => format!("{}", u64::MAX as u128 * (random.below(1000) as u128 + 2)),
            4 => "#t".to_string(),
            5 => "#f".to_string(),
            6 => format!("\"string-{}\"", random.below(100)),
            7 => format!("'symbol-{}", random.below(20)),
            8 => format!("#\\{}", (b'a' + random.below(26) as u8) as char),
            9 => "'()".to_string(),
            10 => format!("(bytes {} {})", random.below(256), random.below(256)),
            11 => format!(
                "(make-rectangular {} {})",
                random.below(9),
                random.below(9) + 1
            ),
            _ => "void".to_string(),
        };
    }

    let mut children = |random: &mut Random| {
        (0..random.below(4))
            .map(|_| random_value(random, depth - 1))
            .collect::<Vec<_>>()
            .join(" ")
    };

    match random.below(10) {
        0 | 1 => format!("(list {})", children(random)),
        2 => format!("(immutable-vector {})", children(random)),
        3 => format!("(mutable-vector {})", children(random)),
        4 => format!("(flat-vector {})", children(random)),
        5 => format!("(box {})", random_value(random, depth - 1)),
        6 => format!(
            "(hash 'a {} \"b\" {})",
            random_value(random, depth - 1),
            random_value(random, depth - 1)
        ),
        7 => format!(
            "(point {} {})",
            random_value(random, depth - 1),
            random_value(random, depth - 1)
        ),
        8 => format!(
            "(record {} {} {})",
            random_value(random, depth - 1),
            random_value(random, depth - 1),
            random_value(random, depth - 1)
        ),
        _ => format!(
            "(cons {} {})",
            random_value(random, depth - 1),
            random_value(random, depth - 1)
        ),
    }
}

fn rounds() -> usize {
    std::env::var("STEEL_SERIALIZATION_ROUNDS")
        .ok()
        .and_then(|rounds| rounds.parse().ok())
        .unwrap_or(300)
}

#[test]
fn random_values_come_back_equal() {
    let mut engine = engine();
    engine
        .run(
            "(define (by-value v) (deserialize-value (serialize-value v)))
             (define (by-bytes v)
               (deserialize-value (bytes->serialized (serialized->bytes (serialize-value v)))))",
        )
        .unwrap();

    let mut random = Random(0x9E3779B97F4A7C15);
    for round in 0..rounds() {
        let expression = random_value(&mut random, 5);
        let check = format!(
            "(let ([made {expression}])
               (and (equal? (by-value made) made)
                    (equal? (by-bytes made) made)
                    (equal? (by-bytes (by-value made)) made)))"
        );
        assert!(
            holds(&mut engine, &check),
            "round {}: `{}` did not come back equal",
            round,
            expression
        );
    }
}

#[test]
fn random_values_cross_engines() {
    let mut from = engine();
    let mut to = engine();

    let mut random = Random(0xD1B54A32D192ED03);
    for round in 0..rounds() {
        let expression = random_value(&mut random, 5);
        send(&mut from, &mut to, &expression)
            .unwrap_or_else(|e| panic!("round {}: sending `{}` failed: {}", round, expression, e));
        assert!(
            holds(&mut to, &format!("(equal? received {})", expression)),
            "round {}: `{}` did not arrive equal",
            round,
            expression
        );
    }
}

#[test]
fn the_same_value_gives_the_same_bytes_as_data() {
    // Not the same bytes, since cells are named by where they are, but
    // bytes that read back to equal values however often it is done.
    let mut engine = engine();
    let mut random = Random(0x94D049BB133111EB);
    for _ in 0..50 {
        let expression = random_value(&mut random, 4);
        let check = format!(
            "(let* ([made {expression}]
                    [once (serialized->bytes (serialize-value made))]
                    [twice (serialized->bytes (serialize-value made))])
               (equal? (deserialize-value (bytes->serialized once))
                       (deserialize-value (bytes->serialized twice))))"
        );
        assert!(holds(&mut engine, &check), "`{}`", expression);
    }
}
