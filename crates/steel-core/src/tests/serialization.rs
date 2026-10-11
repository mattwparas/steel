//! Values serialized by one engine and read by another, which is what the
//! bytes are for, and random values sent both ways.
//!
//! The round trips within one engine are in
//! `success/serialization_round_trips.scm`.

use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;
use crate::values::functions::ByteCodeLambda;

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

/// Two engines with the shared definitions, one to send from and one to read
/// in. Made once for a test, since starting an engine is most of the cost.
struct Ends {
    from: Engine,
    to: Engine,
}

impl Ends {
    fn new() -> Self {
        Ends {
            from: engine(),
            to: engine(),
        }
    }

    /// Sends `expression` from one end to the other and checks it there.
    fn arrives(&mut self, expression: &str, check: &str) {
        send(&mut self.from, &mut self.to, expression)
            .unwrap_or_else(|e| panic!("sending `{}` failed: {}", expression, e));
        assert!(
            holds(&mut self.to, check),
            "`{}` arrived as something `{}` doesn't hold of",
            expression,
            check
        );
    }

    /// The same value made at both ends is `equal?`.
    fn arrives_equal(&mut self, expression: &str) {
        self.arrives(expression, &format!("(equal? received {})", expression));
    }
}

#[test]
fn plain_data_crosses_engines() {
    let mut ends = Ends::new();
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
        ends.arrives_equal(expression);
    }
}

#[test]
fn symbols_arrive_as_the_symbols_they_are_there() {
    let mut ends = Ends::new();
    ends.arrives("'a-symbol", "(eq? received 'a-symbol)");
    ends.arrives(
        "(list 'one (list 'two) (hash 'key 'three))",
        "(and (eq? (car received) 'one)
              (eq? (car (cadr received)) 'two)
              (eq? (hash-ref (caddr received) 'key) 'three))",
    );
    // One the other end has never seen.
    ends.arrives(
        "'a-symbol-made-up-for-this-test",
        "(equal? (symbol->string received) \"a-symbol-made-up-for-this-test\")",
    );
}

#[test]
fn cells_cross_engines() {
    let mut ends = Ends::new();
    ends.arrives(
        "(box (list 1 2))",
        "(and (equal? (unbox received) (list 1 2))
              (begin (set-box! received 'changed) (eq? (unbox received) 'changed)))",
    );
    ends.arrives(
        "(mutable-vector 1 2 3)",
        "(begin (vector-push! received 4) (vector-set! received 0 'changed)
                (and (= (vector-length received) 4) (eq? (vector-ref received 0) 'changed)))",
    );
    ends.arrives(
        "(let ([shared (box 1)]) (list shared shared))",
        "(begin (set-box! (car received) 'changed)
                (and (eq? (unbox (cadr received)) 'changed) (eq? (car received) (cadr received))))",
    );
    ends.arrives(
        "(list (box 1) (box 1))",
        "(not (eq? (car received) (cadr received)))",
    );
    ends.arrives(
        "(let ([b (box #f)]) (set-box! b (list b 1)) b)",
        "(eq? (car (unbox received)) received)",
    );
    ends.arrives(
        "(let ([v (mutable-vector #f 1)]) (vector-set! v 0 v) v)",
        "(and (eq? (vector-ref received 0) received) (= (vector-length received) 2))",
    );
}

#[test]
fn structs_cross_engines() {
    let mut ends = Ends::new();
    ends.arrives_equal("(point 1 2)");
    ends.arrives_equal("(list (point 1 (point 2 3)) (point \"a\" 'b))");
    ends.arrives(
        "(point 1 2)",
        "(and (point? received) (= (point-x received) 1) (not (counter? received)))",
    );
    ends.arrives(
        "(counter 5)",
        "(and (counter? received)
              (= (counter-n received) 5)
              (begin (set-counter-n! received 6) (= (counter-n received) 6)))",
    );
    ends.arrives_equal("(record \"name\" (list 1 2) (point 3 4))");
    ends.arrives(
        "(record \"name\" (list 1 2) #f)",
        "(begin (set-record-extra! received 'set) (eq? (record-extra received) 'set))",
    );
    ends.arrives(
        "(let ([c (counter 1)]) (list c (box c) (mutable-vector c)))",
        "(begin (set-counter-n! (car received) 'changed)
                (and (eq? (counter-n (unbox (cadr received))) 'changed)
                     (eq? (counter-n (vector-ref (caddr received) 0)) 'changed)))",
    );
    ends.arrives(
        "(let* ([a (counter #f)] [b (counter a)]) (set-counter-n! a b) a)",
        "(eq? (counter-n (counter-n received)) received)",
    );
    ends.arrives(
        "(let* ([cell (box #f)] [p (point cell 'cyclic)]) (set-box! cell p) p)",
        "(eq? (point-y (unbox (point-x received))) 'cyclic)",
    );
}

#[test]
fn functions_cross_engines() {
    let mut ends = Ends::new();
    ends.arrives("car", "(= (received (list 1 2)) 1)");
    ends.arrives("(lambda (x) (* x 2))", "(= (received 21) 42)");
    ends.arrives("(lambda args (length args))", "(= (received 1 2 3) 3)");
    ends.arrives(
        "(lambda () (list \"a constant\" 'quoted 1/3 12345678901234567890))",
        "(equal? (received) (list \"a constant\" 'quoted 1/3 12345678901234567890))",
    );
    ends.arrives("(let ([n 5]) (lambda (m) (+ n m)))", "(= (received 6) 11)");
    ends.arrives(
        "(let ([data (list 1 2 3)]) (lambda () (apply + data)))",
        "(= (received) 6)",
    );
    ends.arrives(
        "(let ([count (box 0)]) (lambda () (set-box! count (+ 1 (unbox count))) (unbox count)))",
        "(and (= (received) 1) (= (received) 2))",
    );
    ends.arrives(
        "(let ([state (counter 10)]) (lambda () (set-counter-n! state (+ 1 (counter-n state))) (counter-n state)))",
        "(and (= (received) 11) (= (received) 12))",
    );
    ends.arrives(
        "(lambda (xs) (map (lambda (x) (+ x 1)) xs))",
        "(equal? (received (list 1 2 3)) (list 2 3 4))",
    );
    ends.arrives("(lambda (x) (point x x))", "(= (point-y (received 3)) 3)");
    ends.arrives("point-x", "(= (received (point 8 9)) 8)");
    ends.arrives("point", "(= (point-y (received 8 9)) 9)");
    ends.arrives(
        "counter?",
        "(and (received (counter 1)) (not (received 1)))",
    );
    ends.arrives(
        "(let ([cell (box 0)])
           (list (lambda () (set-box! cell (+ 1 (unbox cell)))) (lambda () (unbox cell))))",
        "(begin ((car received)) ((car received)) (= ((cadr received)) 2))",
    );
}

fn received_function(engine: &mut Engine, expression: &str) -> crate::gc::Gc<ByteCodeLambda> {
    match engine.run(expression.to_string()).map(last) {
        Ok(SteelVal::Closure(function)) => function,
        other => panic!("`{}` is not a function here: {:?}", expression, other),
    }
}

/// A function that arrives is compiled like one that was defined here.
#[cfg(feature = "jit2")]
#[test]
fn received_functions_are_compiled() {
    if !crate::config::config().jit {
        return;
    }

    let mut ends = Ends::new();
    ends.from
        .run(
            "(define (sent-step x) (+ x 1))
             (define (sent-count i n acc)
               (if (= i n) acc (sent-count (+ i 1) n (+ acc (sent-step i)))))
             (define (sent-total n) (sent-count 0 n 0))
             (define (sent-adder n) (lambda (m) (+ n m)))",
        )
        .unwrap();

    for (expression, check) in [
        ("sent-total", "(= (received 1000) 500500)"),
        ("sent-count", "(= (received 0 1000 0) 500500)"),
        ("(sent-adder 5)", "(= (received 6) 11)"),
    ] {
        ends.arrives(expression, check);
        crate::steel_vm::vm::jit::wait_for_background_compiles();
        assert!(holds(&mut ends.to, check));
        assert!(
            received_function(&mut ends.to, "received")
                .super_instructions()
                .is_some(),
            "{} was not compiled",
            expression
        );
    }
}

/// A function that is sent again runs the body that was made for it the
/// first time, whatever each copy closes over.
#[test]
fn a_function_sent_again_shares_what_was_made_for_it() {
    let mut ends = Ends::new();
    ends.from
        .run(
            "(define (sent-double x) (* x 2))
             (define (sent-adder n) (lambda (m) (+ n m)))",
        )
        .unwrap();

    ends.arrives("sent-double", "(= (received 21) 42)");
    let first = received_function(&mut ends.to, "received");
    ends.arrives("sent-double", "(= (received 4) 8)");
    let second = received_function(&mut ends.to, "received");
    assert!(crate::gc::Gc::ptr_eq(&first, &second));

    ends.arrives("(sent-adder 1)", "(= (received 10) 11)");
    let one = received_function(&mut ends.to, "received");
    ends.arrives("(sent-adder 2)", "(= (received 10) 12)");
    let two = received_function(&mut ends.to, "received");
    assert!(core::ptr::eq(one.body_exp.as_ptr(), two.body_exp.as_ptr()));

    // Several in one value, and the ones from before still give their own.
    ends.arrives(
        "(map sent-adder (list 10 20 30))",
        "(equal? (map (lambda (add) (add 1)) received) (list 11 21 31))",
    );
    assert_eq!(one.captures().len(), 1);
    ends.to.register_value("kept-adder", SteelVal::Closure(one));
    assert!(holds(&mut ends.to, "(= (kept-adder 10) 11)"));
}

/// Two engines in one process give their globals and constants different
/// places, and a function keeps its id between them. What it refers to is
/// what is here, in its loops and inner functions too.
#[test]
fn functions_refer_to_what_is_here_in_the_same_process() {
    let mut ends = Ends::new();
    // Globals and constants that only the reader has, to move its indices.
    ends.to
        .run(
            "(define only-here-a 'only-here)
             (define only-here-b \"only here\")
             (define (only-here-c x) (list x 'another \"constant\" 77777))",
        )
        .unwrap();
    ends.from
        .run(
            "(define sent-offset 1000)
             (define (sent-label x) (list 'label x \"text\"))
             (define (sent-walk n)
               (let loop ([i 0] [acc '()])
                 (if (= i n)
                     (map (lambda (x) (sent-label (+ x sent-offset))) acc)
                     (loop (+ i 1) (cons (* i 3) acc)))))
             ;; Run where it was defined, so that its inner functions exist
             ;; there too.
             (sent-walk 3)",
        )
        .unwrap();

    for _ in 0..3 {
        ends.arrives(
            "sent-walk",
            "(equal? (received 3)
                     (list (list 'label 1006 \"text\")
                           (list 'label 1003 \"text\")
                           (list 'label 1000 \"text\")))",
        );
        #[cfg(feature = "jit2")]
        crate::steel_vm::vm::jit::wait_for_background_compiles();
        assert!(keeps_giving(
            &mut ends.to,
            "3",
            "(list (list 'label 1006 \"text\") (list 'label 1003 \"text\") (list 'label 1000 \"text\"))"
        ));
    }
    assert!(holds(
        &mut ends.to,
        "(equal? (only-here-c 1) (list 1 'another \"constant\" 77777))"
    ));
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

#[test]
fn what_is_shared_arrives_shared() {
    let mut ends = Ends::new();
    // One value referred to from two places is one entry of what is sent.
    ends.arrives(
        "(let ([shared (immutable-vector 1 2 3)]) (list shared shared))",
        "(eq? (car received) (cadr received))",
    );
    ends.arrives(
        "(let ([shared (hash 'a 1)]) (list shared (box shared)))",
        "(eq? (car received) (unbox (cadr received)))",
    );
    ends.arrives(
        "(let ([shared (point 1 2)]) (mutable-vector shared shared))",
        "(eq? (vector-ref received 0) (vector-ref received 1))",
    );
    ends.arrives(
        "(let ([shared (lambda (x) x)]) (list shared shared))",
        "(eq? (car received) (cadr received))",
    );
    ends.arrives(
        "(let ([shared (cons 1 2)]) (list shared shared))",
        "(eq? (car received) (cadr received))",
    );
    // And two that only look alike arrive as two.
    ends.arrives(
        "(list (immutable-vector 1 2 3) (immutable-vector 1 2 3))",
        "(not (eq? (car received) (cadr received)))",
    );
}

#[test]
fn very_deep_values_cross_engines() {
    let mut from = engine();
    let mut to = engine();
    let define = "(define (pair-chain n acc) (if (= n 0) acc (pair-chain (- n 1) (cons n acc))))
                  (define (lists-in-lists n acc) (if (= n 0) acc (lists-in-lists (- n 1) (list acc))))
                  (define (down-cdrs v n) (if (pair? v) (down-cdrs (cdr v) (+ n 1)) n))
                  (define (down-lists v n) (if (and (list? v) (not (null? v))) (down-lists (car v) (+ n 1)) n))";
    from.run(define).unwrap();
    to.run(define).unwrap();

    send(&mut from, &mut to, "(pair-chain 300000 'end)").unwrap();
    assert!(holds(&mut to, "(= (down-cdrs received 0) 300000)"));
    send(&mut from, &mut to, "(lists-in-lists 300000 'end)").unwrap();
    assert!(holds(&mut to, "(= (down-lists received 0) 300000)"));
}

/// The bytes `expression` serializes to in `from`.
fn bytes_of(from: &mut Engine, expression: &str) -> Result<Vec<u8>, String> {
    let bytes = from
        .run(format!(
            "(serialized->bytes (serialize-value {}))",
            expression
        ))
        .map(last)
        .map_err(|e| e.to_string())?;
    match bytes {
        SteelVal::ByteVector(bytes) => Ok(bytes.vec.read().to_vec()),
        other => Err(format!("not bytes: {}", other)),
    }
}

/// Binds what `to` reads of `bytes` to `received` there.
fn read(to: &mut Engine, bytes: Vec<u8>) -> Result<(), String> {
    to.register_value(
        "incoming-bytes",
        SteelVal::ByteVector(crate::rvals::SteelByteVector::new(bytes)),
    );
    to.run("(define received (deserialize-value (bytes->serialized incoming-bytes)))")
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// What `send` does, with the bytes marked as written by another process.
/// The mark follows the byte the format is named by. Whoever reads them
/// then trusts none of what only means something in one process: function
/// ids, and the names that a compiler or `gensym` made up.
fn send_from_elsewhere(from: &mut Engine, to: &mut Engine, expression: &str) -> Result<(), String> {
    let mut bytes = bytes_of(from, expression)?;
    bytes[1] ^= 0xFF;
    read(to, bytes)
}

type Send = fn(&mut Engine, &mut Engine, &str) -> Result<(), String>;

const WAYS: [(&str, Send); 2] = [
    ("one process", send),
    ("another process", send_from_elsewhere),
];

/// Calls `(received argument)` often enough that it is compiled, and checks
/// that it goes on giving `expected`.
fn keeps_giving(engine: &mut Engine, argument: &str, expected: &str) -> bool {
    holds(
        engine,
        &format!(
            "(let loop ([i 0])
               (cond [(= i 3000) #t]
                     [(equal? (with-handler (lambda (e) 'raised) (received {argument})) {expected}) (loop (+ i 1))]
                     [else #f]))"
        ),
    )
}

#[test]
fn functions_run_where_their_constants_and_helpers_have_never_been() {
    for (way, send) in WAYS {
        let mut from = engine();
        from.run(
            "(define offset 100)
             (define (describe x)
               (list x \"a string made up for this test\" 'a-symbol-made-up-for-this-test 2.5
                     '(1 \"two\" three) (immutable-vector 1 2)))
             (define (sum-to n)
               (let loop ([i 0] [acc 0]) (if (> i n) acc (loop (+ i 1) (+ acc i)))))
             (define (classify n)
               (cond [(< n 0) 'negative] [(= n 0) 'zero] [(even? n) 'even] [else 'odd]))
             (define (adders ns) (map (lambda (n) (lambda (m) (+ n m offset))) ns))
             (define (apply-all ns) (map (lambda (f) (f 1)) (adders ns)))
             (define (count-args . args) (length args))
             (define (nested x)
               (let ([inner (lambda (y) (if (> y 0) (string-append \"positive-\" (number->string y)) \"not\"))])
                 (list (inner x) (inner (- x)))))",
        )
        .unwrap();

        // An engine that has compiled other things first, so that nothing
        // it numbers lines up with the sender.
        let mut to = Engine::new();
        to.run(
            "(define padding-a \"a constant only this end has\")
             (define (padding-b x) (let loop ([i 0]) (if (< i x) (loop (+ i 1)) 'padding)))
             (define padding-c (list (lambda (x) x) (lambda (x y) y)))",
        )
        .unwrap();
        to.run(SHARED).unwrap();

        for (expression, argument, expected) in [
            (
                "describe",
                "7",
                "(list 7 \"a string made up for this test\" 'a-symbol-made-up-for-this-test 2.5
                       '(1 \"two\" three) (immutable-vector 1 2))",
            ),
            ("sum-to", "100", "5050"),
            ("classify", "-3", "'negative"),
            ("classify", "0", "'zero"),
            ("classify", "7", "'odd"),
            ("apply-all", "(list 1 2)", "(list 102 103)"),
            ("count-args", "1", "1"),
            ("nested", "3", "(list \"positive-3\" \"not\")"),
            ("(car (adders (list 5)))", "1", "106"),
            (
                "(lambda (n) (let ([p (point n (counter n))]) (list (point-x p) (counter-n (point-y p)))))",
                "4",
                "(list 4 4)",
            ),
        ] {
            // Twice, since the second time what the first left is here.
            for _ in 0..2 {
                send(&mut from, &mut to, expression)
                    .unwrap_or_else(|e| panic!("{}: sending `{}` failed: {}", way, expression, e));
                assert!(
                    keeps_giving(&mut to, argument, expected),
                    "{}: `({} {})` is not `{}`",
                    way,
                    expression,
                    argument,
                    expected
                );
            }
        }
    }
}

#[test]
fn struct_types_laid_out_differently_are_refused() {
    for (way, send) in WAYS {
        let mut from = engine();

        let mut to = Engine::new();
        to.run(
            "(struct point (x y z) #:transparent)
             (struct counter (n))
             (struct record (name items other) #:mutable #:transparent)",
        )
        .unwrap();

        for (expression, reason) in [
            ("(point 1 2)", "fields"),
            ("(list 1 (box (point 1 2)))", "fields"),
            ("point", "fields"),
            ("point-x", "fields"),
            ("point?", "fields"),
            ("(counter 1)", "mutable"),
            ("counter", "mutable"),
            ("counter-n", "mutable"),
            ("set-counter-n!", "mutable"),
            ("(record 1 2 3)", "fields"),
            ("record-extra", "fields"),
        ] {
            match send(&mut from, &mut to, expression) {
                Ok(()) => panic!("{}: `{}` was read", way, expression),
                Err(e) => assert!(
                    e.contains(reason),
                    "{}: `{}` was refused with `{}`",
                    way,
                    expression,
                    e
                ),
            }
        }

        // Nothing was left half defined.
        assert!(holds(&mut to, "(= 3 (point-z (point 1 2 3)))"));
    }
}

#[test]
fn mutable_struct_functions_find_their_type_where_they_are_read() {
    for (way, send) in WAYS {
        let mut from = engine();

        // The same struct types, numbered differently.
        let mut to = Engine::new();
        to.run(
            "(struct filler-a (x) #:mutable)
             (struct filler-b (x y z w))
             (struct filler-c (x y z) #:mutable)",
        )
        .unwrap();
        to.run(SHARED).unwrap();

        for (expression, check) in [
            ("counter", "(let ([c (received 5)]) (and (counter? c) (= (counter-n c) 5)))"),
            ("counter-n", "(= (received (counter 7)) 7)"),
            (
                "set-counter-n!",
                "(let ([c (counter 1)]) (received c 9) (= (counter-n c) 9))",
            ),
            (
                "(lambda (n) (let ([c (counter n)]) (set-counter-n! c (+ 1 (counter-n c))) c))",
                "(let ([c (received 1)]) (and (counter? c) (not (filler-a? c)) (= (counter-n c) 2)))",
            ),
            (
                "record",
                "(let ([r (received 1 2 3)]) (and (record? r) (= (record-extra r) 3)))",
            ),
            ("record-items", "(= (received (record 1 2 3)) 2)"),
        ] {
            for _ in 0..2 {
                send(&mut from, &mut to, expression)
                    .unwrap_or_else(|e| panic!("{}: sending `{}` failed: {}", way, expression, e));
                assert!(holds(&mut to, check), "{}: `{}` and `{}`", way, expression, check);
            }
        }

        // Compiled, too.
        send(&mut from, &mut to, "(lambda (n) (counter-n (counter n)))").unwrap();
        assert!(keeps_giving(&mut to, "4", "4"), "{}", way);
    }
}

const HELPERS: &str = "arity/helpers.scm";

fn with_module(name: &str, source: &str) -> Engine {
    let mut engine = engine();
    engine.register_steel_module(name.to_owned(), source.to_owned());
    engine
        .run(format!("(require \"{}\")", name))
        .expect("the module loads");
    engine
}

#[test]
fn calls_check_the_arity_of_the_function_they_find() {
    // The sender compiles the call to `helper` without an arity check,
    // since it can see `helper`.
    let there = "(provide run)
                 (define (helper a b) (if (= a 0) b (helper (- a 1) (+ b 1))))
                 (define (run x) (+ 1 (helper x 1)))";
    // Whoever reads it has a module of that name with another `helper`.
    let here = "(provide run-here)
                (define (helper a b c d) (if (= a -1) (helper a b c d) (list a b c d)))
                (define (run-here x) (helper x 2 3 4))";

    for (way, send) in WAYS {
        let mut from = with_module(HELPERS, there);

        let mut to = with_module(HELPERS, here);
        send(&mut from, &mut to, "run")
            .unwrap_or_else(|e| panic!("{}: sending failed: {}", way, e));
        assert!(keeps_giving(&mut to, "5", "'raised"), "{}", way);
        assert!(holds(&mut to, "(equal? (run-here 1) (list 1 2 3 4))"));

        // One with the same module uses its own `helper`.
        let mut to = with_module(HELPERS, there);
        send(&mut from, &mut to, "run").unwrap();
        assert!(keeps_giving(&mut to, "5", "7"), "{}", way);

        // And one without the module is sent `helper`.
        let mut to = engine();
        send(&mut from, &mut to, "run").unwrap();
        assert!(keeps_giving(&mut to, "5", "7"), "{}", way);
    }
}

#[test]
fn a_module_that_is_here_keeps_its_own_state() {
    let counter = "(provide bump! current)
                   (define count 0)
                   (define (bump!) (set! count (+ count 1)) count)
                   (define (current) count)";

    for (way, send) in WAYS {
        let mut from = with_module("state/counter.scm", counter);
        from.run("(bump!) (bump!) (bump!)").unwrap();

        // Its globals are numbered differently.
        let mut to = Engine::new();
        to.run("(define padding-a 1) (define padding-b 2) (define (padding-c) 3)")
            .unwrap();
        to.register_steel_module("state/counter.scm".to_owned(), counter.to_owned());
        to.run("(require \"state/counter.scm\")").unwrap();

        send(&mut from, &mut to, "bump!")
            .unwrap_or_else(|e| panic!("{}: sending failed: {}", way, e));
        assert!(
            holds(
                &mut to,
                "(and (= (received) 1) (= (received) 2) (= (current) 2))"
            ),
            "{}",
            way
        );
        assert!(holds(&mut from, "(= (current) 3)"), "{}", way);
        assert!(holds(
            &mut to,
            "(and (= padding-a 1) (= padding-b 2) (= (padding-c) 3))"
        ));

        // Without the module, the function brings the state it had.
        let mut to = engine();
        send(&mut from, &mut to, "bump!").unwrap();
        assert!(
            holds(&mut to, "(and (= (received) 4) (= (received) 5))"),
            "{}",
            way
        );
    }
}

#[test]
fn bytes_that_were_changed_are_read_or_refused() {
    let mut engine = engine();
    engine
        .run(
            "(define ten 10)
             (define (add-ten x) (+ x ten))
             (define (try-reading bs)
               (with-handler (lambda (e) 'refused)
                 (begin (deserialize-value (bytes->serialized bs)) 'read)))",
        )
        .unwrap();
    let try_reading = engine.extract_value("try-reading").unwrap();

    let mut random = Random(0xA0761D6478BD642F);
    let mut read = 0;

    for expression in [
        "(list 1 \"two\" 'three 4.5 #\\5 (cons 1 2) 1/3 12345678901234567890 (bytes 1 2))",
        "(let ([b (box #f)]) (set-box! b (list b (mutable-vector 1 b))) b)",
        "(list (point 1 2) (counter 3) point-x point counter counter-n)",
        "(let ([n 5]) (lambda (m) (if (> m n) (list (add-ten m) n \"const\" 'sym) (map (lambda (x) (+ x n)) (list m)))))",
        "(hash 'a (hashset 1 (box 2)) (box 'b) (immutable-vector 1 2) (list (record 1 2 3)) 4)",
        "(let* ([b (box #f)] [p (cons b 1)]) (set-box! b (cons p 2)) p)",
    ] {
        let whole = bytes_of(&mut engine, expression).unwrap();

        for round in 0..rounds() * 10 {
            let mut bytes = whole.clone();
            // The first byte names the format, and changing it is always
            // refused.
            let at = 1 + random.below(bytes.len() as u64 - 1) as usize;
            match random.below(6) {
                0 => bytes.truncate(at),
                1 => bytes[at] = 0xFF,
                2 => bytes[at] = 0,
                3 => bytes[at] = bytes[at].wrapping_add(1),
                4 => bytes[at] = bytes[at].wrapping_sub(1),
                _ => bytes[at] ^= 1 << random.below(8),
            }
            if bytes.len() > 1 && random.below(4) == 0 {
                let again = 1 + random.below(bytes.len() as u64 - 1) as usize;
                bytes[again] ^= 1 << random.below(8);
            }

            let argument = SteelVal::ByteVector(crate::rvals::SteelByteVector::new(bytes.clone()));
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                engine.call_function_with_args(try_reading.clone(), vec![argument])
            }));
            match outcome {
                Ok(Ok(SteelVal::SymbolV(outcome))) => read += usize::from(outcome.as_str() == "read"),
                Ok(other) => panic!("`{}` round {}: {:?}", expression, round, other),
                Err(_) => panic!(
                    "`{}` round {}: reading panicked on {:?}",
                    expression, round, bytes
                ),
            }
        }
    }

    // Some changes leave a value that can be read, or nothing was tried.
    assert!(read > 0);
}
