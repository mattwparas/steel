use crate::steel_vm::engine::Engine;
use crate::SteelVal;

const HELPERS: &str = r#"
(provide (for-syntax quadruple) (for-syntax helper-name) (for-syntax own-name))

(begin-for-syntax
  (define (quadruple x) (* 4 x))
  (define helper-name 'from-helpers))

(define-syntax (own-name stx)
  (syntax-case stx ()
    [(_) #`(quote #,helper-name)]))
"#;

const OTHER: &str = r#"
(provide (for-syntax other-name))

(begin-for-syntax
  (define helper-name 'from-other))

(define-syntax (other-name stx)
  (syntax-case stx ()
    [(_) #`(quote #,helper-name)]))
"#;

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_steel_module("phase1/helpers".to_owned(), HELPERS.to_owned());
    engine.register_steel_module("phase1/other".to_owned(), OTHER.to_owned());
    engine
}

fn last(engine: &mut Engine, program: &str) -> SteelVal {
    engine
        .run(program.to_owned())
        .unwrap()
        .last()
        .cloned()
        .unwrap()
}

#[test]
fn transformers_use_the_begin_for_syntax_of_the_top_level() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(begin-for-syntax
  (define (double x) (* 2 x)))

(define-syntax (doubled stx)
  (syntax-case stx ()
    [(_ v) (double (syntax-e #'v))]))

(defmacro (doubled-too stx)
  (double (syntax-e (cadr (syntax-e stx)))))

(define-syntax (shadowed stx)
  (define (double x) (* 10 x))
  (syntax-case stx ()
    [(_ v) (double (syntax-e #'v))]))

(equal? (list (doubled 2) (doubled-too 3) (shadowed 4)) '(4 6 40))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn transformers_use_the_begin_for_syntax_of_their_module() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(require "phase1/helpers" "phase1/other")

(equal? (list (own-name) (other-name)) '(from-helpers from-other))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn begin_for_syntax_bindings_provided_for_syntax() {
    let mut engine = engine();

    engine.register_steel_module(
        "phase1/user".to_owned(),
        r#"
(require "phase1/helpers")
(require (prefix-in h. "phase1/helpers"))
(require (only-in "phase1/helpers" (quadruple times-four)))
(provide results)

(define-syntax (quadrupled stx)
  (syntax-case stx ()
    [(_ v) (quadruple (syntax-e #'v))]))

(defmacro (quadrupled-too stx)
  (quadruple (syntax-e (cadr (syntax-e stx)))))

(define-syntax (prefixed stx)
  (syntax-case stx ()
    [(_ v) (h.quadruple (syntax-e #'v))]))

(define-syntax (renamed stx)
  (syntax-case stx ()
    [(_ v) (times-four (syntax-e #'v))]))

(define-syntax (name-of stx)
  (syntax-case stx ()
    [(_) #`(quote #,helper-name)]))

(define results (list (quadrupled 1) (quadrupled-too 2) (prefixed 3) (renamed 4) (name-of)))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "phase1/user")

(equal? results '(4 8 12 16 from-helpers))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn begin_for_syntax_bindings_at_the_top_level_from_a_module() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(require "phase1/helpers")

(define-syntax (quadrupled stx)
  (syntax-case stx ()
    [(_ v) (quadruple (syntax-e #'v))]))

(equal? (quadrupled 5) 20)
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const COUNTER: &str = r#"
(provide next!)
(define n 0)
(define (next!) (set! n (+ n 1)) n)
"#;

fn counted(name: &str) -> String {
    format!(
        r#"
(require (for-syntax "phase1/counter"))
(provide {name})
(define-syntax (count stx)
  (syntax-case stx ()
    [(_) (next!)]))
(define {name} (list (count) (count)))
"#
    )
}

#[test]
fn each_module_gets_its_own_instance_for_syntax() {
    let mut engine = engine();
    engine.register_steel_module("phase1/counter".to_owned(), COUNTER.to_owned());
    engine.register_steel_module("phase1/a".to_owned(), counted("a"));
    engine.register_steel_module("phase1/b".to_owned(), counted("b"));

    let result = last(
        &mut engine,
        r#"
(require "phase1/a" "phase1/b" "phase1/counter")

(equal? (list a b (next!)) '((1 2) (1 2) 1))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn a_for_syntax_require_imports_nothing_at_run_time() {
    let mut engine = engine();
    engine.register_steel_module("phase1/counter".to_owned(), COUNTER.to_owned());

    super::assert_free_identifier(
        engine.run(r#"(require (for-syntax "phase1/counter")) (next!)"#),
        "next!",
    );
}

#[test]
fn for_syntax_requires_take_only_in_and_prefix_in() {
    let mut engine = engine();
    engine.register_steel_module("phase1/counter".to_owned(), COUNTER.to_owned());

    let result = last(
        &mut engine,
        r#"
(require (for-syntax (only-in "phase1/counter" (next! bump!))))
(require (for-syntax (prefix-in c. "phase1/counter")))

(define-syntax (counts stx)
  (syntax-case stx ()
    [(_) #`(list #,(bump!) #,(bump!) #,(c.next!))]))

(equal? (counts) '(1 2 3))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const READER: &str = r#"
(provide first-line)
(define (first-line path) (read-line-from-port (open-input-file path)))
"#;

fn reads_a_file(engine: &mut Engine) -> crate::rvals::Result<Vec<SteelVal>> {
    engine.register_steel_module("phase1/reader".to_owned(), READER.to_owned());

    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/Cargo.toml");

    engine.run(format!(
        r#"
(require (for-syntax "phase1/reader"))

(define-syntax (manifest-line stx)
  (syntax-case stx ()
    [(_) (first-line {path:?})]))

(manifest-line)
"#
    ))
}

#[test]
fn macros_are_sandboxed() {
    let mut engine = engine();

    let error = reads_a_file(&mut engine).unwrap_err();
    assert!(error.to_string().contains("sandboxed"));
}

#[test]
fn an_engine_can_let_its_macros_out_of_the_sandbox() {
    let mut engine = Engine::new_with_unsandboxed_macros();

    let result = reads_a_file(&mut engine).unwrap();

    assert_eq!(
        result.last(),
        Some(&SteelVal::StringV("[package]\n".into()))
    );
}

#[test]
fn an_engine_can_share_its_modules_with_macros() {
    fn engine_with_host_module() -> Engine {
        let mut engine = Engine::new();

        let mut module = crate::steel_vm::builtin::BuiltInModule::new("phase1/host");
        module.register_value("host-value", SteelVal::IntV(41));
        engine.register_module(module);

        engine.register_steel_module(
            "phase1/uses-host".to_owned(),
            r#"
(require-builtin phase1/host)
(provide value-from-host)
(define (value-from-host) (+ host-value 1))
"#
            .to_owned(),
        );

        engine
    }

    let program = r#"
(require (for-syntax "phase1/uses-host"))

(define-syntax (from-host stx)
  (syntax-case stx ()
    [(_) (value-from-host)]))

(from-host)
"#;

    let error = engine_with_host_module().run(program).unwrap_err();
    assert!(error.to_string().contains("phase1/host"));

    let mut engine = engine_with_host_module();
    engine.share_modules_with_macros();

    assert_eq!(
        engine.run(program).unwrap().last(),
        Some(&SteelVal::IntV(42))
    );
}

const COUNTING_MACRO: &str = r#"
(require (for-syntax "phase1/counter"))
(provide count)
(define-syntax (count stx)
  (syntax-case stx ()
    [(_) (next!)]))
"#;

fn counts(name: &str) -> String {
    format!(
        r#"
(require "phase1/counting-macro")
(provide {name})
(define {name} (list (count) (count)))
"#
    )
}

#[test]
fn each_module_using_macros_gets_its_own_instance() {
    let mut engine = engine();
    engine.register_steel_module("phase1/counter".to_owned(), COUNTER.to_owned());
    engine.register_steel_module(
        "phase1/counting-macro".to_owned(),
        COUNTING_MACRO.to_owned(),
    );
    engine.register_steel_module("phase1/uses-a".to_owned(), counts("a"));
    engine.register_steel_module("phase1/uses-b".to_owned(), counts("b"));

    let result = last(
        &mut engine,
        r#"
(require "phase1/uses-a" "phase1/uses-b")

(equal? (list a b) '((1 2) (1 2)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn begin_for_syntax_state_is_shared_within_a_module_using_it() {
    let mut engine = engine();

    engine.register_steel_module(
        "phase1/stateful".to_owned(),
        r#"
(provide (for-syntax bump!) bumped)
(begin-for-syntax
  (define n 0)
  (define (bump!) (set! n (+ n 1)) n))
(define-syntax (bumped stx) (syntax-case stx () [(_) (bump!)]))
"#
        .to_owned(),
    );

    engine.register_steel_module(
        "phase1/also-bumps".to_owned(),
        r#"
(require "phase1/stateful")
(provide also-bumped)
(define-syntax (also-bumped stx) (syntax-case stx () [(_) (bump!)]))
"#
        .to_owned(),
    );

    for name in ["first", "second"] {
        engine.register_steel_module(
            format!("phase1/{name}"),
            format!(
                r#"
(require "phase1/stateful" "phase1/also-bumps")
(provide {name})
(define {name} (list (bumped) (also-bumped) (bumped) (also-bumped)))
"#
            ),
        );
    }

    let result = last(
        &mut engine,
        r#"
(require "phase1/first" "phase1/second")

(and (equal? first second)
     (equal? (sort first <) '(1 2 3 4)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const TOOLS: &str = r#"
(provide twice-of)
(define (private-double x) (* 2 x))
(define-syntax twice-of
  (syntax-rules ()
    [(_ e) (private-double e)]))
"#;

#[test]
fn transformers_use_macros_required_for_syntax() {
    let mut engine = engine();
    engine.register_steel_module("phase1/tools".to_owned(), TOOLS.to_owned());

    engine.register_steel_module(
        "phase1/uses-tools".to_owned(),
        r#"
(require (for-syntax "phase1/tools"))
(provide doubled doubled-too helped)
(begin-for-syntax
  (define helped-value (twice-of 10)))
(define-syntax (doubled stx)
  (syntax-case stx ()
    [(_ v) (twice-of (syntax-e #'v))]))
(defmacro (doubled-too stx) (twice-of (syntax-e (cadr (syntax-e stx)))))
(define-syntax (helped stx)
  (syntax-case stx ()
    [(_) helped-value]))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "phase1/uses-tools")

(define-syntax twice-of
  (syntax-rules ()
    [(_ e) (list 'run-time e)]))

(define-syntax (in-a-template stx)
  (syntax-case stx ()
    [(_) #'(twice-of 1)]))

(equal? (list (doubled 4) (doubled-too 5) (helped) (in-a-template))
        '(8 10 20 (run-time 1)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn the_top_level_uses_macros_required_for_syntax() {
    let mut engine = engine();
    engine.register_steel_module("phase1/tools".to_owned(), TOOLS.to_owned());

    engine
        .run(
            r#"
(require (for-syntax "phase1/tools"))
(define-syntax (doubled stx)
  (syntax-case stx ()
    [(_ v) (twice-of (syntax-e #'v))]))
"#,
        )
        .unwrap();

    assert_eq!(last(&mut engine, "(doubled 21)"), SteelVal::IntV(42));
}

const SCOPED_TOOLS: &str = r#"
(provide twice-of my-or x-first)
(define (private-double x) (* 2 x))
(define-syntax twice-of
  (syntax-rules ()
    [(_ e) (private-double e)]))
(define-syntax my-or
  (syntax-rules ()
    [(_ a b) (let ([t a]) (if t t b))]))
(define-syntax x-first
  (syntax-rules ()
    [(_ l) (car l)]))
"#;

#[test]
fn macros_required_for_syntax_respect_scope_in_transformer_code() {
    let mut engine = engine();
    engine.register_steel_module("phase1/scoped-tools".to_owned(), SCOPED_TOOLS.to_owned());

    engine.register_steel_module(
        "phase1/scoped".to_owned(),
        r#"
(require (for-syntax "phase1/scoped-tools"))
(provide by-parameter by-let by-binder by-builtin)

;; A local of the same name hides the macro
(define-syntax (by-parameter stx)
  (syntax-case stx ()
    [(_) ((lambda (twice-of) (twice-of 3)) (lambda (x) (+ 100 x)))]))

(define-syntax (by-let stx)
  (syntax-case stx ()
    [(_) (let ([twice-of 7]) (+ twice-of 1))]))

;; The macro's own binders don't capture the transformer's variables
(define-syntax (by-binder stx)
  (syntax-case stx ()
    [(_) (let ([t 5]) (my-or #f t))]))

;; The macro's template means the builtin, whatever the transformer binds
(define-syntax (by-builtin stx)
  (syntax-case stx ()
    [(_) (let ([car cdr]) (x-first '(1 2)))]))

(define results (list (by-parameter) (by-let) (by-binder) (by-builtin)))
(provide results)
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "phase1/scoped")

(equal? (list results (list (by-parameter) (by-let) (by-binder) (by-builtin)))
        '((103 8 5 1) (103 8 5 1)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn unsyntax_anywhere_in_a_template_sees_begin_for_syntax() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(begin-for-syntax
  (define (hundred x) (* x 100))
  (define (some) (list 1 2)))

(define-syntax (first-place stx)
  (syntax-case stx ()
    [(_) #`(list #,(hundred 1) 2)]))

(define-syntax (alone stx)
  (syntax-case stx ()
    [(_) #`(+ #,(hundred 2))]))

(define-syntax (spliced stx)
  (syntax-case stx ()
    [(_) #`(list 0 #,@(some) 3)]))

(define-syntax (outside stx)
  (syntax-case stx ()
    [(_) (let ([n (hundred 3)]) #`(list #,n 4))]))

(equal? (list (first-place) (alone) (spliced) (outside))
        '((100 2) 200 (0 1 2 3) (300 4)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const REMEMBERING: &str = r#"
(provide remember emit names-here)

(begin-for-syntax
  (define saved '())
  (define (get) saved))

(define-syntax (remember stx)
  (syntax-case stx ()
    [(_ id) (begin (set! saved (cons (syntax->datum #'id) saved)) #'(quote id))]))

(define-syntax (emit stx)
  (syntax-case stx ()
    [(_) #`(quote #,(get))]))

(remember here)

(define names-here (emit))
"#;

#[test]
fn transformers_share_begin_for_syntax_variables() {
    let mut engine = engine();
    engine.register_steel_module("phase1/remembering".to_owned(), REMEMBERING.to_owned());

    let result = last(
        &mut engine,
        r#"
(require "phase1/remembering")

(begin-for-syntax
  (define seen '())
  (define (push! x) (set! seen (cons x seen))))

(define-syntax (see stx)
  (syntax-case stx ()
    [(_ id) (begin (push! (syntax->datum #'id)) #'(quote id))]))

(define-syntax (seen-so-far stx)
  (syntax-case stx ()
    [(_) #`(quote #,seen)]))

(see a)
(see b)
(remember x)
(remember y)

(equal? (list names-here (emit) (seen-so-far)) '((here) (y x) (b a)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn begin_for_syntax_across_runs() {
    let mut engine = engine();

    engine
        .run(r#"(begin-for-syntax (define (scale x) (* 2 x)))"#)
        .unwrap();

    engine
        .run(r#"(defmacro (scaled stx) (scale (syntax-e (cadr (syntax-e stx)))))"#)
        .unwrap();

    assert_eq!(last(&mut engine, "(scaled 4)"), SteelVal::IntV(8));

    engine
        .run(r#"(begin-for-syntax (define (scale x) (* 5 x)))"#)
        .unwrap();

    let result = last(
        &mut engine,
        r#"
(define-syntax (scaled-again stx)
  (syntax-case stx ()
    [(_ v) (scale (syntax-e #'v))]))

(scaled-again 4)
"#,
    );

    assert_eq!(result, SteelVal::IntV(20));
}

const LOG: &str = r#"
(provide log record!)

(define log (box '()))

(define (record! x) (set-box! log (cons x (unbox log))))
"#;

const NOISY: &str = r#"
(require "phase1/log")
(provide noisy-value)

(record! 'noisy)

(define noisy-value 'noisy-value)
"#;

const USES_NOISY: &str = r#"
(require (for-syntax "phase1/noisy"))
(provide noisy-name)

(define-syntax (noisy-name stx)
  (syntax-case stx ()
    [(_) #`(quote #,noisy-value)]))
"#;

fn noisy() -> Engine {
    let mut engine = engine();
    engine.register_steel_module("phase1/log".to_owned(), LOG.to_owned());
    engine.register_steel_module("phase1/noisy".to_owned(), NOISY.to_owned());
    engine.register_steel_module("phase1/uses-noisy".to_owned(), USES_NOISY.to_owned());
    engine
}

#[test]
fn a_module_required_for_syntax_runs_only_at_phase_1() {
    let result = last(
        &mut noisy(),
        r#"
(require "phase1/uses-noisy" "phase1/log")

(equal? (list (noisy-name) (unbox log)) '(noisy-value ()))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn a_module_required_for_syntax_and_later_at_phase_0_runs_once_there() {
    let result = last(
        &mut noisy(),
        r#"
(require "phase1/uses-noisy")
(require "phase1/noisy")
(require "phase1/log")

(equal? (list (noisy-name) noisy-value (unbox log)) '(noisy-value noisy-value (noisy)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));

    let result = last(
        &mut noisy(),
        r#"
(require "phase1/noisy" (for-syntax "phase1/noisy"))
(require "phase1/log")

(define-syntax (value-at-phase-1 stx)
  (syntax-case stx ()
    [(_) #`(quote #,noisy-value)]))

(equal? (list (value-at-phase-1) (unbox log)) '(noisy-value (noisy)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const SPAWNS: &str = r#"
(provide spawned)
(define (spawned) (spawn-process (command "true" '())))
(define (never-called) (delete-file! "never-called"))
"#;

#[test]
fn sandboxed_macros_cannot_run_processes() {
    let mut engine = engine();
    engine.register_steel_module("phase1/spawns".to_owned(), SPAWNS.to_owned());

    let error = engine
        .run(
            r#"
(require (for-syntax "phase1/spawns"))

(define-syntax (spawns stx)
  (syntax-case stx ()
    [(_) (begin (spawned) #''spawned)]))

(spawns)
"#,
        )
        .unwrap_err();

    assert!(error.to_string().contains("sandboxed"));
}

#[test]
fn sandboxed_macros_can_require_code_that_names_what_they_cannot_use() {
    let mut engine = engine();
    engine.register_steel_module("phase1/spawns".to_owned(), SPAWNS.to_owned());

    let result = last(
        &mut engine,
        r#"
(require (for-syntax "phase1/spawns"))

(define-syntax (fine stx)
  (syntax-case stx ()
    [(_) #''fine]))

(fine)
"#,
    );

    assert_eq!(result.to_string(), "fine");
}

#[test]
fn sharing_modules_keeps_the_network_from_sandboxed_macros() {
    let mut engine = engine();
    engine.share_modules_with_macros();
    engine.register_steel_module(
        "phase1/network".to_owned(),
        r#"
(require-builtin steel/tcp)
(provide connects)
(define (connects) (if (procedure? tcp-connect) 'connects 'missing))
"#
        .to_owned(),
    );

    let error = engine
        .run(
            r#"
(require (for-syntax "phase1/network"))

(define-syntax (connected stx)
  (syntax-case stx ()
    [(_) #`(quote #,(connects))]))

(connected)
"#,
        )
        .unwrap_err();

    assert!(error.to_string().contains("steel/tcp"));
}

#[test]
fn a_large_module_required_for_syntax() {
    let mut engine = engine();

    let mut module = String::from("(provide value-0)\n");

    for i in 0..3000 {
        module.push_str(&format!("(define (value-{i} x) (+ x {i}))\n"));
    }

    engine.register_steel_module("phase1/large".to_owned(), module);
    engine.register_steel_module(
        "phase1/uses-large".to_owned(),
        r#"
(require (for-syntax "phase1/large"))
(provide from-large)

(define-syntax (from-large stx)
  (syntax-case stx ()
    [(_) #`(quote #,(value-0 7))]))
"#
        .to_owned(),
    );

    let result = last(&mut engine, r#"(require "phase1/uses-large") (from-large)"#);

    assert_eq!(result, SteelVal::IntV(7));
}

const TWICE: &str = r#"
(provide twice)
(define-syntax twice
  (syntax-rules ()
    [(_ e) (list e e)]
    [(_) 'no-argument]))
"#;

#[test]
fn transformer_code_leaves_case_datums_and_with_syntax_patterns_alone() {
    let mut engine = engine();
    engine.register_steel_module("phase1/twice".to_owned(), TWICE.to_owned());

    let result = last(
        &mut engine,
        r#"
(require (for-syntax "phase1/twice"))

(define-syntax (which stx)
  (syntax-case stx ()
    [(_ a) (let ([v (syntax-e #'a)])
             #`(quote #,(case v
                          [(twice) 'matched]
                          [else (twice v)])))]))

(define-syntax (bound stx)
  (syntax-case stx ()
    [(_ v) (with-syntax ([twice #'v])
             #'(list twice))]))

(equal? (list (which twice) (which 3) (bound 4)) '(matched (3 3) (4)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn templates_keep_names_that_are_also_phase_1_variables() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(begin-for-syntax
  (define (shared-name) 'phase-1))

(define (shared-name) 'phase-0)

(define-syntax (uses-both stx)
  (syntax-case stx ()
    [(_) #`(list (quote #,(shared-name)) 'shared-name (shared-name))]))

(uses-both)
"#,
    );

    assert_eq!(result.to_string(), "(phase-1 shared-name phase-0)");
}
