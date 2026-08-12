use steel::steel_vm::engine::Engine;

static DEEP: &str = r#"(provide deep-fn)

(define (deep-fn x) (+ x 1))
"#;

static MIDDLE: &str = r#"(require "deep.scm")
(provide deep-fn)
"#;

#[test]
fn a_binding_can_be_re_provided_through_an_intermediate_module() {
    let workspace = tempfile::tempdir().unwrap();
    let root = workspace.path().canonicalize().unwrap();

    std::fs::write(root.join("deep.scm"), DEEP).unwrap();
    std::fs::write(root.join("middle.scm"), MIDDLE).unwrap();

    let call = |module: &std::path::Path| {
        run(&format!(
            r#"(require {:?})
(deep-fn 41)
"#,
            module
        ))
    };

    let through_middle = call(&root.join("middle.scm"));
    let direct = call(&root.join("deep.scm"));

    assert_eq!(through_middle, direct);
    assert_eq!(through_middle, "42");
}

#[test]
fn a_contracted_function_enforces_its_arity_at_runtime() {
    let definition = r#"(define/contract (add x y)
  (->/c int? int? int?)
  (+ x y))
"#;

    let called_with = |args: &str| {
        format!(
            r#"{}
(add {})
"#,
            definition, args
        )
    };

    assert_eq!(run(&called_with("1 2")), "3");

    let mut engine = Engine::new();
    let error = engine
        .compile_and_run_raw_program(called_with("1"))
        .expect_err("calling a contracted function with too few arguments should fail")
        .to_string();

    assert!(
        error.contains("ArityMismatch"),
        "expected an arity error, got {}",
        error
    );
}

fn run(program: &str) -> String {
    let mut engine = Engine::new();

    let values = engine
        .compile_and_run_raw_program(program.to_string())
        .unwrap_or_else(|e| panic!("{} failed to run: {}", program, e));

    values
        .last()
        .unwrap_or_else(|| panic!("{} produced no values", program))
        .to_string()
}
