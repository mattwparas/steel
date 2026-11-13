//! help and steel/markdown: help loads the renderer the first time it runs

use crate::steel_vm::engine::Engine;
use crate::SteelVal;

fn run(engine: &mut Engine, program: &str) -> Result<Vec<SteelVal>, String> {
    engine.run(program.to_owned()).map_err(|err| err.to_string())
}

fn string(engine: &mut Engine, program: &str) -> String {
    match run(engine, program).unwrap().last().unwrap() {
        SteelVal::StringV(s) => s.as_str().to_owned(),
        other => panic!("not a string: {other}"),
    }
}

#[test]
fn help_works_at_the_top_level() {
    let mut engine = Engine::new();
    run(&mut engine, "(help string-append)").unwrap();
    run(&mut engine, "(help steel/strings string-length)").unwrap();
    run(
        &mut engine,
        r#"
        ;;@doc
        ;; Doubles **x**
        (define (double x) (* x 2))
        (help double)
        "#,
    )
    .unwrap();
}

#[test]
fn help_needs_the_top_level() {
    let mut engine = Engine::new();
    let err = run(&mut engine, "(define (show) (help car)) (show)").unwrap_err();
    assert!(err.contains("require is only allowed at the top level"), "{err}");
}

#[test]
fn help_keeps_its_import_to_itself() {
    let mut engine = Engine::new();
    run(&mut engine, "(help car)").unwrap();
    let err = run(&mut engine, r#"(render-doc "x")"#).unwrap_err();
    assert!(err.contains("render-doc"), "{err}");
}

#[cfg(feature = "markdown")]
mod rendering {
    use super::*;

    fn render(markdown: &str, columns: usize) -> String {
        let mut engine = Engine::new();
        string(
            &mut engine,
            &format!(
                "(require \"steel/markdown\") (render-markdown {markdown:?} {columns} #f)"
            ),
        )
    }

    #[test]
    fn blocks() {
        assert_eq!(
            render(
                "# Title\n\nSome **bold**, *italic* and `code`.\n\n* a bullet that is long enough to wrap\n  * nested\n1. first\n10. tenth\n> quoted\n---\n| a | b |",
                30
            ),
            "Title\n\nSome bold, italic and code.\n\n\
             • a bullet that is long\n  enough to wrap\n  • nested\n\
             1. first\n10. tenth\n│ quoted\n─────────────────────────────\n| a | b |"
        );
    }

    #[test]
    fn code_blocks() {
        // Fences go, code is indented and never wrapped; an unclosed fence
        // runs to the end
        assert_eq!(
            render(
                "Example:\n```scheme\n> (string-append \"foo\" \"bar\") ;; => \"foobar\"\n```\nafter\n```\nopen",
                20
            ),
            "Example:\n    > (string-append \"foo\" \"bar\") ;; => \"foobar\"\nafter\n    open"
        );
    }

    #[test]
    fn stars_that_are_not_emphasis() {
        assert_eq!(
            render("(* x y) and a * b, \\*not\\*, a*b*c", 80),
            "(* x y) and a * b, *not*, a*b*c"
        );
    }

    #[test]
    fn colours() {
        let mut engine = Engine::new();
        let esc = "\u{1b}";
        assert_eq!(
            string(
                &mut engine,
                r##"(require "steel/markdown") (render-markdown "# Hi\nuse `f`\n```\n(define x 1) ; c\n```" 40 #t)"##
            ),
            format!(
                "{esc}[1;4mHi{esc}[0m\nuse {esc}[36mf{esc}[0m\n    ({esc}[95mdefine{esc}[0m x {esc}[93m1{esc}[0m) {esc}[90m; c{esc}[0m"
            )
        );
    }

    #[test]
    fn wide_characters_wrap_by_columns() {
        // 6 + 1 + 6 + 1 + 6 = 20 columns, and the last column is left free
        assert_eq!(render("日本語 日本語 日本語", 21), "日本語 日本語 日本語");
        assert_eq!(render("日本語 日本語 日本語 日本語", 21), "日本語 日本語 日本語\n日本語");
    }
}
