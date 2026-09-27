mod common;

use common::*;
use steel_language_server::backend::OffsetEncoding;
use tower_lsp::lsp_types::*;

fn flat_symbols(response: Option<DocumentSymbolResponse>) -> Vec<SymbolInformation> {
    match response {
        Some(DocumentSymbolResponse::Flat(symbols)) => symbols,
        other => panic!("expected a flat symbol response, got {:?}", other),
    }
}

mod goto_definition_tests {
    use super::*;

    #[tokio::test]
    async fn definition_of_a_function_argument() {
        let source = r#"(define (add-one x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let location = server
            .definition_location(&uri, find_nth(source, "x", 1))
            .await;

        assert_eq!(location.uri, uri);
        assert_eq!(location.range, range_of_nth(source, "x", 0));
    }

    #[tokio::test]
    async fn definition_of_a_top_level_define_from_a_call_site() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let location = server
            .definition_location(&uri, find_nth(source, "add-one", 1))
            .await;

        assert_eq!(location.uri, uri);
        assert_eq!(location.range, range_of_nth(source, "add-one", 0));
    }

    #[tokio::test]
    async fn definition_of_a_top_level_value() {
        let source = r#"(define total 10)

(define (double)
  (+ total total))
"#;

        let mut server = TestServer::new();
        let uri = server.open("value.scm", source);

        let location = server
            .definition_location(&uri, find_nth(source, "total", 1))
            .await;

        assert_eq!(location.uri, uri);
        assert_eq!(location.range, range_of_nth(source, "total", 0));
    }

    #[tokio::test]
    async fn definition_of_a_let_binding() {
        let source = r#"(define (compute)
  (let ([total 10]
        [other 20])
    (+ total other)))
"#;

        let mut server = TestServer::new();
        let uri = server.open("let.scm", source);

        let location = server
            .definition_location(&uri, find_nth(source, "total", 1))
            .await;

        assert_eq!(location.uri, uri);
        assert_eq!(location.range, range_of_nth(source, "total", 0));
    }

    #[tokio::test]
    async fn definition_crosses_into_a_required_file() {
        let lib_source = r#"(provide greet counter)

(define (greet name)
  (string-append "hello " name))

(define counter 0)
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))

(define total counter)
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.index_workspace();

        let app = server.open("app.scm", app_source);

        let location = server
            .definition_location(&app, find(app_source, "greet"))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of_nth(lib_source, "greet", 1));

        let location = server
            .definition_location(&app, find(app_source, "counter"))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of_nth(lib_source, "counter", 1));
    }

    #[tokio::test]
    async fn definition_resolves_through_only_in() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require (only-in "lib.scm" greet))

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.index_workspace();

        let app = server.open("only-in.scm", app_source);

        let location = server
            .definition_location(&app, find_nth(app_source, "greet", 1))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of_nth(lib_source, "greet", 1));
    }

    #[tokio::test]
    async fn definition_resolves_through_prefix_in() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require (prefix-in lib. "lib.scm"))

(define (main)
  (lib.greet "world"))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.index_workspace();

        let app = server.open("prefix-in.scm", app_source);

        let location = server
            .definition_location(&app, find(app_source, "lib.greet"))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of_nth(lib_source, "greet", 1));
    }

    #[tokio::test]
    async fn definition_of_a_macro_in_the_same_file() {
        let source = r#"(define-syntax twice
  (syntax-rules ()
    [(_ e) (begin e e)]))

(define (run)
  (twice (displayln "hi")))
"#;

        let mut server = TestServer::new();
        server.write("macro.scm", source);
        server.index_workspace();

        let uri = server.open("macro.scm", source);

        // macros land on the define-syntax keyword, not on the macro name
        let location = server
            .definition_location(&uri, find_nth(source, "twice", 1))
            .await;

        assert_eq!(location.uri, uri);
        assert_eq!(location.range, range_of(source, "define-syntax"));
    }

    #[tokio::test]
    async fn definition_of_a_macro_from_a_required_file() {
        let lib_source = r#"(provide my-macro)

(define-syntax my-macro
  (syntax-rules ()
    [(_ a) (+ a 1)]))
"#;
        let app_source = r#"(require "lib.scm")

(define (run)
  (my-macro 1))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.write("imported.scm", app_source);
        server.index_workspace();

        let uri = server.open("imported.scm", app_source);

        let location = server
            .definition_location(&uri, find(app_source, "my-macro"))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of(lib_source, "define-syntax"));
    }

    #[tokio::test]
    async fn definition_of_a_builtin_is_not_reported() {
        let source = r#"(define (double xs)
  (map (lambda (y) (* y 2)) xs))
"#;

        let mut server = TestServer::new();
        let uri = server.open("builtin.scm", source);

        assert_eq!(
            server.goto_definition(&uri, find(source, "map")).await,
            None
        );
    }

    #[tokio::test]
    async fn definition_at_a_position_with_no_identifier_is_not_reported() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        // the blank line between the two top level forms
        assert_eq!(server.goto_definition(&uri, position(2, 0)).await, None);
    }

    #[tokio::test]
    async fn definition_in_an_unopened_document_is_not_reported() {
        let server = TestServer::new();
        let uri = server.unopened("never-opened.scm");

        assert_eq!(server.goto_definition(&uri, position(0, 10)).await, None);
    }
}

mod find_references_tests {
    use super::*;

    #[tokio::test]
    async fn references_to_a_function_argument() {
        let source = r#"(define (add-one x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let locations = server
            .references(&uri, find_nth(source, "x", 1), true)
            .await
            .expect("expected references to the argument");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), range_of_nth(source, "x", 0)),
                Location::new(uri, range_of_nth(source, "x", 1)),
            ]
        );
    }

    #[tokio::test]
    async fn references_to_a_top_level_define() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))

(define (combine)
  (add-one result))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let locations = server
            .references(&uri, find_nth(source, "add-one", 1), true)
            .await
            .expect("expected references to the define");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), range_of_nth(source, "add-one", 0)),
                Location::new(uri.clone(), range_of_nth(source, "add-one", 1)),
                Location::new(uri, range_of_nth(source, "add-one", 2)),
            ]
        );
    }

    #[tokio::test]
    async fn references_can_exclude_the_declaration() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))

(define (combine)
  (add-one result))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let locations = server
            .references(&uri, find_nth(source, "add-one", 1), false)
            .await
            .expect("expected references to the define");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), range_of_nth(source, "add-one", 1)),
                Location::new(uri, range_of_nth(source, "add-one", 2)),
            ]
        );
    }

    #[tokio::test]
    async fn references_to_a_let_binding() {
        let source = r#"(define (compute)
  (let ([total 10]
        [other 20])
    (+ total other total)))
"#;

        let mut server = TestServer::new();
        let uri = server.open("let.scm", source);

        let locations = server
            .references(&uri, find_nth(source, "total", 1), true)
            .await
            .expect("expected references to the let binding");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), range_of_nth(source, "total", 0)),
                Location::new(uri.clone(), range_of_nth(source, "total", 1)),
                Location::new(uri.clone(), range_of_nth(source, "total", 2)),
            ]
        );

        let locations = server
            .references(&uri, find_nth(source, "other", 1), true)
            .await
            .expect("expected references to the other let binding");

        assert_eq!(locations.len(), 2);
    }

    #[tokio::test]
    async fn references_reach_across_files() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.write("app.scm", app_source);
        server.index_workspace();

        let app = server.open("app.scm", app_source);

        let locations = server
            .references(&app, find(app_source, "greet"), true)
            .await
            .expect("expected references across the module boundary");

        assert_eq!(
            deduped(locations),
            vec![
                Location::new(app.clone(), range_of(app_source, "greet")),
                Location::new(lib, range_of_nth(lib_source, "greet", 1)),
            ]
        );
    }

    #[tokio::test]
    async fn references_reach_across_files_through_only_in() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require (only-in "lib.scm" greet))

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.write("only-in.scm", app_source);
        server.index_workspace();

        let app = server.open("only-in.scm", app_source);

        let locations = server
            .references(&app, find_nth(app_source, "greet", 1), true)
            .await
            .expect("expected references across the module boundary");

        // sorted by uri, so lib.scm comes first
        assert_eq!(
            deduped(locations),
            vec![
                Location::new(lib, range_of_nth(lib_source, "greet", 1)),
                Location::new(app, range_of_nth(app_source, "greet", 1)),
            ]
        );
    }

    // The `TODO: Dedupe the found locations` in Backend::references. Once a file is indexed
    // as a module, a use inside it is found twice - once by the analysis of the open
    // document, and again when the reverse dependency walk re-analyzes that same file.
    #[tokio::test]
    #[ignore = "references reports the same location twice for an indexed requiring file"]
    async fn references_does_not_report_duplicate_locations() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        let lib = server.write("lib.scm", lib_source);
        server.write("app.scm", app_source);
        server.index_workspace();

        let app = server.open("app.scm", app_source);

        let locations = server
            .references(&app, find(app_source, "greet"), true)
            .await
            .expect("expected references across the module boundary");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(app, range_of(app_source, "greet")),
                Location::new(lib, range_of_nth(lib_source, "greet", 1)),
            ]
        );
    }

    #[tokio::test]
    async fn references_to_a_builtin_stay_inside_the_workspace() {
        let lib_source = r#"(provide scale)

(define (scale xs)
  (map (lambda (y) (* y 2)) xs))
"#;
        let source = r#"(define (run xs)
  (map (lambda (y) (+ y 1)) xs))
"#;

        let mut server = TestServer::new();
        server.write("lib.scm", lib_source);
        server.index_workspace();

        let uri = server.open("builtin.scm", source);

        let locations = server
            .references(&uri, find(source, "map"), true)
            .await
            .expect("expected at least the clicked occurrence");

        assert!(
            locations
                .iter()
                .all(|location| file_name(&location.uri) == "builtin.scm"),
            "references to a builtin leaked outside the workspace: {:?}",
            locations
                .iter()
                .map(|x| file_name(&x.uri))
                .collect::<Vec<_>>()
        );
    }

    // Builtins have no refers_to edge, so the span scan has nothing to match on and only
    // the occurrence under the cursor comes back. find_references_builtin doesn't fill the
    // hole either - it only reports hits inside modules that define the same name.
    #[tokio::test]
    #[ignore = "references to a builtin only return the occurrence under the cursor"]
    async fn references_to_a_builtin_find_every_use_in_the_file() {
        let source = r#"(define (run xs)
  (map (lambda (y) (+ y 1)) xs))

(define (run-again xs)
  (map (lambda (y) (- y 1)) xs))
"#;

        let mut server = TestServer::new();
        let uri = server.open("builtin.scm", source);

        let locations = server
            .references(&uri, find(source, "map"), true)
            .await
            .expect("expected references to the builtin");

        assert_eq!(
            deduped(locations),
            vec![
                Location::new(uri.clone(), range_of_nth(source, "map", 0)),
                Location::new(uri, range_of_nth(source, "map", 1)),
            ]
        );
    }

    #[tokio::test]
    async fn references_in_an_unopened_document_are_not_reported() {
        let server = TestServer::new();
        let uri = server.unopened("never-opened.scm");

        assert_eq!(server.references(&uri, position(0, 10), true).await, None);
    }
}

// spans_to_locations drops anything that is neither in the vfs nor under the root, so
// results never point into the stdlib or an unrelated checkout we have no watcher for
mod workspace_tests {
    use super::*;

    #[tokio::test]
    async fn references_work_with_both_files_open() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        server.write("lib.scm", lib_source);
        server.write("app.scm", app_source);
        server.index_workspace();

        let lib = server.open("lib.scm", lib_source);
        let app = server.open("app.scm", app_source);

        let expected = vec![
            Location::new(app.clone(), range_of(app_source, "greet")),
            Location::new(lib.clone(), range_of_nth(lib_source, "greet", 1)),
        ];

        let from_app = server
            .references(&app, find(app_source, "greet"), true)
            .await
            .expect("expected references from the consumer");
        assert_eq!(deduped(from_app), expected);

        let from_lib = server
            .references(&lib, find_nth(lib_source, "greet", 1), true)
            .await
            .expect("expected references from the definition");
        assert_eq!(deduped(from_lib), expected);
    }

    #[tokio::test]
    async fn definition_reaches_a_file_outside_the_workspace() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;

        let mut server = TestServer::new();
        let outside = server.write_outside("outside-lib.scm", lib_source);
        server.index_path(&outside);

        let app_source = require_by_path(&outside);
        let app = server.open("app.scm", &app_source);

        let location = server
            .definition_location(&app, call_site(&app_source))
            .await;

        assert_eq!(location.uri, outside);
        assert_eq!(location.range, range_of_nth(lib_source, "greet", 1));
    }

    #[tokio::test]
    async fn references_skip_closed_files_outside_the_workspace() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;

        let mut server = TestServer::new();
        let outside = server.write_outside("outside-lib.scm", lib_source);
        server.index_path(&outside);

        let app_source = require_by_path(&outside);
        let app = server.open("app.scm", &app_source);

        let locations = server
            .references(&app, call_site(&app_source), true)
            .await
            .expect("expected at least the local use");

        // the declaration is outside the workspace in a file the editor never opened, so
        // we leave it out
        assert_eq!(
            by_file(locations),
            vec![("app.scm".to_string(), range_of(&app_source, "greet"))]
        );
    }

    #[tokio::test]
    async fn references_include_files_outside_the_workspace_once_they_are_open() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;

        let mut server = TestServer::new();
        let outside = server.write_outside("outside-lib.scm", lib_source);
        server.index_path(&outside);

        let app_source = require_by_path(&outside);
        let app = server.open("app.scm", &app_source);

        // opening it puts it in the vfs, which is what lifts the filter
        server.did_open(outside.clone(), lib_source);

        let locations = server
            .references(&app, call_site(&app_source), true)
            .await
            .expect("expected references once the library is open");

        assert_eq!(
            by_file(locations),
            vec![
                ("app.scm".to_string(), range_of(&app_source, "greet")),
                (
                    "outside-lib.scm".to_string(),
                    range_of_nth(lib_source, "greet", 1)
                ),
            ]
        );
    }

    fn require_by_path(uri: &Url) -> String {
        format!(
            r#"(require {:?})

(define (main)
  (greet "world"))
"#,
            uri.to_file_path().unwrap()
        )
    }

    // the require form holds an absolute path, so the fixture can't be a constant
    fn call_site(source: &str) -> Position {
        let open_paren = find(source, "(greet");
        Position::new(open_paren.line, open_paren.character + 1)
    }
}

mod require_graph_tests {
    use super::*;

    #[tokio::test]
    async fn references_span_a_diamond_require_graph() {
        let base = r#"(provide base-fn)

(define (base-fn x) x)
"#;
        let left = r#"(require "base.scm")
(provide left-call)

(define (left-call) (base-fn 1))
"#;
        let right = r#"(require "base.scm")
(provide right-call)

(define (right-call) (base-fn 2))
"#;
        let top = r#"(require "left.scm")
(require "right.scm")

(define (main)
  (list (left-call) (right-call)))
"#;

        let mut server = TestServer::new();
        server.write("base.scm", base);
        server.write("left.scm", left);
        server.write("right.scm", right);
        server.write("diamond.scm", top);
        server.index_workspace();

        let uri = server.open("base.scm", base);

        let locations = server
            .references(&uri, find_nth(base, "base-fn", 1), true)
            .await
            .expect("expected references from both branches of the diamond");

        assert_eq!(
            by_file(locations),
            vec![
                ("base.scm".to_string(), range_of_nth(base, "base-fn", 1)),
                ("left.scm".to_string(), range_of(left, "base-fn")),
                ("right.scm".to_string(), range_of(right, "base-fn")),
            ]
        );
    }

    // deep-fn reaches top.scm through middle.scm, which re-provides it. resolution only
    // walks one hop, so nothing comes back
    #[tokio::test]
    #[ignore = "definitions are not resolved through a re-providing intermediate module"]
    async fn definition_follows_a_transitive_re_provide() {
        let deep = r#"(provide deep-fn)

(define (deep-fn x) (+ x 1))
"#;
        let middle = r#"(require "deep.scm")
(provide deep-fn)
"#;
        let top = r#"(require "middle.scm")

(define (main)
  (deep-fn 1))
"#;

        let mut server = TestServer::new();
        let deep_uri = server.write("deep.scm", deep);
        server.write("middle.scm", middle);
        server.write("top.scm", top);
        server.index_workspace();

        let uri = server.open("top.scm", top);

        let location = server.definition_location(&uri, find(top, "deep-fn")).await;

        assert_eq!(location.uri, deep_uri);
        assert_eq!(location.range, range_of_nth(deep, "deep-fn", 1));
    }

    #[tokio::test]
    #[ignore = "references are not resolved through a re-providing intermediate module"]
    async fn references_follow_a_transitive_re_provide() {
        let deep = r#"(provide deep-fn)

(define (deep-fn x) (+ x 1))
"#;
        let middle = r#"(require "deep.scm")
(provide deep-fn)
"#;
        let top = r#"(require "middle.scm")

(define (main)
  (deep-fn 1))
"#;

        let mut server = TestServer::new();
        server.write("deep.scm", deep);
        server.write("middle.scm", middle);
        server.write("top.scm", top);
        server.index_workspace();

        let uri = server.open("top.scm", top);

        let locations = server
            .references(&uri, find(top, "deep-fn"), true)
            .await
            .expect("expected references through the intermediate module");

        assert_eq!(
            by_file(locations),
            vec![
                ("deep.scm".to_string(), range_of_nth(deep, "deep-fn", 1)),
                ("top.scm".to_string(), range_of(top, "deep-fn")),
            ]
        );
    }
}

mod document_symbol_tests {
    use super::*;

    #[tokio::test]
    async fn document_symbols_list_top_level_definitions() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))

(define (combine)
  (add-one result))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let symbols = flat_symbols(server.document_symbol(&uri).await);

        let names: Vec<_> = symbols.iter().map(|x| x.name.as_str()).collect();
        assert_eq!(names, vec!["add-one", "result", "combine"]);

        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(
            symbols[0].location.range,
            range_of_nth(source, "add-one", 0)
        );

        assert_eq!(symbols[1].kind, SymbolKind::CONSTANT);
        assert_eq!(symbols[1].location.range, range_of_nth(source, "result", 0));

        assert!(symbols.iter().all(|symbol| symbol.location.uri == uri));
    }

    #[tokio::test]
    async fn document_symbols_include_let_star_bindings() {
        let source = r#"(define (with-let-star z)
  (let* ([inner-star 1])
    (+ z inner-star)))

(let* ([outer-star 1]
       [second-star 2])
  (+ outer-star second-star))
"#;

        let mut server = TestServer::new();
        let uri = server.open("shapes.scm", source);

        let symbols = flat_symbols(server.document_symbol(&uri).await);
        let names: Vec<_> = symbols.iter().map(|x| x.name.as_str()).collect();

        assert_eq!(
            names,
            vec!["with-let-star", "inner-star", "outer-star", "second-star",]
        );

        let binding = |name: &str| {
            symbols
                .iter()
                .find(|symbol| symbol.name == name)
                .unwrap_or_else(|| panic!("expected {} among the symbols", name))
        };

        assert_eq!(binding("inner-star").kind, SymbolKind::VARIABLE);

        assert_eq!(
            binding("inner-star").location.range,
            range_of(source, "[inner-star 1]")
        );
        assert_eq!(
            binding("second-star").location.range,
            range_of(source, "[second-star 2]")
        );
    }

    #[tokio::test]
    async fn document_symbols_leave_out_plain_lets_and_nested_defines() {
        let source = r#"(define (outer x)
  (define nested-define 2)
  (+ x nested-define))

(define (with-let y)
  (let ([plain-let-binding 1])
    (+ y plain-let-binding)))
"#;

        let mut server = TestServer::new();
        let uri = server.open("shapes.scm", source);

        let names: Vec<_> = flat_symbols(server.document_symbol(&uri).await)
            .into_iter()
            .map(|symbol| symbol.name)
            .collect();

        // only let* is scanned for bindings, and only top level defines are collected
        assert_eq!(names, vec!["outer", "with-let"]);
    }

    #[tokio::test]
    async fn document_symbols_in_an_unopened_document_are_not_reported() {
        let server = TestServer::new();
        let uri = server.unopened("never-opened.scm");

        assert!(server.document_symbol(&uri).await.is_none());
    }
}

mod hover_tests {
    use super::*;

    #[tokio::test]
    async fn hover_shows_builtin_documentation() {
        let source = r#"(define (double xs)
  (map (lambda (y) (* y 2)) xs))
"#;

        let mut server = TestServer::new();
        let uri = server.open("builtin.scm", source);

        let hover = server
            .hover(&uri, find(source, "map"))
            .await
            .expect("expected documentation for a builtin");

        assert!(
            hover_text(&hover).contains("(map func lst . lsts) -> list?"),
            "unexpected hover contents: {}",
            hover_text(&hover)
        );
    }

    #[tokio::test]
    async fn hover_shows_the_doc_comment_of_a_define_in_the_same_file() {
        let source = r#";;@doc
;; Increments its argument.
(define (add-one x)
  (+ x 1))

(define (run)
  (add-one 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("documented.scm", source);

        let hover = server
            .hover(&uri, find_nth(source, "add-one", 1))
            .await
            .expect("expected documentation at the call site");
        assert_eq!(hover_text(&hover), "Increments its argument.");

        let hover = server
            .hover(&uri, find_nth(source, "add-one", 0))
            .await
            .expect("expected documentation on the definition");
        assert_eq!(hover_text(&hover), "Increments its argument.");
    }

    #[tokio::test]
    async fn hover_shows_the_doc_comment_of_a_macro() {
        let source = r#";;@doc
;; Evaluates its argument two times.
(define-syntax twice
  (syntax-rules ()
    [(_ e) (begin e e)]))

(define (run)
  (twice 1))
"#;

        let mut server = TestServer::new();
        server.write("documented.scm", source);
        server.index_workspace();

        let uri = server.open("documented.scm", source);

        // macros aren't identifiers in the analysis, so this goes through the separate
        // hover_macro_impl fallback rather than hover_impl
        let hover = server
            .hover(&uri, find_nth(source, "twice", 1))
            .await
            .expect("expected documentation for the macro");

        assert_eq!(hover_text(&hover), "Evaluates its argument two times.");
    }

    #[tokio::test]
    async fn hover_shows_the_doc_comment_of_a_required_function() {
        let lib_source = r#"(provide greet)

;;@doc
;; Greets the given name.
(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        server.write("lib.scm", lib_source);
        server.index_workspace();

        let app = server.open("app.scm", app_source);

        let hover = server
            .hover(&app, find(app_source, "greet"))
            .await
            .expect("expected documentation for the required function");

        let text = hover_text(&hover);
        assert!(
            text.contains("Greets the given name."),
            "unexpected hover contents: {}",
            text
        );

        assert!(
            text.contains("(define greet") && text.contains("(name)"),
            "unexpected hover contents: {}",
            text
        );
    }

    #[tokio::test]
    async fn hover_shows_the_doc_comment_of_an_imported_macro() {
        let lib_source = r#"(provide shout)

;;@doc
;; Appends an exclamation mark.
(define-syntax shout
  (syntax-rules ()
    [(_ e) (string-append e "!")]))
"#;
        let app_source = r#"(require "macro-lib.scm")

(define (run)
  (shout "hi"))
"#;

        let mut server = TestServer::new();
        server.write("macro-lib.scm", lib_source);
        server.write("macro-app.scm", app_source);
        server.index_workspace();

        let uri = server.open("macro-app.scm", app_source);

        let hover = server
            .hover(&uri, find(app_source, "shout"))
            .await
            .expect("expected documentation for the imported macro");

        assert_eq!(hover_text(&hover), "Appends an exclamation mark.");
    }

    #[tokio::test]
    async fn hover_renders_the_signature_of_an_undocumented_required_function() {
        let lib_source = r#"(provide undocumented)

(define (undocumented a b)
  (+ a b))
"#;
        // the file names avoid the word undocumented - the require form holds the path, and
        // find would otherwise match inside it instead of at the call site
        let app_source = r#"(require "plain-lib.scm")

(define (main)
  (undocumented 1 2))
"#;

        let mut server = TestServer::new();
        server.write("plain-lib.scm", lib_source);
        server.index_workspace();

        let uri = server.open("plain-app.scm", app_source);

        let hover = server
            .hover(&uri, find(app_source, "undocumented"))
            .await
            .expect("expected a signature for the undocumented function");

        assert_eq!(
            hover_text(&hover),
            r#"```scheme
(define undocumented
  (λ (a b)
    ...))
```"#
        );
    }

    #[tokio::test]
    async fn hover_on_an_undocumented_local_is_not_reported() {
        let source = r#"(define (add-one x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        assert!(server.hover(&uri, find_nth(source, "x", 1)).await.is_none());
    }

    fn hover_text(hover: &Hover) -> String {
        match &hover.contents {
            HoverContents::Scalar(MarkedString::String(text)) => text.clone(),
            other => panic!("expected a plain string hover, got {:?}", other),
        }
    }
}

mod completion_tests {
    use super::*;

    #[tokio::test]
    async fn completion_offers_globals_and_top_level_definitions() {
        let source = r#"(define (outer alpha)
  (+ alpha 1))

(define gamma 3)
"#;

        let mut server = TestServer::new();
        let uri = server.open("complete.scm", source);

        // inside (+ alpha 1), right before alpha
        let labels = completion_labels(&mut server, &uri, position(1, 5)).await;

        for expected in ["map", "car", "outer", "gamma"] {
            assert!(
                labels.contains(&expected.to_string()),
                "expected {:?} among the completions",
                expected
            );
        }

        for internal in ["mangler#%", "##mm", "__module-", "!!dummy-rest"] {
            assert!(
                !labels.iter().any(|label| label.starts_with(internal)),
                "{:?} leaked into the completions",
                internal
            );
        }

        assert!(
            !labels.iter().any(|label| label.ends_with("__doc__")),
            "documentation bindings leaked into the completions"
        );
    }

    #[tokio::test]
    async fn completion_filters_by_the_character_already_typed() {
        let source = r#"(define (outer alpha)
  (+ alpha 1))

(define gamma 3)
"#;

        let mut server = TestServer::new();
        let uri = server.open("complete.scm", source);

        // immediately after the a of alpha, which follows a space
        let labels = completion_labels(&mut server, &uri, position(1, 6)).await;

        assert!(!labels.is_empty());
        assert!(
            labels.iter().all(|label| label.starts_with('a')),
            "completions were not filtered by the typed prefix: {:?}",
            labels
        );
        assert!(labels.contains(&"abs".to_string()));
    }

    #[tokio::test]
    async fn completion_after_an_open_paren_is_not_filtered() {
        let source = r#"(define (outer alpha)
  (let ([local-binding 1])
    (+ alpha local-binding)))

(define gamma 3)
"#;

        let mut server = TestServer::new();
        let uri = server.open("scopes.scm", source);

        // ( is the trigger character, so it must not be taken for the first letter of what
        // the user is typing. line 2 is `    (+ alpha local-binding)))`
        let labels = completion_labels(&mut server, &uri, position(2, 5)).await;

        assert!(labels.contains(&"gamma".to_string()));
        assert!(labels.contains(&"map".to_string()));
        assert!(
            labels.iter().any(|label| !label.starts_with('(')),
            "the trigger character was mistaken for a filter prefix"
        );
    }

    #[tokio::test]
    async fn completion_offers_macros_that_are_in_scope() {
        let source = r#"(define (outer alpha)
  (let ([local-binding 1])
    (+ alpha local-binding)))

(define gamma 3)
"#;

        let mut server = TestServer::new();
        let uri = server.open("scopes.scm", source);

        let labels = completion_labels(&mut server, &uri, position(2, 5)).await;

        for expected in ["when", "unless"] {
            assert!(
                labels.contains(&expected.to_string()),
                "expected the macro {:?} among the completions",
                expected
            );
        }
    }

    // Completions come off the expanded ast, where an argument called alpha has become
    // ##alpha0 and the let binding is a lambda argument. Both get dropped by the filter
    // that hides # prefixed internals, so the bindings nearest the cursor are the ones the
    // user can't complete.
    #[tokio::test]
    #[ignore = "function arguments and let bindings never appear in completions"]
    async fn completion_offers_local_bindings() {
        let source = r#"(define (outer alpha)
  (let ([local-binding 1])
    (+ alpha local-binding)))

(define gamma 3)
"#;

        let mut server = TestServer::new();
        let uri = server.open("scopes.scm", source);

        let labels = completion_labels(&mut server, &uri, position(2, 5)).await;

        for expected in ["alpha", "local-binding"] {
            assert!(
                labels.contains(&expected.to_string()),
                "expected the local binding {:?} among the completions, got {} items",
                expected,
                labels.len()
            );
        }
    }

    #[tokio::test]
    async fn completion_in_an_unopened_document_is_not_reported() {
        let server = TestServer::new();
        let uri = server.unopened("never-opened.scm");

        assert!(server.completion(&uri, position(1, 5)).await.is_none());
    }

    async fn completion_labels(
        server: &mut TestServer,
        uri: &Url,
        position: Position,
    ) -> Vec<String> {
        match server.completion(uri, position).await {
            Some(CompletionResponse::Array(items)) => {
                items.into_iter().map(|item| item.label).collect()
            }
            other => panic!("expected an array of completions, got {:?}", other),
        }
    }
}

mod rename_tests {
    use super::*;

    #[tokio::test]
    async fn rename_rewrites_every_use_of_a_function_argument() {
        let source = r#"(define (add-one x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        let position = find_nth(source, "x", 1);

        assert_eq!(
            server.prepare_rename(&uri, position).await,
            Ok(Some(PrepareRenameResponse::Range(range_of_nth(
                source, "x", 1
            ))))
        );

        let edit = server
            .rename(&uri, position, "value")
            .await
            .expect("expected a workspace edit");

        let mut edits = edit.changes.expect("expected document changes")[&uri].clone();
        edits.sort_by_key(|edit| (edit.range.start.line, edit.range.start.character));

        assert_eq!(
            edits,
            vec![
                TextEdit::new(range_of_nth(source, "x", 0), "value".to_string()),
                TextEdit::new(range_of_nth(source, "x", 1), "value".to_string()),
            ]
        );
    }

    #[tokio::test]
    async fn rename_rewrites_every_use_of_a_let_binding() {
        let source = r#"(define (compute)
  (let ([total 10]
        [other 20])
    (+ total other total)))
"#;

        let mut server = TestServer::new();
        let uri = server.open("let.scm", source);

        let edit = server
            .rename(&uri, find_nth(source, "total", 1), "sum")
            .await
            .expect("expected a workspace edit");

        let mut edits = edit.changes.expect("expected document changes")[&uri].clone();
        edits.sort_by_key(|edit| (edit.range.start.line, edit.range.start.character));

        assert_eq!(
            edits,
            vec![
                TextEdit::new(range_of_nth(source, "total", 0), "sum".to_string()),
                TextEdit::new(range_of_nth(source, "total", 1), "sum".to_string()),
                TextEdit::new(range_of_nth(source, "total", 2), "sum".to_string()),
            ]
        );
    }

    #[tokio::test]
    async fn rename_refuses_globals_and_builtins() {
        let source = r#"(define (add-one x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        // renaming a global would have to touch every requiring file, which we don't do
        let error = server
            .prepare_rename(&uri, find_nth(source, "add-one", 0))
            .await
            .expect_err("expected renaming a global to be rejected");
        assert_eq!(error.code, tower_lsp::jsonrpc::ErrorCode::InvalidParams);

        assert_eq!(
            server
                .rename(&uri, find_nth(source, "add-one", 0), "nope")
                .await,
            None
        );

        let builtin = r#"(define (double xs)
  (map (lambda (y) (* y 2)) xs))
"#;

        let uri = server.open("builtin.scm", builtin);

        let error = server
            .prepare_rename(&uri, find(builtin, "map"))
            .await
            .expect_err("expected renaming a builtin to be rejected");
        assert_eq!(error.code, tower_lsp::jsonrpc::ErrorCode::InvalidParams);

        assert_eq!(
            server.rename(&uri, find(builtin, "map"), "nope").await,
            None
        );
    }
}

mod diagnostics_tests {
    use super::*;

    #[tokio::test]
    async fn diagnostics_report_free_identifiers_and_unused_arguments() {
        let source = r#"(define (f x)
  (undefined-function 10))
"#;

        let mut server = TestServer::new();
        let uri = server.open("free.scm", source);

        let diagnostics = server.diagnostics(&uri);

        let free = diagnostics
            .iter()
            .find(|d| d.message.contains("free identifier"))
            .expect("expected a free identifier diagnostic");
        assert_eq!(free.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(free.message, "free identifier: undefined-function");
        assert_eq!(free.range, range_of(source, "undefined-function"));

        let unused = diagnostics
            .iter()
            .find(|d| d.message == "unused variable")
            .expect("expected an unused variable diagnostic");
        assert_eq!(unused.severity, Some(DiagnosticSeverity::INFORMATION));
        assert_eq!(unused.range, range_of_nth(source, "x", 0));
    }

    #[tokio::test]
    async fn diagnostics_report_arity_mismatches() {
        let source = r#"(define (f x) x)
(f 1 2 3)
(car)
"#;

        let mut server = TestServer::new();
        let uri = server.open("arity.scm", source);

        let messages: Vec<_> = server
            .diagnostics(&uri)
            .into_iter()
            .map(|d| d.message)
            .collect();

        assert!(
            messages
                .iter()
                .any(|m| m == "ArityMismatch: f expects 1 arguments, found 3"),
            "expected an arity error for the user defined function, got {:?}",
            messages
        );
        assert!(
            messages.iter().any(|m| m.starts_with("ArityMismatch: car")),
            "expected an arity error for the builtin, got {:?}",
            messages
        );
    }

    #[tokio::test]
    async fn diagnostics_report_every_builtin_arity_kind() {
        // one call per Arity variant we know how to report. Arity::AtMost has no test
        // because nothing declares it, so that arm is unreachable today
        let source = r#"(define (run)
  (list (-)
        (log)
        (range 1 2 3)
        (car)))
"#;

        let mut server = TestServer::new();
        let uri = server.open("arities.scm", source);

        let mut diagnostics = server.diagnostics(&uri);
        diagnostics.sort_by_key(|d| d.range.start.line);

        let reported: Vec<_> = diagnostics
            .iter()
            .map(|d| (d.message.as_str(), d.range))
            .collect();

        assert_eq!(
            reported,
            vec![
                // AtLeast
                (
                    "ArityMismatch: - expects at least 1 arguments, found 0",
                    range_of(source, "-")
                ),
                // Range, below the minimum
                (
                    "ArityMismatch: log expects 1 to 2 arguments, found 0",
                    range_of(source, "log")
                ),
                // Range, above the maximum
                (
                    "ArityMismatch: range expects 1 to 2 arguments, found 3",
                    range_of(source, "range")
                ),
                // Exact
                (
                    "ArityMismatch: car expects 1 arguments, found 0",
                    range_of(source, "car")
                ),
            ]
        );

        assert!(diagnostics
            .iter()
            .all(|d| d.severity == Some(DiagnosticSeverity::ERROR)));
    }

    #[tokio::test]
    async fn calls_within_arity_are_not_reported() {
        let source = r#"(define (run)
  (list (- 1)
        (log 1)
        (log 1 2)
        (range 1 2)
        (car '(1))))
"#;

        let mut server = TestServer::new();
        let uri = server.open("in-arity.scm", source);

        assert_eq!(server.diagnostics(&uri), vec![]);
    }

    #[tokio::test]
    async fn diagnostics_report_arity_for_a_required_function() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let app_source = r#"(require "arity-lib.scm")

(define (main)
  (greet "a" "b"))
"#;

        let mut server = TestServer::new();
        server.write("arity-lib.scm", lib_source);
        server.index_workspace();

        let uri = server.open("arity-app.scm", app_source);

        let diagnostics = server.diagnostics(&uri);

        assert_eq!(
            diagnostics
                .iter()
                .map(|d| (d.message.as_str(), d.range))
                .collect::<Vec<_>>(),
            vec![(
                "ArityMismatch: greet expects 1 arguments, found 2",
                range_of(app_source, "greet")
            )]
        );
    }

    // define/contract no longer expands into the bind/c form that
    // StaticArityChecking::visit_begin matches on, so we never learn the arity. The engine
    // still catches it at runtime - see modules.rs. known_contracts, function_contract and
    // the whole StaticContract type are dead for the same reason, and the
    // diagnostics::resolve_contracts test doesn't notice because it only prints its result.
    #[tokio::test]
    #[ignore = "arity is not checked for functions defined with define/contract"]
    async fn diagnostics_report_arity_for_a_contracted_function() {
        let source = r#"(define/contract (add x y)
  (->/c int? int? int?)
  (+ x y))

(define (main)
  (add 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("contracted.scm", source);

        let messages: Vec<_> = server
            .diagnostics(&uri)
            .into_iter()
            .map(|d| d.message)
            .collect();

        assert!(
            messages
                .iter()
                .any(|m| m == "ArityMismatch: add expects 2 arguments, found 1"),
            "expected an arity error for the contracted function, got {:?}",
            messages
        );
    }

    #[tokio::test]
    async fn diagnostics_report_parse_errors() {
        let source = r#"(define (f x)
"#;

        let mut server = TestServer::new();
        let uri = server.open("broken.scm", source);

        let diagnostics = server.diagnostics(&uri);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(
            diagnostics[0].message.contains("Unexpected EOF"),
            "unexpected diagnostic: {}",
            diagnostics[0].message
        );
    }

    #[tokio::test]
    async fn a_clean_file_produces_no_diagnostics() {
        let source = r#"(define (add-one x)
  (+ x 1))

(define result (add-one 10))

(define (combine)
  (add-one result))
"#;

        let mut server = TestServer::new();
        let uri = server.open("single.scm", source);

        assert_eq!(server.diagnostics(&uri), vec![]);
    }

    #[tokio::test]
    async fn diagnostics_are_cleared_once_the_problem_is_fixed() {
        let source = r#"(define (f x)
  (undefined-function x))
"#;
        let fixed = r#"(define (f x)
  (+ x 1))
"#;

        let mut server = TestServer::new();
        let uri = server.open("fixup.scm", source);

        assert!(!server.diagnostics(&uri).is_empty());

        server.did_change(uri.clone(), fixed);

        assert_eq!(server.diagnostics(&uri), vec![]);
    }
}

mod document_sync_tests {
    use super::*;

    #[tokio::test]
    async fn edits_are_reflected_in_later_requests() {
        let original = r#"(define (f x) x)
(f 1)
"#;
        let updated = r#"(define (renamed y) y)
(renamed 1)
"#;

        let mut server = TestServer::new();
        let uri = server.open("change.scm", original);

        assert_eq!(
            flat_symbols(server.document_symbol(&uri).await)
                .into_iter()
                .map(|x| x.name)
                .collect::<Vec<_>>(),
            vec!["f"]
        );

        server.did_change(uri.clone(), updated);

        assert_eq!(
            flat_symbols(server.document_symbol(&uri).await)
                .into_iter()
                .map(|x| x.name)
                .collect::<Vec<_>>(),
            vec!["renamed"]
        );

        let location = server
            .definition_location(&uri, find_nth(updated, "renamed", 1))
            .await;

        assert_eq!(location.range, range_of_nth(updated, "renamed", 0));
    }

    #[tokio::test]
    async fn edits_to_a_library_are_visible_within_that_library() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        // the same library with the definition pushed one line down
        let lib_edited = r#"(provide greet)

;; a comment that was not here before
(define (greet name)
  (string-append "howdy " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        server.write("lib.scm", lib_source);
        server.write("app.scm", app_source);
        server.index_workspace();

        let lib = server.open("lib.scm", lib_source);
        server.open("app.scm", app_source);

        server.did_change(lib.clone(), lib_edited);

        let symbols = flat_symbols(server.document_symbol(&lib).await);
        let greet = symbols
            .iter()
            .find(|symbol| symbol.name == "greet")
            .expect("expected greet among the symbols");

        assert_eq!(greet.location.range, range_of_nth(lib_edited, "greet", 1));
    }

    // The TODO on Backend::did_save. The open buffer never hits disk, the compiled module
    // is cached against the file timestamp, and nothing recompiles the consumer when one of
    // its dependencies changes, so every cross file jump keeps pointing at stale lines.
    #[tokio::test]
    #[ignore = "edits to a library are not propagated to files that require it"]
    async fn edits_to_a_library_are_visible_from_a_consumer() {
        let lib_source = r#"(provide greet)

(define (greet name)
  (string-append "hello " name))
"#;
        let lib_edited = r#"(provide greet)

;; a comment that was not here before
(define (greet name)
  (string-append "howdy " name))
"#;
        let app_source = r#"(require "lib.scm")

(define (main)
  (greet "world"))
"#;

        let mut server = TestServer::new();
        server.write("lib.scm", lib_source);
        server.write("app.scm", app_source);
        server.index_workspace();

        let lib = server.open("lib.scm", lib_source);
        let app = server.open("app.scm", app_source);

        server.did_change(lib.clone(), lib_edited);

        // re-analyze the consumer, the way switching tabs would
        server.did_change(app.clone(), app_source);

        let location = server
            .definition_location(&app, find(app_source, "greet"))
            .await;

        assert_eq!(location.uri, lib);
        assert_eq!(location.range, range_of_nth(lib_edited, "greet", 1));
    }
}

mod offset_encoding_tests {
    use super::*;

    #[tokio::test]
    async fn positions_are_utf16_code_units() {
        // grüße and naïve-name take a different number of bytes, utf16 code units and
        // characters, which is what the conversions have to get right
        let source = r#"(define (grüße naïve-name)
  (list naïve-name naïve-name))
"#;

        let mut server = TestServer::new();
        let uri = server.open("unicode.scm", source);

        // `(define (grüße ` is 15 code units, and naïve-name is 10
        let declaration = Range::new(Position::new(0, 15), Position::new(0, 25));
        // `  (list ` is 8, then two 10 unit uses separated by a space
        let first_use = Range::new(Position::new(1, 8), Position::new(1, 18));
        let second_use = Range::new(Position::new(1, 19), Position::new(1, 29));

        let location = server.definition_location(&uri, first_use.start).await;
        assert_eq!(location.range, declaration);

        let locations = server
            .references(&uri, second_use.start, true)
            .await
            .expect("expected references to the argument");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), declaration),
                Location::new(uri.clone(), first_use),
                Location::new(uri, second_use),
            ]
        );
    }

    #[tokio::test]
    async fn positions_are_bytes_in_utf8() {
        // grüße and naïve-name take a different number of bytes, utf16 code units and
        // characters, which is what the conversions have to get right
        let source = r#"(define (grüße naïve-name)
  (list naïve-name naïve-name))
"#;

        let mut server = TestServer::with_encoding(OffsetEncoding::Utf8);
        let uri = server.open("unicode.scm", source);

        // same source, but now ü and ï count as two each
        let declaration = Range::new(Position::new(0, 17), Position::new(0, 28));
        let first_use = Range::new(Position::new(1, 8), Position::new(1, 19));
        let second_use = Range::new(Position::new(1, 20), Position::new(1, 31));

        let location = server.definition_location(&uri, first_use.start).await;
        assert_eq!(location.range, declaration);

        let locations = server
            .references(&uri, second_use.start, true)
            .await
            .expect("expected references to the argument");

        assert_eq!(
            sorted(locations),
            vec![
                Location::new(uri.clone(), declaration),
                Location::new(uri.clone(), first_use),
                Location::new(uri, second_use),
            ]
        );
    }
}
