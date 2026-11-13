//! steel/line-editor against scripted input: what it returns, and what a
//! terminal shows after the bytes it wrote.

use steel::rvals::SteelVal;
use steel::steel_vm::engine::Engine;
use unicode_width::UnicodeWidthChar;

/// No more bytes for now: a read with a timeout times out, one without
/// skips past it
const PAUSE: i32 = -1;

const ESC: i32 = 27;

fn text(text: &str) -> Vec<i32> {
    text.bytes().map(i32::from).collect()
}

fn keys(parts: &[&[i32]]) -> Vec<i32> {
    parts.concat()
}

fn ctrl(letter: char) -> Vec<i32> {
    vec![letter as i32 - 'a' as i32 + 1]
}

fn alt(letter: char) -> Vec<i32> {
    vec![ESC, letter as i32]
}

fn csi(sequence: &str) -> Vec<i32> {
    let mut bytes = vec![ESC, '[' as i32];
    bytes.extend(text(sequence));
    bytes
}

const ENTER: &[i32] = &[13];
const BACKSPACE: &[i32] = &[127];

struct Run {
    results: Vec<String>,
    /// Everything written, and each write on its own (a redraw is one write)
    output: String,
    writes: Vec<String>,
}

/// One read per entry of `reads`, all on the same editor, with the
/// terminal `columns` wide
fn edit_with(columns: usize, history: Option<&str>, reads: &[Vec<i32>]) -> Run {
    edit_with_setup(columns, history, "", reads)
}

/// `setup` runs with `editor` bound before the reads
fn edit_with_setup(columns: usize, history: Option<&str>, setup: &str, reads: &[Vec<i32>]) -> Run {
    let mut vm = Engine::new();
    crate::register_line_editor(&mut vm);

    let lists: Vec<String> = reads
        .iter()
        .map(|bytes| {
            let items: Vec<String> = bytes
                .iter()
                .map(|&byte| {
                    if byte == PAUSE {
                        "'pause".to_owned()
                    } else {
                        byte.to_string()
                    }
                })
                .collect();
            format!("(list {})", items.join(" "))
        })
        .collect();

    let history = match history {
        Some(path) => format!("{path:?}"),
        None => "#f".to_owned(),
    };

    let program = format!(
        r##"
        (require "steel/line-editor")
        (define input (box '()))
        (define output (box '()))
        (define (fake-read timeout)
          (let loop ()
            (define pending (unbox input))
            (cond
              [(null? pending) (if timeout #f (eof-object))]
              [(equal? (car pending) 'pause)
               (set-box! input (cdr pending))
               (if timeout #f (loop))]
              [else
               (set-box! input (cdr pending))
               (car pending)])))
        (define io
          (make-terminal-io fake-read
                            (lambda (text) (set-box! output (cons text (unbox output))))
                            (lambda () (list {columns} 24))
                            (lambda () #t)
                            (lambda () #t)))
        (define editor (make-line-editor {history} io))
        {setup}
        (define results
          (map (lambda (bytes)
                 (set-box! input bytes)
                 (let ([result (line-editor-read-line! editor "> ")])
                   (if (string? result) result (string-append "#" (symbol->string result)))))
               (list {reads})))
        (list results (reverse (unbox output)))
        "##,
        reads = lists.join(" ")
    );

    let values = vm
        .compile_and_run_raw_program(program)
        .unwrap_or_else(|err| panic!("{}", err));

    let Some(SteelVal::ListV(pair)) = values.last() else {
        panic!("no result")
    };
    let SteelVal::ListV(results) = &pair[0] else {
        panic!("results")
    };
    let SteelVal::ListV(writes) = &pair[1] else {
        panic!("output")
    };
    let writes: Vec<String> = writes
        .iter()
        .map(|write| match write {
            SteelVal::StringV(s) => s.as_str().to_owned(),
            other => panic!("wrote {other}"),
        })
        .collect();

    Run {
        results: results
            .iter()
            .map(|value| match value {
                SteelVal::StringV(s) => s.as_str().to_owned(),
                other => other.to_string(),
            })
            .collect(),
        output: writes.concat(),
        writes,
    }
}

/// Just enough of a terminal for what the editor writes: autowrap with the
/// pending wrap at the last column, `\n` as CR LF (output processing is on in
/// raw mode), CSI A/B/C/D/H/J and SGR (ignored).
struct Screen {
    columns: usize,
    rows: Vec<Vec<char>>,
    row: usize,
    column: usize,
}

impl Screen {
    fn new(columns: usize) -> Self {
        Screen {
            columns,
            rows: vec![Vec::new()],
            row: 0,
            column: 0,
        }
    }

    fn line(&mut self, row: usize) -> &mut Vec<char> {
        while self.rows.len() <= row {
            self.rows.push(Vec::new());
        }
        &mut self.rows[row]
    }

    fn put(&mut self, c: char) {
        let width = c.width().unwrap_or(0);
        if width == 0 {
            return;
        }
        if self.column + width > self.columns {
            self.row += 1;
            self.column = 0;
        }
        let (row, column) = (self.row, self.column);
        let line = self.line(row);
        while line.len() < column + width {
            line.push(' ');
        }
        line[column] = c;
        if width == 2 {
            // The second cell of a wide character
            line[column + 1] = '\0';
        }
        self.column += width;
    }

    fn feed(&mut self, output: &str) {
        let mut chars = output.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\r' => self.column = 0,
                '\n' => {
                    self.column = 0;
                    self.row += 1;
                }
                '\u{1b}' => {
                    assert_eq!(chars.next(), Some('['), "only CSI sequences");
                    let mut parameter = String::new();
                    let fin = loop {
                        let c = chars.next().expect("unfinished CSI");
                        if ('@'..='~').contains(&c) {
                            break c;
                        }
                        parameter.push(c);
                    };
                    let n: usize = parameter.parse().unwrap_or(1);
                    let last = self.columns - 1;
                    match fin {
                        'A' => {
                            self.row = self.row.saturating_sub(n);
                            self.column = self.column.min(last);
                        }
                        'B' => {
                            self.row += n;
                            self.column = self.column.min(last);
                        }
                        'C' => self.column = (self.column + n).min(last),
                        'D' => self.column = self.column.saturating_sub(n).min(last),
                        'H' => {
                            self.row = 0;
                            self.column = 0;
                        }
                        'J' if parameter == "2" => self.rows.clear(),
                        'J' => {
                            let (row, column) = (self.row, self.column.min(last));
                            self.line(row).truncate(column);
                            self.rows.truncate(row + 1);
                        }
                        // SGR, and modes such as bracketed paste
                        'm' | 'h' | 'l' => {}
                        other => panic!("unexpected CSI {parameter}{other}"),
                    }
                }
                c => self.put(c),
            }
        }
    }

    /// Rows as text, trailing blanks and rows dropped
    fn lines(&self) -> Vec<String> {
        let mut lines: Vec<String> = self
            .rows
            .iter()
            .map(|row| {
                row.iter()
                    .filter(|&&c| c != '\0')
                    .collect::<String>()
                    .trim_end()
                    .to_owned()
            })
            .collect();
        while lines.last().is_some_and(|line| line.is_empty()) {
            lines.pop();
        }
        lines
    }

    fn cursor(&self) -> (usize, usize) {
        (self.row, self.column)
    }
}

fn screen(columns: usize, output: &str) -> Screen {
    let mut screen = Screen::new(columns);
    screen.feed(output);
    screen
}

#[test]
fn typing_and_enter() {
    let run = edit_with(40, None, &[keys(&[&text("(+ 1 2)"), ENTER])]);
    assert_eq!(run.results, ["(+ 1 2)"]);
    let screen = screen(40, &run.output);
    assert_eq!(screen.lines(), ["> (+ 1 2)"]);
    // On the line after the input
    assert_eq!(screen.cursor(), (1, 0));
}

#[test]
fn moving_and_deleting() {
    let cases: Vec<(Vec<i32>, &str)> = vec![
        (
            keys(&[&text("helo"), &ctrl('b'), &text("l"), ENTER]),
            "hello",
        ),
        (
            keys(&[&text("ello"), &ctrl('a'), &text("h"), ENTER]),
            "hello",
        ),
        (
            keys(&[&text("hell"), &ctrl('a'), &ctrl('e'), &text("o"), ENTER]),
            "hello",
        ),
        (
            keys(&[&text("hellxo"), &csi("D"), BACKSPACE, ENTER]),
            "hello",
        ),
        (
            keys(&[&text("hellxo"), &csi("D"), &csi("D"), &ctrl('d'), ENTER]),
            "hello",
        ),
        (
            keys(&[&text("hellxo"), &csi("H"), &csi("3~"), ENTER]),
            "ellxo",
        ),
        (
            keys(&[&text("hello world"), &alt('b'), &ctrl('k'), ENTER]),
            "hello ",
        ),
        (keys(&[&text("hello world"), &ctrl('u'), ENTER]), ""),
        (keys(&[&text("one two"), &ctrl('w'), ENTER]), "one "),
        (
            keys(&[&text("one two"), &ctrl('a'), &alt('d'), ENTER]),
            " two",
        ),
        (
            keys(&[&text("(one two"), &vec![ESC, 127], &text(")"), ENTER]),
            "(one )",
        ),
        // Like emacs, from after a ) it takes the ) and the word
        (
            keys(&[&text("(one two)"), &vec![ESC, 127], &text(")"), ENTER]),
            "(one )",
        ),
        (
            keys(&[&text("one two"), &ctrl('a'), &alt('f'), &text("!"), ENTER]),
            "one! two",
        ),
        (
            keys(&[&text("one two"), &csi("1;5D"), &text("_"), ENTER]),
            "one _two",
        ),
        (keys(&[&text("ab"), &ctrl('t'), ENTER]), "ba"),
        (keys(&[&text("abc"), &ctrl('b'), &ctrl('t'), ENTER]), "acb"),
    ];
    for (bytes, expected) in cases {
        let run = edit_with(40, None, &[bytes.clone()]);
        assert_eq!(run.results, [expected], "for {bytes:?}");
    }
}

#[test]
fn kill_ring() {
    // Kills in a row add up, yank puts them back, M-y swaps in older ones
    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("a b c"),
            &ctrl('w'),
            &ctrl('w'),
            &ctrl('y'),
            &text("|"),
            &ctrl('y'),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["a b c|b c"]);

    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("first"),
            &ctrl('u'),
            &text("second"),
            &ctrl('u'),
            &ctrl('y'),
            &alt('y'),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["first"]);
}

#[test]
fn graphemes_and_wide_characters() {
    // e + combining acute is one grapheme: C-b goes over both, backspace
    // takes both
    let run = edit_with(
        40,
        None,
        &[
            keys(&[&text("e\u{301}"), &ctrl('b'), &text("x"), ENTER]),
            keys(&[&text("ae\u{301}"), BACKSPACE, ENTER]),
        ],
    );
    assert_eq!(run.results, ["xe\u{301}", "a"]);

    // Wide characters wrap as a whole: "> 日本" is 6 columns of 7, so 語
    // goes to the next row
    let run = edit_with(7, None, &[keys(&[&text("日本語"), ENTER])]);
    assert_eq!(run.results, ["日本語"]);
    let screen = screen(7, &run.output);
    assert_eq!(screen.lines(), ["> 日本", "語"]);
}

#[test]
fn wrapping_and_moving_back_over_it() {
    // "> " + 8 fills a 10 column row exactly, the cursor goes to the next row
    let run = edit_with(10, None, &[keys(&[&text("abcdefgh"), ENTER])]);
    let screen_after = screen(10, &run.output);
    assert_eq!(screen_after.lines(), ["> abcdefgh"]);
    assert_eq!(screen_after.cursor(), (1, 0));

    // Editing in the first row of a wrapped input redraws both rows
    let run = edit_with(
        10,
        None,
        &[keys(&[
            &text("abcdefghijkl"),
            &ctrl('a'),
            &text("X"),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["Xabcdefghijkl"]);
    let screen = screen(10, &run.output);
    assert_eq!(screen.lines(), ["> Xabcdefg", "hijkl"]);
    assert_eq!(screen.cursor(), (2, 0));
}

#[test]
fn cursor_lands_where_the_text_says() {
    // Input runs out after these keys, so the read ends with a final redraw
    // (cursor to the end) and a newline. Everything before those two writes
    // shows the cursor where the keys left it.
    let cases: Vec<(Vec<i32>, (usize, usize))> = vec![
        (keys(&[&text("abcdefghijkl"), &ctrl('a')]), (0, 2)),
        (
            keys(&[&text("abcdefghijkl"), &ctrl('a'), &ctrl('f')]),
            (0, 3),
        ),
        // Over the wrap and back
        (
            keys(&[
                &text("abcdefghijkl"),
                &csi("D"),
                &csi("D"),
                &csi("D"),
                &csi("D"),
            ]),
            (1, 0),
        ),
        (
            keys(&[
                &text("abcdefghijkl"),
                &csi("D"),
                &csi("D"),
                &csi("D"),
                &csi("D"),
                &csi("D"),
            ]),
            (0, 9),
        ),
        // Exactly full: the cursor waits on the next row
        (text("abcdefgh"), (1, 0)),
        (
            keys(&[&text("(a"), &[10], &text("bc)"), &ctrl('a')]),
            (1, 2),
        ),
    ];
    for (bytes, cursor) in cases {
        let run = edit_with(10, None, &[bytes.clone()]);
        // A read ends with the final redraw, the newline after it unless the
        // redraw already ended on a fresh row, and turning bracketed paste
        // off. Everything before those shows the cursor where the keys left it.
        let mut writes = run.writes.clone();
        assert_eq!(writes.pop().as_deref(), Some("\u{1b}[?2004l"));
        if writes.last().is_some_and(|w| w == "\n") {
            writes.pop();
        }
        writes.pop();
        let shown = writes.concat();
        let screen = screen(10, &shown);
        assert_eq!(
            screen.cursor(),
            cursor,
            "for {bytes:?}, showing {:?}",
            screen.lines()
        );
    }
}

#[test]
fn multi_line_input() {
    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("(define (f x)"),
            ENTER,
            &text("  (* x 2))"),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["(define (f x)\n  (* x 2))"]);
    let screen = screen(40, &run.output);
    assert_eq!(screen.lines(), ["> (define (f x)", "    (* x 2))"]);
    assert_eq!(screen.cursor(), (2, 0));

    // Up and down move between lines before going to history
    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("(a"),
            ENTER,
            &text("bc)"),
            &csi("A"),
            &text("!"),
            &csi("B"),
            &text("?"),
            ENTER,
        ])],
    );
    // Down keeps the column, 3, which is past the end of "bc)"
    assert_eq!(run.results, ["(a!\nbc)?"]);

    // C-j always adds a line
    let run = edit_with(40, None, &[keys(&[&text("x"), &[10], &text("y"), ENTER])]);
    assert_eq!(run.results, ["x\ny"]);
}

#[test]
fn history_navigation() {
    let run = edit_with(
        40,
        None,
        &[
            keys(&[&text("one"), ENTER]),
            keys(&[&text("two"), ENTER]),
            // Up twice for "one", down for "two", down again for what was typed
            keys(&[
                &text("typed"),
                &csi("A"),
                &csi("A"),
                &csi("B"),
                &csi("B"),
                ENTER,
            ]),
            keys(&[&csi("A"), &csi("A"), ENTER]),
            // Repeats aren't added twice
            keys(&[&csi("A"), ENTER]),
            keys(&[&csi("A"), &csi("A"), ENTER]),
        ],
    );
    assert_eq!(run.results, ["one", "two", "typed", "two", "two", "typed"]);
}

#[test]
fn history_file_round_trip() {
    let dir = std::env::temp_dir().join(format!("steel-line-editor-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("history");
    let _ = std::fs::remove_file(&path);
    let path_text = path.to_str().unwrap();

    edit_with(
        40,
        Some(path_text),
        &[
            keys(&[&text("plain"), ENTER]),
            keys(&[&text("(a"), ENTER, &text(r#""\\")"#), ENTER]),
        ],
    );
    let written = std::fs::read_to_string(&path).unwrap();
    assert_eq!(written, "#V2\nplain\n(a\\n\"\\\\\\\\\")\n");

    // A new editor reads it back
    let run = edit_with(
        40,
        Some(path_text),
        &[
            keys(&[&csi("A"), ENTER]),
            // Up moves within a multi-line entry before going further back
            keys(&[&csi("A"), &csi("A"), &csi("A"), ENTER]),
        ],
    );
    assert_eq!(run.results, ["(a\n\"\\\\\")", "plain"]);

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn interrupt_and_eof() {
    let run = edit_with(
        40,
        None,
        &[
            keys(&[&text("abc"), &ctrl('c')]),
            ctrl('c'),
            ctrl('d'),
            // C-d with text deletes instead
            keys(&[&text("ab"), &ctrl('a'), &ctrl('d'), ENTER]),
            // Input running out
            text("partial"),
        ],
    );
    assert_eq!(
        run.results,
        ["#interrupted", "#interrupted-empty", "#eof", "b", "partial"]
    );
}

#[test]
fn escape_sequences() {
    // A lone ESC (nothing follows in time) doesn't swallow the next key
    let run = edit_with(
        40,
        None,
        &[keys(&[&text("ab"), &[ESC, PAUSE], &text("c"), ENTER])],
    );
    assert_eq!(run.results, ["abc"]);

    // SS3 arrows, ~ style Home and End, unknown sequences ignored
    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("bc"),
            &[ESC, 'O' as i32, 'D' as i32],
            &[ESC, 'O' as i32, 'D' as i32],
            &text("a"),
            &csi("4~"),
            &text("d"),
            &csi("1~"),
            &text("<"),
            &csi("99~"),
            &csi("?1;2c"),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["<abcd"]);
}

#[test]
fn keymap_changes_take_effect() {
    let mut vm = Engine::new();
    crate::register_line_editor(&mut vm);
    let values = vm
        .compile_and_run_raw_program(
            r#"
            (require "steel/line-editor")
            (define input (box (list 1 2 13)))
            (define io
              (make-terminal-io (lambda (timeout)
                                  (let ([pending (unbox input)])
                                    (set-box! input (cdr pending))
                                    (car pending)))
                                (lambda (text) #t)
                                (lambda () (list 40 24))
                                (lambda () #t)
                                (lambda () #t)))
            (define editor (make-line-editor #f io))
            ;; C-a inserts a word, C-b accepts whatever is there
            (keymap-set! (line-editor-keymap (current-line-editor)) "C-a"
                         (lambda (state) (edit-insert! state "custom")))
            (keymap-set! (line-editor-keymap editor) "C-b" edit-accept!)
            (line-editor-read-line! editor "> ")
            "#,
        )
        .unwrap_or_else(|err| panic!("{}", err));
    assert_eq!(values.last().unwrap().to_string(), "\"custom\"");
}

#[test]
fn failing_command_is_reported_and_editing_goes_on() {
    let mut vm = Engine::new();
    crate::register_line_editor(&mut vm);
    let values = vm
        .compile_and_run_raw_program(
            r#"
            (require "steel/line-editor")
            ;; a, C-o, b, Enter
            (define input (box (list 97 15 98 13)))
            (define output (box '()))
            (define io
              (make-terminal-io (lambda (timeout)
                                  (let ([pending (unbox input)])
                                    (set-box! input (cdr pending))
                                    (car pending)))
                                (lambda (text) (set-box! output (cons text (unbox output))))
                                (lambda () (list 40 24))
                                (lambda () #t)
                                (lambda () #t)))
            (define editor (make-line-editor #f io))
            (keymap-set! (line-editor-keymap editor) "C-o" (lambda (state) (car 5)))
            (list (line-editor-read-line! editor "> ")
                  (apply string-append (reverse (unbox output))))
            "#,
        )
        .unwrap_or_else(|err| panic!("{}", err));
    let Some(SteelVal::ListV(pair)) = values.last() else {
        panic!("no result")
    };
    assert_eq!(pair[0].to_string(), "\"ab\"");
    let SteelVal::StringV(output) = &pair[1] else {
        panic!("output")
    };
    let screen = screen(40, output.as_str());
    let lines = screen.lines();
    assert_eq!(lines[0], "> a");
    assert!(
        lines[1].starts_with("The command for C-o failed:"),
        "{lines:?}"
    );
    assert_eq!(lines.last().unwrap(), "> ab");
}

/// Runs `program` (which can use steel/line-editor's exports) and gives back
/// its last value as a string
fn steel(program: &str) -> String {
    let mut vm = Engine::new();
    crate::register_line_editor(&mut vm);
    let values = vm
        .compile_and_run_raw_program(format!(
            "(require \"steel/line-editor\") (require-builtin #%private/steel/terminal) {program}"
        ))
        .unwrap_or_else(|err| panic!("{}", err));
    match values.last().unwrap() {
        SteelVal::StringV(s) => s.as_str().to_owned(),
        other => other.to_string(),
    }
}

#[test]
fn tokens_in_chars() {
    // λ is two bytes and one char; #( starts at the paren
    assert_eq!(
        steel(r#"(tokenize "(λ x \"s\" 1 #t #(1) ; c\n define)")"#),
        // λ lexes as lambda
        "((open 0 1) (form 1 2) (identifier 3 4) (string 5 8) (number 9 10) \
         (boolean 11 13) (open 15 16) (number 16 17) (close 17 18) (comment 19 23) (form 24 30) \
         (close 30 31))"
    );
}

const NAMES: &str = r##"(define names
  (lambda () (list "string-append" "string?" "string-length" "display" "displayln"
                   "#%string-builtin" "substring" "map")))
  (set-line-editor-completer! editor (identifier-completer names))"##;

const TAB: &[i32] = &[9];

#[test]
fn completion() {
    let run = edit_with_setup(
        60,
        None,
        NAMES,
        &[
            // One candidate goes straight in
            keys(&[&text("ma"), TAB, ENTER]),
            // In the middle of a word the whole word is replaced
            keys(&[&text("dis"), &ctrl('b'), TAB, ENTER]),
            // Several: what the ones starting with it share goes in, then Tab
            // goes through all of them (substring matches too) and back
            keys(&[&text("str"), TAB, ENTER]),
            keys(&[&text("str"), TAB, TAB, ENTER]),
            keys(&[&text("str"), TAB, TAB, TAB, TAB, ENTER]),
            keys(&[&text("str"), TAB, TAB, TAB, TAB, TAB, ENTER]),
            keys(&[&text("str"), TAB, TAB, TAB, TAB, TAB, TAB, ENTER]),
            // Nothing matching does nothing
            keys(&[&text("zzz"), TAB, ENTER]),
            // Nothing to complete at the cursor indents
            keys(&[&text("("), TAB, &text("x)"), ENTER]),
        ],
    );
    assert_eq!(
        run.results,
        [
            "map",
            "display",
            "string",
            "string-append",
            "string?",
            "substring",
            "string",
            "zzz",
            "(  x)",
        ]
        .map(|s| s.to_owned())
    );
}

#[test]
fn completion_ranking() {
    // Prefix matches, then the ones containing it; #% names left out
    assert_eq!(
        steel(&format!(
            r#"{} ((identifier-completer names) "(string" 4)"#,
            NAMES.replace(
                "(set-line-editor-completer! editor (identifier-completer names))",
                ""
            )
        )),
        r##"(1 7 ("string-append" "string-length" "string?" "substring"))"##
    );
}

#[test]
fn internal_names_only_with_a_hash() {
    let names = NAMES.replace(
        "(set-line-editor-completer! editor (identifier-completer names))",
        "",
    );
    assert_eq!(
        steel(&format!(
            r##"{names} ((identifier-completer names) "#%str" 5)"##
        )),
        r##"(0 5 ("#%string-builtin"))"##
    );
}

#[test]
fn completion_menu_on_screen() {
    // Stop with the menu up, then check what's shown
    let run = edit_with_setup(40, None, NAMES, &[keys(&[&text("(dis"), TAB, TAB])]);
    let mut writes = run.writes.clone();
    writes.pop(); // bracketed paste off
    if writes.last().is_some_and(|w| w == "\n") {
        writes.pop();
    }
    writes.pop(); // final redraw
    let screen = screen(40, &writes.concat());
    assert_eq!(screen.lines(), ["> (display", "display    displayln"]);
    assert_eq!(screen.cursor(), (0, 10));

    // The final redraw takes the menu away
    let screen_after = self::screen(40, &run.output);
    assert_eq!(screen_after.lines(), ["> (display"]);
}

#[test]
fn highlighting() {
    let esc = "\u{1b}";
    let highlighted = steel(
        r#"((steel-highlighter (lambda (name) (equal? name "car"))) "(define x (car \"s\" 1 #t))" 0)"#,
    );
    assert_eq!(
        highlighted,
        format!(
            // The cursor is on the first paren, so its partner, the last
            // paren, is the bold one
            "({esc}[95mdefine{esc}[0m x ({esc}[94mcar{esc}[0m {esc}[92m\"s\"{esc}[0m \
             {esc}[93m1{esc}[0m {esc}[95m#t{esc}[0m){esc}[1;34m){esc}[0m"
        )
    );
}

#[test]
fn bracket_partners() {
    let partner = |text: &str, cursor: usize| {
        steel(&format!(
            r#"(let ([out ((steel-highlighter) {text:?} {cursor})])
                 (let loop ([at 0])
                   (cond
                     [(>= at (string-length out)) #f]
                     [(starts-with? (substring out at (string-length out))
                                    (string-append (string (integer->char 27)) "[1;34m"))
                      at]
                     [else (loop (+ at 1))])))"#
        ))
    };
    // No colours other than the partner here, so where the bold escape
    // starts is where the partner is
    assert_eq!(partner("(a (b))", 7), "0"); // after the last )
    assert_eq!(partner("(a (b))", 6), "3"); // after the inner )
    assert_eq!(partner("(a (b))", 0), "6"); // on the first (
    assert_eq!(partner("(a (b))", 1), "6"); // just after the first (
    assert_eq!(partner("(a b)", 2), "#false"); // touching nothing
    assert_eq!(partner("(a \"(\")", 7), "0"); // the ( in the string doesn't count
    assert_eq!(partner("(a]", 3), "#false"); // mismatched
}

#[test]
fn highlighting_keeps_the_layout() {
    let reads = [keys(&[
        &text("(define (f x) (car x))"),
        &ctrl('a'),
        &text(" "),
        ENTER,
    ])];
    let plain = edit_with(12, None, &reads);
    let colored = edit_with_setup(
        12,
        None,
        r#"(set-line-editor-highlighter! editor (steel-highlighter (lambda (name) #t)))"#,
        &reads,
    );
    assert_eq!(plain.results, colored.results);
    assert_eq!(
        screen(12, &plain.output).lines(),
        screen(12, &colored.output).lines()
    );
    assert!(colored.output.contains("\u{1b}[95mdefine"));
}

fn paste(text_bytes: &[i32]) -> Vec<i32> {
    keys(&[&csi("200~"), text_bytes, &csi("201~")])
}

#[test]
fn bracketed_paste() {
    let run = edit_with(
        40,
        None,
        &[keys(&[
            &text("x "),
            // A newline (as CR or CR LF) and a tab in a paste go in as text
            &paste(&keys(&[
                &text("(+ 1"),
                &[13, 10],
                &text("2)"),
                &[13],
                &text("\ty"),
            ])),
            ENTER,
        ])],
    );
    assert_eq!(run.results, ["x (+ 1\n2)\n  y"]);
    // Paste mode is on while reading and off after
    assert!(run.writes.first().unwrap().contains("\u{1b}[?2004h"));
    assert_eq!(run.writes.last().unwrap(), "\u{1b}[?2004l");
}

const HISTORY: &str = r#"(for-each (lambda (line) (history-add! (line-editor-history editor) line))
  (list "(define a 1)" "(+ 1 2)" "(display a)"))"#;

#[test]
fn history_search() {
    // Each on a fresh editor, since accepted lines go into history
    let cases: Vec<(Vec<i32>, &str)> = vec![
        // Newest match first, C-r for older, Enter takes it
        (keys(&[&ctrl('r'), &text("a"), ENTER]), "(display a)"),
        (
            keys(&[&ctrl('r'), &text("a"), &ctrl('r'), ENTER]),
            "(define a 1)",
        ),
        // No older match: stays on the last one
        (
            keys(&[&ctrl('r'), &text("a"), &ctrl('r'), &ctrl('r'), ENTER]),
            "(define a 1)",
        ),
        // C-g goes back to what was typed
        (
            keys(&[&text("typed"), &ctrl('r'), &text("+"), &ctrl('g'), ENTER]),
            "typed",
        ),
        // Any other key keeps the match and does its thing
        (
            keys(&[&ctrl('r'), &text("1 2"), &ctrl('e'), &text(" 3"), ENTER]),
            "(+ 1 2) 3",
        ),
        // Backspace searches again with less
        (
            keys(&[&ctrl('r'), &text("dx"), BACKSPACE, ENTER]),
            "(display a)",
        ),
    ];
    for (bytes, expected) in cases {
        let run = edit_with_setup(60, None, HISTORY, &[bytes.clone()]);
        assert_eq!(run.results, [expected], "for {bytes:?}");
    }
}

#[test]
fn history_search_on_screen() {
    // Up to the last frame drawn while searching
    let shown = |bytes: Vec<i32>| {
        let run = edit_with_setup(60, None, HISTORY, &[bytes]);
        let last = run
            .writes
            .iter()
            .rposition(|w| w.contains("reverse-i-search"))
            .unwrap();
        screen(60, &run.writes[..=last].concat()).lines()
    };
    assert_eq!(
        shown(keys(&[&ctrl('r'), &text("1 2")])),
        ["(reverse-i-search)`1 2': (+ 1 2)"]
    );
    assert_eq!(
        shown(keys(&[&ctrl('r'), &text("1 2z")])),
        ["(failed reverse-i-search)`1 2z': (+ 1 2)"]
    );
}

#[test]
fn no_bracket_highlight_left_behind() {
    let run = edit_with_setup(
        40,
        None,
        r#"(set-line-editor-highlighter! editor (steel-highlighter))"#,
        &[keys(&[&text("(a)"), ENTER])],
    );
    // While typing, the ( was matched with the ) before the cursor
    assert!(run.output.contains("\u{1b}[1;34m"));
    // The last draw, the one that stays, has no bold bracket
    let last_draw = run.writes.iter().rev().find(|w| w.contains("(a)")).unwrap();
    assert!(!last_draw.contains("\u{1b}[1;34m"), "{last_draw:?}");
}
