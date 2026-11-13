//! The REPL's side of `steel/line-editor` (src/scheme/line-editor.scm). The
//! editor runs in the REPL's own engine, so `(require "steel/line-editor")`
//! at the prompt reaches the editor in use, keymap and all.

use std::collections::HashSet;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use steel::rerrs::{ErrorKind, SteelErr};
use steel::rvals::{SteelString, SteelVal};
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel::steel_vm::ThreadStateController;
use steel_parser::interner::InternedString;

use crate::editor::Input;

pub(crate) struct SteelEditor {
    editor: SteelVal,
    read_line: SteelVal,
    safepoint: ThreadStateController,
    close_on_interrupt: Arc<AtomicBool>,
    /// Whether the last read ended with C-c on an empty line. Two in a row
    /// close the REPL.
    empty_interrupted: bool,
}

impl SteelEditor {
    pub(crate) fn new(
        vm: &mut Engine,
        history_path: &Path,
        globals: Arc<Mutex<HashSet<InternedString>>>,
        safepoint: ThreadStateController,
        close_on_interrupt: Arc<AtomicBool>,
    ) -> Result<Self, SteelErr> {
        crate::register_line_editor(vm);

        // The globals the REPL keeps up to date, for completing and
        // colouring identifiers
        let known = globals.clone();
        vm.register_fn("#%repl-global?", move |name: SteelString| {
            InternedString::try_get(name.as_str())
                .is_some_and(|name| known.lock().unwrap().contains(&name))
        });
        vm.register_fn("#%repl-global-names", move || {
            globals
                .lock()
                .unwrap()
                .iter()
                .map(|name| name.resolve().to_owned())
                .collect::<Vec<_>>()
        });

        // Prefixed, so the editor's names stay out of the user's way
        let procedures = vm.compile_and_run_raw_program(
            r#"(require (prefix-in #%line-editor/ "steel/line-editor"))
               (list #%line-editor/make-line-editor
                     #%line-editor/line-editor-read-line!
                     (lambda (editor)
                       (#%line-editor/set-line-editor-completer!
                        editor
                        (#%line-editor/identifier-completer #%repl-global-names))
                       (#%line-editor/set-line-editor-highlighter!
                        editor
                        (#%line-editor/steel-highlighter #%repl-global?))))"#,
        )?;

        let Some(SteelVal::ListV(procedures)) = procedures.last() else {
            return Err(SteelErr::new(
                ErrorKind::Generic,
                "steel/line-editor didn't load".to_owned(),
            ));
        };
        let make = procedures.get(0).cloned().unwrap_or(SteelVal::Void);
        let read_line = procedures.get(1).cloned().unwrap_or(SteelVal::Void);
        let set_up = procedures.get(2).cloned().unwrap_or(SteelVal::Void);

        let editor = vm.call_function_with_args(
            make,
            vec![SteelVal::StringV(
                history_path.to_string_lossy().into_owned().into(),
            )],
        )?;
        vm.call_function_with_args(set_up, vec![editor.clone()])?;

        Ok(SteelEditor {
            editor,
            read_line,
            safepoint,
            close_on_interrupt,
            empty_interrupted: false,
        })
    }

    pub(crate) fn read_line(&mut self, vm: &mut Engine, prompt: &str) -> Result<Input, SteelErr> {
        // An interrupt left over from the last evaluation would stop the
        // editor before it reads anything
        self.safepoint.resume();

        let result = vm.call_function_with_args(
            self.read_line.clone(),
            vec![self.editor.clone(), SteelVal::StringV(prompt.into())],
        );

        let result = match result {
            Ok(result) => result,
            Err(err) => {
                // The editor turns raw mode off when it raises, but not if
                // it never got that far
                let _ = crate::terminal::disable_raw_mode();
                return Err(err);
            }
        };

        let empty_interrupted = std::mem::take(&mut self.empty_interrupted);

        match result {
            SteelVal::StringV(line) => Ok(Input::Line(line.as_str().to_owned())),
            SteelVal::SymbolV(symbol) => match symbol.as_str() {
                "eof" => Ok(Input::Eof),
                "interrupted" => Ok(Input::Interrupted),
                "interrupted-empty" => {
                    if empty_interrupted {
                        self.close_on_interrupt.store(true, Ordering::Release);
                    }
                    self.empty_interrupted = true;
                    Ok(Input::Interrupted)
                }
                other => Err(unexpected(other)),
            },
            other => Err(unexpected(&other.to_string())),
        }
    }
}

fn unexpected(value: &str) -> SteelErr {
    SteelErr::new(
        ErrorKind::Generic,
        format!("line-editor-read-line! returned {value}"),
    )
}
