extern crate rustyline;
use colored::*;

use std::sync::mpsc::channel;

use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::validate::{
    MatchingBracketValidator, ValidationContext, ValidationResult, Validator,
};
use rustyline::Editor;
use rustyline::{hint::Hinter, CompletionType, Context};
use rustyline_derive::Helper;
use std::path::{Path, PathBuf};
use steel::rvals::SteelVal;

use rustyline::completion::Completer;
use rustyline::completion::Pair;

use std::borrow::Cow;

use steel::steel_vm::engine::Engine;

use std::io::Read;
use steel::stdlib::{CONTRACTS, DISPLAY, PRELUDE};

use std::time::Instant;

impl Completer for RustylineHelper {
    type Candidate = Pair;
}

#[derive(Helper)]
struct RustylineHelper {
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
}

impl Validator for RustylineHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

impl Hinter for RustylineHelper {
    type Hint = String;
    fn hint(&self, _line: &str, _pos: usize, _context: &Context) -> Option<String> {
        None
    }
}

impl Highlighter for RustylineHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        self.highlighter.highlight_prompt(prompt, default)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        self.highlighter.highlight_hint(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: CompletionType,
    ) -> Cow<'c, str> {
        self.highlighter.highlight_candidate(candidate, completion)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

fn display_help() {
    println!(
        "{}",
        r#"
        :time       -- toggles the timing of expressions
        :? | :help  -- displays help dialog
        :quit       -- exits the REPL
        :pwd        -- displays the current working directory
        "#
    );
}

fn finish_load_or_interrupt(vm: &mut Engine, exprs: String, path: PathBuf) {
    let file_name = path.to_str().unwrap().to_string();

    let res = vm.compile_and_run_raw_program_with_path(exprs.as_str(), path);

    match res {
        Ok(r) => r.iter().for_each(|x| match x {
            SteelVal::Void => {}
            x if x.is_struct() => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_printing_method_in_context(x.clone())
                    .expect("Failed to convert this value to a string representation to display");
            }
            _ => println!("{} {}", "=>".bright_blue().bold(), x),
        }),
        Err(e) => {
            e.emit_result(file_name.as_str(), exprs.as_str());
            eprintln!("{}", e.to_string().bright_red());
        }
    }
}

fn finish_or_interrupt(vm: &mut Engine, line: String, print_time: bool) {
    let now = Instant::now();

    let res = vm.compile_and_run_raw_program(&line);

    match res {
        Ok(r) => r.iter().for_each(|x| match x {
            SteelVal::Void => {}
            x if x.is_struct() => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_printing_method_in_context(x.clone())
                    .expect("Failed to convert this value to a string representation to display");
            }
            _ => println!("{} {}", "=>".bright_blue().bold(), x),
        }),
        Err(e) => {
            e.emit_result("repl.stl", line.as_str());
        }
    }

    if print_time {
        println!("Time taken: {:?}", now.elapsed());
    }
}

/// Entire point for the repl
/// Automatically adds the prelude and contracts for the core library
pub fn repl_base(mut vm: Engine) -> std::io::Result<()> {
    println!(
        "{}",
        r#"
     _____ __            __
    / ___// /____  ___  / /          Version 0.1.0
    \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
   ___/ / /_/  __/  __/ /            :? for help
  /____/\__/\___/\___/_/ 
    "#
        .bright_yellow()
        .bold()
    );
    let prompt = format!("{}", "λ > ".bright_green().bold().italic());

    let mut rl = Editor::<RustylineHelper>::new();
    rl.set_helper(Some(RustylineHelper {
        highlighter: MatchingBracketHighlighter::default(),
        validator: MatchingBracketValidator::default(),
    }));

    // let buffer = String::new();

    // TODO make this better
    // let core_libraries = &[PRELUDE, DISPLAY, CONTRACTS];

    let current_dir = std::env::current_dir()?;

    // for core in core_libraries {
    //     let res = vm.compile_and_run_raw_program(core);

    //     match res {
    //         Ok(r) => r.iter().for_each(|x| match x {
    //             SteelVal::Void => {}
    //             _ => println!("{} {}", "=>".bright_blue().bold(), x),
    //         }),
    //         Err(e) => {
    //             e.emit_result("stdlib.stl", buffer.as_str());
    //             eprintln!("{}", e.to_string().bright_red());
    //         }
    //     }
    // }

    let mut print_time = false;

    let (tx, rx) = channel();

    ctrlc::set_handler(move || tx.send(()).expect("Could not send signal on channel."))
        .expect("Error setting Ctrl-C handler");

    vm.on_progress(move |x| {
        if x % 1000 == 0 {
            match rx.try_recv() {
                Ok(_) => return false,
                _ => {}
            }
        }
        true
    });

    loop {
        let readline = rl.readline(&prompt);

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match line.as_str() {
                    ":quit" => return Ok(()),
                    // ":reset" => interpreter.reset(),
                    ":time" => {
                        print_time = !print_time;
                        println!(
                            "{} {}",
                            "Expression timer set to:".bright_purple(),
                            print_time.to_string().bright_green()
                        );
                    }
                    ":pwd" => println!("{:#?}", current_dir),
                    // ":env" => vm.print_bindings(),
                    ":?" | ":help" => display_help(),
                    line if line.contains(":load") => {
                        let line = line.trim_start_matches(":load").trim();
                        let path = Path::new(line);

                        let file = std::fs::File::open(path);

                        if let Err(e) = file {
                            eprintln!("{}", e);
                            continue;
                        }

                        let mut file = file?;

                        let mut exprs = String::new();
                        file.read_to_string(&mut exprs)?;

                        finish_load_or_interrupt(&mut vm, exprs, path.to_path_buf());
                    }
                    _ => {
                        // TODO also include this for loading files
                        finish_or_interrupt(&mut vm, line, print_time);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
