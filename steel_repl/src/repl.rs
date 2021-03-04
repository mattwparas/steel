extern crate rustyline;
use colored::*;
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
// use steel::vm::VirtualMachine;

use steel_vm::engine::Engine;

use std::io::Read;
use steel::stdlib::{CONTRACTS, PRELUDE};

use std::time::Instant;

#[macro_export]
macro_rules! build_repl {
    ($($type:ty),*) => {
        {
            use crate::build_engine;
            let mut interpreter = build_engine!{
                $(
                    $type
                ),*
            };
            repl_base(interpreter)
        }
    };
}

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
        :o          -- toggles optimizations
        :quit       -- exits the REPL
        "#
    );
}

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

    let buffer = String::new();

    // TODO make this better
    let core_libraries = &[PRELUDE, CONTRACTS];

    let current_dir = std::env::current_dir()?;

    println!("{:?}", current_dir);

    for core in core_libraries {
        let res = vm.parse_and_execute_without_optimizations(core);

        match res {
            Ok(r) => r.iter().for_each(|x| match x {
                SteelVal::Void => {}
                _ => println!("{} {}", "=>".bright_blue().bold(), x),
            }),
            Err(e) => {
                e.emit_result("stdlib.stl", buffer.as_str());
                eprintln!("{}", e.to_string().bright_red());
            }
        }
    }

    let mut print_time = false;
    let mut optimizations = false;

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
                    ":o" => {
                        optimizations = !optimizations;
                        println!(
                            "{} {}",
                            "Optimizations set to:".bright_purple(),
                            optimizations.to_string().bright_green()
                        );
                    }
                    // TODO come back
                    // ":env" => vm.print_bindings(),
                    ":?" | ":help" => display_help(),
                    line if line.contains(":require") => {
                        let line = line.trim_start_matches(":require").trim();
                        let path = Path::new(line);

                        let file = std::fs::File::open(path);

                        if let Err(e) = file {
                            eprintln!("{}", e);
                            continue;
                        }

                        let mut file = file?;

                        let mut exprs = String::new();
                        file.read_to_string(&mut exprs)?;

                        let res = if optimizations {
                            vm.run_with_path(exprs.as_str(), path.to_path_buf())
                        } else {
                            vm.run_with_path(exprs.as_str(), path.to_path_buf())
                            // vm.parse_and_execute_without_optimizations(exprs.as_str())
                        };

                        match res {
                            Ok(r) => r.iter().for_each(|x| match x {
                                SteelVal::Void => {}
                                _ => println!("{} {}", "=>".bright_blue().bold(), x),
                            }),
                            Err(e) => {
                                e.emit_result("repl.stl", exprs.as_str());
                                eprintln!("{}", e.to_string().bright_red());
                            }
                        }
                    }
                    _ => {
                        // println!("Active Object Count: {:?}", steel::gc::OBJECT_COUNT);

                        let now = Instant::now();

                        let res = if optimizations {
                            vm.parse_and_execute(&line)
                        } else {
                            vm.parse_and_execute_without_optimizations(&line)
                        };

                        match res {
                            Ok(r) => r.iter().for_each(|x| match x {
                                SteelVal::Void => {}
                                _ => println!("{} {}", "=>".bright_blue().bold(), x),
                            }),
                            Err(e) => {
                                e.emit_result("repl.stl", line.as_str());
                                // eprintln!("{}", e.to_string().bright_red());
                            }
                        }

                        if print_time {
                            println!("Time taken: {:?}", now.elapsed());
                        }

                        // println!("Active Object Count: {:?}", steel::gc::OBJECT_COUNT);
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

pub fn repl() -> std::io::Result<()> {
    unimplemented!()
    // repl_base(interpreter::SteelInterpreter::new())
}
