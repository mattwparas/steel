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
use std::path::Path;
use steel::rvals::SteelVal;

use rustyline::completion::Completer;
use rustyline::completion::Pair;

use std::borrow::Cow;
use steel::vm::VirtualMachine;

use std::io::Read;
use steel::parser::span::Span;
use steel::stdlib::PRELUDE;

#[macro_export]
macro_rules! build_repl {
    ($($type:ty),*) => {
        {
            use crate::build_vm;
            let mut interpreter = build_vm!{
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
    println!("Help TBD")
}

pub fn repl_base(mut vm: VirtualMachine) -> std::io::Result<()> {
    println!(
        "{}",
        r#"
     _____ __            __
    / ___// /____  ___  / /          Version 0.1.0
    \__ \/ __/ _ \/ _ \/ /           https://github.com.mattwparas/steel
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

    let res = vm.parse_and_execute(PRELUDE);

    match res {
        Ok(r) => r.iter().for_each(|x| match x.as_ref() {
            SteelVal::Void => {}
            _ => println!("{} {}", "=>".bright_blue().bold(), x),
        }),
        Err(e) => {
            e.emit_result("stdlib.stl", buffer.as_str(), Span::new(0, 0));
            eprintln!("{}", e.to_string().bright_red());
        }
    }

    loop {
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match line.as_str() {
                    ":quit" => return Ok(()),
                    // ":reset" => interpreter.reset(),
                    ":env" => vm.print_bindings(),
                    ":?" => display_help(),
                    line if line.contains(":require") => {
                        let line = line.trim_start_matches(":require").trim();
                        let path = Path::new(line);

                        let mut file = std::fs::File::open(path)?;
                        let mut exprs = String::new();
                        file.read_to_string(&mut exprs)?;

                        let res = vm.parse_and_execute(exprs.as_str());

                        match res {
                            Ok(r) => r.iter().for_each(|x| match x.as_ref() {
                                SteelVal::Void => {}
                                _ => println!("{} {}", "=>".bright_blue().bold(), x),
                            }),
                            Err(e) => {
                                e.emit_result("repl.stl", exprs.as_str(), Span::new(0, 0));
                                eprintln!("{}", e.to_string().bright_red());
                            }
                        }
                    }
                    _ => {
                        // println!("Active Object Count: {:?}", steel::gc::OBJECT_COUNT);

                        let res = vm.parse_and_execute(&line);

                        match res {
                            Ok(r) => r.iter().for_each(|x| match x.as_ref() {
                                SteelVal::Void => {}
                                _ => println!("{} {}", "=>".bright_blue().bold(), x),
                            }),
                            Err(e) => {
                                e.emit_result("repl.stl", line.as_str(), Span::new(0, 0));
                                eprintln!("{}", e.to_string().bright_red());
                            }
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
