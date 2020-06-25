use crate::interpreter;
extern crate rustyline;
use crate::rvals::SteelVal;
// use crate::stdlib::PRELUDE;
use colored::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::Path;
// use std::time::Instant;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::validate::{
    MatchingBracketValidator, ValidationContext, ValidationResult, Validator,
};
use rustyline::{hint::Hinter, CompletionType, Context};
use rustyline_derive::Helper;

use rustyline::completion::Completer;
use rustyline::completion::Pair;

use std::borrow::Cow;

#[macro_export]
macro_rules! build_repl {
    ($($type:ty),*) => {
        {
            use crate::build_interpreter;
            let mut interpreter = build_interpreter!{
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

    // fn complete(
    //     &self,
    //     line: &str,
    //     cursor_pos: usize,
    //     context: &Context,
    // ) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
    //     Err()
    // }
    // fn update(&self, line: &mut LineBuffer, start: usize, elected: &str) {
    //     self.filename_completer.update(line, start, elected)
    // }
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

// Found on Hoth...
pub fn repl_base(mut interpreter: interpreter::SteelInterpreter) -> std::io::Result<()> {
    // let now = Instant::now();
    // if let Err(e) = interpreter.require(PRELUDE) {
    //     eprintln!("Error loading prelude: {}", e)
    // }
    // println!("Time to load prelude: {:?}", now.elapsed());
    // println!("{}", "Welcome to:".bright_blue().bold());
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

    // let highlighter = MatchingBracketHighlighter::new();

    let mut rl = Editor::<RustylineHelper>::new();
    rl.set_helper(Some(RustylineHelper {
        highlighter: MatchingBracketHighlighter::default(),
        validator: MatchingBracketValidator::default(),
    }));

    // let mut rl = Editor::<RustylineHelper>::new();
    // let mut rl = Editor::<MatchingBracketHighlighter>::new();
    loop {
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match line.as_str() {
                    ":quit" => return Ok(()),
                    ":reset" => interpreter.reset(),
                    ":env" => interpreter.print_bindings(),
                    ":?" => display_help(),
                    line if line.contains(":require") => {
                        let line = line.trim_start_matches(":require").trim();
                        let path = Path::new(line);
                        if let Err(e) = interpreter.require_path(path) {
                            eprintln!("Error loading {:?}: {}", path, e)
                        }
                    }
                    _ => {
                        // let now = Instant::now();
                        let res = interpreter.evaluate(&line);
                        // it prints '2'
                        // println!("{:?}", now.elapsed());
                        match res {
                            Ok(r) => r.iter().for_each(|x| match x {
                                SteelVal::Void => {}
                                _ => println!("{} {}", "=>".bright_blue().bold(), x),
                            }),
                            Err(e) => eprintln!("{}", e.to_string().bright_red()),
                        }
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
    repl_base(interpreter::SteelInterpreter::new())
}
