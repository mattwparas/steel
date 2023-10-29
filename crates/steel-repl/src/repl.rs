extern crate rustyline;
use colored::*;

use std::{cell::RefCell, rc::Rc, sync::mpsc::channel};

use rustyline::error::ReadlineError;

use rustyline::validate::MatchingBracketValidator;
use rustyline::Editor;

use std::path::{Path, PathBuf};
use steel::{rvals::SteelVal, steel_vm::register_fn::RegisterFn};

use steel::steel_vm::engine::Engine;

use std::io::Read;

use std::time::Instant;

use crate::highlight::RustylineHelper;

fn display_help() {
    println!(
        "
        :time       -- toggles the timing of expressions
        :? | :help  -- displays help dialog
        :quit       -- exits the REPL
        :pwd        -- displays the current working directory
        "
    );
}

fn finish_load_or_interrupt(vm: &mut Engine, exprs: String, path: PathBuf) {
    // let file_name = path.to_str().unwrap().to_string();

    let res = vm.compile_and_run_raw_program_with_path(exprs.as_str(), path);

    match res {
        Ok(r) => r.into_iter().for_each(|x| match x {
            SteelVal::Void => {}
            SteelVal::StringV(s) => {
                println!("{} {:?}", "=>".bright_blue().bold(), s);
            }
            _ => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_function_by_name_with_args("displayln", vec![x])
                    .unwrap();
            }
        }),
        Err(e) => {
            vm.raise_error(e);
        }
    }
}

fn finish_or_interrupt(vm: &mut Engine, line: String, print_time: bool) {
    let now = Instant::now();

    let res = vm.compile_and_run_raw_program(&line);

    match res {
        Ok(r) => r.into_iter().for_each(|x| match x {
            SteelVal::Void => {}
            SteelVal::StringV(s) => {
                println!("{} {:?}", "=>".bright_blue().bold(), s);
            }
            _ => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_function_by_name_with_args("displayln", vec![x])
                    .unwrap();
            }
        }),
        Err(e) => {
            vm.raise_error(e);
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
    / ___// /____  ___  / /          Version 0.3.0
    \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
   ___/ / /_/  __/  __/ /            :? for help
  /____/\__/\___/\___/_/ 
    "#
        .bright_yellow()
        .bold()
    );
    let mut prompt = format!("{}", "λ > ".bright_green().bold().italic());

    let mut rl = Editor::<RustylineHelper>::new().expect("Unable to instantiate the repl!");

    // let buffer = String::new();

    let current_dir = std::env::current_dir()?;

    let mut print_time = false;

    let (tx, rx) = channel();
    let tx = std::sync::Mutex::new(tx);

    let cancellation_function = move || {
        tx.lock().unwrap().send(()).unwrap();
    };

    vm.register_fn("quit", cancellation_function);

    let engine = Rc::new(RefCell::new(vm));
    rl.set_helper(Some(RustylineHelper::new(
        // highlighter: MatchingBracketHighlighter::default(),
        MatchingBracketValidator::default(),
        engine.clone(),
    )));

    // ctrlc::set_handler(move || tx.send(()).expect("Could not send signal on channel."))
    // .expect("Error setting Ctrl-C handler");

    // vm.on_progress(move |x| {
    //     if x % 1000 == 0 {
    //         match rx.try_recv() {
    //             Ok(_) => return false,
    //             _ => {}
    //         }
    //     }
    //     true
    // });

    while rx.try_recv().is_err() {
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
                    ":pwd" => println!("{current_dir:#?}"),
                    // ":env" => vm.print_bindings(),
                    ":?" | ":help" => display_help(),
                    line if line.contains(":load") => {
                        let line = line.trim_start_matches(":load").trim();

                        // Update the prompt to now include the new context
                        prompt = format!(
                            "{}",
                            format!("λ ({line}) > ").bright_green().bold().italic(),
                        );

                        let path = Path::new(line);

                        let file = std::fs::File::open(path);

                        if let Err(e) = file {
                            eprintln!("{e}");
                            continue;
                        }

                        let mut file = file?;

                        let mut exprs = String::new();
                        file.read_to_string(&mut exprs)?;

                        finish_load_or_interrupt(
                            &mut engine.borrow_mut(),
                            exprs,
                            path.to_path_buf(),
                        );
                    }
                    _ => {
                        // TODO also include this for loading files
                        finish_or_interrupt(&mut engine.borrow_mut(), line, print_time);
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
                println!("Error: {err:?}");
                break;
            }
        }
    }

    Ok(())
}
