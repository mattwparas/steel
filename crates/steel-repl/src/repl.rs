extern crate rustyline;
use colored::*;
use steel::compiler::modules::steel_home;

use std::{cell::RefCell, rc::Rc, sync::mpsc::channel};

use rustyline::error::ReadlineError;

use rustyline::{config::Configurer, Editor};

use std::path::{Path, PathBuf};
use steel::{rvals::SteelVal, steel_vm::register_fn::RegisterFn};

use steel::steel_vm::engine::Engine;

use std::io::Read;

use std::time::Instant;

use std::env;

use std::fs::File;

use dirs;

use crate::highlight::RustylineHelper;

fn display_help() {
    println!(
        "
        :time       -- toggles the timing of expressions
        :? | :help  -- displays help dialog
        :quit       -- exits the REPL
        :pwd        -- displays the current working directory
        :load       -- loads a file
        "
    );
}

fn display_startup() {
    println!(
        "{}",
        format!(
            r#"
     _____ __            __
    / ___// /____  ___  / /          Version {} 
    \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
   ___/ / /_/  __/  __/ /            :? for help
  /____/\__/\___/\___/_/
    "#,
            env!("CARGO_PKG_VERSION")
        )
        .bright_yellow()
        .bold()
    );
}

fn get_repl_history_path() -> String {
    let steel_home = steel_home();
    let path = match steel_home {
        Some(val) => {
            let mut parsed_path = PathBuf::from(&val);
            parsed_path = parsed_path.canonicalize().unwrap_or(parsed_path);
            parsed_path.push("history");
            parsed_path.to_string_lossy().into_owned()
        }
        None => {
            let mut default_path = dirs::home_dir().unwrap_or_default();
            default_path.push(".steel/history");
            default_path.to_string_lossy().into_owned()
        }
    };

    path
}

fn finish_load_or_interrupt(vm: &mut Engine, exprs: String, path: PathBuf) {
    // let file_name = path.to_str().unwrap().to_string();

    let res = vm.compile_and_run_raw_program_with_path(exprs, path);

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

fn finish_or_interrupt(vm: &mut Engine, line: String) {
    let values = match vm.compile_and_run_raw_program(line) {
        Ok(values) => values,
        Err(error) => {
            vm.raise_error(error);

            return;
        }
    };

    let len = values.len();

    for (i, value) in values.into_iter().enumerate() {
        let last = i == len - 1;

        if last {
            vm.register_value("$1", value.clone());
        }

        match value {
            SteelVal::Void => {}
            SteelVal::StringV(s) => {
                println!("{} {:?}", "=>".bright_blue().bold(), s);
            }
            _ => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_function_by_name_with_args("displayln", vec![value])
                    .unwrap();
            }
        }
    }
}

/// Entire point for the repl
/// Automatically adds the prelude and contracts for the core library
pub fn repl_base(mut vm: Engine) -> std::io::Result<()> {
    display_startup();

    #[cfg(target_os = "windows")]
    let mut prompt = String::from("λ > ");

    #[cfg(not(target_os = "windows"))]
    let mut prompt = format!("{}", "λ > ".bright_green().bold().italic());

    let mut rl = Editor::<RustylineHelper>::new().expect("Unable to instantiate the repl!");
    rl.set_check_cursor_position(true);

    // Load repl history
    let history_path = get_repl_history_path();
    if let Err(_) = rl.load_history(&history_path) {
        if let Err(_) = File::create(&history_path) {
            eprintln!("Unable to create repl history file {:?}", history_path)
        }
    };

    let current_dir = std::env::current_dir()?;

    let mut print_time = false;

    let (tx, rx) = channel();
    let tx = std::sync::Mutex::new(tx);

    let cancellation_function = move || {
        tx.lock().unwrap().send(()).unwrap();
    };

    vm.register_fn("quit", cancellation_function);
    let safepoint = vm.get_thread_state_controller();

    let engine = Rc::new(RefCell::new(vm));
    rl.set_helper(Some(RustylineHelper::new(engine.clone())));

    {
        let safepoint = safepoint.clone();

        ctrlc::set_handler(move || {
            safepoint.interrupt();
        })
        .unwrap();
    }

    let clear_interrupted = move || {
        safepoint.resume();
    };

    while rx.try_recv().is_err() {
        let readline = rl.readline(&prompt);

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match line.as_str() {
                    ":quit" => return Ok(()),
                    ":time" => {
                        print_time = !print_time;
                        println!(
                            "{} {}",
                            "Expression timer set to:".bright_purple(),
                            print_time.to_string().bright_green()
                        );
                    }
                    ":pwd" => println!("{current_dir:#?}"),
                    ":?" | ":help" => display_help(),
                    line if line.contains(":load") => {
                        let line = line.trim_start_matches(":load").trim();
                        if line.is_empty() {
                            eprintln!("No file provided");
                            continue;
                        }

                        let path = Path::new(line);

                        let file = std::fs::File::open(path);

                        if let Err(e) = file {
                            eprintln!("{e}");
                            continue;
                        }

                        // Update the prompt to now include the new context
                        prompt = format!(
                            "{}",
                            format!("λ ({line}) > ").bright_green().bold().italic(),
                        );

                        let mut file = file?;

                        let mut exprs = String::new();
                        file.read_to_string(&mut exprs)?;

                        clear_interrupted();

                        finish_load_or_interrupt(
                            &mut engine.borrow_mut(),
                            exprs,
                            path.to_path_buf(),
                        );
                    }
                    _ => {
                        // TODO also include this for loading files
                        let now = Instant::now();

                        clear_interrupted();

                        finish_or_interrupt(&mut engine.borrow_mut(), line);

                        if print_time {
                            println!("Time taken: {:?}", now.elapsed());
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                continue;
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
    if let Err(err) = rl.save_history(&history_path) {
        eprintln!("Failed to save REPL history: {}", err);
    }

    Ok(())
}
