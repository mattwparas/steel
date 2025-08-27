extern crate rustyline;
use colored::{ColoredString, Colorize};
use rustyline::history::FileHistory;
use rustyline::{
    Cmd, ConditionalEventHandler, Event, EventContext, EventHandler, KeyEvent, RepeatCount,
};
use steel::compiler::modules::steel_home;
use steel::rvals::{Custom, SteelString};

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};

use rustyline::error::ReadlineError;

use rustyline::{config::Configurer, Editor};

use std::path::{Path, PathBuf};
use steel::{rvals::SteelVal, steel_vm::register_fn::RegisterFn};

use steel::steel_vm::engine::Engine;

use std::io::Read;

use std::time::Instant;

use std::env;

use std::fs::File;

use crate::highlight::RustylineHelper;

fn display_help() {
    println!(
        "
        :time       -- toggles the timing of expressions
        :? | :help  -- displays help dialog
        :q | :quit  -- exits the REPL
        :pwd        -- displays the current working directory
        :load       -- loads a file
        "
    );
}

fn get_default_startup() -> ColoredString {
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
}

fn get_default_repl_history_path() -> PathBuf {
    if let Some(val) = steel_home() {
        let mut parsed_path = PathBuf::from(&val);
        parsed_path = parsed_path.canonicalize().unwrap_or(parsed_path);
        parsed_path.push("history");
        parsed_path
    } else {
        let mut default_path = env_home::env_home_dir().unwrap_or_default();
        default_path.push(".steel/history");
        default_path.to_string_lossy().into_owned();
        default_path
    }
}

fn finish_load_or_interrupt(vm: &mut Engine, exprs: String, path: PathBuf) {
    // let file_name = path.to_str().unwrap().to_string();

    let res = vm.compile_and_run_raw_program_with_path(exprs, path);

    match res {
        Ok(r) => r.into_iter().for_each(|x| match x {
            SteelVal::Void => {}
            _ => println!("{} {}", "=>".bright_blue().bold(), x),
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
            _ => println!("{} {}", "=>".bright_blue().bold(), value),
        }
    }
}

#[derive(Debug)]
struct RustyLine(Editor<RustylineHelper, FileHistory>);
impl Custom for RustyLine {}

#[derive(Debug)]
#[allow(unused)]
struct RustyLineError(rustyline::error::ReadlineError);

impl Custom for RustyLineError {}

impl std::fmt::Display for RustyLineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for RustyLineError {}

pub fn readline_module(vm: &mut Engine) {
    let mut module = steel::steel_vm::builtin::BuiltInModule::new("#%private/steel/readline");

    module
        .register_fn("#%repl-display-startup", || {
            println!("{}", get_default_startup())
        })
        .register_fn(
            "#%repl-add-history-entry",
            |rl: &mut RustyLine, entry: SteelString| rl.0.add_history_entry(entry.as_str()).ok(),
        )
        .register_fn("#%create-repl", || {
            let mut rl = Editor::<RustylineHelper, rustyline::history::DefaultHistory>::new()
                .expect("Unable to instantiate the repl!");
            rl.set_check_cursor_position(true);

            let history_path = get_default_repl_history_path();
            if let Err(_) = rl.load_history(&history_path) {
                if let Err(_) = File::create(&history_path) {
                    eprintln!("Unable to create repl history file {:?}", history_path)
                }
            };
            RustyLine(rl)
        })
        .register_fn("#%read-line", |rl: &mut RustyLine| {
            let prompt = format!("{}", "λ > ".bright_green().bold().italic());
            rl.0.readline(&prompt).map_err(RustyLineError)
        });

    vm.register_module(module);
}

struct CtrlCHandler {
    close_on_interrupt: Arc<AtomicBool>,
    empty_line_cancelled: AtomicBool,
}

impl CtrlCHandler {
    fn new(close_on_interrupt: Arc<AtomicBool>) -> Self {
        CtrlCHandler {
            close_on_interrupt,
            empty_line_cancelled: AtomicBool::new(false),
        }
    }
}

impl ConditionalEventHandler for CtrlCHandler {
    fn handle(&self, _: &Event, _: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
        if !ctx.line().is_empty() {
            // if the line is not empty, reset the PREVIOUS_LINE_CANCELLED state
            self.empty_line_cancelled.store(false, Ordering::Release);
        } else if self.empty_line_cancelled.swap(true, Ordering::Release) {
            self.close_on_interrupt.store(true, Ordering::Release);
        }

        Some(Cmd::Interrupt)
    }
}

pub struct Repl<S: Display, P: AsRef<Path> + Debug> {
    vm: Engine,
    startup_text: Option<S>,
    history_path: Option<P>,
}

impl Repl<String, PathBuf> {
    pub fn new(vm: Engine) -> Self {
        Repl {
            vm,
            startup_text: None,
            history_path: None,
        }
    }
}

impl<S: Display, P: AsRef<Path> + Debug> Repl<S, P> {
    pub fn with_startup<NS: Display>(self, startup_text: NS) -> Repl<NS, P> {
        Repl {
            vm: self.vm,
            history_path: self.history_path,
            startup_text: Some(startup_text),
        }
    }

    pub fn with_history_path<NP: AsRef<Path> + Debug>(self, history_path: NP) -> Repl<S, NP> {
        Repl {
            vm: self.vm,
            history_path: Some(history_path),
            startup_text: self.startup_text,
        }
    }

    pub fn run(mut self) -> std::io::Result<()> {
        if let Some(startup) = self.startup_text {
            println!("{}", startup);
        } else {
            println!("{}", get_default_startup());
        }

        #[cfg(target_os = "windows")]
        let mut prompt = String::from("λ > ");

        #[cfg(not(target_os = "windows"))]
        let mut prompt = format!("{}", "λ > ".bright_green().bold().italic());

        let mut rl = Editor::<RustylineHelper, rustyline::history::DefaultHistory>::new()
            .expect("Unable to instantiate the repl!");
        rl.set_check_cursor_position(true);

        // Load repl history
        let history_path: Cow<Path> = self
            .history_path
            .as_ref()
            .map(|p| Cow::Borrowed(p.as_ref()))
            .unwrap_or_else(|| Cow::Owned(get_default_repl_history_path()));

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

        self.vm.register_fn("quit", cancellation_function);
        let safepoint = self.vm.get_thread_state_controller();

        let globals = Arc::new(Mutex::new(self.vm.globals().iter().copied().collect()));

        rl.set_helper(Some(RustylineHelper::new(globals.clone())));

        let safepoint = safepoint.clone();
        let ctrlc_safepoint = safepoint.clone();

        ctrlc::set_handler(move || {
            ctrlc_safepoint.clone().interrupt();
        })
        .unwrap();

        let clear_interrupted = move || {
            safepoint.resume();
        };

        let close_on_interrupt = Arc::new(AtomicBool::new(false));
        let ctrlc = Box::new(CtrlCHandler::new(close_on_interrupt.clone()));
        rl.bind_sequence(KeyEvent::ctrl('c'), EventHandler::Conditional(ctrlc));

        while rx.try_recv().is_err() {
            // Update globals for highlighting
            // TODO: Come up with some kind of subscription API?
            let known_globals_length = globals.lock().unwrap().len();
            let updated_globals_length = self.vm.globals().len();
            if updated_globals_length > known_globals_length {
                let mut guard = globals.lock().unwrap();
                if let Some(range) = self.vm.globals().get(known_globals_length..) {
                    for var in range {
                        guard.insert(*var);
                    }
                }
            }

            let readline = self.vm.enter_safepoint(|| rl.readline(&prompt));

            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str()).ok();
                    match line.as_str().trim() {
                        ":q" | ":quit" => return Ok(()),
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

                            finish_load_or_interrupt(&mut self.vm, exprs, path.to_path_buf());
                        }
                        _ => {
                            // TODO also include this for loading files
                            let now = Instant::now();

                            clear_interrupted();

                            finish_or_interrupt(&mut self.vm, line);

                            if print_time {
                                println!("Time taken: {:?}", now.elapsed());
                            }
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    if close_on_interrupt.load(Ordering::Acquire) {
                        break;
                    } else {
                        println!("CTRL-C");
                        continue;
                    }
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
}

/// Entry point for the repl
/// Automatically adds the prelude and contracts for the core library
pub fn repl_base(vm: Engine) -> std::io::Result<()> {
    let repl = Repl::new(vm);

    repl.run()
}
