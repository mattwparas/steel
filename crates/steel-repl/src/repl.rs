use colored::{ColoredString, Colorize};
use std::time::Duration;
use steel::compiler::modules::steel_home;
use steel::rerrs::{ErrorKind, SteelErr};
use steel::rvals::{Custom, SteelString};
use steel::steel_vm::builtin::BuiltInModule;

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};

use std::path::{Path, PathBuf};
use steel::{rvals::SteelVal, steel_vm::register_fn::RegisterFn};

use steel::steel_vm::engine::Engine;

use std::io::{Read, Write};

use std::time::Instant;

use std::env;

use crate::editor::{self, Input, PlainEditor};
use crate::steel_editor::SteelEditor;

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
            SteelVal::StringV(s) => {
                println!("{} {:?}", "=>".bright_blue().bold(), s);
            }
            _ => {
                print!("{} ", "=>".bright_blue().bold());
                vm.call_function_by_name_with_args("println", vec![x])
                    .unwrap();
            }
        }),
        Err(e) => {
            vm.raise_error(e);
        }
    }

    let _ = std::io::stdout().flush();
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
                if let Err(e) = vm.call_function_by_name_with_args("println", vec![value]) {
                    vm.raise_error(e);
                }
            }
        }
    }

    let _ = std::io::stdout().flush();
}

/// What `#%create-repl` hands out: plain lines. The Steel line editor is
/// `steel/line-editor`, used directly from Steel.
struct LineReader(PlainEditor);
impl Custom for LineReader {}

#[derive(Debug)]
enum ReadLineError {
    Interrupted,
    Eof,
    Io(std::io::Error),
}

impl Custom for ReadLineError {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

impl std::fmt::Display for ReadLineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadLineError::Interrupted => write!(f, "Interrupted"),
            ReadLineError::Eof => write!(f, "Eof"),
            ReadLineError::Io(err) => write!(f, "Io({err})"),
        }
    }
}

impl Error for ReadLineError {}

fn io_error(err: std::io::Error) -> SteelErr {
    SteelErr::new(ErrorKind::Io, err.to_string())
}

fn tokenize(text: &str) -> Vec<SteelVal> {
    use steel_parser::lexer::TokenStream;
    use steel_parser::parser::SourceId;
    use steel_parser::tokens::TokenType;

    // Spans are in bytes, Steel strings index by char
    let mut chars_before = vec![0usize; text.len() + 1];
    let mut count = 0;
    for (byte, c) in text.char_indices() {
        for slot in &mut chars_before[byte..byte + c.len_utf8()] {
            *slot = count;
        }
        count += 1;
    }
    chars_before[text.len()] = count;

    TokenStream::new(text, true, SourceId::none())
        .filter_map(Result::ok)
        .map(|token| {
            let (kind, span) = match &token.ty {
                TokenType::OpenParen(_, paren_mod) => {
                    ("open", TokenType::open_span(token.span, *paren_mod))
                }
                TokenType::CloseParen(_) => ("close", token.span),
                TokenType::If
                | TokenType::Define
                | TokenType::Let
                | TokenType::Lambda
                | TokenType::Require
                | TokenType::Begin
                | TokenType::Set
                | TokenType::Quote
                | TokenType::DefineSyntax
                | TokenType::SyntaxRules => ("form", token.span),
                TokenType::Identifier(_) => ("identifier", token.span),
                TokenType::Keyword(_) => ("keyword", token.span),
                TokenType::StringLiteral(_) => ("string", token.span),
                TokenType::Number(_) => ("number", token.span),
                TokenType::CharacterLiteral(_) => ("char", token.span),
                TokenType::BooleanLiteral(_) => ("boolean", token.span),
                _ => ("other", token.span),
            };
            let start = chars_before[(span.start as usize).min(text.len())];
            let end = chars_before[(span.end as usize).min(text.len())];
            SteelVal::ListV(
                vec![
                    SteelVal::SymbolV(kind.into()),
                    SteelVal::IntV(start as isize),
                    SteelVal::IntV(end as isize),
                ]
                .into(),
            )
        })
        .collect()
}

/// `#%private/steel/terminal`, the primitives the Steel line editor is built on
pub(crate) fn terminal_module(vm: &mut Engine) {
    use crate::terminal::{self, Byte};

    let mut module = BuiltInModule::new("#%private/steel/terminal");

    module
        .register_fn("terminal?", terminal::is_terminal)
        .register_fn("enable-raw-mode!", || {
            terminal::enable_raw_mode().map_err(io_error)
        })
        .register_fn("disable-raw-mode!", || {
            terminal::disable_raw_mode().map_err(io_error)
        })
        .register_fn("raw-mode?", terminal::is_raw_mode)
        // A byte, `#f` once `timeout-ms` passes, or eof. `#f` for the timeout
        // waits forever.
        .register_fn(
            "read-byte/timeout",
            |timeout_ms: Option<usize>| -> Result<SteelVal, SteelErr> {
                let timeout = timeout_ms.map(|ms| Duration::from_millis(ms as u64));
                match terminal::read_byte(timeout).map_err(io_error)? {
                    Byte::Byte(byte) => Ok(SteelVal::IntV(byte as isize)),
                    Byte::Timeout => Ok(SteelVal::BoolV(false)),
                    Byte::Eof => Ok(steel::primitives::ports::eof()),
                }
            },
        )
        // `(columns rows)`, or `#f` when the size can't be found out
        .register_fn("terminal-size", || {
            terminal::size().map(|(columns, rows)| vec![columns as isize, rows as isize])
        })
        .register_fn("string-display-width", |text: SteelString| {
            terminal::display_width(text.as_str())
        })
        // Whether `text` is a whole form, or the user is still typing one
        .register_fn("input-complete?", |text: SteelString| {
            editor::input_complete(text.as_str())
        })
        // `(kind start end)` per token, in chars. kind is open, close, form
        // (define, lambda and the like), identifier, keyword, string, number,
        // char, boolean or other. Comments are skipped, and so is anything
        // the lexer rejects.
        .register_fn("tokenize", |text: SteelString| tokenize(text.as_str()))
        .register_fn("string-graphemes", |text: SteelString| {
            terminal::graphemes(text.as_str())
                .map(|grapheme| grapheme.to_owned())
                .collect::<Vec<_>>()
        });

    vm.register_module(module);
}

pub fn readline_module(vm: &mut Engine) {
    let mut module = steel::steel_vm::builtin::BuiltInModule::new("#%private/steel/readline");

    module
        .register_fn("#%repl-display-startup", || {
            println!("{}", get_default_startup())
        })
        .register_fn(
            "#%repl-add-history-entry",
            // Plain lines keep no history
            |_: &mut LineReader, _: SteelString| {},
        )
        // Where the REPL keeps its history, for line editors of other REPLs
        .register_fn("#%repl-history-path", || {
            get_default_repl_history_path()
                .to_string_lossy()
                .into_owned()
        })
        .register_fn("#%create-repl", || {
            LineReader(PlainEditor)
        })
        .register_fn("#%read-line", |rl: &mut LineReader| {
            let prompt = format!("{}", "λ > ".bright_green().bold().italic());
            match rl.0.read_line(&prompt) {
                Ok(Input::Line(line)) => Ok(line),
                Ok(Input::Interrupted) => Err(ReadLineError::Interrupted),
                Ok(Input::Eof) => Err(ReadLineError::Eof),
                Err(err) => Err(ReadLineError::Io(err)),
            }
        });

    vm.register_module(module);
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

        let history_path: Cow<Path> = self
            .history_path
            .as_ref()
            .map(|p| Cow::Borrowed(p.as_ref()))
            .unwrap_or_else(|| Cow::Owned(get_default_repl_history_path()));

        let current_dir = std::env::current_dir()?;

        let mut print_time = false;

        let (tx, rx) = channel();
        let tx = std::sync::Mutex::new(tx);

        let cancellation_function = move || {
            tx.lock().unwrap().send(()).unwrap();
        };

        let mut module = BuiltInModule::new("#%private/steel/repl");
        module.register_fn("quit", cancellation_function);

        for (key, value) in module.inner_map().iter() {
            self.vm.register_value(key, value.clone());
        }

        self.vm.register_module(module);

        let safepoint = self.vm.get_thread_state_controller();

        let globals = Arc::new(Mutex::new(self.vm.globals().iter().copied().collect()));

        // Ctrl-C while code runs interrupts it. Where there's no such event
        // (Hermit) it just doesn't.
        let interrupt_safepoint = safepoint.clone();
        let _ = crate::terminal::install_interrupt_handler(move || {
            interrupt_safepoint.clone().interrupt();
        });

        let close_on_interrupt = Arc::new(AtomicBool::new(false));

        // The Steel line editor on a terminal, plain lines otherwise or when
        // asked for (STEEL_REPL_PLAIN=1)
        let mut steel_editor = None;
        if crate::terminal::is_terminal() && !editor::plain_requested() {
            match SteelEditor::new(
                &mut self.vm,
                &history_path,
                globals.clone(),
                safepoint.clone(),
                close_on_interrupt.clone(),
            ) {
                Ok(editor) => steel_editor = Some(editor),
                Err(err) => {
                    eprintln!("The Steel line editor didn't start, reading plain lines instead:");
                    self.vm.raise_error(err);
                }
            }
        }

        let clear_interrupted = move || {
            safepoint.resume();
        };

        // The Steel editor keeps the history file up to date itself
        let mut rl = PlainEditor;

        while rx.try_recv().is_err() {
            // Update globals for completion and highlighting
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

            let readline = match steel_editor
                .as_mut()
                .map(|editor| editor.read_line(&mut self.vm, &prompt))
            {
                Some(Ok(input)) => Ok(input),
                Some(Err(err)) => {
                    // The editor may have stopped mid-line
                    eprintln!("\r\nThe Steel line editor failed, reading plain lines from now on:");
                    self.vm.raise_error(err);
                    steel_editor = None;
                    self.vm.enter_safepoint(|| rl.read_line(&prompt))
                }
                None => self.vm.enter_safepoint(|| rl.read_line(&prompt)),
            };

            match readline {
                Ok(Input::Line(line)) => {
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
                Ok(Input::Interrupted) => {
                    if close_on_interrupt.load(Ordering::Acquire) {
                        break;
                    } else {
                        println!("CTRL-C");
                        continue;
                    }
                }
                Ok(Input::Eof) => {
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
}

/// Entry point for the repl
/// Automatically adds the prelude and contracts for the core library
pub fn repl_base(vm: Engine) -> std::io::Result<()> {
    let repl = Repl::new(vm);

    repl.run()
}
