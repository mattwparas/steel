use steel_derive::function;

use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::rvals::{AsRefMutSteelVal, AsRefSteelVal, FromSteelVal, IntoSteelVal, SteelString};
use crate::values::lists::List;
use crate::values::port::{Peekable, SteelPort, SteelPortRepr};
use crate::values::structs::SteelResult;
use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};
use crate::{stop, SteelVal};
use std::io::{BufReader, BufWriter};
use std::process::{Child, Command, Stdio};

pub fn process_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/process".to_string());

    module
        .register_native_fn_definition(COMMAND_DEFINITION)
        .register_native_fn_definition(CURRENT_DIR_DEFINITION)
        .register_native_fn_definition(STDOUT_PIPED_DEFINITION)
        .register_native_fn_definition(STDOUT_PIPED_OTHER_DEFINITION)
        .register_native_fn_definition(SPAWN_PROCESS_DEFINITION)
        .register_native_fn_definition(WAIT_DEFINITION)
        .register_fn("wait->stdout", ChildProcess::wait_with_stdout)
        .register_fn("which", binary_exists_on_path)
        .register_native_fn_definition(CHILD_STDERR_DEFINITION)
        .register_native_fn_definition(CHILD_STDOUT_DEFINITION)
        .register_native_fn_definition(CHILD_STDIN_DEFINITION)
        .register_native_fn_definition(SET_ENV_VAR_DEFINITION)
        .register_native_fn_definition(CHILD_KILL_DEFINITION)
        .register_native_fn_definition(WITH_STDERR_DEFINITION)
        .register_native_fn_definition(WITH_STDIN_DEFINITION)
        .register_native_fn_definition(WITH_STDOUT_DEFINITION)
        .register_native_fn_definition(WITH_STDERR_PIPED_DEFINITION)
        .register_native_fn_definition(WITH_STDOUT_PIPED_DEFINITION)
        .register_native_fn_definition(WITH_STDIN_PIPED_DEFINITION)
        .register_native_fn_definition(REMOVE_ENV_VAR_DEFINITION)
        .register_native_fn_definition(CLEAR_ENV_VAR_DEFINITION)
        .register_native_fn_definition(IS_SUBPROCESS_DEFINITION)
        .register_native_fn_definition(IS_COMMAND_BUILDER_DEFINITION);

    module
}

#[function(name = "subprocess?", alias = "ChildProcess?")]
pub fn is_subprocess(value: &SteelVal) -> Result<SteelVal, SteelErr> {
    Ok(SteelVal::BoolV(ChildProcess::as_ref(value).is_ok()))
}

#[function(name = "command-builder?", alias = "CommandBuilder?")]
pub fn is_command_builder(value: &SteelVal) -> Result<SteelVal, SteelErr> {
    Ok(SteelVal::BoolV(CommandBuilder::as_ref(value).is_ok()))
}

/// Create a `CommandBuilder` from a command and a list of arguments. Used to spawn
/// a subprocess.
///
/// (command cmd args) -> CommandBuilder?
///
/// * cmd : string?
/// * args : (listof string?)
///
/// ```scheme
/// > (spawn-process (command "echo" (list "hello" "world")))
/// ```
#[function(name = "command")]
pub fn command(name: SteelString, args: &List<SteelVal>) -> Result<SteelVal, SteelErr> {
    let mut command = Command::new(name.as_str());

    for arg in args {
        let arg = SteelString::from_steelval(arg)?;
        command.arg(arg.as_str());
    }

    CommandBuilder { command }.into_steelval()
}

/// Sets the current directory for the child. `set-current-dir!` is an alias.
///
/// (with-current-dir process dir) -> CommandBuilder?
///
/// * process - CommandBuilder?
/// * dir - string?
///
/// ```scheme
/// > (define pb (command "echo" (list "hello")))
/// > (with-current-dir pb "/home/foo")
/// > (~> (command "echo" (list "hello"))
///       (with-current-dir "/home/foo")
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-current-dir", alias = "set-current-dir!")]
pub fn current_dir(builder: SteelVal, directory: String) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.current_dir(directory);
    drop(guard);
    Ok(builder)
}

/// Sets an environment variable for the child. `set-env-var!` is an alias.
///
/// (with-env-var process key value) -> CommandBuilder?
///
/// * process - CommandBuilder?
/// * key - string?
/// * value - string?
///
/// ```scheme
/// > (define pb (command "echo" (list "hello")))
/// > (~> (command "echo" (list "hello"))
///       (with-env-var "FOO" "BAR")
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-env-var", alias = "set-env-var!")]
pub fn set_env_var(builder: SteelVal, key: String, value: String) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.env_var(key, value);
    drop(guard);
    Ok(builder)
}

/// Removes an environment variable for the child.
///
/// (with-env-var process key) -> CommandBuilder?
///
/// * process - CommandBuilder?
/// * key - string?
///
/// ```scheme
/// > (define pb (command "echo" (list "hello")))
/// > (~> (command "echo" (list "hello"))
///       (without-env-var "FOO")
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "without-env-var")]
pub fn remove_env_var(builder: SteelVal, key: String) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.command.env_remove(key);
    drop(guard);
    Ok(builder)
}

/// Removes all environment variables for the child.
///
/// (with-cleared-env-vars process) -> CommandBuilder?
///
/// * process - CommandBuilder?
///
/// ```scheme
/// > (define pb (command "echo" (list "hello")))
/// > (~> (command "echo" (list "hello"))
///       (with-cleared-env-vars "FOO")
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-cleared-env-vars")]
pub fn clear_env_var(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.command.env_clear();
    drop(guard);
    Ok(builder)
}

// TODO: @matt - add ways to override stdout, stderr, stdin, with ports
#[function(name = "set-stdout-piped!")]
pub fn stdout_piped(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.stdout_piped();
    drop(guard);
    Ok(builder)
}

#[function(name = "set-piped-stdout!")]
pub fn stdout_piped_other(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.stdout_piped();
    drop(guard);
    Ok(builder)
}

/// Constructs a pipe to be arranged to connect to stdout.
///
/// (with-stdout-piped process) -> CommandBuilder?
///
/// * process : CommandBuilder?
///
/// ```scheme
/// > (~> (command "echo" (list "hello"))
///       with-stdout-piped
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stdout-piped")]
pub fn with_stdout_piped(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.command.stdout(Stdio::piped());
    drop(guard);
    Ok(builder)
}

/// Constructs a pipe to be arranged to connect to stderr.
///
/// (with-stderr-piped process) -> CommandBuilder?
///
/// * process : CommandBuilder?
///
/// ```scheme
/// > (~> (command "echo" (list "hello"))
///       with-stderr-piped
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stderr-piped")]
pub fn with_stderr_piped(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.command.stderr(Stdio::piped());
    drop(guard);
    Ok(builder)
}

/// Constructs a pipe to be arranged to connect to stdin.
///
/// (with-stdin-piped process) -> CommandBuilder?
///
/// * process : CommandBuilder?
///
/// ```scheme
/// > (~> (command "echo" (list "hello"))
///       with-stdin-piped
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stdin-piped")]
pub fn with_stdin_piped(builder: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut guard = CommandBuilder::as_mut_ref(&builder)?;
    guard.command.stdin(Stdio::piped());
    drop(guard);
    Ok(builder)
}

/// Redirect stdout from the process to the given port
///
/// (with-stdout process port) -> CommandBuilder?
///
/// * process : CommandBuilder?
/// * port : (and output-port? file-port?)
///
/// ```scheme
/// > (define output (open-output-file "test.txt"))
/// > (~> (command "echo" (list "hello"))
///       (with-stdout output)
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stdout")]
pub fn with_stdout(builder: SteelVal, port: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut command_guard = CommandBuilder::as_mut_ref(&builder)?;

    if let SteelVal::PortV(port) = port {
        if port.is_output() {
            let mut guard = port.port.write();
            let inner = guard.take();
            let as_stdio = inner.as_stdio();

            match as_stdio {
                Ok(value) => {
                    command_guard.command.stdout(value);
                }

                Err(this) => {
                    // Bail out, this isn't something that
                    // can be done
                    *port.port.write() = this;
                    stop!(TypeMismatch => format!("with-stdout expected an output file port in the second position, found: {}", port))
                }
            }
        }
    } else {
        stop!(TypeMismatch => format!("with-stdout expected a port in the second position, found: {}", port))
    }

    drop(command_guard);
    Ok(builder)
}

/// Redirect stderr from the process to the given port
///
/// (with-stderr process port) -> CommandBuilder?
///
/// * process : CommandBuilder?
/// * port : (and output-port? file-port?)
///
/// ```scheme
/// > (define output (open-output-file "test.txt"))
/// > (~> (command "echo" (list "hello"))
///       (with-stderr output)
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stderr")]
pub fn with_stderr(builder: SteelVal, port: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut command_guard = CommandBuilder::as_mut_ref(&builder)?;

    if let SteelVal::PortV(port) = port {
        if port.is_output() {
            let mut guard = port.port.write();
            let inner = guard.take();
            let as_stdio = inner.as_stdio();

            match as_stdio {
                Ok(value) => {
                    command_guard.command.stderr(value);
                }

                Err(this) => {
                    // Bail out, this isn't something that
                    // can be done
                    *port.port.write() = this;
                    stop!(TypeMismatch => format!("with-stderr expected an output port in the second position, found: {}", port))
                }
            }
        }
    } else {
        stop!(TypeMismatch => format!("with-stderr expected a port in the second position, found: {}", port))
    }

    drop(command_guard);
    Ok(builder)
}

/// Redirect stdin from the process to the given port
///
/// (with-stdin process port) -> CommandBuilder?
///
/// * process : CommandBuilder?
/// * port : (and input-port? file-port?)
///
/// ```scheme
/// > (define output (open-input-file "test.txt"))
/// > (~> (command "echo" (list "hello"))
///       (with-stdin output)
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "with-stdin")]
pub fn with_stdin(builder: SteelVal, port: SteelVal) -> Result<SteelVal, SteelErr> {
    let mut command_guard = CommandBuilder::as_mut_ref(&builder)?;

    if let SteelVal::PortV(port) = port {
        if port.is_input() {
            let mut guard = port.port.write();
            let inner = guard.take();
            let as_stdio = inner.as_stdio();

            match as_stdio {
                Ok(value) => {
                    command_guard.command.stdin(value);
                }

                Err(this) => {
                    // Bail out, this isn't something that
                    // can be done
                    *port.port.write() = this;
                    stop!(TypeMismatch => format!("with-stderr expected an output port in the second position, found: {}", port))
                }
            }
        }
    } else {
        stop!(TypeMismatch => format!("with-stderr expected a port in the second position, found: {}", port))
    }

    drop(command_guard);
    Ok(builder)
}

/// Spawn the given process. Returns a result indicating whether the process was
/// able to be spawned.
///
/// (spawn-process process) -> (Result? ChildProcess?)
///
/// * process : CommandBuilder?
///
/// ```scheme
/// > (require "steel/result")
/// > (define spawned (spawn-process (command "/bin/ls" '()))) ;; => (Ok #<steel::primitives::process::ChildProcess>)
/// > (define child (unwrap-ok spawned))
/// ```
#[function(name = "spawn-process")]
pub fn spawn_process(builder: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut command_guard = CommandBuilder::as_mut_ref(&builder)?;
    command_guard.spawn_process().into_steelval()
}

/// Wait for the subprocess to finish. Returns a result with the status code
/// of the awaited subprocess.
///
/// (wait process) -> (Result? int?)
///
/// * process : ChildProcess?
///
/// ```scheme
/// > (~> (command "echo" (list "hello"))
///       spawn-process
///       unwrap-ok
///       wait)
/// ```
#[function(name = "wait", alias = "process-wait")]
pub fn wait(builder: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut command_guard = ChildProcess::as_mut_ref(&builder)?;
    command_guard.wait().into_steelval()
}

/// Get a handle to the stdout handle of the child process. The process
/// must have been started with the `with-stdout-piped` option for this
/// to be available, otherwise stdout will be inherited. This will return
/// false if the handle has already been consumed.
///
/// (child-stdout subprocess) -> (or output-port? #false)
///
/// subprocess : ChildProcess?
///
/// ```scheme
/// (define handle (~> (command "/bin/ls" '())
///                    with-stdout-piped
///                    spawn-process
///                    unwrap-ok))
/// (read-port-to-string (child-stdout handle)) ;; The resulting string
/// ```
#[function(name = "child-stdout")]
pub fn child_stdout(child: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut child = ChildProcess::as_mut_ref(&child)?;
    child.stdout().into_steelval()
}

/// Get a handle to the stdin handle of the child process. The process
/// must have been started with the `with-stdin-piped` option for this
/// to be available, otherwise stdin will be inherited. This will return
/// false if the handle has already been consumed.
///
/// (child-stdout subprocess) -> (or input-port? #false)
///
/// subprocess : ChildProcess?
#[function(name = "child-stdin")]
pub fn child_stdin(child: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut child = ChildProcess::as_mut_ref(&child)?;
    child.stdin().into_steelval()
}

/// Get a handle to the stderr handle of the child process. The process
/// must have been started with the `with-stderr-piped` option for this
/// to be available, otherwise stderr will be inherited. This will return
/// false if the handle has already been consumed.
///
/// (child-stderr subprocess) -> (or input-port? #false)
///
/// subprocess : ChildProcess?
#[function(name = "child-stderr")]
pub fn child_stderr(child: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut child = ChildProcess::as_mut_ref(&child)?;
    child.stderr().into_steelval()
}

/// Terminate the subprocess.
///
/// (subprocess-kill subprocess)
///
/// subprocess : ChildProcess?
#[function(name = "kill", alias = "subprocess-kill")]
pub fn child_kill(child: &SteelVal) -> Result<SteelVal, SteelErr> {
    let mut child = ChildProcess::as_mut_ref(&child)?;
    child.kill()
}

#[derive(Debug)]
struct CommandBuilder {
    command: Command,
}

#[derive(Debug)]
struct ChildProcess {
    child: Option<Child>,
}

fn binary_exists_on_path(binary: String) -> Option<String> {
    #[cfg(not(any(target_family = "wasm", target_env = "newlib")))]
    match which::which(binary) {
        Ok(v) => Some(v.into_os_string().into_string().unwrap()),
        Err(_) => None,
    }

    #[cfg(any(target_family = "wasm", target_env = "newlib"))]
    None
}

impl ChildProcess {
    pub fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    pub fn stdout(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stdout.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_lock(SteelPortRepr::ChildStdOutput(Peekable::new(
                    BufReader::new(x),
                ))),
            })
        });

        stdout
    }

    pub fn stderr(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stderr.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_lock(SteelPortRepr::ChildStdError(Peekable::new(BufReader::new(
                    x,
                )))),
            })
        });

        stdout
    }

    pub fn stdin(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stdin.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_lock(SteelPortRepr::ChildStdInput(BufWriter::new(x))),
            })
        });

        stdout
    }

    fn wait_impl(&mut self) -> Result<SteelVal, SteelErr> {
        let exit_status = self
            .child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already awaited!"))?
            .wait()
            .map_err(SteelErr::from)?;

        match exit_status.code() {
            Some(code) => Ok(code.into()),
            None => Ok(false.into()),
        }
    }

    pub fn wait(&mut self) -> SteelResult<SteelVal, SteelErr> {
        self.wait_impl().into()
    }

    pub fn kill(&mut self) -> Result<SteelVal, SteelErr> {
        self.child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already killed!"))?
            .kill()
            .map_err(SteelErr::from)
            .map(|_| SteelVal::Void)
    }

    fn wait_with_stdout_impl(&mut self) -> Result<String, SteelErr> {
        let stdout = self
            .child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already awaited!"))?
            .wait_with_output()?
            .stdout;

        String::from_utf8(stdout)
            .map_err(|e| SteelErr::new(crate::rerrs::ErrorKind::ConversionError, e.to_string()))
    }

    pub fn wait_with_stdout(&mut self) -> SteelResult<String, SteelErr> {
        self.wait_with_stdout_impl().into()
    }
}

impl CommandBuilder {
    pub fn current_dir(&mut self, directory: String) {
        self.command.current_dir(directory);
    }

    pub fn env_var(&mut self, key: String, value: String) {
        self.command.env(key, value);
    }

    pub fn stdout_piped(&mut self) {
        self.command.stdout(Stdio::piped());
        self.command.stderr(Stdio::piped());
        self.command.stdin(Stdio::piped());
    }

    pub fn spawn_process(&mut self) -> SteelResult<ChildProcess, SteelErr> {
        self.command
            .spawn()
            .map(ChildProcess::new)
            .map_err(|x| x.into())
            .into()
    }
}

impl Custom for CommandBuilder {}
impl Custom for ChildProcess {}
