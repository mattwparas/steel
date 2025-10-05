use crate::gc::Gc;
use crate::values::port::{Peekable, SteelPort, SteelPortRepr};
use crate::values::structs::SteelResult;
use crate::SteelVal;
use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};
use std::io::{BufReader, BufWriter};
use std::process::{Child, Command, Stdio};

pub fn process_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/process".to_string());

    module
        .register_fn("command", CommandBuilder::new)
        .register_fn("set-current-dir!", CommandBuilder::current_dir)
        .register_fn("set-piped-stdout!", CommandBuilder::stdout_piped)
        .register_fn("spawn-process", CommandBuilder::spawn_process)
        .register_fn("wait", ChildProcess::wait)
        .register_fn("wait->stdout", ChildProcess::wait_with_stdout)
        .register_fn("which", binary_exists_on_path)
        .register_fn("child-stdout", ChildProcess::stdout)
        .register_fn("child-stderr", ChildProcess::stderr)
        .register_fn("child-stdin", ChildProcess::stdin)
        .register_fn("set-env-var!", CommandBuilder::env_var)
        .register_fn("kill", ChildProcess::kill);

    module
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
    #[cfg(all(feature = "std", not(target_env = "newlib")))]
    match which::which(binary) {
        Ok(v) => Some(v.into_os_string().into_string().unwrap()),
        Err(_) => None,
    }

    #[cfg(any(not(feature = "std"), target_env = "newlib"))]
    None
}

impl ChildProcess {
    pub fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    pub fn stdout(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stdout.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_mut(SteelPortRepr::ChildStdOutput(Peekable::new(
                    BufReader::new(x),
                ))),
            })
        });

        stdout
    }

    pub fn stderr(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stderr.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_mut(SteelPortRepr::ChildStdError(Peekable::new(BufReader::new(
                    x,
                )))),
            })
        });

        stdout
    }

    pub fn stdin(&mut self) -> Option<SteelVal> {
        let stdout = self.child.as_mut().and_then(|x| x.stdin.take()).map(|x| {
            SteelVal::PortV(SteelPort {
                port: Gc::new_mut(SteelPortRepr::ChildStdInput(BufWriter::new(x))),
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
    pub fn new(command: String, args: crate::values::lists::SteelList<String>) -> CommandBuilder {
        let mut command = Command::new(command);

        command.args(&args);

        Self { command }
    }

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
