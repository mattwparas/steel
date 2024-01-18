use std::cell::RefCell;
use std::io::{BufReader, BufWriter};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::rc::Rc;

use crate::values::port::{SteelPort, SteelPortRepr};
use crate::values::structs::SteelResult;
use crate::SteelVal;
use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};

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
        .register_fn("child-stdin", ChildProcess::stdin);

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

#[derive(Debug)]
struct ProcessExitStatus {
    _exit_status: ExitStatus,
}

fn binary_exists_on_path(binary: String) -> Option<String> {
    #[cfg(not(target_arch = "wasm32"))]
    match which::which(binary) {
        Ok(v) => Some(v.into_os_string().into_string().unwrap()),
        Err(_) => None,
    }

    #[cfg(target_arch = "wasm32")]
    None
}

impl ProcessExitStatus {
    pub fn new(_exit_status: ExitStatus) -> Self {
        Self { _exit_status }
    }
}

impl ChildProcess {
    pub fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    pub fn stdout(&mut self) -> Option<SteelVal> {
        let stdout = self
            .child
            .as_mut()
            .and_then(|x| x.stdout.take())
            .and_then(|x| {
                Some(SteelVal::PortV(SteelPort {
                    port: Rc::new(RefCell::new(SteelPortRepr::ChildStdOutput(BufReader::new(
                        x,
                    )))),
                }))
            });

        stdout

        //     todo!()
    }

    pub fn stdin(&mut self) -> Option<SteelVal> {
        let stdout = self
            .child
            .as_mut()
            .and_then(|x| x.stdin.take())
            .and_then(|x| {
                Some(SteelVal::PortV(SteelPort {
                    port: Rc::new(RefCell::new(SteelPortRepr::ChildStdInput(BufWriter::new(
                        x,
                    )))),
                }))
            });

        stdout

        //     todo!()
    }
    fn wait_impl(&mut self) -> Result<ProcessExitStatus, SteelErr> {
        self.child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already awaited!"))?
            .wait()
            .map(ProcessExitStatus::new)
            .map_err(|x| x.into())
    }

    pub fn wait(&mut self) -> SteelResult<ProcessExitStatus, SteelErr> {
        self.wait_impl().into()
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
impl Custom for ProcessExitStatus {}
