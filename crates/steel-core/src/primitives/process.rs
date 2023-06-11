use std::process::{Child, Command, ExitStatus};

use im_lists::list::List;

use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};

pub fn process_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/process".to_string());

    module
        .register_fn("command", CommandBuilder::new)
        .register_fn("set-current-dir!", CommandBuilder::current_dir)
        .register_fn("spawn-process", CommandBuilder::spawn_process)
        .register_fn("wait", ChildProcess::wait)
        .register_fn("wait->stdout", ChildProcess::wait_with_stdout)
        .register_fn("which", binary_exists_on_path);

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
    match which::which(binary) {
        Ok(v) => Some(v.into_os_string().into_string().unwrap()),
        Err(_) => None,
    }
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

    pub fn wait(&mut self) -> Result<ProcessExitStatus, SteelErr> {
        self.child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already awaited!"))?
            .wait()
            .map(ProcessExitStatus::new)
            .map_err(|x| x.into())
    }

    pub fn wait_with_stdout(&mut self) -> Result<String, SteelErr> {
        let stdout = self
            .child
            .take()
            .ok_or_else(crate::throw!(Generic => "Child already awaited!"))?
            .wait_with_output()?
            .stdout;

        String::from_utf8(stdout)
            .map_err(|e| SteelErr::new(crate::rerrs::ErrorKind::ConversionError, e.to_string()))
    }
}

impl CommandBuilder {
    pub fn new(command: String, args: List<String>) -> CommandBuilder {
        let mut command = Command::new(command);

        command.args(&args);

        Self { command }
    }

    pub fn current_dir(&mut self, directory: String) {
        self.command.current_dir(directory);
    }

    pub fn spawn_process(&mut self) -> Result<ChildProcess, SteelErr> {
        self.command
            .spawn()
            .map(ChildProcess::new)
            .map_err(|x| x.into())
    }
}

impl Custom for CommandBuilder {}
impl Custom for ChildProcess {}
impl Custom for ProcessExitStatus {}
