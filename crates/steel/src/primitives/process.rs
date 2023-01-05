use std::process::{Child, Command, ExitStatus};

use im_lists::list::List;

use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};
use crate::{steel_vm::register_fn::RegisterFn, SteelErr};

pub fn process_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/process".to_string());

    module
        .register_fn("command", CommandBuilder::new)
        .register_fn("spawn-process", CommandBuilder::spawn_process)
        .register_fn("wait", ChildProcess::wait);

    module
}

#[derive(Debug)]
struct CommandBuilder {
    command: Command,
}

#[derive(Debug)]
struct ChildProcess {
    child: Child,
}

#[derive(Debug)]
struct ProcessExitStatus {
    _exit_status: ExitStatus,
}

impl ProcessExitStatus {
    pub fn new(_exit_status: ExitStatus) -> Self {
        Self { _exit_status }
    }
}

impl ChildProcess {
    pub fn new(child: Child) -> Self {
        Self { child }
    }

    pub fn wait(&mut self) -> Result<ProcessExitStatus, SteelErr> {
        self.child
            .wait()
            .map(ProcessExitStatus::new)
            .map_err(|x| x.into())
    }
}

impl CommandBuilder {
    pub fn new(command: String, args: List<String>) -> CommandBuilder {
        let mut command = Command::new(command);

        command.args(&args);

        Self { command }
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
