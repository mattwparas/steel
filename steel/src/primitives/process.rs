use std::process::{Child, Command};

use im_lists::list::List;

use crate::{rvals::Custom, SteelVal};

#[derive(Debug)]
struct CommandBuilder {
    command: Command,
}

#[derive(Debug)]
struct ChildProcess {
    child: Child,
}

impl ChildProcess {
    pub fn new(child: Child) -> Self {
        Self { child }
    }
}

impl CommandBuilder {
    pub fn new(command: Command) -> Self {
        Self { command }
    }

    pub fn command_builder(command: String, args: List<String>) -> CommandBuilder {
        let mut command = Command::new(command);

        command.args(&args);

        CommandBuilder::new(command)
    }

    pub fn spawn_process(&mut self) -> Result<ChildProcess, std::io::Error> {
        self.command.spawn().map(ChildProcess::new)
    }
}

impl Custom for CommandBuilder {}
