use std::{io::Read, path::Path, rc::Rc};

use crate::vm::VirtualMachineCore;
// use steel::rerrs::SteelErr;
use steel::rvals::{Result, SteelVal};
use steel::steel_compiler::{compiler::Compiler, constants::ConstantMap};

use steel::gc::Gc;

use steel::core::instructions::DenseInstruction;
use steel::steel_compiler::program::Program;

#[macro_export]
macro_rules! build_engine {

    ($($type:ty),* $(,)?) => {
        {
            let mut interpreter = Engine::new();
            $ (
                interpreter.register_values(<$type>::generate_bindings());
            ) *
            interpreter
        }
    };

    (Structs => {$($type:ty),* $(,)?} Functions => {$($binding:expr => $func:expr),* $(,)?}) => {
        {
            let mut interpreter = Engine::new();
            $ (
                interpreter.register_values(<$type>::generate_bindings());
            ) *

            $ (
                interpreter.register_value($binding.to_string().as_str(), SteelVal::FuncV($func));
            ) *

            interpreter
        }
    };
}

pub struct Engine {
    virtual_machine: VirtualMachineCore,
    compiler: Compiler,
}

impl Engine {
    pub fn new() -> Self {
        Engine {
            virtual_machine: VirtualMachineCore::new(),
            compiler: Compiler::default(),
        }
    }

    pub fn emit_program(&mut self, expr: &str) -> Result<Program> {
        self.compiler.compile_program(expr)
    }

    pub fn execute(
        &mut self,
        bytecode: Rc<[DenseInstruction]>,
        constant_map: &ConstantMap,
    ) -> Result<Gc<SteelVal>> {
        self.virtual_machine.execute(bytecode, constant_map)
    }

    pub fn emit_instructions(&mut self, exprs: &str) -> Result<Vec<Vec<DenseInstruction>>> {
        self.compiler.emit_instructions(exprs)
    }

    pub fn execute_program(&mut self, program: Program) -> Result<Vec<Gc<SteelVal>>> {
        self.virtual_machine.execute_program(program)
    }

    pub fn register_value(&mut self, name: &str, value: SteelVal) {
        let idx = self.compiler.register(name);
        self.virtual_machine.insert_binding(idx, value);
    }

    pub fn register_values(&mut self, values: Vec<(String, SteelVal)>) {
        for (name, value) in values {
            self.register_value(name.as_str(), value);
        }
    }

    pub fn parse_and_execute_without_optimizations(
        &mut self,
        expr: &str,
    ) -> Result<Vec<Gc<SteelVal>>> {
        let program = self.compiler.compile_program(expr)?;
        self.virtual_machine.execute_program(program)
    }

    pub fn parse_and_execute(&mut self, expr: &str) -> Result<Vec<Gc<SteelVal>>> {
        self.parse_and_execute_without_optimizations(expr)
    }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    pub fn parse_and_execute_from_path<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<Vec<Gc<SteelVal>>> {
        let mut file = std::fs::File::open(path)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;
        self.parse_and_execute(exprs.as_str())
    }
}
