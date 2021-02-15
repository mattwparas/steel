use crate::core::instructions::DenseInstruction;
use crate::rvals::Result;
use crate::steel_compiler::constants::ConstantMap;
use serde::{Deserialize, Serialize};

pub struct ProgramBuilder(Vec<Vec<DenseInstruction>>);

impl ProgramBuilder {
    pub fn new() -> Self {
        ProgramBuilder(Vec::new())
    }

    pub fn push(&mut self, val: Vec<DenseInstruction>) {
        self.0.push(val);
    }
}

#[derive(Serialize, Deserialize)]
pub struct Program {
    pub instructions: Vec<Vec<DenseInstruction>>,
    pub constant_map: Vec<u8>,
}

impl Program {
    pub fn new(instructions: Vec<Vec<DenseInstruction>>, constant_map: Vec<u8>) -> Self {
        Program {
            instructions,
            constant_map,
        }
    }

    pub fn new_from_map(
        instructions: Vec<Vec<DenseInstruction>>,
        constant_map: ConstantMap,
    ) -> Result<Self> {
        Ok(Program::new(instructions, constant_map.to_bytes()?))
    }

    pub fn write_to_file(&self, filename: &str) -> Result<()> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::create(format!("{}.txt", filename)).unwrap();

        let buffer = bincode::serialize(self).unwrap();

        file.write_all(&buffer)?;
        Ok(())
    }

    pub fn read_from_file(&self, filename: &str) -> Result<Self> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::open(format!("{}.txt", filename)).unwrap();

        let mut buffer = Vec::new();

        let _ = file.read(&mut buffer).unwrap();

        let program: Program = bincode::deserialize(&buffer).unwrap();

        Ok(program)
    }
}
