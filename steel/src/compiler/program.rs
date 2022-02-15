use crate::compiler::constants::ConstantMap;
use crate::core::instructions::DenseInstruction;
use crate::parser::ast::ExprKind;
use crate::rvals::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
pub struct SerializableProgram {
    pub instructions: Vec<Vec<DenseInstruction>>,
    pub constant_map: Vec<u8>,
}

impl SerializableProgram {
    pub fn write_to_file(&self, filename: &str) -> Result<()> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::create(format!("{}.txt", filename)).unwrap();

        let buffer = bincode::serialize(self).unwrap();

        file.write_all(&buffer)?;
        Ok(())
    }

    pub fn read_from_file(filename: &str) -> Result<Self> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::open(format!("{}.txt", filename)).unwrap();

        let mut buffer = Vec::new();

        let _ = file.read_to_end(&mut buffer).unwrap();

        let program: SerializableProgram = bincode::deserialize(&buffer).unwrap();

        Ok(program)
    }

    pub fn into_program(self) -> Program {
        let constant_map = ConstantMap::from_bytes(&self.constant_map).unwrap();
        Program {
            constant_map,
            instructions: self.instructions,
            ast: HashMap::new(),
        }
    }
}

/// Represents a Steel program
/// The program holds the instructions and the constant map, serialized to bytes
pub struct Program {
    pub instructions: Vec<Vec<DenseInstruction>>,
    pub constant_map: ConstantMap,
    pub ast: HashMap<usize, ExprKind>,
}

impl Program {
    pub fn new(
        instructions: Vec<Vec<DenseInstruction>>,
        constant_map: ConstantMap,
        ast: HashMap<usize, ExprKind>,
    ) -> Self {
        Program {
            instructions,
            constant_map,
            ast,
        }
    }

    pub fn into_serializable_program(self) -> Result<SerializableProgram> {
        Ok(SerializableProgram {
            instructions: self.instructions,
            constant_map: self.constant_map.to_bytes()?,
        })
    }
}
