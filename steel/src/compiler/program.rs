use crate::parser::ast::ExprKind;
use crate::rvals::Result;
use crate::{compiler::constants::ConstantMap, core::instructions::Instruction};
use crate::{core::instructions::DenseInstruction, parser::span::Span};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::SystemTime};

use super::{compiler::replace_defines_with_debruijn_indices, map::SymbolMap};

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

// An inspectable program with debug symbols still included on the instructions
//
pub struct RawProgramWithSymbols {
    struct_functions: Vec<String>,
    instructions: Vec<Vec<Instruction>>,
}

impl RawProgramWithSymbols {
    fn debug_print(&self) {
        self.instructions
            .iter()
            .for_each(|i| println!("{}\n\n", crate::core::instructions::disassemble(i)))
    }

    fn strip_symbols(&mut self, symbol_map: &mut SymbolMap) -> Result<&mut Self> {
        for expression in &mut self.instructions {
            replace_defines_with_debruijn_indices(expression, symbol_map)?
        }

        Ok(self)
    }
}

// A program stripped of its debug symbols, but only constructable by running a pass
// over it with the symbol map to intern all of the symbols in the order they occurred
pub struct Executable {
    time_stamp: SystemTime, // TODO -> don't use system time, probably not as portable, prefer date time
    struct_functions: Vec<String>,
    instructions: Vec<Vec<DenseInstruction>>,
    constant_map: ConstantMap,
    spans: Vec<Span>,
}
