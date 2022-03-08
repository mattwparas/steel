use crate::{
    compiler::constants::ConstantMap,
    core::{instructions::Instruction, opcode::OpCode},
    parser::parser::{ParseError, Parser},
    stop,
    values::structs::StructFuncBuilder,
    SteelVal,
};
use crate::{core::instructions::densify, parser::ast::ExprKind};
use crate::{core::instructions::DenseInstruction, parser::span::Span};
use crate::{rvals::Result, values::structs::StructFuncBuilderConcrete};
use log::{debug, log_enabled};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    time::{Instant, SystemTime},
};

use crate::rerrs::ErrorKind;
use crate::SteelErr;

use super::{
    code_generator::{convert_call_globals, loop_condition_local_const_arity_two},
    compiler::{replace_defines_with_debruijn_indices, DebruijnIndicesInterner},
    constants::ConstantTable,
    map::SymbolMap,
};

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
// ConstantMap needs to get passed in to the run time to execute the program
// This way, the VM knows where to look up values
pub struct RawProgramWithSymbols {
    struct_functions: Vec<StructFuncBuilderConcrete>,
    instructions: Vec<Vec<Instruction>>,
    constant_map: ConstantMap,
    version: String, // TODO -> this should be semver
}

#[derive(Serialize, Deserialize)]
pub struct SerializableRawProgramWithSymbols {
    struct_functions: Vec<StructFuncBuilderConcrete>,
    instructions: Vec<Vec<Instruction>>,
    constant_map: Vec<u8>,
    version: String,
}

impl SerializableRawProgramWithSymbols {
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
        let program: Self = bincode::deserialize(&buffer).unwrap();

        Ok(program)
    }

    pub fn into_raw_program(self) -> RawProgramWithSymbols {
        let constant_map = ConstantMap::from_bytes(&self.constant_map).unwrap();
        RawProgramWithSymbols {
            struct_functions: self.struct_functions,
            instructions: self.instructions,
            constant_map,
            version: self.version,
        }
    }
}

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

impl RawProgramWithSymbols {
    pub fn new(
        struct_functions: Vec<StructFuncBuilderConcrete>,
        instructions: Vec<Vec<Instruction>>,
        constant_map: ConstantMap,
        version: String,
    ) -> Self {
        Self {
            struct_functions,
            instructions,
            constant_map,
            version,
        }
    }

    // Definitely can be improved
    pub fn parse_from_self_hosted_file<P>(file: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let mut lines = read_lines(file)?;

        // First line should be the constant map label
        // let constant_map =

        if let Some(constant_map_label) = lines.next() {
            if constant_map_label? != "'ConstantMap" {
                stop!(Generic => "Compiled file expected constant map label")
            }
        } else {
            stop!(Generic => "Missing constant map label")
        }

        // Temportary interner
        let mut intern = HashMap::new();

        let constant_map = if let Some(constant_map) = lines.next() {
            let constant_map = constant_map?;

            let constant_map = constant_map
                .trim_start_matches('[')
                .trim_end_matches(']')
                .split(',')
                .map(|x| {
                    // Parse the input
                    let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
                        Parser::new(&x, &mut intern).collect();
                    let parsed = parsed?;

                    Ok(SteelVal::try_from(parsed[0].clone()).unwrap())
                })
                .collect::<Result<Vec<_>>>()
                .map(ConstantMap::from_vec)?;

            constant_map
        } else {
            stop!(Generic => "Missing constant map")
        };

        if let Some(instructions_label) = lines.next() {
            if instructions_label? != "'Instructions" {
                stop!(Generic => "Compiled file expected instructions label")
            }
        } else {
            stop!(Generic => "Missing instructions label")
        }

        let mut instruction_set = Vec::new();

        let mut instructions = Vec::new();

        // Skip past the first 'Expression
        lines.next();

        for instruction_string in lines {
            let instruction_string = instruction_string?;

            if instruction_string == "'Expression" {
                // instructions = Vec::new();
                // if instruction_set.is_empty() {
                instruction_set.push(instructions);
                instructions = Vec::new();
                // }

                continue;
            }

            let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
                Parser::new(&instruction_string, &mut intern).collect();
            let parsed = parsed?;

            let value = SteelVal::try_from(parsed[0].clone()).unwrap();

            if let SteelVal::ListV(v) = value {
                // Get the op code here
                let op_code =
                    OpCode::from_str(v.get(1).unwrap().symbol_or_else(|| unreachable!()).unwrap());

                // Get the payload
                let payload = v.get(2).unwrap().int_or_else(|| unreachable!()).unwrap() as usize;

                // Get the contents
                // If I can't parse the object, just move on
                let contents = ExprKind::try_from(v.get(3).unwrap())
                    .ok()
                    .map(|x| x.atom_syntax_object())
                    .flatten();

                let instruction = Instruction::new_from_parts(op_code, payload, contents);

                instructions.push(instruction)
            } else {
                stop!(Generic => "Instruction serialized incorrectly")
            }
        }

        instruction_set.push(instructions);

        Ok(Self::new(
            Vec::new(),
            instruction_set,
            constant_map,
            "0.0.1".to_string(),
        ))
    }

    pub fn into_serializable_program(self) -> Result<SerializableRawProgramWithSymbols> {
        Ok(SerializableRawProgramWithSymbols {
            instructions: self.instructions,
            constant_map: self.constant_map.to_bytes()?,
            struct_functions: self.struct_functions,
            version: self.version,
        })
    }

    pub fn debug_print(&self) {
        self.instructions
            .iter()
            .for_each(|i| println!("{}\n\n", crate::core::instructions::disassemble(i)))
    }

    /// Applies a peephole style optimization to the underlying instruction set
    pub fn with_optimization<F: Fn(&mut [Instruction]) -> ()>(&mut self, f: F) {
        for instructions in &mut self.instructions {
            f(instructions)
        }
    }

    // Apply the optimizations to raw bytecode
    pub(crate) fn apply_optimizations(&mut self) -> &mut Self {
        // Run down the optimizations here
        for instructions in &mut self.instructions {
            convert_call_globals(instructions);
            loop_condition_local_const_arity_two(instructions);
        }

        self
    }

    // TODO -> check out the spans part of this
    // also look into having the constant map be correct mapping
    // I think the run time will have to swap the constant map in and out
    pub fn build(mut self, name: String, symbol_map: &mut SymbolMap) -> Result<Executable> {
        let now = Instant::now();

        let mut struct_instructions = Vec::new();

        for builder in &self.struct_functions {
            // Add the eventual function names to the symbol map
            let indices = symbol_map.insert_struct_function_names_from_concrete(builder);

            // Get the value we're going to add to the constant map for eventual use
            // Throw the bindings in as well
            let constant_values = builder.to_constant_val(indices);
            let idx = self.constant_map.add_or_get(constant_values);

            struct_instructions.push(vec![Instruction::new_struct(idx), Instruction::new_pop()]);
        }

        let mut interner = DebruijnIndicesInterner::default();

        for expression in &mut self.instructions {
            interner.collect_first_pass_defines(expression, symbol_map)?
        }

        // let mut first_pass_defines = HashSet::new();
        // let mut second_pass_defines = HashSet::new();

        // TODO -> the instructions need to be combined into a flat array
        // representing each expression, otherwise this will error out on a free identifier
        // since the global defines are not all collected in one pass
        // just split the debruijn index function into 2 passes
        // and make into a struct to collect the information
        for expression in &mut self.instructions {
            interner.collect_second_pass_defines(expression, symbol_map)?
        }

        // Put the new struct functions at the front
        struct_instructions.append(&mut self.instructions);
        self.instructions = struct_instructions;

        let res = Ok(Executable {
            name,
            version: self.version,
            time_stamp: SystemTime::now(),
            instructions: self.instructions.into_iter().map(|x| densify(x)).collect(),
            constant_map: self.constant_map,
            spans: Vec::new(),
        });

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Executable Build Time: {:?}", now.elapsed());
        }

        res
    }
}

// TODO -> replace spans on instructions with index into span vector
// this is kinda nasty but it _should_ work
fn extract_spans(instructions: &[&[Instruction]]) -> Vec<(usize, Span)> {
    instructions
        .iter()
        .flat_map(|x| {
            x.iter().map(|x| {
                if let Some(syn) = &x.contents {
                    syn.span
                } else {
                    Span::new(0, 0)
                }
            })
        })
        .enumerate()
        .collect()
}

// A program stripped of its debug symbols, but only constructable by running a pass
// over it with the symbol map to intern all of the symbols in the order they occurred
pub struct Executable {
    pub(crate) name: String,
    pub(crate) version: String,
    pub(crate) time_stamp: SystemTime, // TODO -> don't use system time, probably not as portable, prefer date time
    pub(crate) instructions: Vec<Vec<DenseInstruction>>,
    pub(crate) constant_map: ConstantMap,
    pub(crate) spans: Vec<Span>,
}

impl Executable {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn time_stamp(&self) -> &SystemTime {
        &self.time_stamp
    }
}
