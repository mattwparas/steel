use crate::core::opcode::OpCode;
use serde::{Deserialize, Serialize};
use std::convert::TryInto;

use super::labels::Expr;

/// Instruction loaded with lots of information prior to being condensed
/// Includes the opcode and the payload size, plus some information
/// used for locating spans and pretty error messages
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Instruction {
    pub op_code: OpCode,
    pub payload_size: u24,
    pub contents: Option<Expr>,
}

impl Instruction {
    pub fn new_from_parts(
        op_code: OpCode,
        payload_size: u24,
        contents: Option<Expr>,
    ) -> Instruction {
        Instruction {
            op_code,
            payload_size,
            contents,
        }
    }
}

pub fn densify(instructions: Vec<Instruction>) -> Vec<DenseInstruction> {
    instructions.into_iter().map(|x| x.into()).collect()
}

pub fn pretty_print_dense_instructions(instrs: &[DenseInstruction]) {
    for (i, instruction) in instrs.iter().enumerate() {
        println!(
            "{}    {:?} : {}",
            i, instruction.op_code, instruction.payload_size
        );
    }
}

pub fn disassemble(instructions: &[Instruction]) -> String {
    let first_column_width = instructions.len().to_string().len();
    let second_column_width = instructions
        .iter()
        .map(|x| format!("{:?}", x.op_code).len())
        .max()
        .unwrap();
    let third_column_width = instructions
        .iter()
        .map(|x| x.payload_size.to_string().len())
        .max()
        .unwrap();

    let mut buffer = String::new();

    for (i, instruction) in instructions.iter().enumerate() {
        let index = i.to_string();

        buffer.push_str(index.as_str());
        for _ in 0..(first_column_width - index.len()) {
            buffer.push(' ');
        }

        buffer.push_str("    ");

        let op_code = format!("{:?}", instruction.op_code);
        buffer.push_str(op_code.as_str());
        for _ in 0..(second_column_width - op_code.len()) {
            buffer.push(' ');
        }

        buffer.push_str(" : ");

        let payload_size = instruction.payload_size.to_string();
        buffer.push_str(payload_size.as_str());
        for _ in 0..(third_column_width - payload_size.len()) {
            buffer.push(' ');
        }

        buffer.push_str("    ");

        if let Some(syn) = instruction.contents.as_ref() {
            match syn {
                Expr::Atom(syn) => {
                    let contents = syn.ty.to_string();
                    buffer.push_str(contents.as_str());
                }
                Expr::List(l) => {
                    let contents = l.to_string();
                    buffer.push_str(contents.as_str());
                }
            }
        }

        buffer.push('\n');
    }

    buffer
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DenseInstruction {
    pub op_code: OpCode,
    // Function IDs need to be interned _again_ before patched into the code?
    // Also: We should be able to get away with a u16 here. Just grab places where u16
    // won't fit and convert to something else.
    pub payload_size: u24,
}

use std::ops::Add;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Serialize, Deserialize, Debug)]
#[allow(non_camel_case_types)]
#[repr(transparent)]
pub struct u24([u8; 3]);
impl Add for u24 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self::from_u32(self.to_u32() + rhs.to_u32())
    }
}

impl u24 {
    #[inline(always)]
    pub fn to_u32(self) -> u32 {
        let u24([a, b, c]) = self;
        u32::from_le_bytes([a, b, c, 0])
    }

    #[inline(always)]
    pub fn from_u32(n: u32) -> Self {
        let [a, b, c, d] = n.to_le_bytes();
        debug_assert!(d == 0);
        u24([a, b, c])
    }

    #[inline(always)]
    pub fn from_usize(n: usize) -> Self {
        let [a, b, c, d, ..] = n.to_le_bytes();
        debug_assert!(d == 0);
        u24([a, b, c])
    }

    #[inline(always)]
    pub fn to_usize(self) -> usize {
        let u24([a, b, c]) = self;
        #[cfg(target_arch = "wasm32")]
        {
            return usize::from_le_bytes([a, b, c, 0]);
        }

        #[cfg(not(target_arch = "wasm32"))]
        usize::from_le_bytes([a, b, c, 0, 0, 0, 0, 0])
    }
}

impl std::fmt::Display for u24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_u32())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DenseInstruction2 {
    pub op_code: OpCode,
    // Function IDs need to be interned _again_ before patched into the code?
    // Also: We should be able to get away with a u16 here. Just grab places where u16
    // won't fit and convert to something else.
    pub payload_size: u24,
}

impl DenseInstruction {
    pub fn new(op_code: OpCode, payload_size: u24) -> DenseInstruction {
        DenseInstruction {
            op_code,
            payload_size,
        }
    }
}

// TODO don't actually pass around the span w/ the instruction
// pass around an index into the span to reduce the size of the instructions
// generate an equivalent
impl From<Instruction> for DenseInstruction {
    fn from(val: Instruction) -> DenseInstruction {
        DenseInstruction::new(val.op_code, val.payload_size)
    }
}
