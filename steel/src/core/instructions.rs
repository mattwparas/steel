use crate::core::opcode::OpCode;
use crate::parser::parser::SyntaxObject;
use crate::parser::span::Span;
use serde::{Deserialize, Serialize};
use std::convert::TryInto;

/// Instruction loaded with lots of information prior to being condensed
/// Includes the opcode and the payload size, plus some information
/// used for locating spans and pretty error messages
#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub op_code: OpCode,
    pub payload_size: usize,
    pub contents: Option<SyntaxObject>,
    pub constant: bool,
}

impl Instruction {
    pub fn new(
        op_code: OpCode,
        payload_size: usize,
        contents: SyntaxObject,
        constant: bool,
    ) -> Instruction {
        Instruction {
            op_code,
            payload_size,
            contents: Some(contents),
            constant,
        }
    }

    pub fn new_panic(span: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::PANIC,
            payload_size: 0,
            contents: Some(span),
            constant: false,
        }
    }

    pub fn new_clear() -> Instruction {
        Instruction {
            op_code: OpCode::CLEAR,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_push_const(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::PUSHCONST,
            payload_size: idx,
            contents: None,
            constant: true,
        }
    }

    pub fn new_eval() -> Instruction {
        Instruction {
            op_code: OpCode::EVAL,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_ndef(payload_size: usize) -> Instruction {
        Instruction {
            op_code: OpCode::NDEFS,
            payload_size,
            contents: None,
            constant: false,
        }
    }

    pub fn new_func(arity: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::FUNC,
            payload_size: arity,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_pop() -> Instruction {
        Instruction {
            op_code: OpCode::POP,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_pop_with_upvalue(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::POP,
            payload_size: idx,
            contents: None,
            constant: false,
        }
    }

    pub fn new_if(true_jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::IF,
            payload_size: true_jump,
            contents: None,
            constant: false,
        }
    }

    pub fn new_jmp(jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::JMP,
            payload_size: jump,
            contents: None,
            constant: false,
        }
    }

    pub fn new_tco_jmp() -> Instruction {
        Instruction {
            op_code: OpCode::TCOJMP,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    // TODO -> this should not be as awful
    // Marking multi_arity to be whether or not this is a multi arity function
    // It should work, but its kind of horrendous to deal with and remember
    pub fn new_sclosure() -> Instruction {
        Instruction {
            op_code: OpCode::SCLOSURE,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_eclosure(arity: usize) -> Instruction {
        Instruction {
            op_code: OpCode::ECLOSURE,
            payload_size: arity,
            contents: None,
            constant: false,
        }
    }

    pub fn new_bind(contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::BIND,
            payload_size: 0,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_sdef() -> Instruction {
        Instruction {
            op_code: OpCode::SDEF,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_edef() -> Instruction {
        Instruction {
            op_code: OpCode::EDEF,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_void() -> Instruction {
        Instruction {
            op_code: OpCode::VOID,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_pass(arity: usize) -> Instruction {
        Instruction {
            op_code: OpCode::PASS,
            payload_size: arity,
            contents: None,
            constant: false,
        }
    }

    pub fn new_set() -> Instruction {
        Instruction {
            op_code: OpCode::SET,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_read() -> Instruction {
        Instruction {
            op_code: OpCode::READ,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_struct(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::STRUCT,
            payload_size: idx,
            contents: None,
            constant: true,
        }
    }

    pub fn new_inner_struct(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::INNERSTRUCT,
            payload_size: idx,
            contents: None,
            constant: true,
        }
    }

    pub fn new_call_cc() -> Instruction {
        Instruction {
            op_code: OpCode::CALLCC,
            payload_size: 0,
            contents: None,
            constant: true,
        }
    }

    pub fn new_local(idx: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::READLOCAL,
            payload_size: idx,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_set_local(idx: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::SETLOCAL,
            payload_size: idx,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_read_upvalue(idx: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::READUPVALUE,
            payload_size: idx,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_set_upvalue(idx: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::SETUPVALUE,
            payload_size: idx,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_local_upvalue(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::FILLLOCALUPVALUE,
            payload_size: idx,
            contents: None,
            constant: false,
        }
    }

    pub fn new_upvalue(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::FILLUPVALUE,
            payload_size: idx,
            contents: None,
            constant: false,
        }
    }

    pub fn new_close_upvalue(flag: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::CLOSEUPVALUE,
            payload_size: flag,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_end_scope(payload_size: usize) -> Instruction {
        Instruction {
            op_code: OpCode::ENDSCOPE,
            payload_size: payload_size,
            contents: None,
            constant: false,
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
            let contents = syn.ty.to_string();
            buffer.push_str(contents.as_str());
        }

        buffer.push('\n');
    }

    buffer
}

#[derive(Copy, Clone, Debug, PartialEq, Hash, Serialize, Deserialize)]
pub struct DenseInstruction {
    pub op_code: OpCode,
    pub payload_size: u32,
    pub span: Span,
}

impl DenseInstruction {
    pub fn new(op_code: OpCode, payload_size: u32, span: Span) -> DenseInstruction {
        DenseInstruction {
            op_code,
            payload_size,
            span,
        }
    }
}

// TODO don't actually pass around the span w/ the instruction
// pass around an index into the span to reduce the size of the instructions
// generate an equivalent
impl From<Instruction> for DenseInstruction {
    fn from(val: Instruction) -> DenseInstruction {
        DenseInstruction::new(
            val.op_code,
            val.payload_size.try_into().unwrap(),
            if let Some(syn) = val.contents {
                syn.span
            } else {
                Span::new(0, 0)
            },
        )
    }
}
