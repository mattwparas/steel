use crate::parser::SyntaxObject;
use crate::vm::OpCode;

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub(crate) op_code: OpCode,
    pub(crate) payload_size: usize,
    pub(crate) contents: Option<SyntaxObject>,
    pub(crate) constant: bool,
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

    pub fn new_map() -> Instruction {
        Instruction {
            op_code: OpCode::MAP,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_filter() -> Instruction {
        Instruction {
            op_code: OpCode::FILTER,
            payload_size: 0,
            contents: None,
            constant: false,
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

    pub fn new_apply(span: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::APPLY,
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

    pub fn new_pass() -> Instruction {
        Instruction {
            op_code: OpCode::PASS,
            payload_size: 0,
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

    pub fn new_collect() -> Instruction {
        Instruction {
            op_code: OpCode::COLLECT,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_collect_to() -> Instruction {
        Instruction {
            op_code: OpCode::COLLECTTO,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_transduce() -> Instruction {
        Instruction {
            op_code: OpCode::TRANSDUCE,
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
}
