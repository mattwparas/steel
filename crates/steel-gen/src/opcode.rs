use serde::{Deserialize, Serialize};

macro_rules! declare_opcodes {

    ( { $($variant:tt);* } { $( [ $super:tt => $(($k:path, $v:expr),)* ] );* } ) => {
        #[repr(u8)]
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Serialize, Deserialize, Eq, PartialOrd, Ord)]
        pub enum OpCode {
            $($variant),*

            ,

            $($super),*
        }

        // $( const $super: &'static [(TestOpCode, usize)] = &[ $($v),* ]; )*

        pub const fn op_code_to_super_instruction_pattern(op_code: OpCode) -> Option<&'static [(OpCode, usize)]> {
            match op_code {
                $(OpCode::$super => Some( &[ $(($k, $v)),* ]) ),* ,
                _ => None
            }
        }

        pub const fn sequence_to_opcode(pattern: &[(OpCode, usize)]) -> Option<OpCode> {
            match pattern {
                $(&[ $(($k, _)),* ] => Some(OpCode::$super) ),* ,
                _ => None
            }
        }

        pub static PATTERNS: &'static [&'static [(OpCode, usize)]] = &[
                $( &[ $(($k, $v)),* ] ),* ,
        ];
    }

}

declare_opcodes! {
    {
        VOID;
        PUSH;
        IF;
        JMP;
        FUNC;
        SCLOSURE;
        ECLOSURE;
        BIND;
        SDEF;
        EDEF;
        POPPURE;
        POPN;
        PASS;
        PUSHCONST;
        NDEFS;
        PANIC;
        TAILCALL;
        SET;
        READLOCAL;
        READLOCAL0;
        READLOCAL1;
        READLOCAL2;
        READLOCAL3;
        SETLOCAL;
        COPYCAPTURESTACK;
        COPYCAPTURECLOSURE;
        COPYHEAPCAPTURECLOSURE;
        FIRSTCOPYHEAPCAPTURECLOSURE;
        TCOJMP;
        CALLGLOBAL;
        CALLGLOBALTAIL;
        LOADINT0; // Load const 0
        LOADINT1;
        LOADINT2;
        CGLOCALCONST;
        MOVEREADLOCAL;
        MOVEREADLOCAL0;
        MOVEREADLOCAL1;
        MOVEREADLOCAL2;
        MOVEREADLOCAL3;
        READCAPTURED;
        BEGINSCOPE;
        LETENDSCOPE;
        PUREFUNC;
        ADD;
        SUB;
        MUL;
        DIV;
        EQUAL;
        LTE;
        NEWSCLOSURE;
        ADDREGISTER;
        SUBREGISTER;
        LTEREGISTER;
        SUBREGISTER1;
        ALLOC;
        READALLOC;
        SETALLOC;
        DynSuperInstruction;
        Arity;
        LetVar;
        ADDIMMEDIATE;
        SUBIMMEDIATE;
        LTEIMMEDIATE;
        BINOPADD;
        LTEIMMEDIATEIF
    }

    // Super instructions
    {
        // [ FooBarBaz => (OpCode::VOID, 0), (OpCode::VOID, 0), (OpCode::VOID, 0), ];

        [ ReadLocal1PushConstEqualIf => (OpCode::READLOCAL1, 1),
                                        (OpCode::PUSHCONST, 335),
                                        (OpCode::EQUAL, 2),
                                        (OpCode::PASS, 0),
                                        (OpCode::IF, 8),
        ]

        // [ MOVERLLIS2CG => (OpCode::MOVEREADLOCAL, 0), (OpCode::LOADINT2, 225), (OpCode::SUB, 2), (OpCode::CALLGLOBAL, 1), ];

        // [ MOVERLLIS2CGFOO => (OpCode::MOVEREADLOCAL, 1), (OpCode::LOADINT2, 225), (OpCode::SUB, 2), (OpCode::CALLGLOBAL, 1), ]


        //         (MOVEREADLOCAL0, 0),
        //         (LOADINT2, 225),
        //         (SUB, 2),
        //         (CALLGLOBAL, 1),

    }
}

// // expansion
// enum EntryPoints {
//     SomeLibCallback(u64),

//     A(),
//     B(),
// }

impl OpCode {
    /// Is this op code created as part of the aggregation of multiple op codes?
    pub fn is_super_instruction(&self) -> bool {
        // TODO: Check where super instructions start!

        return *self as u32 > Self::LTEIMMEDIATEIF as u32;
    }

    /// Statically create the mapping we need for super instruction. Also, as part of the op code map generating,
    /// the macro used for calling super instructions should also be generated.
    pub fn super_instructions(&self) -> &'static [&'static [(OpCode, usize)]] {
        todo!()
    }

    pub fn is_ephemeral_opcode(&self) -> bool {
        use OpCode::*;

        match self {
            ECLOSURE
            | PASS
            | Arity
            | NDEFS
            | COPYCAPTURECLOSURE
            | COPYCAPTURESTACK
            | COPYHEAPCAPTURECLOSURE => true,
            _ => false,
        }
    }

    // TODO better error handling here
    pub fn from(s: &str) -> Self {
        use OpCode::*;
        match s {
            "VOID" => VOID,
            "PUSH" => PUSH,
            "IF" => IF,
            "JMP" => JMP,
            "FUNC" => FUNC,
            "SCLOSURE" => SCLOSURE,
            "ECLOSURE" => ECLOSURE,
            // "STRUCT" => STRUCT,
            "BIND" => BIND,
            "SDEF" => SDEF,
            "EDEF" => EDEF,
            "PASS" => PASS,
            "PUSHCONST" => PUSHCONST,
            "NDEFS" => NDEFS,
            "PANIC" => PANIC,
            "TAILCALL" => TAILCALL,
            "SET" => SET,
            "READLOCAL" => READLOCAL,
            "SETLOCAL" => SETLOCAL,
            "TCOJMP" => TCOJMP,
            "CALLGLOBAL" => CALLGLOBAL,
            "CALLGLOBALTAIL" => CALLGLOBALTAIL,
            "LOADINT0" => LOADINT0, // Load const 0
            "LOADINT1" => LOADINT1,
            "LOADINT2" => LOADINT2,
            "CGLOCALCONST" => CGLOCALCONST,
            "MOVEREADLOCAL" => MOVEREADLOCAL,
            "BEGINSCOPE" => BEGINSCOPE,
            "ADD" => ADD,
            "SUB" => SUB,
            "MUL" => MUL,
            "DIV" => DIV,
            "EQUAL" => EQUAL,
            "LTE" => LTE,
            "LETENDSCOPE" => LETENDSCOPE,
            "PUREFUNC" => PUREFUNC,
            "POP_PURE" => POPPURE,
            "READCAPTURED" => READCAPTURED,
            "COPYCAPTURESTACK" => COPYCAPTURESTACK,
            "COPYCAPTURECLOSURE" => COPYCAPTURECLOSURE,
            "COPYHEAPCAPTURECLOSURE" => COPYHEAPCAPTURECLOSURE,
            "NEWSCLOSURE" => NEWSCLOSURE,
            "ADDREGISTER" => ADDREGISTER,
            "SUBREGISTER" => SUBREGISTER,
            "LTEREGISTER" => LTEREGISTER,
            "SUBREGISTER1" => SUBREGISTER1,
            "ALLOC" => ALLOC,
            "READALLOC" => READALLOC,
            "SETALLOC" => SETALLOC,
            _ => panic!("Unable to map string to opcode"),
        }
    }

    pub fn width(&self) -> usize {
        match self {
            OpCode::VOID => todo!(),
            OpCode::PUSH => todo!(),
            OpCode::IF => todo!(),
            OpCode::JMP => todo!(),
            OpCode::FUNC => todo!(),
            OpCode::SCLOSURE => todo!(),
            OpCode::ECLOSURE => todo!(),
            // OpCode::STRUCT => todo!(),
            OpCode::BIND => todo!(),
            OpCode::SDEF => todo!(),
            OpCode::EDEF => todo!(),
            OpCode::POPPURE => todo!(),
            OpCode::PASS => todo!(),
            OpCode::PUSHCONST => todo!(),
            OpCode::NDEFS => todo!(),
            OpCode::PANIC => todo!(),
            OpCode::TAILCALL => todo!(),
            OpCode::SET => todo!(),
            OpCode::READLOCAL => todo!(),
            OpCode::SETLOCAL => todo!(),
            OpCode::COPYCAPTURESTACK => todo!(),
            OpCode::COPYCAPTURECLOSURE => todo!(),
            OpCode::TCOJMP => todo!(),
            OpCode::CALLGLOBAL => 2,
            OpCode::CALLGLOBALTAIL => todo!(),
            OpCode::LOADINT0 => todo!(),
            OpCode::LOADINT1 => todo!(),
            OpCode::LOADINT2 => todo!(),
            OpCode::CGLOCALCONST => todo!(),
            OpCode::MOVEREADLOCAL => todo!(),
            OpCode::READCAPTURED => todo!(),
            OpCode::BEGINSCOPE => todo!(),
            OpCode::LETENDSCOPE => todo!(),
            OpCode::PUREFUNC => todo!(),
            OpCode::ADD => 2,
            OpCode::SUB => todo!(),
            OpCode::MUL => todo!(),
            OpCode::DIV => todo!(),
            OpCode::EQUAL => todo!(),
            OpCode::LTE => todo!(),
            OpCode::NEWSCLOSURE => todo!(),
            OpCode::ADDREGISTER => 2,
            OpCode::SUBREGISTER => 2,
            OpCode::LTEREGISTER => 2,
            OpCode::SUBREGISTER1 => todo!(),
            OpCode::ALLOC => todo!(),
            _ => todo!(),
        }
    }
}
