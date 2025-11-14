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

        pub const MAX_OPCODE_SIZE: usize = OpCode::SELFTAILCALLNOARITY as usize + 1;

        pub const OPCODES_ARRAY: [OpCode; MAX_OPCODE_SIZE] = [
            $(OpCode::$variant),*
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
        POPSINGLE;
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
        NUMEQUAL;
        NULL;
        LTE;
        GTE;
        GT;
        LT;
        CONS; // Cons should be... probably specialized
        LIST;
        CAR;
        CDR;
        NEWBOX;
        SETBOX;
        UNBOX;
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
        BINOPSUB;
        LTEIMMEDIATEIF;
        NOT;
        VEC;
        Apply;
        POPJMP;
        BINOPADDTAIL;
        LOADINT0POP; // Load const 0
        LOADINT1POP;
        LOADINT2POP;
        READLOCAL0CALLGLOBAL;
        READLOCAL1CALLGLOBAL;
        LISTREF;
        VECTORREF;
        TRUE;
        FALSE;
        NULLIF;
        UNBOXCALL;
        UNBOXTAIL;
        EQUALCONST;
        EQUAL2;
        // Calling functions w/o arity checks, if we can statically assert
        // that the arity check is correct. If the arity check _isn't_
        // correct, then we won't report it at compile time.
        CALLGLOBALNOARITY;
        CALLGLOBALTAILNOARITY;
        FUNCNOARITY;
        TAILCALLNOARITY;
        SELFTAILCALLNOARITY
    }

    // Super instructions
    {


        [
            CaseLambdaDispatch =>
                                  (OpCode::BEGINSCOPE, 0),
                                  (OpCode::READLOCAL0, 0),
                                  (OpCode::CALLGLOBAL, 75),
                                  (OpCode::FUNC, 1),
                                  (OpCode::READLOCAL1, 1),
                                  (OpCode::PUSHCONST, 565),
                                  (OpCode::CALLGLOBAL, 75),
                                  (OpCode::FUNC, 1),
                                  (OpCode::NUMEQUAL, 2),
                                  (OpCode::PASS, 2),
                                  (OpCode::IF, 22),
        ]

        // [
        //     ReadLocal1PushConstEqualIf => (OpCode::READLOCAL1, 1),
        //                                   (OpCode::PUSHCONST, 335),
        //                                   (OpCode::EQUAL, 2),
        //                                   (OpCode::PASS, 0),
        //                                   (OpCode::IF, 8),
        // ];

        // [
        //     ReadLocal2CallGlobalIf => (OpCode::READLOCAL2, 2),
        //                               (OpCode::CALLGLOBAL, 1),
        //                               (OpCode::FUNC, 1),
        //                               (OpCode::IF, 8),
        // ]

        // 16    READLOCAL0         : 0      ##args
        // 17    CALLGLOBAL         : 1      length
        // 18    Arity              : 82     length
        // 19    READLOCAL1         : 1      l
        // 20    LOADINT0           : 274    0
        // 21    CALLGLOBAL         : 2      =
        // 22    Arity              : 180    =
        // 23    IF                 : 22

        // [
        //     CaseLambdaDispatch => (OpCode::READLOCAL0, 0),
        //                           (OpCode::CALLGLOBAL, 1),
        //                           (OpCode::Arity, 92),
        //                           (OpCode::READLOCAL1, 1),
        //                           (OpCode::LOADINT0, 0),
        //                           (OpCode::CALLGLOBAL, 2),
        //                           (OpCode::Arity, 181),
        //                           (OpCode::IF, 22),
        // ]

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
        *self as u32 > Self::LTEIMMEDIATEIF as u32
    }

    /// Statically create the mapping we need for super instruction. Also, as part of the op code map generating,
    /// the macro used for calling super instructions should also be generated.
    pub fn super_instructions(&self) -> &'static [&'static [(OpCode, usize)]] {
        todo!()
    }

    pub fn is_ephemeral_opcode(&self) -> bool {
        use OpCode::*;

        matches!(
            self,
            ECLOSURE
                | PASS
                | Arity
                | NDEFS
                | COPYCAPTURECLOSURE
                | COPYCAPTURESTACK
                | COPYHEAPCAPTURECLOSURE,
        )
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

    pub fn width(&self) -> Option<usize> {
        match self {
            OpCode::VOID => Some(1),
            OpCode::PUSH => Some(1),
            OpCode::IF => None,
            OpCode::JMP => None,
            OpCode::FUNC => Some(1),
            OpCode::SCLOSURE => None,
            OpCode::ECLOSURE => Some(1),
            OpCode::BIND => Some(1),
            OpCode::SDEF => Some(1),
            OpCode::EDEF => Some(1),
            OpCode::POPPURE => Some(1),
            OpCode::POPN => Some(1),
            OpCode::POPSINGLE => Some(1),
            OpCode::PASS => Some(1),
            OpCode::PUSHCONST => Some(1),
            OpCode::NDEFS => todo!(),
            OpCode::PANIC => todo!(),
            OpCode::TAILCALL => Some(1),
            OpCode::SET => Some(1),
            OpCode::READLOCAL => Some(1),
            OpCode::READLOCAL0 => Some(1),
            OpCode::READLOCAL1 => Some(1),
            OpCode::READLOCAL2 => Some(1),
            OpCode::READLOCAL3 => Some(1),
            OpCode::SETLOCAL => todo!(),
            OpCode::COPYCAPTURESTACK => todo!(),
            OpCode::COPYCAPTURECLOSURE => todo!(),
            OpCode::COPYHEAPCAPTURECLOSURE => todo!(),
            OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => todo!(),
            OpCode::TCOJMP => None,
            OpCode::CALLGLOBAL => Some(2),
            OpCode::CALLGLOBALTAIL => None,
            OpCode::LOADINT0 => Some(1),
            OpCode::LOADINT1 => Some(1),
            OpCode::LOADINT2 => Some(1),
            OpCode::MOVEREADLOCAL => Some(1),
            OpCode::MOVEREADLOCAL0 => Some(1),
            OpCode::MOVEREADLOCAL1 => Some(1),
            OpCode::MOVEREADLOCAL2 => Some(1),
            OpCode::MOVEREADLOCAL3 => Some(1),
            OpCode::READCAPTURED => Some(1),
            OpCode::BEGINSCOPE => Some(1),
            OpCode::LETENDSCOPE => Some(1),
            OpCode::PUREFUNC => None,
            OpCode::ADD => Some(2),
            OpCode::SUB => Some(2),
            OpCode::MUL => Some(2),
            OpCode::DIV => Some(2),
            OpCode::EQUAL => Some(2),
            OpCode::NUMEQUAL => Some(2),
            OpCode::NULL => Some(2),
            OpCode::LTE => Some(2),
            OpCode::GTE => Some(2),
            OpCode::GT => Some(2),
            OpCode::LT => Some(2),
            OpCode::CONS => Some(2),
            OpCode::LIST => Some(2),
            OpCode::CAR => Some(2),
            OpCode::CDR => Some(2),
            OpCode::NEWBOX => Some(2),
            OpCode::SETBOX => Some(2),
            OpCode::UNBOX => Some(2),
            OpCode::NEWSCLOSURE => None,
            OpCode::ADDREGISTER => Some(2),
            OpCode::SUBREGISTER => Some(2),
            OpCode::LTEREGISTER => Some(2),
            OpCode::SUBREGISTER1 => Some(2),
            OpCode::ALLOC => todo!(),
            OpCode::READALLOC => todo!(),
            OpCode::SETALLOC => todo!(),
            OpCode::DynSuperInstruction => todo!(),
            OpCode::Arity => todo!(),
            OpCode::LetVar => todo!(),
            OpCode::ADDIMMEDIATE => Some(2),
            OpCode::SUBIMMEDIATE => Some(2),
            OpCode::LTEIMMEDIATE => Some(2),
            OpCode::BINOPADD => Some(2),
            OpCode::BINOPSUB => Some(2),
            OpCode::LTEIMMEDIATEIF => None,
            OpCode::NOT => Some(2),
            OpCode::VEC => todo!(),
            OpCode::Apply => todo!(),
            OpCode::POPJMP => todo!(),
            OpCode::BINOPADDTAIL => todo!(),
            OpCode::LOADINT0POP => todo!(),
            OpCode::LOADINT1POP => todo!(),
            OpCode::LOADINT2POP => todo!(),
            OpCode::CaseLambdaDispatch => todo!(),
            _ => todo!(),
        }
    }
}
