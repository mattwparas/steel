use crate::core::instructions::u24;
use crate::core::labels::Expr;
use crate::gc::shared::StandardShared;
use crate::gc::Shared;
use crate::parser::span_visitor::get_span;
use crate::rvals::Result;
use crate::{
    compiler::constants::ConstantMap,
    core::{instructions::Instruction, opcode::OpCode},
    stop, SteelVal,
};
use crate::{core::instructions::DenseInstruction, parser::span::Span};
use crate::{
    parser::{
        ast::ExprKind,
        interner::InternedString,
        parser::{RawSyntaxObject, SyntaxObject},
        tokens::TokenType,
    },
    rvals::IntoSteelVal,
};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::SystemTime};

#[cfg(feature = "profiling")]
use std::time::Instant;

#[cfg(feature = "profiling")]
use log::{debug, log_enabled};

use super::{compiler::DebruijnIndicesInterner, map::SymbolMap};

const _TILE_SUPER_INSTRUCTIONS: bool = true;

/// Evaluates an atom expression in given environment.
fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => Ok((*b).into()),
        TokenType::Number(n) => (&**n).into_steelval(),
        TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.to_string().into())),
        TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
        // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
        TokenType::Keyword(k) => Ok(SteelVal::SymbolV(k.clone().into())),
        what => {
            // println!("getting here in the eval_atom - code_generator");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}

// Call global if -> merge with if, when possible
pub fn merge_call_global_if(instructions: &mut [Instruction]) {
    if cfg!(feature = "jit2") {
        return;
    }

    if instructions.len() < 3 {
        return;
    }

    for i in 0..instructions.len() - 2 {
        let maybe_call_global = instructions.get(i);
        let maybe_if = instructions.get(i + 2);

        match (
            maybe_call_global.map(|x| x.op_code),
            maybe_if.map(|x| x.op_code),
        ) {
            // (Some(OpCode::CALLGLOBAL), Some(OpCode::IF)) => {
            //     if let Some(x) = instructions.get_mut(i) {
            //         x.op_code = OpCode::CALLGLOBALIF;
            //     }
            // }
            (Some(OpCode::NULL), Some(OpCode::IF)) => {
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::NULLIF;
                }
            }
            _ => {}
        }
    }
}

pub fn specialize_read_local(instructions: &mut [Instruction]) {
    for i in 0..instructions.len() {
        let read_local = instructions.get(i);

        match read_local {
            Some(Instruction {
                op_code: OpCode::MOVEREADLOCAL,
                payload_size,
                ..
            }) => {
                let op_code = match payload_size.to_u32() {
                    0 => OpCode::MOVEREADLOCAL0,
                    1 => OpCode::MOVEREADLOCAL1,
                    2 => OpCode::MOVEREADLOCAL2,
                    3 => OpCode::MOVEREADLOCAL3,
                    _ => continue,
                };

                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = op_code;
                }
            }

            Some(Instruction {
                op_code: OpCode::READLOCAL,
                payload_size,
                ..
            }) => {
                let op_code = match payload_size.to_u32() {
                    0 => OpCode::READLOCAL0,
                    1 => OpCode::READLOCAL1,
                    2 => OpCode::READLOCAL2,
                    3 => OpCode::READLOCAL3,
                    _ => continue,
                };

                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = op_code;
                }
            } // instructions[i + 1].op_code = OpCode::PASS;
            // instructions[i + 2].op_code = OpCode::PASS;
            // instructions[i + 4].op_code = OpCode::PASS;
            _ => continue,
        }
    }
}

pub fn specialize_constants(instructions: &mut [Instruction]) -> Result<()> {
    for i in 0..instructions.len() - 1 {
        let instruction = instructions.get(i);
        let next = instructions.get(i + 1);

        match (instruction, next) {
            (
                Some(Instruction {
                    op_code: OpCode::PUSHCONST,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(_),
                            ..
                        })),
                    ..
                }),
                ..,
            ) => continue,

            #[cfg(feature = "experimental")]
            (
                Some(Instruction {
                    op_code: OpCode::PUSHCONST,
                    contents: Some(Expr::Atom(syn)),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::POPJMP | OpCode::POPPURE,
                    ..
                }),
            ) => {
                let value = eval_atom(&syn)?;
                let opcode = match &value {
                    SteelVal::IntV(0) => OpCode::LOADINT0POP,
                    SteelVal::IntV(1) => OpCode::LOADINT1POP,
                    SteelVal::IntV(2) => OpCode::LOADINT2POP,
                    _ => continue,
                };
                instructions.get_mut(i).unwrap().op_code = opcode;
            }

            (
                Some(Instruction {
                    op_code: OpCode::PUSHCONST,
                    contents: Some(Expr::Atom(syn)),
                    ..
                }),
                ..,
            ) => {
                let value = eval_atom(syn)?;
                let opcode = match &value {
                    SteelVal::IntV(0) => OpCode::LOADINT0,
                    SteelVal::IntV(1) => OpCode::LOADINT1,
                    SteelVal::IntV(2) => OpCode::LOADINT2,
                    SteelVal::BoolV(true) => OpCode::TRUE,
                    SteelVal::BoolV(false) => OpCode::FALSE,
                    _ => continue,
                };
                instructions.get_mut(i).unwrap().op_code = opcode;
            }
            _ => continue,
        }
    }
    Ok(())
}

// (READLOCAL0, CALLGLOBAL): 1200919
// (READLOCAL1, CALLGLOBAL): 1088780
pub fn specialize_call_global_local(instructions: &mut [Instruction]) {
    if instructions.is_empty() {
        return;
    }

    for i in 0..instructions.len() - 1 {
        let readlocal = instructions.get(i);
        let callglobal = instructions.get(i + 1);

        match (readlocal, callglobal) {
            (
                Some(Instruction {
                    op_code: OpCode::READLOCAL0,
                    // payload_size: index,
                    // contents: Some(Expr::Atom(ident)),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CALLGLOBAL,
                    // payload_size: arity,
                    ..
                }),
            ) => {
                // TODO:
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::READLOCAL0CALLGLOBAL;
                    // x.payload_size = index;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    // Leave this as the OpCode::FUNC;
                    x.op_code = OpCode::PASS;
                    // x.payload_size = u24::from_usize(arity);
                }
            }

            (
                Some(Instruction {
                    op_code: OpCode::READLOCAL1,
                    // payload_size: index,
                    // contents: Some(Expr::Atom(ident)),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CALLGLOBAL,
                    // payload_size: arity,
                    ..
                }),
            ) => {
                // TODO:
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::READLOCAL1CALLGLOBAL;
                    // x.payload_size = index;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    // Leave this as the OpCode::FUNC;
                    x.op_code = OpCode::PASS;
                    // x.payload_size = u24::from_usize(arity);
                }
            }

            _ => {}
        }
    }
}

pub fn unbox_function_call(instructions: &mut [Instruction]) {
    if instructions.is_empty() {
        return;
    }

    // Should look like:
    for i in 0..instructions.len() {
        let push = instructions.get(i);
        let func = instructions.get(i + 1);
        let unbox_call = instructions.get(i + 2);

        match (push, func, unbox_call) {
            (
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents: Some(Expr::Atom(ident)),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    payload_size: unbox_arity,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::FUNC | OpCode::TAILCALL,
                    payload_size: unbox_call_arity,
                    ..
                }),
            ) => {
                let arity = unbox_arity.to_usize();
                let unbox_call_arity = *unbox_call_arity;

                let call_kind = unbox_call.unwrap().op_code;

                if let TokenType::Identifier(ident) = ident.ty {
                    match ident {
                        _ if ident == *UNBOX || ident == *PRIM_UNBOX && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::UNBOXCALL;

                                if call_kind == OpCode::FUNC {
                                    x.op_code = OpCode::UNBOXCALL;
                                } else {
                                    x.op_code = OpCode::UNBOXTAIL;
                                }

                                x.payload_size = unbox_call_arity;
                                continue;
                            }
                        }

                        _ => {
                            // println!("Converting call global: {}", ident);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn convert_call_globals(instructions: &mut [Instruction]) {
    if instructions.is_empty() {
        return;
    }

    for i in 0..instructions.len() - 1 {
        let push = instructions.get(i);
        let func = instructions.get(i + 1);

        match (push, func) {
            (
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    payload_size: index,
                    contents: Some(Expr::Atom(ident)),
                    ..
                }),
                Some(Instruction {
                    op_code: func_op @ OpCode::FUNC | func_op @ OpCode::FUNCNOARITY,
                    payload_size: arity,
                    ..
                }),
            ) => {
                let arity = arity.to_usize();
                let index = *index;
                let func_op = *func_op;

                if let TokenType::Identifier(ident) = ident.ty {
                    match ident {
                        _ if ident == *PRIM_CONS_SYMBOL && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::CONS;
                                x.payload_size = u24::from_u32(2);
                                continue;
                            }
                        }

                        // Specialize lists, cons, hashmap, etc. - anything that we expect to be used often in
                        // real code.
                        // _ if ident == *PRIM_LIST_SYMBOL => {
                        //     if let Some(x) = instructions.get_mut(i) {
                        //         x.op_code = OpCode::LIST;
                        //         x.payload_size = arity;
                        //         continue;
                        //     }
                        // }
                        _ if ident == *BOX || ident == *PRIM_BOX && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::NEWBOX;
                                continue;
                            }
                        }

                        _ if ident == *UNBOX || ident == *PRIM_UNBOX && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::UNBOX;
                                continue;
                            }
                        }

                        _ if ident == *SETBOX || ident == *PRIM_SETBOX && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::SETBOX;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_CAR && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::CAR;
                                continue;
                            }
                        }

                        // TODO: Figure out why this isn't working correctly?
                        _ if ident == *PRIM_LIST_SYMBOL => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::LIST;
                                x.payload_size = u24::from_usize(arity);
                                continue;
                            }
                        }

                        _ if ident == *PRIM_LIST_REF && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::LISTREF;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_VECTOR_REF && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::VECTORREF;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_CDR && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::CDR;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_NOT && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::NOT;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_NULL && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::NULL;
                                continue;
                            }
                        }

                        // _ if ident == *CDR_SYMBOL || ident == *PRIM_CAR => {
                        //     if let Some(x) = instructions.get_mut(i) {
                        //         x.op_code = OpCode::CAR;
                        //         continue;
                        //     }
                        // }
                        _ => {
                            // println!("Converting call global: {}", ident);
                        }
                    }
                }

                // TODO:
                if let Some(x) = instructions.get_mut(i) {
                    if func_op == OpCode::FUNC {
                        x.op_code = OpCode::CALLGLOBAL;
                    } else {
                        x.op_code = OpCode::CALLGLOBALNOARITY;
                    }
                    x.payload_size = index;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    // Leave this as the OpCode::FUNC;
                    // x.op_code = OpCode::Arity;
                    x.payload_size = u24::from_usize(arity);
                }
            }
            (
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    payload_size: index,
                    contents: Some(Expr::Atom(ident)),
                    ..
                }),
                Some(Instruction {
                    op_code: tail_op @ OpCode::TAILCALL | tail_op @ OpCode::TAILCALLNOARITY,
                    payload_size: arity,
                    ..
                }),
            ) => {
                let arity = arity.to_usize();
                let index = *index;
                let tail_op = *tail_op;

                if let TokenType::Identifier(ident) = ident.ty {
                    match ident {
                        _ if ident == *PRIM_CONS_SYMBOL => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::CONS;
                                x.payload_size = u24::from_u32(2);
                                continue;
                            }
                        }

                        _ if ident == *BOX || ident == *PRIM_BOX && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::NEWBOX;
                                continue;
                            }
                        }

                        _ if ident == *UNBOX || ident == *PRIM_UNBOX && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::UNBOX;
                                continue;
                            }
                        }

                        _ if ident == *SETBOX || ident == *PRIM_SETBOX && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::SETBOX;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_CAR && arity == 1 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::CAR;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_LIST_REF && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::LISTREF;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_VECTOR_REF && arity == 2 => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::VECTORREF;
                                continue;
                            }
                        }

                        _ if ident == *PRIM_LIST_SYMBOL => {
                            if let Some(x) = instructions.get_mut(i) {
                                x.op_code = OpCode::LIST;
                                x.payload_size = u24::from_usize(arity);
                                continue;
                            }
                        }

                        // Specialize lists, cons, hashmap, etc. - anything that we expect to be used often in
                        // real code.
                        _ if ident == *LIST_SYMBOL || ident == *PRIM_LIST_SYMBOL => {}

                        _ => {}
                    }
                }

                if let Some(x) = instructions.get_mut(i) {
                    if tail_op == OpCode::TAILCALL {
                        x.op_code = OpCode::CALLGLOBALTAIL;
                    } else {
                        x.op_code = OpCode::CALLGLOBALTAILNOARITY;
                    }
                    x.payload_size = index;
                }

                // if let Some(x) = instructions.get_mut(i + 1) {
                // x.op_code = OpCode::Arity;
                // x.payload_size = arity;
                // }
            }
            _ => {}
        }
    }
}

#[macro_export]
macro_rules! define_primitive_symbols {
    ($(($prim_name:tt, $name:tt) => $str:expr,) * ) => {
        $(
            pub static $name: once_cell::sync::Lazy<InternedString> = once_cell::sync::Lazy::new(|| InternedString::from_static($str));

            pub static $prim_name: once_cell::sync::Lazy<InternedString> = once_cell::sync::Lazy::new(|| InternedString::from_static(concat!("#%prim.", $str)));
        )*
    };
}

#[macro_export]
macro_rules! define_symbols {
    ($($name:tt => $str:expr,) * ) => {
        $(
            pub static $name: once_cell::sync::Lazy<InternedString> = once_cell::sync::Lazy::new(|| InternedString::from_static($str));
        )*
    };
}

define_primitive_symbols! {
    (PRIM_PLUS, PLUS) => "+",
    (PRIM_MINUS, MINUS) => "-",
    (PRIM_DIV, DIV) => "/",
    (PRIM_STAR, STAR) => "*",
    (PRIM_EQUAL, EQUAL) => "equal?",
    (PRIM_NUM_EQUAL, NUM_EQUAL) => "=",
    (PRIM_LTE, LTE) => "<=",
    (PRIM_GTE, GTE) => ">=",
    (PRIM_LT, LT) => "<",
    (PRIM_GT, GT) => ">",
    (PRIM_CAR, CAR_SYMBOL) => "car",
    (PRIM_CDR, CDR_SYMBOL) => "cdr",
    (PRIM_NOT, NOT_SYMBOL) => "not",
    (PRIM_NULL, NULL_SYMBOL) => "null?",
}

define_symbols! {
    UNREADABLE_MODULE_GET => "##__module-get",
    STANDARD_MODULE_GET => "%module-get%",
    CONTRACT_OUT => "contract/out",
    REQUIRE_IDENT_SPEC => "%require-ident-spec",
    PROVIDE => "provide",
    FOR_SYNTAX => "for-syntax",
    PREFIX_IN => "prefix-in",
    ONLY_IN => "only-in",
    DATUM_SYNTAX => "datum->syntax",
    SYNTAX_SPAN => "#%syntax-span",
    IF => "if",
    DEFINE => "define",
    LET => "let",
    QUOTE =>"quote",
    RETURN => "return!",
    REQUIRE => "require",
    SET => "set!",
    PLAIN_LET => "%plain-let",
    LAMBDA => "lambda",
    LAMBDA_SYMBOL => "λ",
    LAMBDA_FN => "fn",
    BEGIN => "begin",
    DOC_MACRO => "@doc",
    REQUIRE_BUILTIN => "require-builtin",
    REQUIRE_DYLIB => "#%require-dylib",
    STRUCT_KEYWORD => "struct",
    BETTER_LAMBDA => "#%better-lambda",
    DEFINE_VALUES => "define-values",
    AS_KEYWORD => "as",
    SYNTAX_CONST_IF => "syntax-const-if",
    UNQUOTE => "unquote",
    UNQUOTE_COMMA => "#%unquote-comma",
    RAW_UNQUOTE => "#%unquote",
    UNQUOTE_SPLICING => "unquote-splicing",
    RAW_UNQUOTE_SPLICING => "#%unquote-splicing",
    QUASIQUOTE => "quasiquote",
    RAW_QUOTE => "#%quote",
    QUASISYNTAX => "quasisyntax",
    UNSYNTAX => "unsyntax",
    RAW_UNSYNTAX => "#%unsyntax",
    UNSYNTAX_SPLICING => "unsyntax-splicing",
    RAW_UNSYNTAX_SPLICING => "#%unsyntax-splicing",
    SYNTAX_QUOTE => "syntax",
    CONS_SYMBOL => "cons",
    PRIM_CONS_SYMBOL => "#%prim.cons",
    LIST_SYMBOL => "list",
    PRIM_LIST_SYMBOL => "#%prim.list",
    PRIM_LIST_REF => "#%prim.list-ref",
    PRIM_VECTOR_REF => "#%prim.vector-ref",
    BOX => "#%box",
    PRIM_BOX => "#%prim.box",
    UNBOX => "#%unbox",
    PRIM_UNBOX => "#%prim.unbox",
    SETBOX => "#%set-box!",
    PRIM_SETBOX => "#%prim.set-box!",
    DEFMACRO => "defmacro",
    BEGIN_FOR_SYNTAX => "begin-for-syntax",
    ELLIPSES_SYMBOL => "...",
    DOT => ".",
}

pub fn flatten_equal_const(instructions: &mut [Instruction]) {
    if cfg!(feature = "jit2") {
        return;
    }
    for i in 0..instructions.len() {
        let push_const = instructions.get(i);
        let equal = instructions.get(i + 1);

        match (push_const.map(|x| x.op_code), equal.map(|x| x.op_code)) {
            (Some(OpCode::PUSHCONST), Some(OpCode::EQUAL2)) => {
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::EQUALCONST;
                }
            }

            _ => {}
        }
    }
}

pub fn inline_num_operations(instructions: &mut [Instruction]) {
    for i in 0..instructions.len() - 1 {
        let push = instructions.get(i);
        let func = instructions.get(i + 1);

        if let (
            Some(Instruction {
                op_code:
                    OpCode::PUSH
                    | OpCode::CALLGLOBAL
                    | OpCode::CALLGLOBALTAIL
                    | OpCode::CALLGLOBALNOARITY
                    | OpCode::CALLGLOBALTAILNOARITY,
                ..
            }),
            Some(Instruction {
                op_code: op @ OpCode::FUNC | op @ OpCode::TAILCALL,
                contents:
                    Some(Expr::Atom(RawSyntaxObject {
                        ty: TokenType::Identifier(ident),
                        ..
                    })),
                payload_size,
                ..
            }),
        ) = (push, func)
        {
            let payload_size = payload_size.to_u32();

            let replaced = match *ident {
                #[cfg(not(feature = "jit2"))]
                x if x == *PRIM_PLUS && payload_size == 2 && *op == OpCode::TAILCALL => {
                    Some(OpCode::BINOPADDTAIL)
                }

                #[cfg(not(feature = "jit2"))]
                x if x == *PRIM_PLUS && payload_size == 2 => Some(OpCode::BINOPADD),

                x if x == *PRIM_PLUS && payload_size > 0 => Some(OpCode::ADD),
                // x if x == *PRIM_MINUS && *payload_size == 2 => Some(OpCode::BINOPSUB),
                x if x == *PRIM_MINUS && payload_size > 0 => Some(OpCode::SUB),
                x if x == *PRIM_DIV && payload_size > 0 => Some(OpCode::DIV),
                x if x == *PRIM_STAR && payload_size > 0 => Some(OpCode::MUL),
                x if x == *PRIM_NUM_EQUAL && payload_size == 2 => Some(OpCode::NUMEQUAL),
                x if x == *PRIM_EQUAL && payload_size == 2 => Some(OpCode::EQUAL2),
                x if x == *PRIM_EQUAL && payload_size > 0 => Some(OpCode::EQUAL),
                x if x == *PRIM_LTE && payload_size > 0 => Some(OpCode::LTE),
                x if x == *PRIM_GTE && payload_size > 0 => Some(OpCode::GTE),
                x if x == *PRIM_GT && payload_size > 0 => Some(OpCode::GT),
                x if x == *PRIM_LT && payload_size > 0 => Some(OpCode::LT),
                _ => None,
            };

            if let Some(new_op_code) = replaced {
                // let payload_size = *payload_size;
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = new_op_code;
                    x.payload_size = u24::from_u32(payload_size);
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::PASS;
                }
            }
        }
    }
}

pub const fn sequence_to_opcode(pattern: &[(OpCode, usize)]) -> &'static [steel_gen::Pattern] {
    match pattern {
        &[(OpCode::MOVEREADLOCAL, _)] => &[steel_gen::Pattern::Single(OpCode::MOVEREADLOCAL)],
        _ => todo!(),
    }
}

#[allow(unused)]
pub fn tile_super_instructions(instructions: &mut [Instruction]) {
    #[cfg(feature = "dynamic")]
    {
        pub fn tile<const N: usize>(instructions: &mut [Instruction]) {
            // let mut list: List<(usize, OpCode)> = List::new();

            let mut buffer = [(OpCode::VOID, 0); N];

            let mut pattern_buffer = Vec::with_capacity(N);

            // Cell::from_mut()

            if N > instructions.len() {
                return;
            }

            for i in 0..instructions.len() - N {
                for j in 0..N {
                    buffer[j] = (
                        instructions[i + j].op_code,
                        instructions[i + j].payload_size,
                    );
                }

                // If this is a candidate to match the pattern, let's try to apply it!
                if let Some(op_code) = steel_gen::opcode::sequence_to_opcode(&buffer) {
                    // Check if this pattern genuinely matches one of the code gen'd ones
                    steel_gen::Pattern::from_opcodes_with_buffer(&buffer, &mut pattern_buffer);

                    if crate::steel_vm::vm::pattern_exists(&pattern_buffer) {
                        // log::debug!(target: "super-instructions", "Applying tiling for: {:?}", op_code);

                        // println!("Applying tiling for: {:?}", op_code);
                        // println!("{:?}", pattern_buffer);

                        instructions[i].op_code = op_code;

                        continue;
                    }
                }
            }

            // for (index, op) in list {
            //     instructions[index].op_code = op;
            // }
        }

        // Super instruction tiling here!

        if _TILE_SUPER_INSTRUCTIONS {
            tile::<11>(instructions);
            tile::<10>(instructions);
            tile::<9>(instructions);
            tile::<8>(instructions);
            tile::<7>(instructions);
            tile::<6>(instructions);
            tile::<5>(instructions);
            tile::<4>(instructions);
            tile::<3>(instructions);
            tile::<2>(instructions);
        }
    }
}

pub fn merge_conditions_with_if(instructions: &mut [Instruction]) {
    if cfg!(feature = "jit2") {
        return;
    }

    for i in 0..instructions.len() - 1 {
        let condition = instructions.get(i);
        let guard = instructions.get(i + 2);

        if let (
            Some(Instruction {
                op_code: OpCode::LTEIMMEDIATE,
                ..
            }),
            Some(Instruction {
                op_code: OpCode::IF,
                ..
            }),
        ) = (condition, guard)
        {
            if let Some(x) = instructions.get_mut(i) {
                x.op_code = OpCode::LTEIMMEDIATEIF;
            }

            // let replaced = match *ident {
            //     x if x == *PLUS && *payload_size == 2 => Some(OpCode::BINOPADD),
            //     x if x == *PLUS => Some(OpCode::ADD),
            //     x if x == *MINUS => Some(OpCode::SUB),
            //     x if x == *DIV => Some(OpCode::DIV),
            //     x if x == *STAR => Some(OpCode::MUL),
            //     x if x == *EQUAL => Some(OpCode::EQUAL),
            //     x if x == *LTE => Some(OpCode::LTE),
            //     _ => None,
            // };

            // if let Some(new_op_code) = replaced {
            //     let payload_size = *payload_size;
            //     if let Some(x) = instructions.get_mut(i) {
            //         x.op_code = new_op_code;
            //         x.payload_size = payload_size;
            //     }

            //     if let Some(x) = instructions.get_mut(i + 1) {
            //         x.op_code = OpCode::PASS;
            //     }
            // }
        }
    }
}

pub struct ProgramBuilder(Vec<Vec<DenseInstruction>>);
impl Default for ProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

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
        use std::io::prelude::*;

        let mut file = File::create(format!("{filename}.txt")).unwrap();

        let buffer = bincode::serialize(self).unwrap();

        file.write_all(&buffer)?;
        Ok(())
    }

    pub fn read_from_file(filename: &str) -> Result<Self> {
        use std::io::prelude::*;

        let mut file = File::open(format!("{filename}.txt")).unwrap();

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
#[derive(Clone)]
pub struct RawProgramWithSymbols {
    pub(crate) instructions: Vec<Vec<Instruction>>,
    pub(crate) constant_map: ConstantMap,
    version: String, // TODO -> this should be semver
}

#[derive(Serialize, Deserialize)]
pub struct SerializableRawProgramWithSymbols {
    instructions: Vec<Vec<Instruction>>,
    constant_map: Vec<u8>,
    version: String,
}

impl SerializableRawProgramWithSymbols {
    pub fn write_to_file(&self, filename: &str) -> Result<()> {
        use std::io::prelude::*;

        let mut file = File::create(format!("{filename}.txt")).unwrap();

        let buffer = bincode::serialize(self).unwrap();

        file.write_all(&buffer)?;
        Ok(())
    }

    pub fn read_from_file(filename: &str) -> Result<Self> {
        use std::io::prelude::*;

        let mut file = File::open(format!("{filename}.txt")).unwrap();
        let mut buffer = Vec::new();
        let _ = file.read_to_end(&mut buffer).unwrap();
        let program: Self = bincode::deserialize(&buffer).unwrap();

        Ok(program)
    }

    pub fn into_raw_program(self) -> RawProgramWithSymbols {
        let constant_map = ConstantMap::from_bytes(&self.constant_map).unwrap();
        RawProgramWithSymbols {
            // struct_functions: self.struct_functions,
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
fn _read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

// trait Profiler {
//     #[inline(always)]
//     fn process() -> bool;

//     fn report(&self);
// }

impl RawProgramWithSymbols {
    pub fn new(
        // struct_functions: Vec<StructFuncBuilderConcrete>,
        instructions: Vec<Vec<Instruction>>,
        constant_map: ConstantMap,
        version: String,
    ) -> Self {
        Self {
            // struct_functions,
            instructions,
            constant_map,
            version,
        }
    }

    pub fn profile_instructions(&self) {
        let iter = self
            .instructions
            .iter()
            .flat_map(|x| x.iter())
            .filter(|x| !matches!(x.op_code, OpCode::PASS));

        let mut occurrences = HashMap::new();
        for instr in iter {
            *occurrences.entry(instr.op_code).or_default() += 1;
        }

        let total: usize = occurrences.values().sum();

        let mut counts = occurrences
            .into_iter()
            .map(|x| (x.0, (x.1 as f64 / total as f64) * 100.0))
            .collect::<Vec<(OpCode, f64)>>();

        counts.sort_by(|x, y| y.1.partial_cmp(&x.1).unwrap());

        println!("{counts:#?}");
    }

    // Definitely can be improved
    // pub fn parse_from_self_hosted_file<P>(file: P) -> Result<Self>
    // where
    //     P: AsRef<Path>,
    // {
    //     let mut lines = read_lines(file)?;

    //     // First line should be the constant map label
    //     // let constant_map =

    //     if let Some(constant_map_label) = lines.next() {
    //         if constant_map_label? != "'ConstantMap" {
    //             stop!(Generic => "Compiled file expected constant map label")
    //         }
    //     } else {
    //         stop!(Generic => "Missing constant map label")
    //     }

    //     // Temportary interner
    //     let mut intern = HashMap::new();

    //     let constant_map = if let Some(constant_map) = lines.next() {
    //         let constant_map = constant_map?;

    //         let constant_map = constant_map
    //             .trim_start_matches('[')
    //             .trim_end_matches(']')
    //             .split(',')
    //             .map(|x| {
    //                 // Parse the input
    //                 let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
    //                     Parser::new(&x, &mut intern).collect();
    //                 let parsed = parsed?;

    //                 Ok(SteelVal::try_from(parsed[0].clone()).unwrap())
    //             })
    //             .collect::<Result<Vec<_>>>()
    //             .map(ConstantMap::from_vec)?;

    //         constant_map
    //     } else {
    //         stop!(Generic => "Missing constant map")
    //     };

    //     if let Some(instructions_label) = lines.next() {
    //         if instructions_label? != "'Instructions" {
    //             stop!(Generic => "Compiled file expected instructions label")
    //         }
    //     } else {
    //         stop!(Generic => "Missing instructions label")
    //     }

    //     let mut instruction_set = Vec::new();

    //     let mut instructions = Vec::new();

    //     // Skip past the first 'Expression
    //     lines.next();

    //     for instruction_string in lines {
    //         let instruction_string = instruction_string?;

    //         if instruction_string == "'Expression" {
    //             // instructions = Vec::new();
    //             // if instruction_set.is_empty() {
    //             instruction_set.push(instructions);
    //             instructions = Vec::new();
    //             // }

    //             continue;
    //         }

    //         let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
    //             Parser::new(&instruction_string, &mut intern).collect();
    //         let parsed = parsed?;

    //         let value = SteelVal::try_from(parsed[0].clone()).unwrap();

    //         if let SteelVal::ListV(v) = value {
    //             // Get the op code here
    //             let op_code =
    //                 OpCode::from_str(v.get(1).unwrap().symbol_or_else(|| unreachable!()).unwrap());

    //             // Get the payload
    //             let payload = v.get(2).unwrap().int_or_else(|| unreachable!()).unwrap() as usize;

    //             // Get the contents
    //             // If I can't parse the object, just move on
    //             let contents = ExprKind::try_from(v.get(3).unwrap())
    //                 .ok()
    //                 .and_then(|x| x.atom_syntax_object().cloned());

    //             let instruction = Instruction::new_from_parts(op_code, payload, contents);

    //             instructions.push(instruction)
    //         } else {
    //             stop!(Generic => "Instruction serialized incorrectly")
    //         }
    //     }

    //     instruction_set.push(instructions);

    //     Ok(Self::new(
    //         instruction_set,
    //         constant_map,
    //         "0.0.1".to_string(),
    //     ))
    // }

    pub fn into_serializable_program(self) -> Result<SerializableRawProgramWithSymbols> {
        Ok(SerializableRawProgramWithSymbols {
            instructions: self.instructions,
            constant_map: self.constant_map.to_bytes()?,
            version: self.version,
        })
    }

    pub fn debug_print(&self) {
        self.instructions
            .iter()
            .for_each(|i| println!("{}\n\n", crate::core::instructions::disassemble(i)))
    }

    pub fn debug_print_log(&self) {
        self.instructions
            .iter()
            .for_each(|i| log::info!("{}\n\n", crate::core::instructions::disassemble(i)))
    }

    /// Applies a peephole style optimization to the underlying instruction set
    pub fn with_optimization<F: Fn(&mut [Instruction])>(&mut self, f: F) {
        for instructions in &mut self.instructions {
            f(instructions)
        }
    }

    // Apply the optimizations to raw bytecode
    pub(crate) fn apply_optimizations(&mut self) -> &mut Self {
        // Run down the optimizations here
        for instructions in &mut self.instructions {
            inline_num_operations(instructions);
            flatten_equal_const(instructions);
            unbox_function_call(instructions);
            convert_call_globals(instructions);
            specialize_read_local(instructions);
            merge_conditions_with_if(instructions);
            specialize_constants(instructions).unwrap();
            tile_super_instructions(instructions);
        }

        self
    }

    pub fn debug_generate_instructions(
        mut self,
        symbol_map: &mut SymbolMap,
    ) -> Result<Vec<String>> {
        let mut interner = DebruijnIndicesInterner::default();

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_first_pass_defines(index, expression, symbol_map)?
        }

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_second_pass_defines(index, expression, symbol_map)?
        }

        // TODO try here - the loop condition local const arity two seems to rely on the
        // existence of having been already adjusted by the interner
        for instructions in &mut self.instructions {
            // loop_condition_local_const_arity_two(instructions);
            specialize_constants(instructions)?;
        }

        // Put the new struct functions at the front
        // struct_instructions.append(&mut self.instructions);
        // self.instructions = struct_instructions;

        Ok(self
            .instructions
            .into_iter()
            .map(|i| crate::core::instructions::disassemble(&i))
            .collect())
    }

    pub fn debug_build(mut self, _name: String, symbol_map: &mut SymbolMap) -> Result<()> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        // let mut struct_instructions = Vec::new();

        // for builder in &self.struct_functions {
        //     // Add the eventual function names to the symbol map
        //     let indices = symbol_map.insert_struct_function_names_from_concrete(builder);

        //     // Get the value we're going to add to the constant map for eventual use
        //     // Throw the bindings in as well
        //     let constant_values = builder.to_constant_val(indices);
        //     let idx = self.constant_map.add_or_get(constant_values);

        //     struct_instructions.push(vec![Instruction::new_struct(idx), Instruction::new_pop()]);
        // }

        let mut interner = DebruijnIndicesInterner::default();

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_first_pass_defines(index, expression, symbol_map)?
        }

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_second_pass_defines(index, expression, symbol_map)?
        }

        // TODO try here - the loop condition local const arity two seems to rely on the
        // existence of having been already adjusted by the interner
        for instructions in &mut self.instructions {
            // loop_condition_local_const_arity_two(instructions);
            specialize_constants(instructions)?;
        }

        // Put the new struct functions at the front
        // struct_instructions.append(&mut self.instructions);
        // self.instructions = struct_instructions;

        self.instructions
            .iter()
            .for_each(|i| println!("{}\n\n", crate::core::instructions::disassemble(i)));

        #[cfg(feature = "profiling")]
        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Executable Build Time: {:?}", now.elapsed());
        }

        // let mut sorted_symbol_map = symbol_map.map.iter().collect::<Vec<_>>();
        // sorted_symbol_map.sort_by_key(|x| x.1);

        // println!("Symbol Map: {:#?}", sorted_symbol_map);

        Ok(())
    }

    // TODO -> check out the spans part of this
    // also look into having the constant map be correct mapping
    // I think the run time will have to swap the constant map in and out
    pub fn build(mut self, name: String, symbol_map: &mut SymbolMap) -> Result<Executable> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let mut interner = DebruijnIndicesInterner::default();

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_first_pass_defines(index, expression, symbol_map)?
        }

        for (index, expression) in self.instructions.iter_mut().enumerate() {
            interner.collect_second_pass_defines(index, expression, symbol_map)?
        }

        // if std::env::var("CODE_GEN_V2").is_err() {
        // TODO try here - the loop condition local const arity two seems to rely on the
        // existence of having been already adjusted by the interner
        for instructions in &mut self.instructions {
            // TODO: Re-enable optimizations
            // loop_condition_local_const_arity_two(instructions);
            specialize_constants(instructions)?;
            // gimmick_super_instruction(instructions);
            // move_read_local_call_global(instructions);
            specialize_read_local(instructions);

            merge_call_global_if(instructions);

            // specialize_call_global_local(instructions);
        }
        // }

        let (spans, instructions) = extract_spans(self.instructions);

        // let mut sorted_symbol_map = symbol_map.map.iter().collect::<Vec<_>>();
        // sorted_symbol_map.sort_by_key(|x| x.1);

        // println!("Symbol Map: {:#?}", sorted_symbol_map);

        #[cfg(feature = "profiling")]
        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Executable Build Time: {:?}", now.elapsed());
        }

        Ok(Executable {
            name: Shared::new(name),
            version: Shared::new(self.version),
            #[cfg(not(target_family = "wasm"))]
            time_stamp: Some(SystemTime::now()),
            #[cfg(target_family = "wasm")]
            time_stamp: None,
            instructions: instructions
                .into_iter()
                .map(|x| StandardShared::from(x.into_boxed_slice()))
                .collect(),
            constant_map: self.constant_map,
            spans,
        })
    }
}

// TODO -> replace spans on instructions with index into span vector
// this is kinda nasty but it _should_ work
fn extract_spans(
    instructions: Vec<Vec<Instruction>>,
) -> (Vec<Shared<[Span]>>, Vec<Vec<DenseInstruction>>) {
    // let mut span_vec = Vec::with_capacity(instructions.iter().map(|x| x.len()).sum());

    // for instruction_set in &instructions {
    //     for instruction in instruction_set {
    //         if let Some(syn) = &instruction.contents {
    //             span_vec.push(syn.span)
    //         } else {
    //             span_vec.push(Span::default())
    //         }
    //     }
    // }

    let span_vec = instructions
        .iter()
        .map(|x| {
            x.iter()
                .map(|x| {
                    x.contents
                        .as_ref()
                        .map(|x| match x {
                            Expr::Atom(a) => a.span,
                            Expr::List(l) => get_span(l),
                        })
                        .unwrap_or_default()
                })
                .collect()
        })
        .collect();

    let instructions: Vec<_> = instructions
        .into_iter()
        .map(|x| {
            // let len = x.len();
            x.into_iter()
                .map(|i| DenseInstruction::new(i.op_code, i.payload_size))
                .collect()
        })
        .collect();

    (span_vec, instructions)
}

// A program stripped of its debug symbols, but only constructable by running a pass
// over it with the symbol map to intern all of the symbols in the order they occurred
#[allow(unused)]
#[derive(Clone)]
pub struct Executable {
    pub(crate) name: Shared<String>,
    pub(crate) version: Shared<String>,
    pub(crate) time_stamp: Option<SystemTime>, // TODO -> don't use system time, probably not as portable, prefer date time
    pub(crate) instructions: Vec<StandardShared<[DenseInstruction]>>,
    pub(crate) constant_map: ConstantMap,
    pub(crate) spans: Vec<Shared<[Span]>>,
}

impl Executable {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn time_stamp(&self) -> &Option<SystemTime> {
        &self.time_stamp
    }
}
