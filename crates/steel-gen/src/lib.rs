// TODO: Create stack to ssa representation of the op codes, via macros
#![allow(unused)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod opcode;
#[cfg(feature = "codegen")]
pub mod permutations;

use alloc::vec::Vec;
use core::fmt;

pub use opcode::OpCode;

#[cfg(feature = "codegen")]
use alloc::{
    borrow::Cow,
    string::{String, ToString},
};
#[cfg(feature = "codegen")]
use codegen::{Function, Scope};
#[cfg(feature = "codegen")]
use core::fmt::Write;

// If we can provide hints on the types, this can help with constant folding of operations
// that we know the types of
//
// For instance, this can also allow specializing _lots_ of the list operations if we can pull it off
#[cfg(feature = "codegen")]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TypeHint {
    Int,
    Bool,
    Float,
    Void,
    None,
}

#[cfg(feature = "codegen")]
#[derive(PartialEq, Clone, Debug)]
struct LocalVariable {
    id: u16,
    type_hint: TypeHint,
}

#[cfg(feature = "codegen")]
impl LocalVariable {
    pub fn kind(mut self, type_hint: TypeHint) -> Self {
        self.type_hint = type_hint;
        self
    }
}

#[cfg(feature = "codegen")]
struct GenSym {
    count: u16,
}

#[cfg(feature = "codegen")]
impl GenSym {
    pub fn new() -> Self {
        Self { count: 0 }
    }

    // Generate a fresh variable
    fn fresh(&mut self) -> LocalVariable {
        let count = self.count;
        self.count += 1;
        LocalVariable {
            id: count,
            type_hint: TypeHint::None,
        }
    }
}

#[cfg(feature = "codegen")]
impl fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x{}", self.id)
    }
}

#[cfg(feature = "codegen")]
fn op_code_to_handler(op_code: Pattern) -> String {
    format!("opcode_to_ssa_handler!({op_code})")
}

#[cfg(feature = "codegen")]
struct StackToSSAConverter {
    generator: GenSym,
    stack: Vec<LocalVariable>,
    local_offset: Option<usize>,
}

#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern {
    Single(OpCode),
    Double(OpCode, usize),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Single(op) => write!(f, "{op:?}"),
            Pattern::Double(op, payload) => write!(f, "{op:?}{payload}"),
        }
    }
}

#[cfg(feature = "codegen")]
#[derive(Default)]
struct FunctionLines {
    lines: Vec<String>,
}

#[cfg(feature = "codegen")]
impl FunctionLines {
    fn line(&mut self, line: impl Into<String>) {
        self.lines.push(line.into())
    }
}

#[cfg(feature = "codegen")]
impl StackToSSAConverter {
    pub fn new() -> Self {
        Self {
            generator: GenSym::new(),
            stack: Vec::new(),
            local_offset: None,
        }
    }

    pub fn reset(&mut self) {
        self.generator.count = 0;
        self.stack.clear();
        self.local_offset = None;
    }

    // Get the current local offset
    // pub fn local_offset(&self) -> usize {
    //     self.local_offset
    // }

    pub fn push(&mut self) -> LocalVariable {
        let var = self.generator.fresh();
        self.stack.push(var.clone());
        var
    }

    pub fn push_with_hint(&mut self, type_hint: TypeHint) -> LocalVariable {
        let var = self.generator.fresh().kind(type_hint);
        self.stack.push(var.clone());
        var
    }

    pub fn pop(&mut self) -> LocalVariable {
        self.stack.pop().unwrap()
    }

    pub fn process_sequence(&mut self, op_codes: &[Pattern]) -> Function {
        use OpCode::*;
        use Pattern::*;

        // let mut scope = Scope::new();
        let mut function = Function::new(
            op_codes
                .iter()
                .map(|x| format!("{x}").to_lowercase())
                .join("_"),
        );

        function.vis("pub(crate)");

        function.arg("ctx", codegen::Type::new("&mut VmCore<'_>"));
        function.arg("payload", codegen::Type::new("usize"));
        function.ret(codegen::Type::new("Result<()>"));

        let mut lines = FunctionLines::default();

        lines.line("use OpCode::*;");
        lines.line("use steel_gen::Pattern::*;");

        let mut max_local_offset_read = 0;
        let mut max_ip_read = 0;

        // TODO: Don't use the function line pushing directly, use a vec manually, and then insert an assertion for the instruction count
        // to help reduce the bounds checks that we have to pay

        // READLOCAL0,
        // LOADINT2,
        // MUL,
        // MOVEREADLOCAL1,
        // LOADINT1,
        // SUB,
        // READLOCAL2,
        // LOADINT1,
        // SUB,
        // READLOCAL3,
        // CALLGLOBAL

        let last = op_codes.len();

        for (index, op) in op_codes.iter().enumerate() {
            match op {
                Double(BEGINSCOPE, n) => {
                    self.local_offset = Some(*n);
                    lines.line("ctx.ip += 1;");
                }

                Single(PUSHCONST) => {
                    let local = self.push();
                    lines.line(format!(
                        "let {local} = opcode_to_ssa_handler!(PUSHCONST)(ctx)?;"
                    ));
                }

                // Single(LetVar) => {
                //     let local = self.pop();
                //     function.line(format!("ctx.stack.push({}.into());", local));
                // }
                Single(VOID) => {
                    let local = self.push_with_hint(TypeHint::Void);
                    lines.line(format!("let {local} = SteelVal::Void;"));
                    lines.line("ctx.ip += 1;");
                }
                Single(LOADINT0) => {
                    let local = self.push_with_hint(TypeHint::Int);
                    // Load the immediate for 0
                    lines.line(format!("let {} = {};", local, 0));
                    lines.line("ctx.ip += 1;");
                }
                Single(LOADINT1) => {
                    let local = self.push_with_hint(TypeHint::Int);
                    // Load the immediate for 1
                    lines.line(format!("let {} = {};", local, 1));
                    lines.line("ctx.ip += 1;");

                    max_ip_read += 1;
                }
                Single(LOADINT2) => {
                    let local = self.push_with_hint(TypeHint::Int);
                    // Load the immediate for 2
                    lines.line(format!("let {} = {};", local, 2));
                    lines.line("ctx.ip += 1;");

                    max_ip_read += 1;
                }
                Double(CALLGLOBAL, n) => {
                    let args = self
                        .stack
                        .split_off(self.stack.len() - n)
                        .into_iter()
                        .map(|x| x.to_string() + ".into(), ")
                        .collect::<String>();

                    if index == last - 1 {
                        // For whatever is left, push on to the SteelThread stack
                        for value in &self.stack {
                            match value.type_hint {
                                TypeHint::Int | TypeHint::Bool | TypeHint::Float => {
                                    lines.line(format!("ctx.thread.stack.push({value}.into());"))
                                }
                                // It is already confirmed to be... something thats non primitive.
                                _ => lines.line(format!("ctx.thread.stack.push({value});")),
                            };
                        }

                        self.stack.clear();

                        lines.line(format!(
                            "opcode_to_ssa_handler!(CALLGLOBAL, Tail)(ctx, &mut [{args}])?;"
                        ));
                    } else {
                        let local = self.push();

                        lines.line(format!(
                            "let Some({local}) = opcode_to_ssa_handler!(CALLGLOBAL)(ctx, &mut [{args}])? else {{
                                return Ok(());
                            }};"
                        ));
                    }
                }

                // TODO: Handle the numeric equality case
                Double(NUMEQUAL, n) => {
                    let args = self
                        .stack
                        .split_off(self.stack.len() - n)
                        .into_iter()
                        .map(|x| x.to_string() + ".into(), ")
                        .collect::<String>();

                    if index == last - 1 {
                        // For whatever is left, push on to the SteelThread stack
                        for value in &self.stack {
                            match value.type_hint {
                                TypeHint::Int | TypeHint::Bool | TypeHint::Float => {
                                    lines.line(format!("ctx.thread.stack.push({value}.into());"))
                                }
                                // It is already confirmed to be... something thats non primitive.
                                _ => lines.line(format!("ctx.thread.stack.push({value});")),
                            };
                        }

                        self.stack.clear();

                        lines.line(format!(
                            "opcode_to_ssa_handler!(CALLGLOBAL, Tail)(ctx, &mut [{args}])?;"
                        ));
                    } else {
                        let local = self.push_with_hint(TypeHint::Bool);

                        lines.line(format!(
                            "let {local} = opcode_to_ssa_handler!(NUMEQUAL)(ctx, {args})?;"
                        ));
                    }
                }

                Double(READLOCAL, n) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > *n
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(*n);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(*n).unwrap();
                        lines.line(format!("let {local} = {var}.clone();"));
                    }
                }
                Single(READLOCAL0) => {
                    // If we're dealing with a let var, then we can only specialize it _iff_ the
                    // value we're reading exceeds the current function arguments.
                    // For instance, if we have a function like this:
                    // (lambda (x y z) (let ((foo 10)) (+ x y z foo))
                    // Then foo should be something like READLOCAL(4)
                    // In this case, we can safely snag it from the Rust stack
                    // Otherwise, we need to read it from the steel stack.
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 0
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(0);
                    } else {
                        let local = self.push();
                        let var = self.stack.first().unwrap();

                        if &local == var {
                            lines.line(format!(
                                "let {} = {}(ctx)?;",
                                local,
                                op_code_to_handler(*op)
                            ));
                        } else {
                            lines.line(format!("let {local} = {var}.clone();"));
                        }
                    }
                }
                Single(READLOCAL1) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 1
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(1);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(1).unwrap();

                        if &local == var {
                            // let local = self.pop();

                            let var = self.stack.first().unwrap();

                            lines.line(format!("let {local} = {var}.clone();"));
                            lines.line("ctx.ip += 1;")

                            // lines.line(format!(
                            //     "let {} = {}(ctx)?;",
                            //     local,
                            //     op_code_to_handler(*op)
                            // ));
                        } else {
                            lines.line(format!("let {local} = {var}.clone();"));
                        }

                        // lines.line(format!("let {local} = {var}.clone();"));
                    }
                }
                Single(READLOCAL2) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 2
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(2);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(2).unwrap();
                        lines.line(format!("let {local} = {var}.clone();"));
                    }
                }
                Single(READLOCAL3) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 3
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(3);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(3).unwrap();
                        lines.line(format!("let {local} = {var}.clone();"));
                    }
                }
                Double(MOVEREADLOCAL, n) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > *n
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(*n);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(*n).unwrap();
                        lines.line(format!("let {local} = {var};"));
                    }
                }
                Single(MOVEREADLOCAL0) => {
                    // If we're dealing with a let var, then we can only specialize it _iff_ the
                    // value we're reading exceeds the current function arguments.
                    // For instance, if we have a function like this:
                    // (lambda (x y z) (let ((foo 10)) (+ x y z foo))
                    // Then foo should be something like READLOCAL(4)
                    // In this case, we can safely snag it from the Rust stack
                    // Otherwise, we need to read it from the steel stack.
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 0
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(0);
                    } else {
                        let local = self.push();
                        let var = self.stack.first().unwrap();
                        lines.line(format!("let {local} = {var};"));
                    }
                }
                Single(MOVEREADLOCAL1) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 1
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(1);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(1).unwrap();
                        lines.line(format!("let {local} = {var};"));
                    }
                }
                Single(MOVEREADLOCAL2) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 2
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(2);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(2).unwrap();
                        lines.line(format!("let {local} = {var};"));
                    }
                }
                Single(MOVEREADLOCAL3) => {
                    if self.local_offset.is_none()
                        || self.local_offset.is_some() && self.local_offset.unwrap() > 3
                    {
                        let local = self.push();
                        lines.line(format!(
                            "let {} = {}(ctx)?;",
                            local,
                            op_code_to_handler(*op)
                        ));

                        max_local_offset_read = max_local_offset_read.max(3);
                    } else {
                        let local = self.push();
                        let var = self.stack.get(3).unwrap();
                        lines.line(format!("let {local} = {var};"));
                    }
                }
                Single(PUSH | READCAPTURED | TAILCALL | CALLGLOBAL) => {
                    let local = self.push();
                    lines.line(format!(
                        "let {} = {}(ctx)?;",
                        local,
                        op_code_to_handler(*op)
                    ));
                }
                // Single(PUSH) => {
                //     // Consider embedding some type hints on these for even more specialization if possible
                //     let local = self.push();
                //     function.line(format!("{} = {}(ctx)?;", local, op_code_to_handler(*op)));
                // }
                // Single(READCAPTURED) => {
                //     let local = self.push();
                //     function.line(format!("{} = {}(ctx)?;", local, op_code_to_handler(*op)));
                // }
                Single(IF) => {
                    // (if <test> <then> <else>)
                    let test_condition = self.pop();

                    // If we're dealing with an int, just unbox it directly
                    match test_condition.type_hint {
                        TypeHint::Bool => {
                            lines.line(format!(
                                "if_to_ssa_handler!(IF, Bool)(ctx, {test_condition});"
                            ));
                        }
                        _ => {
                            lines.line(format!(
                                "if_to_ssa_handler!(IF)(ctx, {test_condition}.into());"
                            ));
                        }
                    }
                }
                Double(LTE | EQUAL, 2) => {
                    let right = self.pop();
                    let left = self.pop();

                    lines.line("ctx.ip += 2;");

                    max_ip_read += 2;

                    match (left.type_hint, right.type_hint) {
                        (a, b) if a == b => {
                            let local = self.push_with_hint(TypeHint::Bool);

                            lines.line(format!("let {local} = {left} == {right};"));
                            // TODO: This is going to need to be
                            // lines.line("ctx.ip += 1;");
                            max_ip_read += 1;
                        }
                        (TypeHint::Int, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, Int)");

                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Int, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, Float)");

                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Int, TypeHint::None) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, None)");

                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, Int)");

                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, Float)");

                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::None) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, None)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::None, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, None, Int)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::None, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, None, Int)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::Bool);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (_, _) => todo!(),
                    }
                }

                Double(ADDIMMEDIATE, imm) => {
                    let right = self.pop();

                    lines.line("ctx.ip += 2;");

                    match right.type_hint {
                        TypeHint::Int => todo!(),
                        TypeHint::Bool => todo!(),
                        TypeHint::Float => todo!(),
                        TypeHint::Void => todo!(),
                        TypeHint::None => todo!(),
                    }
                }

                // TODO: Need to handle the actual op code as well
                // READLOCAL0, LOADINT2, LTE, IF
                Double(ADD | MUL | SUB | DIV, 2) => {
                    let right = self.pop();
                    let left = self.pop();

                    lines.line("ctx.ip += 2;");

                    max_ip_read += 2;

                    match (left.type_hint, right.type_hint) {
                        (TypeHint::Int, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, Int)");

                            let local = self.push_with_hint(TypeHint::Int);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Int, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, Float)");

                            let local = self.push_with_hint(TypeHint::Float);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Int, TypeHint::None) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Int, None)");

                            let local = self.push_with_hint(TypeHint::None);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, Int)");

                            let local = self.push_with_hint(TypeHint::Float);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, Float)");

                            let local = self.push_with_hint(TypeHint::Float);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::Float, TypeHint::None) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, Float, None)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::Float);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::None, TypeHint::Int) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, None, Int)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::None);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (TypeHint::None, TypeHint::Float) => {
                            // Delegate to the binary handler to return an int
                            let call = format!("binop_opcode_to_ssa_handler!({op}, None, Float)");

                            // Probably needs to be promoted to a float in this case
                            let local = self.push_with_hint(TypeHint::Float);

                            push_binop(&mut lines, local, call, left, right);
                        }
                        (_, _) => todo!(),
                    }
                }
                _ => {
                    todo!()
                }
            }
        }

        // For whatever is left, push on to the SteelThread stack
        for value in &self.stack {
            match value.type_hint {
                TypeHint::Int | TypeHint::Bool | TypeHint::Float => {
                    lines.line(format!("ctx.thread.stack.push({value}.into());"))
                }
                // It is already confirmed to be... something thats non primitive.
                _ => lines.line(format!("ctx.thread.stack.push({value});")),
            };
        }

        lines.line("Ok(())");
        // scope.push_fn(function);

        // Emit the assert for bounds checking purposes
        if max_local_offset_read > 0 {
            // function.line(format!(
            //     "assert!(ctx.sp + {max_local_offset_read} < ctx.thread.stack.len());"
            // ));
        }

        if max_ip_read > 0 {
            function.line(format!(
                "assert!(ctx.ip + {max_ip_read} < ctx.instructions.len());"
            ));
        }

        for line in lines.lines {
            function.line(line);
        }

        // scope.to_string()

        function
    }
}

#[cfg(feature = "codegen")]
fn push_binop(
    function: &mut FunctionLines,
    local: LocalVariable,
    call: String,
    left: LocalVariable,
    right: LocalVariable,
) {
    function.line(format!(
        "let {} = {}",
        local,
        Call::new(
            call.into(),
            // op_code_to_handler(*op).into(),
            vec![
                "ctx".into(),
                left.to_string().into(),
                right.to_string().into()
            ],
        ),
    ));
}

#[cfg(feature = "codegen")]
struct Call<'a> {
    name: Cow<'a, str>,
    args: Vec<Cow<'a, str>>,
}

#[cfg(feature = "codegen")]
impl<'a> Call<'a> {
    pub fn new(name: Cow<'a, str>, args: Vec<Cow<'a, str>>) -> Self {
        Self { name, args }
    }
}

#[cfg(feature = "codegen")]
impl<'a> fmt::Display for Call<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, "(")?;
        for arg in &self.args {
            write!(f, "{arg}, ")?;
        }
        write!(f, ")?;")
    }
}

// fn call_function(name: &str, args: &[&str]) -> String {
//     // format!("{}({})")

//     let mut builder = name.to_string() + "(";

// }

// READCAPTURED, TAILCALL

// TODO:

// Code Gen the match statement on the instruction that we will use to do a sliding window opt
// pub fn identify_pattern(instructions: &mut [Instruction], pattern: &[(OpCode, usize)]) {
//     todo!()
// }

impl Pattern {
    pub fn from_opcodes(op_codes: &[(OpCode, usize)]) -> Vec<Pattern> {
        let mut buffer: Vec<Pattern> = Vec::new();

        Self::from_opcodes_with_buffer(op_codes, &mut buffer);

        buffer
    }

    pub fn from_opcodes_with_buffer(op_codes: &[(OpCode, usize)], patterns: &mut Vec<Pattern>) {
        use OpCode::*;

        patterns.clear();

        let mut iter = op_codes.iter();

        while let Some(op) = iter.next() {
            match op {
                (
                    LOADINT0 | LOADINT1 | LOADINT2 | READLOCAL0 | READLOCAL1 | READLOCAL2
                    | READLOCAL3 | MOVEREADLOCAL0 | MOVEREADLOCAL1 | MOVEREADLOCAL2 | PUSHCONST
                    | MOVEREADLOCAL3 | IF | PUSH | READCAPTURED | TAILCALL,
                    _,
                ) => patterns.push(Pattern::Single(op.0)),
                (READLOCAL | MOVEREADLOCAL | ADDIMMEDIATE, n) => {
                    patterns.push(Pattern::Double(op.0, *n))
                }
                (ADD | SUB | MUL | DIV | EQUAL | LTE, 2) => patterns.push(Pattern::Double(op.0, 2)),
                (BEGINSCOPE, n) => patterns.push(Pattern::Double(op.0, *n)),
                // TODO: Need to introduce a hint to say that these are builtins - these _must_ be builtins
                // or else this hint will fail! If it isn't a primitive function, we're violating the conditions
                // for these op codes, since our basic block will switch.
                //
                // Even better - this can be speculative! We attempt to call a builtin, but if its not,
                // we just fall through to the normal calling behavior!
                (CALLGLOBAL, _) => {
                    let arity = iter.next().unwrap();
                    patterns.push(Pattern::Double(CALLGLOBAL, arity.1))
                }
                (NUMEQUAL, _) => {
                    let arity = iter.next().unwrap();
                    patterns.push(Pattern::Double(NUMEQUAL, arity.1))
                }
                _ => {
                    continue;
                }
            }
        }
    }
}

#[cfg(feature = "codegen")]
pub trait IteratorExtensions: Iterator {
    fn join(&mut self, sep: &str) -> String
    where
        Self::Item: fmt::Display,
    {
        match self.next() {
            None => String::new(),
            Some(first_elt) => {
                // estimate lower bound of capacity needed
                let (lower, _) = self.size_hint();
                let mut result = String::with_capacity(sep.len() * lower);
                write!(&mut result, "{}", first_elt).unwrap();
                self.for_each(|elt| {
                    result.push_str(sep);
                    write!(&mut result, "{}", elt).unwrap();
                });
                result
            }
        }
    }
}

#[cfg(feature = "codegen")]
impl<T> IteratorExtensions for T where T: Iterator {}

// struct SuperInstructionMap {
//     map: std::collections::HashMap<Vec<Pattern>, for<'r> fn (&'r mut VmCore<'_>, usize) -> Result<()>>
// }

#[cfg(feature = "codegen")]
pub fn generate_opcode_map() -> String {
    let patterns = opcode::PATTERNS;

    let mut global_scope = Scope::new();

    // let mut generate = codegen::Function::new("generate_dynamic_op_codes");
    // generate.ret(codegen::Type::new("SuperInstructionMap"));
    // generate.vis("pub(crate)");

    // generate.line("use OpCode::*;");
    // generate.line("use steel_gen::Pattern::*;");

    // generate.line("let mut map = SuperInstructionMap::new();");

    let mut converter = StackToSSAConverter::new();

    let mut pattern_exists_function = codegen::Function::new("pattern_exists");
    pattern_exists_function.ret(codegen::Type::new("bool"));
    pattern_exists_function.vis("pub(crate)");

    pattern_exists_function.arg("pattern", codegen::Type::new("&[steel_gen::Pattern]"));

    pattern_exists_function.line("use OpCode::*;");
    pattern_exists_function.line("use steel_gen::Pattern::*;");

    pattern_exists_function.line("match pattern {");

    let mut vm_match_loop_function = codegen::Function::new("vm_match_dynamic_super_instruction");

    vm_match_loop_function.vis("pub(crate)");

    vm_match_loop_function.arg("ctx", codegen::Type::new("&mut VmCore<'_>"));
    vm_match_loop_function.ret(codegen::Type::new("Result<()>"));
    vm_match_loop_function.arg("instr", codegen::Type::new("DenseInstruction"));

    vm_match_loop_function.line("match instr {");

    for pattern in patterns {
        let original_pattern = pattern;
        let pattern = Pattern::from_opcodes(pattern);

        if pattern.is_empty() {
            dbg!("Pattern produced empty result: {:?}", original_pattern);
            continue;
        }

        let generated_name = pattern
            .iter()
            .map(|x| format!("{x}").to_lowercase())
            .join("_");
        let generated_function = converter.process_sequence(&pattern);

        // let mut scope = Scope::new();

        global_scope.push_fn(generated_function);

        // generate.line(scope.to_string());
        // generate.line(format!("map.insert(vec!{pattern:?}, {generated_name});"));

        converter.reset();

        pattern_exists_function.line(format!("&{pattern:?} => true,"));

        if let Some(op_code) = opcode::sequence_to_opcode(original_pattern) {
            dbg!(&original_pattern);

            vm_match_loop_function.line(format!(
                "DenseInstruction {{ op_code: OpCode::{:?}, payload_size, .. }} => dynamic::{}(ctx, payload_size as usize),",
                op_code, generated_name
            ));
        };
    }

    pattern_exists_function.line("_ => false,");
    pattern_exists_function.line("}");

    vm_match_loop_function.line("_ => {");
    vm_match_loop_function
        .line("crate::core::instructions::pretty_print_dense_instructions(&ctx.instructions);");
    vm_match_loop_function
        .line(r#"panic!("Unhandled opcode: {:?} @ {}", ctx.instructions[ctx.ip], ctx.ip);"#);

    vm_match_loop_function.line("}");
    vm_match_loop_function.line("}");

    global_scope.push_fn(pattern_exists_function);
    global_scope.push_fn(vm_match_loop_function);

    // Return the map now
    // generate.line("map");

    // This gives me the interface to the super instruction stuff
    let top_level_definition = r#"

pub(crate) struct SuperInstructionMap {
    map: std::collections::HashMap<Vec<steel_gen::Pattern>, for<'r> fn (&'r mut VmCore<'_>, usize) -> Result<()>>
}

impl SuperInstructionMap {
    pub(crate) fn new() -> Self {
        Self { map: std::collections::HashMap::new() }
    }

    pub(crate) fn insert(&mut self, pattern: Vec<steel_gen::Pattern>, func: for<'r> fn (&'r mut VmCore<'_>, usize) -> Result<()>) {
        self.map.insert(pattern, func);
    }

    pub(crate) fn get(&self, op_codes: &[(OpCode, usize)]) -> Option<for<'r> fn (&'r mut VmCore<'_>, usize) -> Result<()>> {
        let pattern = steel_gen::Pattern::from_opcodes(&op_codes);
        self.map.get(&pattern).copied()
    }
}

pub(crate) static DYNAMIC_SUPER_PATTERNS: once_cell::sync::Lazy<SuperInstructionMap> = once_cell::sync::Lazy::new(|| generate_dynamic_op_codes());

pub(crate) fn generate_dynamic_op_codes() -> SuperInstructionMap {
    SuperInstructionMap::new()
}
    
    "#;

    // global_scope.push_fn(generate);

    format!("{}\n{}", top_level_definition, global_scope.to_string())
}

#[cfg(all(test, feature = "codegen"))]
fn test() {
    // let op_codes = vec![
    //     Pattern::Double(OpCode::BEGINSCOPE, 0),
    //     Pattern::Single(OpCode::LOADINT0),
    //     // Pattern::Single(OpCode::BEGINSCOPE),
    //     Pattern::Single(OpCode::LOADINT1),
    //     Pattern::Single(OpCode::LOADINT1),
    //     Pattern::Single(OpCode::LOADINT1),
    //     Pattern::Single(OpCode::LOADINT1),
    //     Pattern::Single(OpCode::LOADINT1),
    //     Pattern::Single(OpCode::READLOCAL0),
    //     Pattern::Single(OpCode::READLOCAL1),
    //     Pattern::Pair(OpCode::CALLGLOBAL, OpCode::Arity, 6),
    //     // Pattern::Single(OpCode::BEGINSCOPE), // Pattern::Double(OpCode::ADD, 2),
    //     Pattern::Single(OpCode::LOADINT2),
    //     Pattern::Double(OpCode::EQUAL, 2),
    //     Pattern::Single(OpCode::IF),
    // ];

    use OpCode::*;

    let op_codes = vec![
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
    ];

    let op_codes = Pattern::from_opcodes(&op_codes);

    println!("{op_codes:#?}");

    let mut stack_to_ssa = StackToSSAConverter::new();

    let result = stack_to_ssa.process_sequence(&op_codes);

    let mut scope = Scope::new();

    scope.push_fn(result);

    println!("{}", scope.to_string());

    // println!("{}", ctx_signature().to_string());
}

#[cfg(all(test, feature = "codegen"))]
fn test_generation() {
    use OpCode::*;

    // TODO: Come up with better way for this to make it in
    // let patterns: &'static [&'static [(OpCode, usize)]] = &[&[
    //     (MOVEREADLOCAL0, 0),
    //     (LOADINT2, 225),
    //     (SUB, 2),
    //     (CALLGLOBAL, 1),
    // ]];

    println!("{}", generate_opcode_map());
}
