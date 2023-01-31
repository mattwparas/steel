// TODO: Create stack to ssa representation of the op codes, via macros

pub mod opcode;
use std::borrow::Cow;

use itertools::Itertools;
pub use opcode::OpCode;

use codegen::{Function, Scope};

// If we can provide hints on the types, this can help with constant folding of operations
// that we know the types of
//
// For instance, this can also allow specializing _lots_ of the list operations if we can pull it off
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TypeHint {
    Int,
    Bool,
    Float,
    Void,
    None,
}

#[derive(Clone, Debug)]
struct LocalVariable {
    id: u16,
    type_hint: TypeHint,
}

impl LocalVariable {
    pub fn kind(mut self, type_hint: TypeHint) -> Self {
        self.type_hint = type_hint;
        self
    }
}

struct GenSym {
    count: u16,
}

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

impl std::fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}

fn op_code_to_handler(op_code: Pattern) -> String {
    format!("opcode_to_ssa_handler!({op_code})")
}

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

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Single(op) => write!(f, "{op:?}"),
            Pattern::Double(op, payload) => write!(f, "{op:?}{payload}"),
        }
    }
}

#[derive(Default)]
struct FunctionLines {
    lines: Vec<String>,
}

impl FunctionLines {
    fn line(&mut self, line: impl Into<String>) {
        self.lines.push(line.into())
    }
}

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
        function.arg("ctx", codegen::Type::new("&mut VmCore<'_>"));
        function.arg("payload", codegen::Type::new("usize"));
        function.ret(codegen::Type::new("Result<()>"));

        let mut lines = FunctionLines::default();

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
                    // println!("Stack: {:?}", self.stack);

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
                            "let {local} = opcode_to_ssa_handler!(CALLGLOBAL)(ctx, &mut [{args}])?;"
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
                        let var = self.stack.get(0).unwrap();
                        lines.line(format!("let {local} = {var}.clone();"));
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
                        lines.line(format!("let {local} = {var}.clone();"));
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
                        let var = self.stack.get(0).unwrap();
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
                            lines.line("ctx.ip += 1;");
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
                            let call =
                                format!("binop_opcode_to_ssa_handler!({op}, Float, Float)");

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
                            let call =
                                format!("binop_opcode_to_ssa_handler!({op}, Float, Float)");

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
            function.line(format!(
                "assert!(ctx.sp + {max_local_offset_read} < ctx.thread.stack.len());"
            ));
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

struct Call<'a> {
    name: Cow<'a, str>,
    args: Vec<Cow<'a, str>>,
}

impl<'a> Call<'a> {
    pub fn new(name: Cow<'a, str>, args: Vec<Cow<'a, str>>) -> Self {
        Self {
            name,
            args,
        }
    }
}

impl<'a> std::fmt::Display for Call<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Pattern {
    pub fn from_opcodes(op_codes: &[(OpCode, usize)]) -> Vec<Pattern> {
        use OpCode::*;

        let mut patterns: Vec<Pattern> = Vec::new();
        let iter = op_codes.iter();

        for op in iter {
            match op {
                (
                    LOADINT0 | LOADINT1 | LOADINT2 | READLOCAL0 | READLOCAL1 | READLOCAL2
                    | READLOCAL3 | MOVEREADLOCAL0 | MOVEREADLOCAL1 | MOVEREADLOCAL2
                    | MOVEREADLOCAL3 | IF | PUSH | READCAPTURED | TAILCALL,
                    _,
                ) => patterns.push(Pattern::Single(op.0)),
                (READLOCAL | MOVEREADLOCAL, n) => patterns.push(Pattern::Double(op.0, *n)),
                (ADD | SUB | MUL | DIV | EQUAL | LTE, 2) => patterns.push(Pattern::Double(op.0, 2)),
                (BEGINSCOPE, n) => patterns.push(Pattern::Double(op.0, *n)),
                (CALLGLOBAL, n) => {
                    // let arity = iter.next().unwrap();
                    patterns.push(Pattern::Double(CALLGLOBAL, *n))
                }
                _ => {
                    continue;
                }
            }
        }

        patterns
    }
}

// struct SuperInstructionMap {
//     map: std::collections::HashMap<Vec<Pattern>, for<'r> fn (&'r mut VmCore<'_>, usize) -> Result<()>>
// }

pub fn generate_opcode_map(patterns: Vec<Vec<(OpCode, usize)>>) -> String {
    

    let mut global_scope = Scope::new();

    let mut generate = codegen::Function::new("generate_dynamic_op_codes");
    generate.ret(codegen::Type::new("SuperInstructionMap"));
    generate.vis("pub(crate)");

    generate.line("use OpCode::*;");
    generate.line("use steel_gen::Pattern::*;");

    generate.line("let mut map = SuperInstructionMap::new();");

    let mut converter = StackToSSAConverter::new();

    for pattern in patterns {
        let pattern = Pattern::from_opcodes(&pattern);
        let generated_name = pattern
            .iter()
            .map(|x| format!("{x}").to_lowercase())
            .join("_");
        let generated_function = converter.process_sequence(&pattern);

        let mut scope = Scope::new();

        scope.push_fn(generated_function);

        generate.line(scope.to_string());
        generate.line(format!(
            "map.insert(vec!{pattern:?}, {generated_name});"
        ));

        converter.reset();

        // let block = Block::new(before)

        // generate.push_block(block)
    }

    // Return the map now
    generate.line("map");

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
    
    lazy_static! {
        pub(crate) static ref DYNAMIC_SUPER_PATTERNS: SuperInstructionMap = generate_dynamic_op_codes();
    }
    
    "#;

    global_scope.push_fn(generate);

    format!("{}\n{}", top_level_definition, global_scope.to_string())
}

#[test]
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
        (MOVEREADLOCAL0, 0),
        (LOADINT2, 225),
        (SUB, 2),
        (CALLGLOBAL, 1),
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

#[test]
fn test_generation() {
    use OpCode::*;

    // TODO: Come up with better way for this to make it in
    let patterns: Vec<Vec<(OpCode, usize)>> = vec![vec![
        (MOVEREADLOCAL0, 0),
        (LOADINT2, 225),
        (SUB, 2),
        (CALLGLOBAL, 1),
    ]];

    println!("{}", generate_opcode_map(patterns));
}
