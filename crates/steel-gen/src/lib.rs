// TODO: Create stack to ssa representation of the op codes, via macros

use std::borrow::Cow;

use codegen::{Function, Scope};

macro_rules! opcode_to_function {
    (VOID) => {
        void_handler
    };
    (PUSH) => {
        push_handler
    };
    (FUNC) => {
        func_handler
    };
    (BIND) => {
        bind_handler
    };
    (PUSHCONST) => {
        push_const_handler
    };
    (PANIC) => {
        panic_handler
    };
    (SET) => {
        set_handler
    };
    (READLOCAL0) => {
        local_handler0
    };
    (LOADINT2) => {
        handle_load_int2
    };
    (LTE) => {
        lte_handler
    };
    (MOVEREADLOCAL0) => {
        move_local_handler0
    };
    (SUB) => {
        sub_handler
    };
    (LOADINT1) => {
        handle_load_int1
    };
    (MUL) => {
        multiply_handler
    };
    (MOVEREADLOCAL1) => {
        move_local_handler1
    };
    (READLOCAL1) => {
        local_handler1
    };
    (READLOCAL2) => {
        local_handler2
    };
    (READLOCAL3) => {
        local_handler3
    };
    (LOADINT0) => {
        handle_load_int0
    };
    (CALLGLOBAL) => {
        call_global_handler
    };
    (READCAPTURED) => {
        read_captured_handler
    };
    (IF) => {
        if_handler
    };
    (EQUAL) => {
        equality_handler
    };
    (JMP) => {
        jump_handler
    };
    (ADD) => {
        add_handler
    };
    (TAILCALL) => {
        tail_call_handler
    };
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    VOID = 0,
    PUSH = 1,
    IF = 2,
    JMP = 3,
    FUNC = 4,
    SCLOSURE = 5,
    ECLOSURE = 6,
    BIND,
    SDEF,
    EDEF,
    POPPURE,
    PASS,
    PUSHCONST,
    NDEFS,
    PANIC,
    TAILCALL,
    SET,
    READLOCAL,
    READLOCAL0,
    READLOCAL1,
    READLOCAL2,
    READLOCAL3,
    SETLOCAL,
    COPYCAPTURESTACK,
    COPYCAPTURECLOSURE,
    COPYHEAPCAPTURECLOSURE,
    FIRSTCOPYHEAPCAPTURECLOSURE,
    TCOJMP,
    CALLGLOBAL,
    CALLGLOBALTAIL,
    LOADINT0, // Load const 0
    LOADINT1,
    LOADINT2,
    CGLOCALCONST,
    MOVEREADLOCAL,
    MOVEREADLOCAL0,
    MOVEREADLOCAL1,
    MOVEREADLOCAL2,
    MOVEREADLOCAL3,
    READCAPTURED,
    MOVECGLOCALCONST,
    BEGINSCOPE,
    LETENDSCOPE,
    PUREFUNC,
    ADD,
    SUB,
    MUL,
    DIV,
    EQUAL,
    LTE,
    NEWSCLOSURE,
    ADDREGISTER,
    SUBREGISTER,
    LTEREGISTER,
    SUBREGISTER1,
    ALLOC,
    READALLOC,
    SETALLOC,
    // GIMMICK,
    // MOVEREADLOCALCALLGLOBAL,
    DynSuperInstruction,
}

struct LocalVariable(String);

struct GenSym {
    prefix: &'static str,
    count: u16,
}

impl GenSym {
    // Generate a fresh variable
    fn fresh(&mut self) -> LocalVariable {
        let count = self.count;
        self.count += 1;
        LocalVariable(self.prefix.to_string() + &count.to_string())
    }
}

struct StackToSSAConversion {
    generator: GenSym,
    locals: Vec<LocalVariable>,
}

fn ctx_signature() -> Scope {
    let mut scope = Scope::new();
    let mut function = Function::new("test");
    function.arg("ctx", codegen::Type::new("VmCore<'_>"));
    function.ret(codegen::Type::new("Result<()>"));
    function.line(r#"println!("{:?}, "hello world!")"#);
    function.line(
        Call::new(
            Cow::from("applesauce"),
            vec!["apple".into(), "bananas".into()],
        )
        .to_string(),
    );
    function.line("Ok(())");
    scope.push_fn(function);
    scope
}

struct Call<'a> {
    name: Cow<'a, str>,
    args: Vec<Cow<'a, str>>,
}

impl<'a> Call<'a> {
    pub fn new(name: Cow<'a, str>, args: Vec<Cow<'a, str>>) -> Self {
        Self {
            name: name.into(),
            args,
        }
    }
}

impl<'a> std::fmt::Display for Call<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, "(")?;
        for arg in &self.args {
            write!(f, "{}, ", arg)?;
        }
        write!(f, ")")
    }
}

// fn call_function(name: &str, args: &[&str]) -> String {
//     // format!("{}({})")

//     let mut builder = name.to_string() + "(";

// }

#[test]
fn test() {
    println!("{}", ctx_signature().to_string());
}
