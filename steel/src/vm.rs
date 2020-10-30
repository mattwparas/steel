mod arity;
mod codegen;
mod constants;
mod expand;
mod heap;
pub mod inline_iter;
mod instructions;
mod map;
mod opcode;
mod stack;

pub use arity::Arity;
pub use arity::ArityMap;
pub use constants::ConstantMap;
pub use constants::ConstantTable;
pub use expand::expand;
pub use expand::get_definition_names;
pub use expand::{expand_statements, extract_macro_definitions};
pub use heap::Heap;
pub use instructions::Instruction;
pub use map::SymbolMap;
pub use opcode::OpCode;

use codegen::emit_loop;

pub use stack::{CallStack, EnvStack, Stack, StackFrame};

use crate::env::{Env, FALSE, TRUE, VOID};
use crate::gc::Gc;
use crate::parser::span::Span;
use crate::parser::{tokens::TokenType, Expr, ParseError, Parser, SyntaxObject};
use crate::primitives::{ListOperations, VectorOperations};
use crate::rerrs::SteelErr;
use crate::rvals::{ByteCodeLambda, Result, SteelVal};
use crate::vm::inline_iter::*;
use expand::MacroSet;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::io::Read;
use std::iter::Iterator;
use std::path::Path;
use std::rc::Rc;
use std::result;

use crate::env::CoreModuleConfig;
use std::cell::Cell;

const STACK_LIMIT: usize = 1024;

fn count_and_collect_global_defines(
    exprs: &[Expr],
    symbol_map: &mut SymbolMap,
) -> (usize, usize, usize) {
    let mut new_count = 0;
    let mut old_count = 0;
    let mut non_defines = 0;
    for expr in exprs {
        match expr {
            Expr::Atom(_) => non_defines += 1,
            Expr::VectorVal(list_of_tokens) => {
                match (list_of_tokens.get(0), list_of_tokens.get(1)) {
                    (
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(def),
                            ..
                        })),
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(name),
                            ..
                        })),
                    ) => {
                        if def == "define" || def == "defn" {
                            let (_, added) = symbol_map.get_or_add(name.as_str());
                            if added {
                                new_count += 1;
                            } else {
                                old_count += 1;
                            }
                        } else {
                            non_defines += 1;
                        }
                    }
                    (
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(def),
                            ..
                        })),
                        Some(Expr::VectorVal(_)),
                    ) => {
                        if def == "begin" {
                            let (res_new, res_old, res_non) =
                                count_and_collect_global_defines(&list_of_tokens[1..], symbol_map);

                            new_count += res_new;
                            old_count += res_old;
                            non_defines += res_non;
                        } else {
                            non_defines += 1;
                        }
                    }
                    _ => {
                        non_defines += 1;
                    }
                }
            }
        }
    }

    (new_count, old_count, non_defines)
}

// insert fast path for built in functions
// rather than look up function in env, be able to call it directly?
fn collect_defines_from_current_scope(
    instructions: &[Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<usize> {
    let mut def_stack: usize = 0;
    let mut count = 0;
    let mut bindings: HashSet<&str> = HashSet::new();

    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::SDEF,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span: _sp,
                    }),
                ..
            } => {
                if def_stack == 0 {
                    if bindings.insert(s) {
                        let (_idx, _) = symbol_map.get_or_add(s);
                        count += 1;
                    }
                    // TODO this needs to get fixed
                    // else {
                    //     stop!(Generic => "define-values: duplicate binding name"; *sp)
                    // }
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                // println!("Entering closure scope!");
                def_stack += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                // println!("Exiting closure scope!");
                if def_stack > 0 {
                    def_stack -= 1;
                }
            }
            _ => {}
        }
    }

    Ok(count)
}

fn collect_binds_from_current_scope(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
    start: usize,
    end: usize,
) {
    let mut def_stack: usize = 0;
    for i in start..end {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::BIND,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                if def_stack == 1 {
                    let idx = symbol_map.add(s);
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                        x.constant = false;
                    }
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                def_stack += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                if def_stack > 0 {
                    def_stack -= 1;
                }
            }
            _ => {}
        }
    }
}

fn insert_debruijn_indices(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<()> {
    let mut stack: Vec<usize> = Vec::new();
    // Snag the defines that are going to be available from the global scope
    let _ = collect_defines_from_current_scope(instructions, symbol_map)?;

    // Snag the binds before the defines
    // collect_binds_from_current_scope(instructions, symbol_map);

    // name mangle
    // Replace all identifiers with indices
    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::PUSH,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            }
            | Instruction {
                op_code: OpCode::SET,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                let idx = symbol_map.get(s).map_err(|x| {
                    let sp = if let Some(syn) = &instructions[i].contents {
                        syn.span
                    } else {
                        Span::new(0, 0)
                    };

                    x.set_span(sp)
                })?;
                // println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    x.constant = false;
                }
            }
            // Is this even necessary?
            Instruction {
                op_code: OpCode::BIND,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                let (idx, _) = symbol_map.get_or_add(s);

                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    // x.contents = None;
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                stack.push(symbol_map.len());
                // More stuff goes here
                let payload = *(&instructions[i].payload_size);

                // Go through the current scope and collect binds from the lambds
                collect_binds_from_current_scope(instructions, symbol_map, i, i + payload - 1);

                // Go through the current scope and find defines and the count
                let def_count = collect_defines_from_current_scope(
                    &instructions[i + 1..(i + payload - 1)],
                    symbol_map,
                )?;
                // Set the def count of the NDEFS instruction after the closure
                if let Some(x) = instructions.get_mut(i + 1) {
                    x.payload_size = def_count;
                }
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => symbol_map.roll_back(stack.pop().unwrap()),
            Instruction {
                op_code: OpCode::SDEF,
                ..
            } => {
                if let Some(x) = instructions.get_mut(i) {
                    x.constant = false;
                }
            }
            _ => {}
        }
    }

    Ok(())
}

// Adds a flag to the pop value in order to save the heap to the global heap
// I should really come up with a better name but for now we'll leave it
fn inject_heap_save_to_pop(instructions: &mut [Instruction]) {
    match instructions {
        [.., Instruction {
            op_code: OpCode::EDEF,
            ..
        }, Instruction {
            op_code: OpCode::BIND,
            ..
        }, Instruction {
            op_code: OpCode::VOID,
            ..
        }, Instruction {
            op_code: OpCode::POP,
            payload_size: x,
            ..
        }] => {
            *x = 1;
        }
        _ => {}
    }
}

pub fn densify(instructions: Vec<Instruction>) -> Vec<DenseInstruction> {
    instructions.into_iter().map(|x| x.into()).collect()
}

pub fn pretty_print_instructions(instrs: &[Instruction]) {
    for (i, instruction) in instrs.iter().enumerate() {
        if instruction.contents.is_some() {
            println!(
                "{}    {:?} : {}     {}",
                i,
                instruction.op_code,
                instruction.payload_size,
                instruction.contents.as_ref().unwrap().ty
            );
        } else {
            println!(
                "{}    {:?} : {}",
                i, instruction.op_code, instruction.payload_size
            );
        }
    }
}

pub fn pretty_print_dense_instructions(instrs: &[DenseInstruction]) {
    for (i, instruction) in instrs.iter().enumerate() {
        println!(
            "{}    {:?} : {}",
            i, instruction.op_code, instruction.payload_size
        );
    }
}

fn _coalesce_clears(instructions: &mut Vec<Instruction>) {
    if instructions.len() < 2 {
        return;
    }
    for i in 0..instructions.len() - 2 {
        match (
            instructions.get(i),
            instructions.get(i + 1),
            instructions.get(i + 2),
        ) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CLEAR,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::PASS;
                }
            }
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CLEAR,
                    ..
                }),
                _,
            ) => {}
            _ => {}
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub struct DenseInstruction {
    op_code: OpCode,
    payload_size: usize,
    span: Span,
}

impl DenseInstruction {
    pub fn new(op_code: OpCode, payload_size: usize, span: Span) -> DenseInstruction {
        DenseInstruction {
            op_code,
            payload_size,
            span,
        }
    }
}

pub struct ProfilingInformation {
    counts: HashMap<FunctionCallCtx, usize>,
    threshold: usize,
}

impl ProfilingInformation {
    pub fn new() -> Self {
        ProfilingInformation {
            counts: HashMap::new(),
            threshold: 20,
        }
    }

    // Check if this function was considered already for the JIT
    // add to the profiling information
    pub fn add_or_increment(&mut self, ctx: FunctionCallCtx) -> bool {
        // let ctx = FunctionCallCtx::new()
        let mut t = false;
        if let Some(x) = self.counts.get_mut(&ctx) {
            if *x >= self.threshold {
                t = true;
            }
            *x += 1;
        } else {
            self.counts.insert(ctx, 0);
        }

        t
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct FunctionCallCtx {
    // Rooted functions are assigned an index
    // via the symbol map
    // I should instead use the function pointer as the hash
    // Would probably make a lot more sense given that it is rooted and hopefully won't move
    // I could pin them in place
    pub(crate) function_id: usize,
    pub(crate) instruction_id: usize,
    // pub()
}

impl FunctionCallCtx {
    pub fn new(function_id: usize, instruction_id: usize) -> Self {
        FunctionCallCtx {
            function_id,
            instruction_id,
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

pub struct Ctx<CT: ConstantTable> {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: CT,
    pub(crate) arity_map: ArityMap,
    pub(crate) repl: bool,
}

#[derive(Clone)]
pub struct EvaluationProgress {
    instruction_count: Cell<usize>,
    callback: Option<Callback>,
}

impl EvaluationProgress {
    pub fn new() -> Self {
        EvaluationProgress {
            instruction_count: Cell::new(1),
            callback: None,
        }
    }

    pub fn with_callback(&mut self, callback: Callback) {
        self.callback.replace(callback);
    }

    pub fn callback(&self) -> Option<bool> {
        if let Some(callback) = &self.callback {
            return Some(callback(self.instruction_count.get()));
        }
        None
    }

    pub fn increment(&self) {
        self.instruction_count.set(self.instruction_count.get() + 1);
    }

    pub fn call_and_increment(&self) -> Option<bool> {
        let b = self.callback();
        self.increment();
        b
    }
}

impl<CT: ConstantTable> Ctx<CT> {
    pub fn new(
        symbol_map: SymbolMap,
        constant_map: CT,
        arity_map: ArityMap,
        repl: bool,
    ) -> Ctx<CT> {
        Ctx {
            symbol_map,
            constant_map,
            arity_map,
            repl,
        }
    }

    // This isn't great - default_symbol_map generates some extra code that we do not need
    // also, generating a default environment is not _that_ expensive
    pub fn default() -> Ctx<ConstantMap> {
        Ctx::new(
            Env::default_symbol_map(),
            ConstantMap::new(),
            ArityMap::new(),
            false,
        )
    }

    pub fn default_repl() -> Ctx<ConstantMap> {
        Ctx::new(
            Env::default_symbol_map(),
            ConstantMap::new(),
            ArityMap::new(),
            true,
        )
    }

    pub fn constant_map(&self) -> &CT {
        &self.constant_map
    }

    pub fn roll_back(&mut self, idx: usize) {
        // unimplemented!()
        self.symbol_map.roll_back(idx);
        self.constant_map.roll_back(idx);
        self.arity_map.roll_back(idx);
    }
}

pub type Callback = fn(usize) -> bool;

// pub type FunctionSignature = fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>>;

#[macro_export]
macro_rules! build_vm {

    ($($type:ty),* $(,)?) => {
        {
            let mut interpreter = VirtualMachine::new_with_meta();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *
            interpreter
        }
    };

    (Structs => {$($type:ty),* $(,)?} Functions => {$($binding:expr => $func:expr),* $(,)?}) => {
        {
            let mut interpreter = VirtualMachine::new_with_meta();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *

            $ (
                interpreter.insert_binding($binding.to_string(), SteelVal::FuncV($func));
            ) *

            interpreter
        }
    };
}

pub struct VirtualMachineBuilder {
    modules: CoreModuleConfig,
    callback: EvaluationProgress,
    stack_limit: usize,
}

impl VirtualMachineBuilder {
    pub fn new() -> Self {
        VirtualMachineBuilder {
            modules: CoreModuleConfig::new_core(),
            callback: EvaluationProgress::new(),
            stack_limit: 128,
        }
    }

    pub fn with_network(mut self) -> Self {
        self.modules = self.modules.with_network();
        self
    }

    pub fn with_file_system(mut self) -> Self {
        self.modules = self.modules.with_file_system();
        self
    }

    pub fn with_callback(mut self, callback: Callback) -> Self {
        self.callback.with_callback(callback);
        self
    }

    pub fn with_stack_limit(mut self, stack_limit: usize) -> Self {
        self.stack_limit = stack_limit;
        self
    }
}

/*
VirtualMachineBuilder::new()
    .with_network()
    .with_file_system()
    .with_stack_limit(128)
    .with_callback(|_| true)
    .into() -> Virtual Machine
*/

pub struct VirtualMachine {
    global_env: Rc<RefCell<Env>>,
    global_heap: Heap,
    macro_env: Rc<RefCell<Env>>,
    idents: MacroSet,
    callback: EvaluationProgress,
    ctx: Ctx<ConstantMap>,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            global_heap: Heap::new(),
            macro_env: Rc::new(RefCell::new(Env::root())),
            idents: MacroSet::new(),
            callback: EvaluationProgress::new(),
            ctx: Ctx::<ConstantMap>::default_repl(),
        }
    }

    pub fn new_with_meta() -> VirtualMachine {
        let mut vm = VirtualMachine::new();
        vm.insert_binding("*env*".to_string(), Env::constant_env_to_hashmap());
        vm
    }

    pub fn insert_binding(&mut self, name: String, value: SteelVal) {
        self.global_env
            .borrow_mut()
            .add_rooted_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    }

    pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
        self.global_env
            .borrow_mut()
            .repl_define_zipped_rooted(&mut self.ctx.symbol_map, vals.into_iter());
    }

    pub fn on_progress(&mut self, callback: Callback) {
        &self.callback.with_callback(callback);
    }

    pub fn print_bindings(&self) {
        println!(
            "Env length: {}",
            self.global_env.borrow().bindings_map().len()
        );
        println!("{:?}", self.global_env.borrow().bindings_map());
    }

    pub fn roll_back(&mut self, _idx: usize) {
        unimplemented!()
    }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    pub fn parse_and_execute_from_path<P: AsRef<Path>>(
        &mut self,
        path: P,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Gc<SteelVal>>> {
        let mut file = std::fs::File::open(path)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;
        self.parse_and_execute(exprs.as_str())
    }

    // pub fn new_with_std

    pub fn parse_and_execute(
        &mut self,
        expr_str: &str,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Gc<SteelVal>>> {
        // let now = Instant::now();
        let gen_bytecode = self.emit_instructions(expr_str)?;

        // previous size of the env
        // let length = self.global_env.borrow().len();

        // println!("Bytecode generated in: {:?}", now.elapsed());
        gen_bytecode
            .into_iter()
            .map(|x| {
                let code = Rc::from(x.into_boxed_slice());
                // let now = Instant::now();
                // let constant_map = &self.ctx.constant_map;
                // let repl = self.ctx.repl;
                // let mut heap = Vec::new();
                let res = self.execute(code, self.ctx.repl);
                // println!("Time taken: {:?}", now.elapsed());
                res
            })
            .collect::<Result<Vec<Gc<SteelVal>>>>()
    }

    fn emit_instructions_from_exprs(
        &mut self,
        exprs: Vec<Expr>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut results = Vec::new();
        // populate MacroSet
        self.idents.insert_from_iter(
            get_definition_names(&exprs)
                .into_iter()
                .chain(self.ctx.symbol_map.copy_underlying_vec().into_iter()),
        );

        // Yoink the macro definitions
        // Add them to our macro env
        // TODO change this to be a unique macro env struct
        // Just a thin wrapper around a hashmap
        let extracted_statements = extract_macro_definitions(
            exprs,
            &self.macro_env,
            &self.global_env,
            &mut self.ctx.symbol_map,
            &self.idents,
        )?;

        // Walk through and expand all macros, lets, and defines
        let expanded_statements =
            expand_statements(extracted_statements, &self.global_env, &self.macro_env)?;

        // Collect global defines here first
        let (ndefs_new, ndefs_old, _not) =
            count_and_collect_global_defines(&expanded_statements, &mut self.ctx.symbol_map);

        // At the global level, let the defines shadow the old ones, but call `drop` on all of the old values

        // Reserve the definitions in the global environment
        // TODO find a better way to make sure that the definitions are reserved
        // This works for the normal bytecode execution without the repl
        self.global_env
            .borrow_mut()
            .reserve_defs(if ndefs_new > 0 { ndefs_new - 1 } else { 0 }); // used to be ndefs - 1

        match (ndefs_old, ndefs_new) {
            (_, _) if ndefs_old > 0 && ndefs_new == 0 => {
                // println!("CASE 1: Popping last!!!!!!!!!");
                self.global_env.borrow_mut().pop_last();
            }
            (_, _) if ndefs_new > 0 && ndefs_old == 0 => {
                // println!("Doing nothing");
            }
            (_, _) if ndefs_new > 0 && ndefs_old > 0 => {
                // println!("$$$$$$$$$$ GOT HERE $$$$$$$$");
                self.global_env.borrow_mut().pop_last();
            }
            (_, _) => {}
        }

        // HACK - make the global definitions line up correctly
        // This is basically a repl only feature
        // if ndefs_old > 0 && ndefs_new == 0 {
        //     // Getting here
        //     self.global_env.borrow_mut().pop_last();
        // }

        // if ndefs_old > ndefs_new && ndefs_new == 0 {
        //     self.global_env.borrow_mut().pop_last();
        // }

        // println!(
        //     "^^^^^^^^^^ Global env length after reserving defs: {}",
        //     self.global_env.borrow().len()
        // );

        // TODO move this out into its thing
        // fairly certain this isn't necessary to do this batching
        // but it does work for now and I'll take it for now
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();
        for expr in expanded_statements {
            // TODO add printing out the expression as its own special function
            // println!("{:?}", expr.to_string());
            let mut instructions: Vec<Instruction> = Vec::new();
            emit_loop(
                &expr,
                &mut instructions,
                None,
                &mut self.ctx.arity_map,
                &mut self.ctx.constant_map,
            )?;
            // if !script {
            // instructions.push(Instruction::new_clear());
            instructions.push(Instruction::new_pop());
            // Maybe see if this gets the job done here
            inject_heap_save_to_pop(&mut instructions);
            // }
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        // println!("Got here!");

        insert_debruijn_indices(&mut instruction_buffer, &mut self.ctx.symbol_map)?;
        extract_constants(&mut instruction_buffer, &mut self.ctx.constant_map)?;
        // coalesce_clears(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());
            results.push(densify(extracted));
        }

        Ok(results)
    }

    pub fn emit_instructions(
        &mut self,
        expr_str: &str,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        // the interner needs to be fixed but for now it just is here for legacy reasons
        // it currently does no allocation
        let mut intern = HashMap::new();

        // Parse the input
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();
        let parsed = parsed?;

        self.emit_instructions_from_exprs(parsed)
    }

    pub fn execute(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        // constants: &CT,
        // heap: &mut Vec<Rc<RefCell<Env>>>,
        repl: bool,
    ) -> Result<Gc<SteelVal>> {
        // execute_vm(instructions)

        // println!("Active Object Count: {:?}", OBJECT_COUNT);

        let stack = StackFrame::new();
        let mut heap = Heap::new();

        // give access to the global root via this method
        heap.plant_root(Rc::downgrade(&self.global_env));

        // let mut constants: Vec<Rc<RefCell<Env>>

        // let global_env = Rc::new(RefCell::new(Env::default_env()));
        let result = vm(
            instructions,
            stack,
            &mut heap,
            // heap,
            Rc::clone(&self.global_env),
            &self.ctx.constant_map,
            repl,
            &self.callback,
        );

        // TODO figure this noise out
        // might be easier to just... write a GC
        if self.global_env.borrow().is_binding_context() {
            // println!("Copying over the heap from the run time:");

            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
            // self.global_heap.inspect_heap();
            // inspect_heap(&self.global_heap);
        }

        // Maybe?????
        // self.global_env.borrow_mut().pop_last();

        // self.global_env.borrow_mut().set_binding_offset(false);

        // println!("Global heap length after: {}", self.global_heap.len());

        // heap.inspect_heap();
        heap.clear();
        heap.reset_limit();

        // println!("Active Object Count: {:?}", OBJECT_COUNT);
        // println!("Heap length: {}", self.global_heap.len());
        // println!("local heap length: {}", heap.len());

        result
    }
}

pub fn execute_vm(
    instructions: Rc<[DenseInstruction]>,
    constants: &ConstantMap,
) -> Result<Gc<SteelVal>> {
    let stack = StackFrame::new();
    let mut heap = Heap::new();
    // let mut constants: Vec<Rc<SteelVal>> = Vec::new();
    let global_env = Rc::new(RefCell::new(Env::default_env()));
    let evaluation_progress = EvaluationProgress::new();
    vm(
        instructions,
        stack,
        &mut heap,
        global_env,
        constants,
        false,
        &evaluation_progress,
        // None,
    )
}

// TODO make this not so garbage but its kind of okay
pub fn extract_constants<CT: ConstantTable>(
    instructions: &mut [Instruction],
    constants: &mut CT,
) -> Result<()> {
    for i in 0..instructions.len() {
        let inst = &instructions[i];
        if let OpCode::PUSH = inst.op_code {
            // let idx = constants.len();
            if inst.constant {
                let value = eval_atom(&inst.contents.as_ref().unwrap())?;
                let idx = constants.add_or_get(value);
                // constants.push(eval_atom(&inst.contents.as_ref().unwrap())?);
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::PUSHCONST;
                    x.payload_size = idx;
                    x.contents = None;
                }
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
pub struct InstructionPointer {
    pub(crate) ip: usize,
    instrs: Rc<[DenseInstruction]>,
}

impl InstructionPointer {
    pub fn new_raw() -> Self {
        InstructionPointer {
            ip: 0,
            instrs: Rc::from(Vec::new().into_boxed_slice()),
        }
    }

    pub fn new(ip: usize, instrs: Rc<[DenseInstruction]>) -> Self {
        InstructionPointer { ip, instrs }
    }

    pub fn instrs_ref(&self) -> &Rc<[DenseInstruction]> {
        &self.instrs
    }

    pub fn instrs(self) -> Rc<[DenseInstruction]> {
        self.instrs
    }
}

// static const HEAP_LIMIT: usize =

static HEAP_LIMIT: usize = 5000;
pub static MAXIMUM_OBJECTS: usize = 50000;

// This is just... easier than passing all of the args into the VM every single time
// This should work out better I hope
// Especially with the callbacks and all that jazz
pub struct VmArgs<'a> {
    pub(crate) instructions: Rc<[DenseInstruction]>,
    pub(crate) stack: StackFrame,
    pub(crate) heap: &'a mut Heap,
    pub(crate) global_env: Rc<RefCell<Env>>,
    pub(crate) constants: &'a ConstantMap,
    pub(crate) repl: bool,
    pub(crate) evaluation_progress: &'a EvaluationProgress,
}

impl<'a> VmArgs<'a> {
    pub fn new(
        instructions: Rc<[DenseInstruction]>,
        stack: StackFrame,
        heap: &'a mut Heap,
        global_env: Rc<RefCell<Env>>,
        constants: &'a ConstantMap,
        repl: bool,
        evaluation_progress: &'a EvaluationProgress,
    ) -> Self {
        VmArgs {
            instructions,
            stack,
            heap,
            global_env,
            constants,
            repl,
            evaluation_progress,
        }
    }

    pub fn call(self) -> Result<Gc<SteelVal>> {
        vm(
            self.instructions,
            self.stack,
            self.heap,
            self.global_env,
            self.constants,
            self.repl,
            self.evaluation_progress,
        )
    }
}

// fn eval_eval_expr(
//     list_of_tokens: &[Expr],
//     env: &Rc<RefCell<Env>>,
//     heap: &mut Vec<Rc<RefCell<Env>>>,
//     expr_stack: &mut Vec<Expr>,
//     // last_macro: &mut Option<&Expr>,
// ) -> Result<Gc<SteelVal>> {
//     if let [e] = list_of_tokens {
//         let res_expr = evaluate(e, env, heap, expr_stack)?;
//         match <Expr>::try_from(&(*res_expr).clone()) {
//             Ok(e) => evaluate(&e, env, heap, expr_stack),
//             Err(_) => stop!(ContractViolation => "Eval not given an expression"),
//         }
//     } else {
//         let e = format!(
//             "{}: expected {} args got {}",
//             "Eval",
//             1,
//             list_of_tokens.len()
//         );
//         stop!(ArityMismatch => e)
//     }
// }

pub fn vm<CT: ConstantTable>(
    instructions: Rc<[DenseInstruction]>,
    stack: StackFrame,
    heap: &mut Heap,
    global_env: Rc<RefCell<Env>>,
    constants: &CT,
    repl: bool,
    callback: &EvaluationProgress,
) -> Result<Gc<SteelVal>> {
    let mut ip = 0;
    let mut global_env = global_env;

    if instructions.is_empty() {
        stop!(Generic => "empty stack!");
    }

    // instruction stack for function calls
    let mut instruction_stack: Stack<InstructionPointer> = Stack::new();
    // stacks on stacks baby
    let mut stacks: CallStack = Stack::new();
    // initialize the instruction number pointer
    let mut cur_inst;
    // Pointer to array of instructions
    let mut instructions = instructions;
    // Self explanatory
    let mut stack = stack;
    // Manage current env in its own stack
    let mut env_stack: EnvStack = Stack::new();
    // Manage the depth of instructions to know when to backtrack
    let mut pop_count = 1;
    // Manage the instruction count
    // let mut _instruction_count = 0;

    while ip < instructions.len() {
        // let object_count: usize = Gc::<()>::object_count();

        // // this is how you could go ahead and snatch the memory count in between instructions
        // // this still doesn't answer how to stop a rust built in from exploding the memory though
        // if object_count > MAXIMUM_OBJECTS {
        //     stop!(Generic => "out of memory!");
        // }

        cur_inst = &instructions[ip];

        // trace!()

        match cur_inst.op_code {
            OpCode::PANIC => {
                let error_message = stack.pop().unwrap();
                stop!(Generic => error_message.to_string(); cur_inst.span);
            }
            OpCode::EVAL => {
                let _expr_to_eval = stack.pop().unwrap();

                panic!("eval not yet supported - internal compiler error");
            }
            OpCode::PASS => {
                ip += 1;
            }
            OpCode::VOID => {
                stack.push(VOID.with(|f| Gc::clone(f)));
                ip += 1;
            }
            OpCode::READ => {
                // this needs to be a string
                let expression_to_parse = stack.pop().unwrap();

                if let SteelVal::StringV(expr) = expression_to_parse.as_ref() {
                    // dummy interning hashmap because the parser is bad
                    // please don't judge I'm working on fixing it
                    // TODO
                    let mut intern = HashMap::new();

                    let parsed: result::Result<Vec<Expr>, ParseError> =
                        Parser::new(expr.as_str(), &mut intern).collect();

                    match parsed {
                        Ok(v) => {
                            // for now, only support one expression
                            // otherwise parse into a list of things
                            // if v.len() != 1 {
                            //     stop!(ArityMismatch => "read only supports one expression")
                            // }

                            let converted: Result<Vec<SteelVal>> = v
                                .into_iter()
                                .map(|x| SteelVal::try_from(x.clone()))
                                .collect();

                            // let converted = Gc::new(SteelVal::try_from(v[0].clone())?);
                            stack.push(ListOperations::built_in_list_func_flat_non_gc(converted?)?);
                            ip += 1;
                        }
                        Err(e) => stop!(Generic => format!("{}", e); cur_inst.span),
                    }
                } else {
                    stop!(TypeMismatch => "read expects a string"; cur_inst.span)
                }
            }
            OpCode::COLLECT => {
                let list = stack.pop().unwrap();
                let transducer = stack.pop().unwrap();

                if let SteelVal::IterV(transducer) = transducer.as_ref() {
                    let ret_val =
                        transducer.run(list, constants, &cur_inst.span, repl, callback, None);
                    stack.push(ret_val?);
                } else {
                    stop!(Generic => "Transducer execute takes a list"; cur_inst.span);
                }
                ip += 1;
            }
            OpCode::COLLECTTO => {
                let output_type = stack.pop().unwrap();
                let list = stack.pop().unwrap();
                let transducer = stack.pop().unwrap();

                if let SteelVal::IterV(transducer) = transducer.as_ref() {
                    let ret_val = transducer.run(
                        list,
                        constants,
                        &cur_inst.span,
                        repl,
                        callback,
                        Some(output_type),
                    );
                    stack.push(ret_val?);
                } else {
                    stop!(Generic => "Transducer execute takes a list"; cur_inst.span);
                }
                ip += 1;
            }
            OpCode::TRANSDUCE => {
                let list = stack.pop().unwrap();
                let initial_value = stack.pop().unwrap();
                let reducer = stack.pop().unwrap();
                let transducer = stack.pop().unwrap();

                if let SteelVal::IterV(transducer) = transducer.as_ref() {
                    let ret_val = transducer.transduce(
                        list,
                        initial_value,
                        reducer,
                        constants,
                        &cur_inst.span,
                        repl,
                        callback,
                    );
                    stack.push(ret_val?);
                } else {
                    stop!(Generic => "Transduce must take an iterable");
                }
                ip += 1;
            }
            OpCode::SET => {
                let value_to_assign = stack.pop().unwrap();
                // let variable = stack.pop().unwrap();

                // println!("index: {}", cur_inst.payload_size);

                if repl {
                    let value = global_env
                        .borrow_mut()
                        .repl_set_idx(cur_inst.payload_size, value_to_assign)?;

                    // println!("Old value: {}", value);
                    stack.push(value);
                } else {
                    unimplemented!();
                    // let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                    // stack.push(value);
                }
                ip += 1;

                // global_env.borrow_mut().defin
            }
            OpCode::PUSHCONST => {
                let val = constants.get(cur_inst.payload_size);
                stack.push(val);
                ip += 1;
            }
            OpCode::PUSH => {
                // awful awful awful hack to fix the repl environment noise
                if repl {
                    let value = global_env.borrow().repl_lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                } else {
                    let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                }
                ip += 1;
            }
            OpCode::APPLY => {
                let list = stack.pop().unwrap();
                let func = stack.pop().unwrap();

                let args = match ListOperations::collect_into_vec(&list) {
                    Ok(args) => args,
                    Err(_) => stop!(TypeMismatch => "apply expected a list"; cur_inst.span),
                };

                match func.as_ref() {
                    SteelVal::StructClosureV(factory, func) => {
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let result = f(&args).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::Closure(closure) => {
                        if stacks.len() == STACK_LIMIT {
                            println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        // let args = stack.split_off(stack.len() - cur_inst.payload_size);

                        let parent_env = closure.sub_expression_env();

                        // TODO remove this unwrap
                        let offset = closure.offset()
                            + parent_env.upgrade().unwrap().borrow().local_offset();

                        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                            parent_env.clone(),
                            offset,
                        )));

                        // add this closure to the list of children
                        parent_env
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .add_child(Rc::downgrade(&inner_env));

                        inner_env
                            .borrow_mut()
                            .reserve_defs(if closure.ndef_body() > 0 {
                                closure.ndef_body() - 1
                            } else {
                                0
                            });

                        // let result =
                        // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                        // closure_stack.push(Rc::clone(&stack_func));
                        // TODO this is where the memory leak is
                        env_stack.push(Rc::clone(&global_env));

                        global_env = inner_env;
                        instruction_stack.push(InstructionPointer::new(ip + 1, instructions));
                        pop_count += 1;
                        stacks.push(stack);
                        instructions = closure.body_exp();
                        stack = args.into(); // TODO
                        ip = 0;
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            OpCode::CLEAR => {
                ip += 1;
            }
            OpCode::MAP => {
                let list = stack.pop().unwrap();
                let stack_func = stack.pop().unwrap();

                match stack_func.closure_arity() {
                    Some(s) if s != 1 => {
                        stop!(ArityMismatch => format!("map expected a function that takes 1 arguments, found {}", s));
                    }
                    _ => {}
                }

                match list.as_ref() {
                    SteelVal::Pair(_, _) => {
                        let collected_results = inline_map_normal(
                            SteelVal::iter(list),
                            stack_func,
                            constants,
                            &cur_inst,
                            repl,
                            callback,
                        )?;

                        stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here
                        stack.push(VectorOperations::vec_construct_iter(inline_map_iter(
                            v.into_iter().map(Gc::clone),
                            stack_func,
                            constants,
                            &cur_inst.span,
                            repl,
                            callback,
                        ))?);
                    }
                    _ => stop!(TypeMismatch => "map expected a list"; cur_inst.span),
                }

                ip += 1;
            }
            OpCode::FILTER => {
                let list = stack.pop().unwrap();
                let stack_func = stack.pop().unwrap();

                match stack_func.closure_arity() {
                    Some(s) if s != 1 => {
                        stop!(ArityMismatch => format!("filter expected a function that takes 1 arguments, found {}", s));
                    }
                    _ => {}
                }

                // Change inline_map and inline_filter to return iterators... now that would be cool
                match list.as_ref() {
                    SteelVal::Pair(_, _) => {
                        let collected_results = inline_filter_normal(
                            SteelVal::iter(list),
                            stack_func,
                            constants,
                            cur_inst,
                            repl,
                            callback,
                        )?;
                        stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here

                        stack.push(VectorOperations::vec_construct_iter(inline_filter_iter(
                            v.into_iter().map(Gc::clone),
                            stack_func,
                            constants,
                            &cur_inst.span,
                            repl,
                            callback,
                        ))?);
                    }
                    _ => stop!(TypeMismatch => "map expected a list"; cur_inst.span),
                }

                ip += 1;
            }
            OpCode::FUNC => {
                let stack_func = stack.pop().unwrap();

                // inspect_heap(&heap);

                match stack_func.as_ref() {
                    SteelVal::StructClosureV(factory, func) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let result = f(stack.peek_range(stack.len() - cur_inst.payload_size..))
                            .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size);

                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FutureFunc(f) => {
                        let result = Gc::new(SteelVal::FutureV(f(
                            stack.peek_range(stack.len() - cur_inst.payload_size..)
                        )));
                        // .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size);
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::Closure(closure) => {
                        if closure.arity() != cur_inst.payload_size {
                            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), cur_inst.payload_size); cur_inst.span);
                        }

                        if stacks.len() == STACK_LIMIT {
                            println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        let args = stack.split_off(stack.len() - cur_inst.payload_size);

                        let parent_env = closure.sub_expression_env();

                        // TODO remove this unwrap
                        let offset = closure.offset()
                            + parent_env.upgrade().unwrap().borrow().local_offset();

                        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                            parent_env.clone(),
                            offset,
                        )));

                        // add this closure to the list of children
                        parent_env
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .add_child(Rc::downgrade(&inner_env));

                        inner_env
                            .borrow_mut()
                            .reserve_defs(if closure.ndef_body() > 0 {
                                closure.ndef_body() - 1
                            } else {
                                0
                            });

                        // let result =
                        // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                        // closure_stack.push(Rc::clone(&stack_func));
                        // TODO this is where the memory leak is
                        env_stack.push(Rc::clone(&global_env));

                        global_env = inner_env;
                        instruction_stack.push(InstructionPointer::new(ip + 1, instructions));
                        pop_count += 1;
                        stacks.push(stack);
                        instructions = closure.body_exp();
                        stack = args.into(); // TODO
                        ip = 0;
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            // Tail call basically says "hey this function is exiting"
            // In the closure case, transfer ownership of the stack to the called function
            OpCode::TAILCALL => {
                let stack_func = stack.pop().unwrap();

                match stack_func.as_ref() {
                    SteelVal::StructClosureV(factory, func) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let result = f(stack.peek_range(stack.len() - cur_inst.payload_size..))
                            .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size);

                        stack.push(result);

                        // println!("{:?}")

                        ip += 1;
                    }
                    SteelVal::Closure(closure) => {
                        if stacks.len() == STACK_LIMIT {
                            println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        if closure.arity() != cur_inst.payload_size {
                            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), cur_inst.payload_size); cur_inst.span);
                        }

                        let args = stack.split_off(stack.len() - cur_inst.payload_size);

                        let parent_env = closure.sub_expression_env();
                        // TODO remove this unwrap
                        let offset = closure.offset()
                            + parent_env.upgrade().unwrap().borrow().local_offset();

                        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                            parent_env.clone(),
                            offset,
                        )));

                        parent_env
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .add_child(Rc::downgrade(&inner_env));

                        inner_env
                            .borrow_mut()
                            .reserve_defs(if closure.ndef_body() > 0 {
                                closure.ndef_body() - 1
                            } else {
                                0
                            });

                        heap.gather_mark_and_sweep_2(&global_env, &inner_env);
                        heap.collect_garbage();

                        global_env = inner_env;
                        instructions = closure.body_exp();
                        stack = args.into();
                        ip = 0;
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            OpCode::IF => {
                // change to truthy...
                if stack.pop().unwrap().is_truthy() {
                    ip = cur_inst.payload_size;
                } else {
                    ip += 1;
                }
            }
            OpCode::JMP => {
                ip = cur_inst.payload_size;
                // HACk
                if ip == 0 && heap.len() > HEAP_LIMIT {
                    println!("Jumping back to the start!");
                    println!("Heap length: {}", heap.len());
                    println!("############################");
                    // heap.gather_mark_and_sweep(&global_env);
                    // heap.drop_large_refs();
                    heap.collect_garbage();
                }
            }
            OpCode::POP => {
                pop_count -= 1;
                if pop_count == 0 {
                    env_stack.clear();

                    if cur_inst.payload_size == 1 {
                        global_env.borrow_mut().set_binding_context(true);
                    }

                    let ret_val = stack.try_pop().ok_or_else(|| {
                        SteelErr::Generic("stack empty at pop".to_string(), Some(cur_inst.span))
                    });

                    global_env.borrow_mut().set_binding_offset(false);

                    return ret_val;
                } else {
                    let ret_val = stack.pop().unwrap();
                    let prev_state = instruction_stack.pop().unwrap();

                    if prev_state.instrs_ref().len() != 0 {
                        global_env = env_stack.pop().unwrap();
                        ip = prev_state.ip;
                        instructions = prev_state.instrs();
                    } else {
                        ip += 1;
                    }

                    stack = stacks.pop().unwrap();
                    stack.push(ret_val);
                }
            }
            OpCode::BIND => {
                let offset = global_env.borrow().local_offset();

                if repl {
                    global_env
                        .borrow_mut()
                        .repl_define_idx(cur_inst.payload_size, stack.pop().unwrap());
                } else {
                    global_env
                        .borrow_mut()
                        .define_idx(cur_inst.payload_size - offset, stack.pop().unwrap());
                }

                ip += 1;
            }
            OpCode::SCLOSURE => {
                ip += 1;
                let forward_jump = cur_inst.payload_size - 1;
                // Snag the number of definitions here
                let ndefs = instructions[ip].payload_size;
                ip += 1;
                // Construct the closure body using the offsets from the payload
                // used to be - 1, now - 2
                let closure_body = instructions[ip..(ip + forward_jump - 1)].to_vec();

                // snag the arity from the eclosure instruction
                let arity = instructions[ip + forward_jump - 1].payload_size;

                let capture_env = Rc::clone(&global_env);

                let mut closure_offset = global_env.borrow().len();
                // println!("%%%%%%%%%%% Env length: {} %%%%%%%%%%%", closure_offset);

                // println!("{:?}", global_env.borrow().string_bindings_vec());

                if global_env.borrow().is_binding_context()
                    && !global_env.borrow().is_binding_offset()
                {
                    global_env.borrow_mut().set_binding_offset(true);
                    closure_offset += 1;
                };

                // set the number of definitions for the environment
                capture_env.borrow_mut().set_ndefs(ndefs);

                // println!("Adding the capture_env to the heap!");
                heap.add(Rc::clone(&capture_env));
                // inspect_heap(&heap);
                let constructed_lambda = ByteCodeLambda::new(
                    closure_body,
                    Rc::downgrade(&capture_env),
                    closure_offset,
                    arity,
                    ndefs,
                );

                stack.push(Gc::new(SteelVal::Closure(constructed_lambda)));

                ip += forward_jump;
                // println!("Performed forward jump to instruction: {}", ip);
            }
            // OpCode::ECLOSURE => {
            //     ip += 1;
            // }
            OpCode::SDEF => {
                ip += 1;

                global_env.borrow_mut().set_binding_context(true);
                global_env.borrow_mut().set_binding_offset(false);

                // println!("Setting binding context to TRUE, offset to FALSE");

                stacks.push(stack);
                stack = Stack::new();

                // placeholder on the instruction_stack
                instruction_stack.push(InstructionPointer::new_raw());
                pop_count += 1;
            }
            OpCode::EDEF => {
                // println!("Found end definition");
                global_env.borrow_mut().set_binding_context(false);
                // def_stack -= 1;
                ip += 1;
                // unimplemented!();
            }
            _ => {
                unimplemented!();
            }
        }

        // Check the evaluation progress in some capacity
        // if let Some(callback) = callback {
        //     callback(&instruction_count);
        // }

        // _instruction_count += 1;

        match callback.call_and_increment() {
            Some(b) if !b => stop!(Generic => "Callback forced quit of function!"),
            _ => {}
        }

        // ip += 1;
    }

    // unimplemented!()
    println!("###### Out of bounds instruction ######");
    println!(
        "Instruction pointer: {}, instructions length: {}",
        ip,
        instructions.len()
    );
    println!("Instructions at time:");
    pretty_print_dense_instructions(&instructions);
    panic!("Out of bounds instruction")
}

/// evaluates an atom expression in given environment
fn eval_atom(t: &SyntaxObject) -> Result<Gc<SteelVal>> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => {
            if *b {
                Ok(TRUE.with(|f| Gc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Gc::clone(f)))
            }
        }
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(Gc::new(SteelVal::NumV(*n))),
        TokenType::StringLiteral(s) => Ok(Gc::new(SteelVal::StringV(s.clone()))),
        TokenType::CharacterLiteral(c) => Ok(Gc::new(SteelVal::CharV(*c))),
        TokenType::IntegerLiteral(n) => Ok(Gc::new(SteelVal::IntV(*n))),
        what => {
            println!("getting here in the eval_atom");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}
