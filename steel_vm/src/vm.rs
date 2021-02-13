use steel::steel_compiler::{
    constants::{ConstantMap, ConstantTable},
    program::Program,
};
// pub use expand::expand;
// pub use expand::get_definition_names;
// pub use expand::{expand_statements, extract_macro_definitions};
use crate::{contracts::ContractedFunctionExt, heap::Heap, transducers::TransducerExt};
use steel::core::instructions::DenseInstruction;
// use steel_compiler::map::SymbolMap;
use steel::core::opcode::OpCode;

// use codegen::emit_loop;

use crate::stack::{CallStack, EnvStack, Stack, StackFrame};

use crate::inline_iter::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
// use std::io::Read;
use std::iter::Iterator;
// use std::path::Path;
use std::rc::Rc;
use std::result;
use steel::env::{Env, VOID};
use steel::gc::Gc;
use steel::primitives::{ListOperations, VectorOperations};
use steel::rerrs::SteelErr;
use steel::rvals::{ByteCodeLambda, Result, SteelVal};
// use steel_compiler::expand::MacroSet;
// use steel::new_parser::span::Span;
// use steel::parser::{Expr, ParseError, Parser};

use steel::structs::SteelStruct;

use steel::new_parser::ast::ExprKind;
use steel::new_parser::parser::{ParseError, Parser};

// use std::cell::Cell;
// use steel::env::CoreModuleConfig;

use crate::evaluation_progress::EvaluationProgress;

use steel::stop;
// use steel_compiler::map::SymbolMap;

// use crate::transducers::run;

pub type Callback = fn(usize) -> bool;

// use serde::{Deserialize, Serialize};

use log::{debug, error, info};

const STACK_LIMIT: usize = 100000;

pub struct VirtualMachineCore {
    global_env: Rc<RefCell<Env>>,
    global_heap: Heap,
    callback: EvaluationProgress,
}

impl VirtualMachineCore {
    pub fn new() -> VirtualMachineCore {
        VirtualMachineCore {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            global_heap: Heap::new(),
            callback: EvaluationProgress::new(),
        }
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        self.global_env.borrow_mut().add_root_value(idx, value);
    }

    pub fn insert_gc_binding(&mut self, idx: usize, value: Gc<SteelVal>) {
        self.global_env.borrow_mut().add_gc_root_value(idx, value);
    }

    pub fn insert_bindings(&mut self, vals: Vec<(usize, SteelVal)>) {
        for (idx, value) in vals {
            self.global_env.borrow_mut().add_root_value(idx, value);
        }
    }

    // pub fn new_with_meta() -> VirtualMachine {
    //     let mut vm = VirtualMachineCore::new();
    //     vm.insert_binding("*env*".to_string(), Env::constant_env_to_hashmap());
    //     vm
    // }

    // pub fn insert_binding(&mut self, name: String, value: SteelVal) {
    //     self.global_env
    //         .borrow_mut()
    //         .add_rooted_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    // }

    // pub fn insert_gc_binding(&mut self, name: String, value: Gc<SteelVal>) {
    //     self.global_env
    //         .borrow_mut()
    //         .add_rooted_gc_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    // }

    // pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
    //     self.global_env
    //         .borrow_mut()
    //         .repl_define_zipped_rooted(&mut self.ctx.symbol_map, vals.into_iter());
    // }

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
    // pub fn parse_and_execute_from_path<P: AsRef<Path>>(
    //     &mut self,
    //     path: P,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<Gc<SteelVal>>> {
    //     let mut file = std::fs::File::open(path)?;
    //     let mut exprs = String::new();
    //     file.read_to_string(&mut exprs)?;
    //     self.parse_and_execute(exprs.as_str())
    // }

    // pub fn parse_and_execute_without_optimizations(
    //     &mut self,
    //     expr_str: &str,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<Gc<SteelVal>>> {
    //     // let now = Instant::now();
    //     let gen_bytecode = self.emit_instructions(expr_str, false)?;

    //     gen_bytecode
    //         .into_iter()
    //         .map(|x| {
    //             let code = Rc::from(x.into_boxed_slice());
    //             let res = self.execute(code, true);
    //             res
    //         })
    //         .collect::<Result<Vec<Gc<SteelVal>>>>()
    // }

    pub fn execute_program(&mut self, program: Program) -> Result<Vec<Gc<SteelVal>>> {
        // unimplemented!()

        let Program {
            instructions,
            constant_map,
        } = program;

        let constant_map = ConstantMap::from_bytes(&constant_map)?;

        instructions
            .into_iter()
            .map(|x| {
                let code = Rc::from(x.into_boxed_slice());
                // let now = std::time::Instant::now();
                let res = self.execute(code, &constant_map);
                // println!("{:?}", now.elapsed());
                res
            })
            .collect()
    }

    pub fn execute_program_by_ref(&mut self, program: &Program) -> Result<Vec<Gc<SteelVal>>> {
        // unimplemented!()

        let Program {
            instructions,
            constant_map,
        } = program;

        let constant_map = ConstantMap::from_bytes(&constant_map)?;
        let instructions: Vec<_> = instructions
            .clone()
            .into_iter()
            .map(|x| Rc::from(x.into_boxed_slice()))
            .collect();

        instructions
            .into_iter()
            .map(|code| {
                let res = self.execute(code, &constant_map);
                res
            })
            .collect()
    }

    // pub fn new_with_std

    // pub fn parse_and_execute(
    //     &mut self,
    //     expr_str: &str,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<Gc<SteelVal>>> {
    //     // let now = Instant::now();
    //     let gen_bytecode = self.emit_instructions(expr_str, true)?;

    //     // previous size of the env
    //     // let length = self.global_env.borrow().len();

    //     // println!("Bytecode generated in: {:?}", now.elapsed());
    //     gen_bytecode
    //         .into_iter()
    //         .map(|x| {
    //             let code = Rc::from(x.into_boxed_slice());
    //             // let now = Instant::now();
    //             // let constant_map = &self.ctx.constant_map;
    //             // let repl = self.ctx.repl;
    //             // let mut heap = Vec::new();
    //             let res = self.execute(code);
    //             // println!("Time taken: {:?}", now.elapsed());
    //             res
    //         })
    //         .collect::<Result<Vec<Gc<SteelVal>>>>()
    // }

    // pub fn optimize_exprs<I: IntoIterator<Item = Expr>>(
    //     exprs: I,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<Expr>> {
    //     // println!("About to optimize the input program");

    //     let converted: Result<Vec<_>> = exprs
    //         .into_iter()
    //         .map(|x| SteelVal::try_from(x.clone()))
    //         .collect();

    //     // let converted = Gc::new(SteelVal::try_from(v[0].clone())?);
    //     let exprs = ListOperations::built_in_list_func_flat_non_gc(converted?)?;

    //     let mut vm = VirtualMachine::new_with_meta();
    //     vm.parse_and_execute_without_optimizations(crate::stdlib::PRELUDE)?;
    //     vm.insert_gc_binding("*program*".to_string(), exprs);
    //     let output = vm.parse_and_execute_without_optimizations(crate::stdlib::COMPILER)?;

    //     // println!("{:?}", output.last().unwrap());

    //     // if output.len()  1 {
    //     //     stop!(Generic => "panic! internal compiler error: output did not return a valid program");
    //     // }

    //     // TODO
    //     SteelVal::iter(Gc::clone(output.last().unwrap()))
    //         .into_iter()
    //         .map(|x| Expr::try_from(x.as_ref()).map_err(|x| SteelErr::Generic(x.to_string(), None)))
    //         .collect::<Result<Vec<Expr>>>()

    //     // unimplemented!()

    //     // self.emit_instructions_from_exprs(parsed)
    // }

    pub fn execute(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        constant_map: &ConstantMap,
        // heap: &mut Vec<Rc<RefCell<Env>>>,
        // repl: bool,
    ) -> Result<Gc<SteelVal>> {
        let stack = StackFrame::new();
        let mut heap = Heap::new();

        // give access to the global root via this method
        heap.plant_root(Rc::downgrade(&self.global_env));

        let repl = true;
        // let repl = false;

        let result = vm(
            instructions,
            stack,
            &mut heap,
            Rc::clone(&self.global_env),
            constant_map,
            repl,
            &self.callback,
        );

        if self.global_env.borrow().is_binding_context() {
            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
        }

        heap.clear();
        heap.reset_limit();

        result
    }
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
        // // A generic answer to this would be to require every built in rust function to use
        // // the try allocate function rather than the normal gc::new one
        // // I think it would be easier to do with a feature gate - turn it on as a compiler flag
        // // that way allocation is either checked, or not, and if they opt for uncheck they they CAN check sometimes
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
            OpCode::ADDINT => {
                let args = stack.peek_range(stack.len() - cur_inst.payload_size as usize..);

                let result = Gc::new(SteelVal::IntV(args.into_iter().try_fold(0, |acc, x| {
                    if let SteelVal::IntV(v) = x.as_ref() {
                        Ok(acc + v)
                    } else {
                        stop!(TypeMismatch => "i+ expected a number"; cur_inst.span)
                    }
                })?));

                stack.truncate(stack.len() - cur_inst.payload_size as usize);
                stack.push(result);
                ip += 1;
            }
            OpCode::SUBINT => {
                // unimplemented!();

                let args = stack.peek_range(stack.len() - cur_inst.payload_size as usize..);

                let initial_value = if let SteelVal::IntV(n) = &args[0].as_ref() {
                    *n
                } else {
                    stop!(TypeMismatch => "i- expected a number"; cur_inst.span)
                };

                let result = Gc::new(SteelVal::IntV(args[1..].into_iter().try_fold(
                    initial_value,
                    |acc, x| {
                        if let SteelVal::IntV(v) = x.as_ref() {
                            Ok(acc - v)
                        } else {
                            stop!(TypeMismatch => "i- expected a number"; cur_inst.span)
                        }
                    },
                )?));

                stack.truncate(stack.len() - cur_inst.payload_size as usize);
                stack.push(result);
                ip += 1;

                // let result = f(stack.peek_range(stack.len() - cur_inst.payload_size as usize..))
                //     .map_err(|x| x.set_span(cur_inst.span))?;

                // stack.truncate(stack.len() - cur_inst.payload_size as usize);

                // stack.push(result);
                // ip += 1;
            }
            OpCode::STRUCT => {
                let val = constants.get(cur_inst.payload_size as usize);
                let mut iter = SteelVal::iter(val);

                // List of indices e.g. '(25 26 27 28) to bind struct functions to
                let indices = iter.next().unwrap();

                // The name of the struct
                let name: String = if let SteelVal::StringV(s) = iter.next().unwrap().as_ref() {
                    s.to_string()
                } else {
                    stop!( Generic => "ICE: Struct expected a string name")
                };

                // The fields of the structs
                let fields: Vec<String> = iter
                    .map(|x| {
                        if let SteelVal::StringV(s) = x.as_ref() {
                            Ok(s.clone())
                        } else {
                            stop!(Generic => "ICE: Struct encoded improperly with non string fields")
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;

                // Get them as &str for now
                let other_fields: Vec<&str> = fields.iter().map(|x| x.as_str()).collect();

                // Generate the functions, but they immediately override them with the names
                // Store them with the indices
                let funcs = SteelStruct::generate_from_name_fields(name.as_str(), &other_fields)?;

                for ((_, func), idx) in funcs.into_iter().zip(SteelVal::iter(indices)) {
                    let idx = if let SteelVal::IntV(idx) = idx.as_ref() {
                        *idx as usize
                    } else {
                        stop!(Generic => "Index wrong in structs")
                    };

                    global_env.borrow_mut().repl_define_idx(idx, Gc::new(func));
                }

                return Ok(VOID.with(|f| Gc::clone(f)));
            }
            OpCode::READ => {
                // this needs to be a string
                let expression_to_parse = stack.pop().unwrap();

                if let SteelVal::StringV(expr) = expression_to_parse.as_ref() {
                    // dummy interning hashmap because the parser is bad
                    // please don't judge I'm working on fixing it
                    // TODO
                    let mut intern = HashMap::new();

                    let parsed: result::Result<Vec<ExprKind>, ParseError> =
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

                // println!("index: {}", cur_inst.payload_size as usize);

                if repl {
                    let value = global_env
                        .borrow_mut()
                        .repl_set_idx(cur_inst.payload_size as usize, value_to_assign)?;

                    // println!("Old value: {}", value);
                    stack.push(value);
                } else {
                    unimplemented!();
                    // let value = global_env.borrow().lookup_idx(cur_inst.payload_size as usize)?;
                    // stack.push(value);
                }
                ip += 1;

                // global_env.borrow_mut().defin
            }
            OpCode::PUSHCONST => {
                let val = constants.get(cur_inst.payload_size as usize);
                stack.push(val);
                ip += 1;
            }
            OpCode::PUSH => {
                // TODO future me figure out the annoying offset issue
                // awful awful awful hack to fix the repl environment noise
                if repl {
                    let value = global_env
                        .borrow()
                        .repl_lookup_idx(cur_inst.payload_size as usize)?;
                    stack.push(value);
                } else {
                    let value = global_env
                        .borrow()
                        .lookup_idx(cur_inst.payload_size as usize)?;
                    stack.push(value);
                }

                // let value = global_env.borrow().repl_lookup_idx(cur_inst.payload_size as usize)?;
                // stack.push(value);
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
                            // println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        // let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);

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

                        // TODO future me figure out offsets
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
                        let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let result =
                            f(stack.peek_range(stack.len() - cur_inst.payload_size as usize..))
                                .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size as usize);

                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FutureFunc(f) => {
                        let result = Gc::new(SteelVal::FutureV(f(
                            stack.peek_range(stack.len() - cur_inst.payload_size as usize..)
                        )));
                        // .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size as usize);
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::ContractedFunction(cf) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);

                        let result =
                            cf.apply(args, heap, constants, &cur_inst.span, repl, callback)?;

                        stack.push(result);
                        ip += 1;

                        // constants, &cur_inst.span, repl, callback
                    }
                    SteelVal::Closure(closure) => {
                        if closure.arity() != cur_inst.payload_size as usize {
                            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), cur_inst.payload_size as usize); cur_inst.span);
                        }

                        if stacks.len() == STACK_LIMIT {
                            // println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        // Use smallvec here?
                        let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);

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

                        // TODO future me figure out offsets
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
                        let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let result =
                            f(stack.peek_range(stack.len() - cur_inst.payload_size as usize..))
                                .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size as usize);

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

                        if closure.arity() != cur_inst.payload_size as usize {
                            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), cur_inst.payload_size as usize); cur_inst.span);
                        }

                        let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);

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

                        // TODO future me to figure out with offsets
                        inner_env
                            .borrow_mut()
                            .reserve_defs(if closure.ndef_body() > 0 {
                                closure.ndef_body() - 1
                            } else {
                                0
                            });

                        // info!("Calling mark and sweep");
                        heap.gather_mark_and_sweep_2(&global_env, &inner_env);
                        // info!(
                        //     "Collecting garbage on TAILCALL with heap length: {}",
                        //     heap.len()
                        // );
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
                    ip = cur_inst.payload_size as usize;
                } else {
                    ip += 1;
                }
            }
            OpCode::JMP => {
                ip = cur_inst.payload_size as usize;
                // HACk
                if ip == 0 && heap.len() > heap.limit() {
                    // info!("Collecting garbage on JMP with heap length: {}", heap.len());

                    // println!("Jumping back to the start!");
                    // println!("Heap length: {}", heap.len());
                    // println!("############################");
                    // heap.gather_mark_and_sweep(&global_env);
                    // heap.drop_large_refs();
                    heap.collect_garbage();
                }
            }
            OpCode::POP => {
                pop_count -= 1;
                if pop_count == 0 {
                    env_stack.clear();

                    if cur_inst.payload_size as usize == 1 {
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
                // global_env
                //     .borrow_mut()
                //     .repl_define_idx(cur_inst.payload_size as usize, stack.pop().unwrap());

                // TODO leave this here for future me to figure out the offset stuff
                if repl {
                    global_env
                        .borrow_mut()
                        .repl_define_idx(cur_inst.payload_size as usize, stack.pop().unwrap());
                } else {
                    let offset = global_env.borrow().local_offset();

                    global_env.borrow_mut().define_idx(
                        cur_inst.payload_size as usize - offset,
                        stack.pop().unwrap(),
                    );
                }

                ip += 1;
            }
            OpCode::SCLOSURE => {
                ip += 1;
                let forward_jump = cur_inst.payload_size as usize - 1;
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
                capture_env.borrow_mut().set_ndefs(ndefs as usize);

                // println!("Adding the capture_env to the heap!");
                heap.add(Rc::clone(&capture_env));
                // inspect_heap(&heap);
                let constructed_lambda = ByteCodeLambda::new(
                    closure_body,
                    Rc::downgrade(&capture_env),
                    closure_offset,
                    arity as usize,
                    ndefs as usize,
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

            OpCode::LOOKUP => {}
            OpCode::ECLOSURE => {}
            OpCode::NDEFS => {}
            OpCode::METALOOKUP => {}
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

    error!(
        "Out of bounds instruction!: instruction pointer: {}, instruction length: {}",
        ip,
        instructions.len()
    );
    // error!("Instructions at out of bounds!: {}", pretty_print_dense_instructions(&instructions));
    // unimplemented!()
    // println!("###### Out of bounds instruction ######");
    // println!(
    //     "Instruction pointer: {}, instructions length: {}",
    //     ip,
    //     instructions.len()
    // );
    // println!("Instructions at time:");
    steel::core::instructions::pretty_print_dense_instructions(&instructions);
    panic!("Out of bounds instruction")
}
