use std::{cell::RefCell, convert::TryFrom, rc::Rc};

use super::{
    constants::{ConstantMap, ConstantTable},
    map::SymbolMap,
};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::{ast::Atom, parser::SyntaxObject, span_visitor::get_span, tokens::TokenType},
    values::structs::SteelStruct,
};

use crate::parser::ast::ExprKind;
use crate::parser::visitors::VisitorMut;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;

use log::info;

// use super::codegen::{check_and_transform_mutual_recursion, transform_tail_call};

#[derive(Clone, Debug)]
struct LocalVariable {
    depth: u32,
    name: String,
    is_captured: bool,
    struct_offset: usize,
    syntax_object: SyntaxObject,
}

impl LocalVariable {
    pub fn new(depth: u32, name: String, syntax_object: SyntaxObject) -> Self {
        LocalVariable {
            depth,
            name,
            is_captured: false,
            struct_offset: 0,
            syntax_object,
        }
    }

    pub fn new_struct(
        depth: u32,
        name: String,
        syntax_object: SyntaxObject,
        struct_offset: usize,
    ) -> Self {
        LocalVariable {
            depth,
            name,
            is_captured: false,
            struct_offset,
            syntax_object,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct UpValue {
    // The slot that the upvalue is capturing
    index: usize,
    // Whether or not this is a local variable at all
    is_local: bool,
}

impl UpValue {
    pub fn new(index: usize, is_local: bool) -> Self {
        UpValue { index, is_local }
    }
}

#[derive(Clone, Debug)]
pub struct VariableData {
    locals: Vec<LocalVariable>,
    upvalues: Vec<UpValue>,
    enclosing: Option<Rc<RefCell<VariableData>>>,
}

impl VariableData {
    fn new(
        locals: Vec<LocalVariable>,
        upvalues: Vec<UpValue>,
        enclosing: Option<Rc<RefCell<VariableData>>>,
    ) -> Self {
        VariableData {
            locals,
            upvalues,
            enclosing,
        }
    }

    fn push_local(&mut self, local: LocalVariable) {
        self.locals.push(local)
    }

    // Set a local to be captured for later code generation
    fn mark_captured(&mut self, index: usize) {
        self.locals[index].is_captured = true;
    }

    // Go backwards and attempt to find the index in which a local variable will live on the stack
    // returns (actual, stack)
    fn resolve_local(&self, ident: &str) -> Option<usize> {
        let idx = self
            .locals
            .iter()
            .rev()
            .position(|x| &x.name == ident)
            .map(|x| self.locals.len() - 1 - x)?;

        let var = self.locals.iter().rev().find(|x| &x.name == ident)?;
        Some(idx + var.struct_offset)
    }

    // Resolve the upvalue with some recursion shenanigans
    fn resolve_upvalue(&mut self, ident: &str) -> Option<usize> {
        if self.enclosing.is_none() {
            return None;
        }

        // Check local first
        let local = self
            .enclosing
            .as_ref()
            .map(|x| x.borrow().resolve_local(ident))
            .flatten();

        if let Some(local) = local {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .mark_captured(local);

            return Some(self.add_upvalue(local, true));
        }

        // Check upvalues afterwards
        let upvalue = self
            .enclosing
            .as_ref()
            .map(|x| x.borrow_mut().resolve_upvalue(ident))
            .flatten();
        if let Some(upvalue) = upvalue {
            return Some(self.add_upvalue(upvalue, false));
        }

        // Otherwise we're a global and we should move on
        None
    }

    // fn add_local_upvalue(&mut self, index: usize, is_local: bool) -> usize {

    // }

    // Add the upvalue to the upvalue list, returning the index in the list
    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        // If the upvalue has already been captured, don't capture it again
        if let Some(i) = self
            .upvalues
            .iter()
            .position(|x| x.index == index && x.is_local == is_local)
        {
            return i;
        }

        self.upvalues.push(UpValue::new(index, is_local));
        self.upvalues.len() - 1
    }
}

pub struct CodeGenerator<'a> {
    instructions: Vec<Instruction>,
    constant_map: &'a mut ConstantMap,
    defining_context: Option<String>,
    symbol_map: &'a mut SymbolMap,
    depth: u32,
    variable_data: Option<Rc<RefCell<VariableData>>>, // enclosing: Option<&'a mut CodeGenerator<'a>>,
    let_context: bool,
    stack_offset: usize,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_map: &'a mut ConstantMap, symbol_map: &'a mut SymbolMap) -> Self {
        CodeGenerator {
            instructions: Vec::new(),
            constant_map,
            defining_context: None,
            symbol_map,
            depth: 0,
            variable_data: None,
            let_context: false,
            stack_offset: 0,
            // enclosing: None,
        }
    }

    fn new_from_body_instructions(
        constant_map: &'a mut ConstantMap,
        symbol_map: &'a mut SymbolMap,
        instructions: Vec<Instruction>,
        depth: u32,
        variable_data: Option<Rc<RefCell<VariableData>>>,
    ) -> Self {
        CodeGenerator {
            instructions,
            constant_map,
            defining_context: None,
            symbol_map,
            depth,
            variable_data,
            let_context: false,
            stack_offset: 0,
            // enclosing,
        }
    }

    pub fn compile(mut self, expr: &ExprKind) -> Result<Vec<Instruction>> {
        self.visit(expr)?;
        Ok(self.instructions)
    }

    #[inline]
    fn push(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    #[inline]
    fn len(&self) -> usize {
        self.instructions.len()
    }

    fn specialize_constant(&mut self, syn: &SyntaxObject) -> Result<()> {
        let value = eval_atom(syn)?;

        let opcode = match &value {
            // SteelVal::IntV(1) => OpCode::LOADINT1,
            // SteelVal::IntV(2) => OpCode::LOADINT2,
            _ => OpCode::PUSHCONST,
        };

        let idx = self.constant_map.add_or_get(value);
        self.push(Instruction::new(opcode, idx, syn.clone(), true));
        Ok(())
    }
    // fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
    //     // If the upvalue has already been captured, don't capture it again
    //     if let Some(i) = self
    //         .upvalues
    //         .iter()
    //         .position(|x| x.index == index && x.is_local == is_local)
    //     {
    //         return i;
    //     }

    //     self.upvalues.push(UpValue::new(index, is_local));
    //     return self.upvalues.len() - 1;
    // }
}

impl<'a> VisitorMut for CodeGenerator<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        // load in the test condition
        self.visit(&f.test_expr)?;
        // push in if
        self.push(Instruction::new_if(self.instructions.len() + 2));
        // save spot of jump instruction, fill in after
        let idx = self.len();
        self.push(Instruction::new_jmp(0)); // dummy value

        // emit instructions for then
        self.visit(&f.then_expr)?;
        self.push(Instruction::new_jmp(0));
        let false_start = self.len();

        // emit instructions for else expression
        self.visit(&f.else_expr)?;
        let j3 = self.len(); // first instruction after else

        // set index of jump instruction
        if let Some(elem) = self.instructions.get_mut(idx) {
            (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        if let Some(elem) = self.instructions.get_mut(false_start - 1) {
            (*elem).payload_size = j3;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        Ok(())
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        // todo!()

        let sidx = self.len();
        self.push(Instruction::new_sdef());

        if let ExprKind::Atom(name) = &define.name {
            let defining_context = if let TokenType::Identifier(ident) = &name.syn.ty {
                if let Some(x) = self.instructions.get_mut(sidx) {
                    x.contents = Some(name.syn.clone());
                }
                Some(ident.clone())
            } else {
                None
            };

            // Set this for tail call optimization ease
            self.defining_context = defining_context;

            self.visit(&define.body)?;

            let defn_body_size = self.len() - sidx;
            self.push(Instruction::new_edef());

            if let Some(elem) = self.instructions.get_mut(sidx) {
                (*elem).payload_size = defn_body_size;
            } else {
                stop!(Generic => "out of bounds closure len");
            }

            // println!("binding global: {}", name);
            self.push(Instruction::new_bind(name.syn.clone()));

            self.push(Instruction::new_void());

            // Clean up the defining context state
            self.defining_context = None;
        } else {
            panic!(
                "Complex defines not supported in bytecode generation: {}",
                (define.name).to_string()
            )
        }

        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        // todo!()

        let idx = self.len();
        self.push(Instruction::new_sclosure());

        let mut body_instructions = Vec::new();

        let l = &lambda_function.args;

        let mut locals = Vec::new();

        let arity = l.len();
        // let rev_iter = l.iter().rev();
        let rev_iter = l.iter();
        for symbol in rev_iter {
            if let ExprKind::Atom(atom) = symbol {
                match &atom.syn {
                    SyntaxObject {
                        ty: TokenType::Identifier(i),
                        ..
                    } => {
                        locals.push(LocalVariable::new(
                            self.depth + 1,
                            i.clone(),
                            atom.syn.clone(),
                        ));
                        // println!("Validating the identifiers in the arguments");
                        // body_instructions.push(Instruction::new_bind(atom.syn.clone()));
                    }
                    SyntaxObject {
                        ty: _, span: sp, ..
                    } => {
                        stop!(Generic => "lambda function requires list of identifiers"; *sp);
                    }
                }
            } else {
                // stop!(Generic => "lambda function requires list of identifiers"; symbol.span());
                // TODO come back add the span
                stop!(Generic => "lambda function requires list of identifiers");
            }
        }

        // Snatch access to parent information here
        // That way we can at least have a shot of going backwards
        let variable_data = Rc::new(RefCell::new(VariableData::new(
            locals,
            Vec::new(),
            self.variable_data.as_ref().map(Rc::clone),
        )));

        // make recursive call with "fresh" vector so that offsets are correct
        body_instructions = CodeGenerator::new_from_body_instructions(
            &mut self.constant_map,
            &mut self.symbol_map,
            body_instructions,
            self.depth + 1, // pass through the depth
            Some(Rc::clone(&variable_data)),
        ) // pass through the locals here
        .compile(&lambda_function.body)?;

        // Put the length of the upvalues here
        self.push(Instruction::new_ndef(variable_data.borrow().upvalues.len()));
        // println!("Variable data: {:?}", variable_data.borrow().upvalues);
        // dbg!(&variable_data);

        // Fill out the upvalue information that needs to be down
        // TODO
        for upvalue in &variable_data.borrow().upvalues {
            if upvalue.is_local {
                // println!("Pushing new local upvalue!");
                self.push(Instruction::new_local_upvalue(upvalue.index));
            } else {
                // println!("Pushing new upvalue");
                self.push(Instruction::new_upvalue(upvalue.index));
            }
        }

        // Encode the amount to pop
        body_instructions.push(Instruction::new_pop_with_upvalue(
            variable_data.borrow().locals.len(),
        ));
        if let Some(ctx) = &self.defining_context {
            transform_tail_call(&mut body_instructions, ctx);

            // TODO check this here - reimplement mutual recursion
            let b = check_and_transform_mutual_recursion(&mut body_instructions);

            // let b = false;

            if b {
                info!("Transformed mutual recursion for: {}", ctx);
                // println!("Transformed mutual recursion for: {}", ctx);
            }
        }
        // TODO come back here
        else if self.let_context {
            // TODO
            let b = check_and_transform_mutual_recursion(&mut body_instructions);

            // let b = false;

            if b {
                info!("Transformed mutual recursion inside local context");
                // println!("Transformed mutual recursion inside local context");
            }
        }

        self.instructions.append(&mut body_instructions);

        // Go ahead and include the variable information for the popping
        // This needs to be handled accordingly
        // TODO this could be handled better - just put the index that needs to be saved
        // for now, is_captured is just a noop
        for local in variable_data.borrow().locals.iter() {
            self.push(Instruction::new_close_upvalue(
                if local.is_captured { 1 } else { 0 },
                local.syntax_object.clone(),
            ))
        }

        // pop off the local variables from the run time stack, so we don't have them

        let closure_body_size = self.len() - idx;
        self.push(Instruction::new_eclosure(arity));

        if let Some(elem) = self.instructions.get_mut(idx) {
            (*elem).payload_size = closure_body_size;
        } else {
            stop!(Generic => "out of bounds closure len");
        }

        Ok(())
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        if begin.exprs.is_empty() {
            self.push(Instruction::new_void());
            return Ok(());
        }

        // Mark the offset of where things will live on the stack
        // for each expression, increment the offset and reset it once done
        let offset = self.stack_offset;
        for expr in &begin.exprs {
            self.visit(expr)?;
            self.stack_offset += 1;
        }
        self.stack_offset = offset;

        Ok(())
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)?;
        // pop is equivalent to the last instruction in the function
        self.push(Instruction::new_pop());
        Ok(())
    }

    fn visit_apply(&mut self, apply: &crate::parser::ast::Apply) -> Self::Output {
        // todo!()
        self.visit(&apply.func)?;
        self.visit(&apply.list)?;
        self.push(Instruction::new_apply(apply.location.clone()));
        Ok(())
    }

    fn visit_panic(&mut self, p: &crate::parser::ast::Panic) -> Self::Output {
        // todo!()
        self.visit(&p.message)?;
        self.push(Instruction::new_panic(p.location.clone()));
        Ok(())
    }

    fn visit_transduce(&mut self, transduce: &crate::parser::ast::Transduce) -> Self::Output {
        self.visit(&transduce.transducer)?;
        self.visit(&transduce.func)?;
        self.visit(&transduce.initial_value)?;
        self.visit(&transduce.iterable)?;
        self.push(Instruction::new_transduce());
        Ok(())
    }

    fn visit_read(&mut self, read: &crate::parser::ast::Read) -> Self::Output {
        self.visit(&read.expr)?;
        self.push(Instruction::new_read());
        Ok(())
    }

    fn visit_execute(&mut self, execute: &crate::parser::ast::Execute) -> Self::Output {
        self.visit(&execute.transducer)?;
        self.visit(&execute.collection)?;

        if let Some(output_type) = &execute.output_type {
            self.visit(output_type)?;
            self.push(Instruction::new_collect_to());
        } else {
            self.push(Instruction::new_collect());
        }
        Ok(())
    }

    fn visit_quote(&mut self, quote: &crate::parser::ast::Quote) -> Self::Output {
        let converted = SteelVal::try_from(quote.expr.clone())?;
        let idx = self.constant_map.add_or_get(converted);
        self.push(Instruction::new_push_const(idx));

        Ok(())
    }

    fn visit_struct(&mut self, s: &crate::parser::ast::Struct) -> Self::Output {
        let builder = SteelStruct::generate_from_ast(&s)?;

        // Add the eventual function names to the symbol map
        // let indices = self.symbol_map.insert_struct_function_names(&builder);

        let names = builder.to_struct_function_names();

        // Fake adding the eventual function names to the symbol map
        let indices = vec![0; names.len()];

        // Add the variables to the locals here
        for name in names {
            self.variable_data.as_ref().map(|x| {
                x.borrow_mut().push_local(LocalVariable::new_struct(
                    self.depth,
                    name.clone(),
                    SyntaxObject::default(TokenType::Identifier(name)),
                    self.stack_offset,
                ))
            });
        }

        // Get the value we're going to add to the constant map for eventual use
        // Throw the bindings in as well
        let constant_values = builder.to_constant_val(indices);
        let idx = self.constant_map.add_or_get(constant_values);

        // Inside some nested scope, so these don't need anything more than the instruction
        self.push(Instruction::new_inner_struct(idx));

        Ok(())
    }

    fn visit_macro(&mut self, m: &crate::parser::ast::Macro) -> Self::Output {
        stop!(BadSyntax => "unexpected macro definition"; m.location.span)
    }

    fn visit_eval(&mut self, e: &crate::parser::ast::Eval) -> Self::Output {
        self.visit(&e.expr)?;
        self.push(Instruction::new_eval());
        Ok(())
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        let ident = if let SyntaxObject {
            ty: TokenType::Identifier(i),
            ..
        } = &a.syn
        {
            i
        } else {
            self.specialize_constant(&a.syn)?;
            return Ok(());
        };

        // Attempt to resolve this as a local variable
        if let Some(idx) = self
            .variable_data
            .as_ref()
            .map(|x| x.borrow().resolve_local(ident))
            .flatten()
        {
            self.push(Instruction::new_local(idx, a.syn.clone()));

            // Otherwise attempt to resolve this as an upvalue
        } else if let Some(idx) = self
            .variable_data
            .as_ref()
            .map(|x| x.borrow_mut().resolve_upvalue(ident))
            .flatten()
        {
            self.push(Instruction::new_read_upvalue(idx, a.syn.clone()));

        // Otherwise we resort to it being a global variable for now
        } else {
            self.push(Instruction::new(OpCode::PUSH, 0, a.syn.clone(), true));
        }

        Ok(())
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        // dbg!(l);

        // TODO this panics if l.args is empty

        if l.args.is_empty() {
            stop!(BadSyntax => "function application empty");
        }

        let pop_len = if l.args.len() > 0 {
            l.args[1..].len()
        } else {
            0
        };

        let mut let_context = false;

        // Check if this is a 'let' context
        if let crate::parser::ast::ExprKind::LambdaFunction(_) = &l.args[0] {
            let_context = true;
        }

        let mut tail_call_offsets: Vec<(&str, std::ops::Range<usize>)> = Vec::new();

        // emit instructions for the args
        for (idx, expr) in l.args[1..].iter().enumerate() {
            // Snag the length before the compilation
            let pre = self.len();

            if let_context {
                if let crate::parser::ast::ExprKind::LambdaFunction(_) = expr {
                    self.let_context = true;
                } else {
                    self.let_context = false;
                }
            }

            self.visit(expr)?;

            // Snag the length after
            let post = self.len();

            if self.let_context {
                if let crate::parser::ast::ExprKind::LambdaFunction(l) = &l.args[0] {
                    if let ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            },
                    }) = &l.args[idx]
                    {
                        tail_call_offsets.push((s.as_str(), pre..post));
                    }
                }
            }
        }

        self.let_context = false;

        // Capture the beginning
        let body_begin = self.len();

        // emit instructions for the func
        self.visit(&l.args[0])?;

        // Capture the end for checking
        let body_end = self.len();

        // At this point we should check for the tail call on the offsets provided
        // TODO
        for (name, span) in tail_call_offsets {
            if let Some((set, read, idx)) =
                identify_let_rec(&self.instructions[body_begin..body_end], name)
            {
                // println!("Found set: {}, read: {} @ idx: {}", set, read, idx);

                if !upvalue_func_used_before_set(
                    &self.instructions[body_begin..body_end],
                    &read,
                    idx,
                ) {
                    if identify_letrec_tailcall(&self.instructions[span.clone()], &set) {
                        // println!("Successfully identified letrec tailcall for: {}", set);
                        // crate::core::instructions::pretty_print_instructions(
                        // &self.instructions[span.clone()],
                        // );

                        // modify = true;

                        let b = transform_letrec_tail_call(&mut self.instructions[span], &set);
                        if b {
                            info!("Successfully performed self TCO for: {}", set);
                            // crate::core::instructions::pretty_print_instructions(
                            // &self.instructions,
                            // );

                            // println!(
                            //     "Attempting to unwind and set the variable to un captured: {}",
                            //     set
                            // );
                            // // unwind the reference
                            // self.variable_data
                            //     .as_ref()
                            //     .map(|x| x.borrow_mut().check_locals_for_variable_to_unmark(&set));
                        }
                        // println!(
                        //     "{}",
                        //     crate::core::instructions::disassemble(&self.instructions)
                        // );
                    } else {
                        info!("failed to identify letrec tailcall for {}", set);
                    }
                }
            }
        }

        if let ExprKind::Atom(Atom { syn: s }) = &l.args[0] {
            self.push(Instruction::new_func(pop_len, s.clone()));
        } else {
            // TODO check span information here by coalescing the entire list
            self.push(Instruction::new_func(
                pop_len,
                SyntaxObject::new(
                    TokenType::Identifier("lambda".to_string()),
                    get_span(&l.args[0]),
                ),
            ));
        }

        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        stop!(BadSyntax => "unexpected syntax rules"; l.location.span)
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        self.visit(&s.expr)?;
        if let ExprKind::Atom(Atom { syn: s }) = &s.variable {
            let ident = if let SyntaxObject {
                ty: TokenType::Identifier(i),
                ..
            } = &s
            {
                i
            } else {
                stop!(BadSyntax => "set! takes an identifier")
            };

            // Attempt to resolve this as a local variable
            if let Some(idx) = self
                .variable_data
                .as_ref()
                .map(|x| x.borrow().resolve_local(ident))
                .flatten()
            {
                // println!("new set local on {}", ident);
                self.push(Instruction::new_set_local(idx, s.clone()));

            // Otherwise attempt to resolve this as an upvalue
            } else if let Some(idx) = self
                .variable_data
                .as_ref()
                .map(|x| x.borrow_mut().resolve_upvalue(ident))
                .flatten()
            {
                // println!("new set upvalue on {} @ index: {}", ident, idx);
                self.push(Instruction::new_set_upvalue(idx, s.clone()));

            // Otherwise we resort to it being a global variable for now
            } else {
                // println!("pushing global");
                // println!("new set global on {}", ident);
                self.push(Instruction::new(OpCode::SET, 0, s.clone(), true));
            }
        } else {
            stop!(BadSyntax => "set! takes an identifier")
        }
        Ok(())
    }

    fn visit_require(&mut self, r: &crate::parser::ast::Require) -> Self::Output {
        stop!(BadSyntax => "unexpected require statement in code gen"; r.location.span)
    }

    // There may need to be more magic here
    // but for now, explore how the VM can handle this wth holding
    // the continuation as a value
    fn visit_callcc(&mut self, cc: &crate::parser::ast::CallCC) -> Self::Output {
        self.visit(&cc.expr)?;
        self.push(Instruction::new_call_cc());
        // self.push(Instruction::new_pop());
        Ok(())
    }
}

fn transform_tail_call(instructions: &mut [Instruction], defining_context: &str) -> bool {
    let last_idx = instructions.len() - 1;

    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    payload_size: arity,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                }),
            ) => {
                let arity = *arity;
                if s == defining_context {
                    let new_jmp = Instruction::new_tco_jmp();
                    // inject tail call jump
                    instructions[index - 2] = new_jmp;
                    instructions[index - 1] = Instruction::new_pass(arity);
                    transformed = true;

                    info!("Tail call optimization performed for: {}", defining_context);
                    // println!("Tail call optimization performed for: {}", defining_context);
                }
            }
            _ => {}
        }
    }

    transformed
}

// TODO modify this for finding pop instructions
fn transform_letrec_tail_call(instructions: &mut [Instruction], defining_context: &str) -> bool {
    // let last_idx = instructions.len() - 1;

    let mut last_idx = instructions.len() - 1;
    while last_idx > 0 {
        if let Some(Instruction {
            op_code: OpCode::POP,
            ..
        }) = instructions.get(last_idx)
        {
            break;
        }
        last_idx -= 1;
    }

    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::TAILCALL,
                    payload_size: arity,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::READUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                }),
            ) => {
                let arity = *arity;
                if s == defining_context {
                    let new_jmp = Instruction::new_tco_jmp();
                    // inject tail call jump
                    instructions[index - 2] = new_jmp;
                    instructions[index - 1] = Instruction::new_pass(arity);
                    transformed = true;

                    // println!("{}", crate::core::instructions::disassemble(instructions));

                    info!("Tail call optimization performed for: {}", defining_context);
                    // println!("Tail call optimization performed for: {}", defining_context);
                }
            }
            _ => {}
        }
    }

    transformed
}

// Find if this function has a valid TCO able situation
fn identify_letrec_tailcall(instructions: &[Instruction], ident: &str) -> bool {
    for i in 0..instructions.len() - 1 {
        let read = instructions.get(i);
        let set = instructions.get(i + 1);

        match (read, set) {
            (
                Some(Instruction {
                    op_code: OpCode::READUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(local_value),
                            ..
                        }),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::TAILCALL,
                    ..
                }),
            ) => {
                // println!("FOUND LOCAL VALUE: {}", local_value);
                if local_value == ident {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

// If the upvalue func has been used before the set, we can't TCO it
fn upvalue_func_used_before_set(instructions: &[Instruction], upvalue: &str, idx: usize) -> bool {
    // Iterate up to the set index
    // If the upvalue is used prior to that, don't use it
    for i in 0..idx {
        if let Some(Instruction {
            contents:
                Some(SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                }),
            ..
        }) = instructions.get(i)
        {
            if upvalue == s {
                return true;
            }
        }
    }

    false
}

// Use this to flatten calls to globals such that its just one instruction instead of two
pub fn convert_call_globals(instructions: &mut [Instruction]) {
    for i in 0..instructions.len() - 1 {
        let push = instructions.get(i);
        let func = instructions.get(i + 1);

        match (push, func) {
            (
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::CALLGLOBAL;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::PASS;
                }
            }
            (
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::TAILCALL,
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::CALLGLOBALTAIL;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::PASS;
                }
            }
            _ => {}
        }
    }
}

// 0    READLOCAL : 0
// 1    LOADINT2 : 12
// 2    CALLGLOBAL : 6
// 3    PASS : 2

// Often, there may be a loop condition with something like (= x 10000)
// this identifies these and lazily applies the function, only pushing on to the stack
// until it absolutely needs to
pub fn loop_condition_local_const_arity_two(instructions: &mut [Instruction]) {
    for i in 0..instructions.len() {
        let read_local = instructions.get(i);
        let push_const = instructions.get(i + 1);
        let call_global = instructions.get(i + 2);
        let pass = instructions.get(i + 3);

        match (read_local, push_const, call_global, pass) {
            (
                Some(Instruction {
                    op_code: OpCode::READLOCAL,
                    payload_size: local_idx,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSHCONST,
                    payload_size: const_idx,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CALLGLOBAL,
                    payload_size: ident,
                    contents: identifier,
                    ..
                }),
                // HAS to be arity 2 in this case
                Some(Instruction {
                    op_code: OpCode::PASS,
                    payload_size: 2,
                    ..
                }),
            ) => {
                let local_idx = *local_idx;
                let const_idx = *const_idx;
                let ident = *ident;
                let identifier = identifier.clone();

                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::CGLOCALCONST;
                    x.payload_size = ident;
                    x.contents = identifier;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::READLOCAL;
                    x.payload_size = local_idx;
                }

                if let Some(x) = instructions.get_mut(i + 2) {
                    x.op_code = OpCode::PUSHCONST;
                    x.payload_size = const_idx;
                }
            }
            _ => {}
        }
    }
}

// attempt to find if this is a TCO valid let rec situation
fn identify_let_rec(
    instructions: &[Instruction],
    context: &str,
) -> Option<(String, String, usize)> {
    // println!("Identifying let rec...");
    // crate::core::instructions::pretty_print_instructions(instructions);
    for i in 0..instructions.len() - 1 {
        let read = instructions.get(i);
        let set = instructions.get(i + 1);

        match (read, set) {
            (
                Some(Instruction {
                    op_code: OpCode::READLOCAL,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(local_value),
                            ..
                        }),
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::SETUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(ident_being_set),
                            ..
                        }),
                    ..
                }),
            ) => {
                // println!(
                //     "FOUND LOCAL_VALUE: {} AND IDENT: {}",
                //     local_value, ident_being_set
                // );

                if context == local_value {
                    return Some((ident_being_set.clone(), local_value.clone(), i));
                }
            }
            _ => {}
        }
    }

    None
}

// Note, this should be called AFTER `transform_tail_call`
fn check_and_transform_mutual_recursion(instructions: &mut [Instruction]) -> bool {
    let last_idx = instructions.len() - 1;

    // could panic
    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(_s),
                            ..
                        }),
                    ..
                }),
            )
            | (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::READUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(_s),
                            ..
                        }),
                    ..
                }),
            ) => {
                // let s = s.clone();
                if let Some(x) = instructions.get_mut(index - 1) {
                    x.op_code = OpCode::TAILCALL;
                    transformed = true;
                    // println!("Found tail call with: {}", &s);
                }
            }
            _ => {}
        }
    }

    transformed
}

/// evaluates an atom expression in given environment
fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => Ok((*b).into()),
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(SteelVal::NumV(*n)),
        TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.clone().into())),
        TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
        TokenType::IntegerLiteral(n) => Ok(SteelVal::IntV(*n)),
        what => {
            // println!("getting here in the eval_atom");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}
