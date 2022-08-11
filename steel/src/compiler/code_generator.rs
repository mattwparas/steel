use std::{cell::RefCell, convert::TryFrom, rc::Rc};

use super::constants::{ConstantMap};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::{ast::Atom, parser::{SyntaxObject, RawSyntaxObject}, span_visitor::get_span, tokens::TokenType},
    values::structs::StructFuncBuilder,
};

use crate::parser::ast::ExprKind;
use crate::parser::visitors::VisitorMut;

use crate::rvals::{Result, SteelVal};
use crate::stop;

use itertools::Itertools;
use log::info;

use std::collections::HashSet;

// use super::codegen::{check_and_transform_mutual_recursion, transform_tail_call};

#[derive(Clone, Debug)]
struct LocalVariable {
    name: String,
    is_captured: bool,
    struct_offset: usize,
    syntax_object: SyntaxObject,
}

impl LocalVariable {
    pub fn new(name: String, syntax_object: SyntaxObject, struct_offset: usize) -> Self {
        LocalVariable {
            name,
            is_captured: false,
            struct_offset,
            syntax_object,
        }
    }

    pub fn new_struct(name: String, syntax_object: SyntaxObject, struct_offset: usize) -> Self {
        LocalVariable {
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
    // name
    ident: String,
}

impl UpValue {
    pub fn new(index: usize, is_local: bool, ident: String) -> Self {
        UpValue {
            index,
            is_local,
            ident,
        }
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
        // if self.locals[index].is_local {
        //     return false;
        // } else {
        self.locals[index].is_captured = true;
        //     return true;
        // }
    }

    // Go backwards and attempt to find the index in which a local variable will live on the stack
    // returns (actual, stack)
    // TODO -> come up with a better algorithm for this
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
            .and_then(|x| x.borrow().resolve_local(ident));

        if let Some(local) = local {
            let var_offset = {
                self.enclosing
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .locals
                    .iter()
                    .rev()
                    .find(|x| &x.name == ident)
                    .map(|x| x.struct_offset)
                    .unwrap_or(0)
            };

            self.enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .mark_captured(local - var_offset);

            return Some(self.add_upvalue(local, true, ident.to_string()));
        }

        // Check upvalues afterwards
        let upvalue = self
            .enclosing
            .as_ref()
            .and_then(|x| x.borrow_mut().resolve_upvalue(ident));
        if let Some(upvalue) = upvalue {
            return Some(self.add_upvalue(upvalue, false, ident.to_string()));
        }

        // Otherwise we're a global and we should move on
        None
    }

    // Add the upvalue to the upvalue list, returning the index in the list
    fn add_upvalue(&mut self, index: usize, is_local: bool, ident: String) -> usize {
        // If the upvalue has already been captured, don't capture it again
        if let Some(i) = self
            .upvalues
            .iter()
            .position(|x| x.index == index && x.is_local == is_local)
        {
            return i;
        }
        self.upvalues.push(UpValue::new(index, is_local, ident));
        self.upvalues.len() - 1
    }
}

pub struct CodeGenerator<'a> {
    instructions: Vec<Instruction>,
    constant_map: &'a mut ConstantMap,
    defining_context: Option<String>,
    depth: u32,
    variable_data: Option<Rc<RefCell<VariableData>>>, // enclosing: Option<&'a mut CodeGenerator<'a>>,
    let_context: bool,
    stack_offset: usize,
    top_level_define: bool,
    rooted: bool,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_map: &'a mut ConstantMap) -> Self {
        CodeGenerator {
            instructions: Vec::new(),
            constant_map,
            defining_context: None,
            depth: 0,
            variable_data: None,
            let_context: false,
            stack_offset: 0,
            top_level_define: false,
            rooted: false,
            // enclosing: None,
        }
    }

    fn new_from_body_instructions(
        constant_map: &'a mut ConstantMap,
        instructions: Vec<Instruction>,
        depth: u32,
        variable_data: Option<Rc<RefCell<VariableData>>>,
        defining_context: Option<String>,
    ) -> Self {
        CodeGenerator {
            instructions,
            constant_map,
            defining_context,
            depth,
            variable_data,
            let_context: false,
            stack_offset: 0,
            top_level_define: false,
            rooted: false,
            // enclosing,
        }
    }

    pub fn top_level_compile(mut self, expr: &ExprKind) -> Result<Vec<Instruction>> {
        self.visit(expr)?;
        self.instructions.push(Instruction::new_pop());
        Ok(self.instructions)
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
}

impl<'a> VisitorMut for CodeGenerator<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        // load in the test condition
        self.visit(&f.test_expr)?;
        // Get the if index
        let if_idx = self.instructions.len();
        // push in if
        self.push(Instruction::new_if(self.instructions.len() + 2));
        // save spot of jump instruction, fill in after
        // let idx = self.len();
        // self.push(Instruction::new_jmp(0)); // dummy value

        // emit instructions for then
        self.visit(&f.then_expr)?;
        self.push(Instruction::new_jmp(0));
        let false_start = self.len();

        // emit instructions for else expression
        self.visit(&f.else_expr)?;
        let j3 = self.len(); // first instruction after else


        // println!("false_start: {:?}", false_start);
        // println!("j3: {:?}", j3);

        // // set index of jump instruction
        // if let Some(elem) = self.instructions.get_mut(idx) {
        //     (*elem).payload_size = false_start;
        // } else {
        //     stop!(Generic => "out of bounds jump");
        // }

        if let Some(elem) = self.instructions.get_mut(false_start - 1) {
            (*elem).payload_size = j3;
            // (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        if let Some(elem) = self.instructions.get_mut(if_idx) {
            (*elem).payload_size = false_start;
            // (*elem).payload_size = false_start;
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
            self.top_level_define = true;

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
            self.top_level_define = false;
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

        // TODO
        // Multi arity function...
        self.push(Instruction::new_pass(if lambda_function.rest {
            1
        } else {
            0
        }));

        // println!("Creating closure at depth: {}", self.depth);
        // TODO -> figure out lets here to make this not so ugly
        self.push(Instruction::new_pass(if self.let_context && !self.rooted {
            1
        } else {
            0
        }));

        let mut body_instructions = Vec::new();

        let l = &lambda_function.args;

        let mut locals = Vec::new();

        let arity = l.len();
        // let rev_iter = l.iter().rev();
        let rev_iter = l.iter();

        // We want to keep track of the arguments here to see if any of them are duplicate
        let mut args = HashSet::new();

        for symbol in rev_iter {
            if let ExprKind::Atom(atom) = symbol {
                match &atom.syn {
                    SyntaxObject {
                        ty: TokenType::Identifier(i),
                        span: sp,
                        ..
                    } => {
                        // If we've seen this one before, bail out - we don't want duplicates here
                        if !args.insert(i) {
                            println!("{}", lambda_function);

                            stop!(BadSyntax => "lambda function cannot have duplicate argument names"; *sp);
                        }

                        locals.push(LocalVariable::new(i.clone(), atom.syn.clone(), 0));
                        // println!("Validating the identifiers in the arguments");
                        // body_instructions.push(Instruction::new_bind(atom.syn.clone()));
                    }
                    SyntaxObject {
                        ty: _, span: sp, ..
                    } => {
                        stop!(BadSyntax => "lambda function requires list of identifiers"; *sp);
                    }
                }
            } else {
                // stop!(Generic => "lambda function requires list of identifiers"; symbol.span());
                // TODO come back add the span
                stop!(BadSyntax => "lambda function requires list of identifiers");
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
            body_instructions,
            self.depth + 1, // pass through the depth
            Some(Rc::clone(&variable_data)),
            if self.top_level_define || self.let_context {
                self.defining_context.clone()
            } else {
                None
            },
            // self.defining_context.clone(),
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
                // println!("Pushing new local upvalue!: {:?}", upvalue);
                // self.push(Instruction::new_local_upvalue(upvalue.index));
                self.push(Instruction::new_local_upvalue(upvalue.index));
            } else {
                // println!("Pushing new upvalue!: {:?}", upvalue);
                self.push(Instruction::new_upvalue(upvalue.index));
            }
        }

        // Encode the amount to pop
        body_instructions.push(Instruction::new_pop_with_upvalue(
            variable_data.borrow().locals.len(),
        ));

        if let Some(ctx) = &self.defining_context {
            // if !self.let_context {
            if self.top_level_define || self.let_context {
                if self.top_level_define {
                    // println!("Transforming tail call!");
                    transform_tail_call(&mut body_instructions, ctx);
                }

                // TODO check this here - reimplement mutual recursion
                let b = check_and_transform_mutual_recursion(&mut body_instructions);

                // if self.top_level_define {
                //     // If we're already converted the mutual recursion lower, then we check again for a faster TCO jump
                //     b = convert_mutual_recursion_to_tail_call(&mut body_instructions, &ctx) || b;
                // }

                // println!(
                //     "Converting last usages with let context: {} and top level define: {}",
                //     self.let_context, self.top_level_define,
                // );

                // TODO - this is broken still

                // let b = false;

                if b {
                    info!("Transformed mutual recursion for: {}", ctx);
                    // println!("Transformed mutual recursion for: {}", ctx);
                }
            }
        } else if self.let_context {
            let b = check_and_transform_mutual_recursion(&mut body_instructions);

            // let b = false;

            if b {
                info!("Transformed mutual recursion inside local context");
                // println!("defining context: {:?}", self.defining_context);
                // println!("Transformed mutual recursion inside local context");
            }
        }

        // Go ahead and include the variable information for the popping
        // This needs to be handled accordingly
        // TODO this could be handled better - just put the index that needs to be saved
        // for now, is_captured is just a noop
        for local in variable_data.borrow().locals.iter() {
            body_instructions.push(Instruction::new_close_upvalue(
                if local.is_captured { 1 } else { 0 },
                local.syntax_object.clone(),
            ))
        }

        // Do this here to include the close upvalue information if we need to
        if self.top_level_define {
            convert_last_usages(&mut body_instructions);
        }

        self.instructions.append(&mut body_instructions);

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

    fn visit_quote(&mut self, quote: &crate::parser::ast::Quote) -> Self::Output {
        // let converted = SteelVal::try_from(quote.expr.clone())?;
        let converted =
            SteelVal::try_from(crate::parser::ast::ExprKind::Quote(Box::new(quote.clone())))?;

        let idx = self.constant_map.add_or_get(converted);
        self.push(Instruction::new_push_const(idx));

        Ok(())
    }

    fn visit_struct(&mut self, s: &crate::parser::ast::Struct) -> Self::Output {
        let builder = StructFuncBuilder::generate_from_ast(&s)?;

        // Add the eventual function names to the symbol map
        // let indices = self.symbol_map.insert_struct_function_names(&builder);

        let names = builder.to_struct_function_names();

        // Fake adding the eventual function names to the symbol map
        let indices = vec![0; names.len()];

        // Add the variables to the locals here
        for name in names {
            self.variable_data.as_ref().map(|x| {
                x.borrow_mut().push_local(LocalVariable::new_struct(
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
            .and_then(|x| x.borrow().resolve_local(ident))
        {
            self.push(Instruction::new_local(idx, a.syn.clone()));

            // Otherwise attempt to resolve this as an upvalue
        } else if let Some(idx) = self
            .variable_data
            .as_ref()
            .and_then(|x| x.borrow_mut().resolve_upvalue(ident))
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

        // let defining_context_before = self.defining_context.take();
        let mut defining_context_before = None;

        // emit instructions for the args
        for (idx, expr) in l.args[1..].iter().enumerate() {
            // Snag the length before the compilation
            let pre = self.len();

            if let_context {
                if let crate::parser::ast::ExprKind::LambdaFunction(_) = expr {
                    // println!("Setting let context to true");
                    self.let_context = true;
                    self.rooted = true;

                    // TODO -> looks like this is going to get overwritten on each arg
                    defining_context_before = self.defining_context.take();
                } else {
                    self.let_context = false;
                }
            }

            self.visit(expr)?;

            self.rooted = false;

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

        if self.defining_context.is_none() {
            self.defining_context = defining_context_before;
        }

        // Set the context of the body to be as if we are in a function application
        self.let_context = let_context;

        // self.let_context = false;

        // Capture the beginning
        let body_begin = self.len();

        // emit instructions for the func
        self.visit(&l.args[0])?;

        // self.let_context = false;

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
                stop!(BadSyntax => "set! takes an identifier"; s.span)
            };

            // Attempt to resolve this as a local variable
            if let Some(idx) = self
                .variable_data
                .as_ref()
                .and_then(|x| x.borrow().resolve_local(ident))
            {
                // println!("new set local on {}", ident);
                self.push(Instruction::new_set_local(idx, s.clone()));

            // Otherwise attempt to resolve this as an upvalue
            } else if let Some(idx) = self
                .variable_data
                .as_ref()
                .and_then(|x| x.borrow_mut().resolve_upvalue(ident))
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
            stop!(BadSyntax => "set! takes an identifier"; s.location.span)
        }
        Ok(())
    }

    fn visit_require(&mut self, r: &crate::parser::ast::Require) -> Self::Output {
        stop!(BadSyntax => "unexpected require statement in code gen"; r.location.span)
    }

    // There may need to be more magic here
    // but for now, explore how the VM can handle this wth holding
    // the continuation as a value


    // Certainly the most complicated case
    fn visit_let(&mut self, l: &crate::parser::ast::Let) -> Self::Output {
        // todo!()

        // Start with beginn scope
        // self.push(Instruction::new_instruction(OpCode::BEGINSCOPE));

        // let mut locals = Vec::new();

        let (bindings, exprs): (Vec<_>, Vec<_>) = l.bindings.iter().cloned().unzip();

        // If we need to pop the scope off, do it
        // If we're at the top level, this is going to happen anyway
        let offset = self
            .variable_data
            .as_ref()
            .map(|x| x.borrow().locals.len() + self.stack_offset);

        // let mut locals = Vec::new();

        // Just visit the expressions first
        for expr in &exprs {
            self.visit(&expr)?;
        }

        for symbol in bindings {
            if let ExprKind::Atom(atom) = symbol {
                match &atom.syn {
                    SyntaxObject {
                        ty: TokenType::Identifier(i),
                        ..
                    } => {
                        // locals.push(LocalVariable::new(
                        //     self.depth + 1,
                        //     i.clone(),
                        //     atom.syn.clone(),
                        //     self.stack_offset,
                        // ));

                        match &self.variable_data {
                            Some(variable_data) => variable_data.borrow_mut().locals.push(
                                LocalVariable::new(i.clone(), atom.syn.clone(), self.stack_offset),
                            ),
                            None => {
                                let locals = vec![LocalVariable::new(
                                    i.clone(),
                                    atom.syn.clone(),
                                    self.stack_offset,
                                )];

                                // Initialize it here?
                                self.variable_data =
                                    Some(Rc::new(RefCell::new(VariableData::new(
                                        locals,
                                        Vec::new(),
                                        self.variable_data.as_ref().map(Rc::clone),
                                    ))));
                            }
                        }
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

        // let mut variable_data = Some(Rc::new(RefCell::new(VariableData::new(
        //     locals,
        //     Vec::new(),
        //     self.variable_data.as_ref().map(Rc::clone),
        // ))));

        // std::mem::swap(&mut self.variable_data, &mut variable_data);

        // self.variable_data = Some(variable_data);

        // Visit the args

        // Snatch access to parent information here
        // That way we can at least have a shot of going backwards
        // let variable_data = Rc::new(RefCell::new(VariableData::new(
        //     locals,
        //     Vec::new(),
        //     self.variable_data.as_ref().map(Rc::clone),
        // )));

        // // Use the new one
        // let prev_variable_data = std::mem::replace(&mut self.variable_data, Some(variable_data));

        // Increment the depth before we visit the body
        self.depth += 1;

        // Visit the body now
        self.visit(&l.body_expr)?;

        // unwind the recursion
        self.depth -= 1;

        // self.variable_data = prev_variable_data;

        let mut close_upvalues = Vec::new();

        // info!(
        //     "Finished compiling a let, with variable information: {:#?}",
        //     self.variable_data
        // );

        // Pop off the locals once we exit this scope
        for _ in &exprs {
            if let Some(local) = self
                .variable_data
                .as_ref()
                .and_then(|x| x.borrow_mut().locals.pop())
            {
                close_upvalues.push(Instruction::new_close_upvalue(
                    if local.is_captured { 1 } else { 0 },
                    local.syntax_object.clone(),
                ))
            }
        }

        if self
            .variable_data
            .as_ref()
            .map(|x| x.borrow().locals.is_empty())
            .unwrap_or(false)
        {
            self.variable_data = None;
        }

        if let Some(offset) = offset {
            self.push(Instruction::new_end_scope(offset));
            self.push(Instruction::new_pass(exprs.len()));

            for instr in close_upvalues {
                self.push(instr);
            }
        }

        // Update it post recursion
        // std::mem::swap(&mut self.variable_data, &mut variable_data);

        // Pop
        // self.push(Instruction::new_end_scope(exprs.len()));

        // self.push(Instruction::new_instruction(OpCode::ENDSCOPE));

        Ok(())

        // todo!()
    }
}

// TODO -> assess if this is worth looking at
fn _convert_mutual_recursion_to_tail_call(
    instructions: &mut [Instruction],
    defining_context: &str,
) -> bool {
    let mut transformed = false;

    if instructions.is_empty() {
        return false;
    }

    for index in 2..instructions.len() {
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
                    let new_jmp = Instruction::new_tco_jmp(arity);
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

// TODO - this kind of kills the entirety of the tail call optimization
// if the set! aliases to another function, then just quit
//
// TODO -> this also doesn't work if the function gets overwritten in name
fn check_if_defining_context_is_aliased(
    instructions: &[Instruction],
    defining_context: &str,
) -> bool {
    for instruction in instructions {
        if instruction.op_code == OpCode::SET || instruction.op_code == OpCode::SETUPVALUE {
            if let Some(atom) = &instruction.contents {
                if let TokenType::Identifier(ident) = &atom.ty {
                    if ident == defining_context {
                        return true;
                    }
                }
            }
        }
    }

    false
}

fn transform_tail_call(instructions: &mut [Instruction], defining_context: &str) -> bool {
    // If the defining_context aliases, then we need to quit
    if check_if_defining_context_is_aliased(instructions, defining_context) {
        return false;
    }

    let last_idx = instructions.len() - 1;

    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }

        if instruction.op_code == OpCode::JMP {
            // If we found a jump, go to where it said to jump to
            let mut next = &instructions[instruction.payload_size];
            let mut last;

            while next.op_code == OpCode::JMP {
                if next.payload_size == last_idx {
                    indices.push(idx);
                    break;
                } else {
                    last = next;
                    next = &instructions[instruction.payload_size];

                    if last == next {
                        break;
                    }
                }
            }
        }
    }

    // Clear out any of the end scope stuff
    // What this does - go through the indices and see if this points to an end scope
    // if it does, just continue to try to find _something_ else
    for index in &mut indices {
        loop {
            match instructions.get(*index - 1) {
                Some(Instruction {
                    op_code: OpCode::ENDSCOPE,
                    ..
                })
                | Some(Instruction {
                    op_code: OpCode::PASS,
                    ..
                })
                | Some(Instruction {
                    op_code: OpCode::CLOSEUPVALUE,
                    ..
                }) => *index -= 1,
                _ => break,
            }
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
                    let new_jmp = Instruction::new_tco_jmp(arity);
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

    // println!("Inside transform letrec tail call!");

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

        if instruction.op_code == OpCode::JMP {
            // If we found a jump, go to where it said to jump to
            let mut next = &instructions[instruction.payload_size];
            let mut last;

            while next.op_code == OpCode::JMP {
                if next.payload_size == last_idx {
                    indices.push(idx);
                    break;
                } else {
                    last = next;
                    next = &instructions[instruction.payload_size];

                    if last == next {
                        break;
                    }
                }
            }
        }
    }

    // Clear out any of the end scope stuff
    // What this does - go through the indices and see if this points to an end scope
    // if it does, just continue to try to find _something_ else
    for index in &mut indices {
        loop {
            match instructions.get(*index - 1) {
                Some(Instruction {
                    op_code: OpCode::ENDSCOPE,
                    ..
                })
                | Some(Instruction {
                    op_code: OpCode::PASS,
                    ..
                })
                | Some(Instruction {
                    op_code: OpCode::CLOSEUPVALUE,
                    ..
                }) => *index -= 1,
                _ => break,
            }
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
                    let new_jmp = Instruction::new_tco_jmp(arity);
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
                // println!("Upvalue func used before set returned true");
                return true;
            }
        }
    }

    false
}

// TODO run this for every function, not the entire program
pub fn convert_last_usages(instructions: &mut [Instruction]) {
    if instructions.is_empty() {
        return;
    }

    let mut variables: HashSet<String> = HashSet::new();

    for instruction in instructions.iter() {
        match instruction {
            Instruction {
                op_code: OpCode::READLOCAL,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(local),
                        ..
                    }),
                ..
            }
            | Instruction {
                op_code: OpCode::READUPVALUE,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(local),
                        ..
                    }),
                ..
            } => variables.insert(local.to_string()),
            _ => continue,
        };
    }

    // Always include the starting value
    let mut exit_function_points: Vec<usize> = vec![0];

    // Find the tail call'd exit points
    for (idx, instruction) in instructions.iter().enumerate() {
        match instruction {
            Instruction {
                op_code: OpCode::TCOJMP,
                ..
            }
            // only do this with built ins - thats when we want to transfer ownership
            // Instruction {
            //     op_code: OpCode::CALLGLOBALTAIL,
            //     ..
            // }
            | Instruction {
                op_code: OpCode::TAILCALL,
                ..
            } => exit_function_points.push(idx),
            _ => continue,
        }
    }

    // println!("Exit points: {:?}", exit_function_points);

    for (right, left) in exit_function_points.into_iter().rev().tuple_windows() {
        let mut stack = Vec::new();

        // println!("Looking at window: {:?}", (left, right));

        // TODO need to use the original slice indices, so offset by left
        for (index, instruction) in instructions[left..right].iter().rev().enumerate() {
            match instruction {
                // TODO
                // If theres another function call in this branch we want to ignore it
                Instruction {
                    op_code: OpCode::JMP,
                    ..
                } => {
                    // println!("Found branch, ignoring");
                    break;
                }
                Instruction {
                    op_code: OpCode::READLOCAL,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(local),
                            ..
                        }),
                    ..
                }
                //// TODO -> consider looking into this
                | Instruction {
                    op_code: OpCode::READUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(local),
                            ..
                        }),
                    ..
                } 
                => {
                    stack.push((right - index - 1, local.to_string()));
                }
                _ => continue,
            }
        }

        // TODO -> if variable is closed over
        // just ignore it entirely

        let mut closed_over_vars_to_ignore = HashSet::new();

        for instruction in instructions.iter() {
            match instruction {
                Instruction {
                    op_code: OpCode::CLOSEUPVALUE,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(local),
                            ..
                        }),
                    ..
                } => closed_over_vars_to_ignore.insert(local.to_string()),
                _ => continue,
            };
        }

        // println!("closed over: {:?}", closed_over_vars_to_ignore);
        // println!("Instructions at this point: {:#?}", instructions);

        let mut seen = HashSet::new();
        let filtered: Vec<_> = stack
            .iter()
            // .rev()
            .filter(|pair| {
                let valid = variables.contains(&pair.1);
                let var_seen = seen.contains(&pair.1);
                let closed_over = closed_over_vars_to_ignore.contains(&pair.1);
                if valid && !var_seen && !closed_over {
                    seen.insert(&pair.1);
                    true
                } else {
                    false
                }
            })
            .collect();

        // Sort the stack by the variable name
        // stack.sort_by(|x, y| x.1.cmp(&y.1));

        // let filtered: Vec<_> = stack.into_iter().dedup_by(|x, y| x.1 == y.1).collect();

        for (index, _var) in filtered {
            match instructions[*index].op_code {
                OpCode::READLOCAL => {
                    println!("Transforming move read local: {}", _var);
                    instructions[*index].op_code = OpCode::MOVEREADLOCAL;
                }
                OpCode::READUPVALUE => {
                    // println!("Skipping read upvalue: {}", _var);
                    continue;
                }
                // OpCode::CALLGLOBAL | OpCode::FUNC => {
                //     continue;
                // }

                // Instruction {
                //     op_code: OpCode::
                // }
                // Instruction {
                //     op_code: OpCode::READLOCAL,
                //     ..
                // } => {

                // }
                // Instruction {
                //     op_code: OpCode::READUPVALUE,
                //     ..
                // } => {
                //     continue;
                //     // println!("Transforming move read upvalue");
                //     // instructions[*index].op_code = OpCode::MOVEREADUPVALUE;
                // }
                _ => {} // instr => println!("{:?}", instr),
            }
        }

        // let mut filtered = Vec::new();

        // for pair in filtered.iter().rev() {

        // }

        // TODO - iterate backwards to see if the value is there first

        // println!("{:?}", filtered);
    }

    // println!("Right: {}, Left: {}", right, left);
}

// for exit_point in exit_function_points {
//     let mut stack = Vec::new();

//     for instruction in instructions[0..exit_point].iter_mut() {
//         match instruction {
//             Instruction {
//                 op_code: OpCode::READLOCAL,
//                 contents:
//                     Some(SyntaxObject {
//                         ty: TokenType::Identifier(local),
//                         ..
//                     }),
//                 ..
//             }
//             | Instruction {
//                 op_code: OpCode::READUPVALUE,
//                 contents:
//                     Some(SyntaxObject {
//                         ty: TokenType::Identifier(local),
//                         ..
//                     }),
//                 ..
//             } => {
//                 stack.push(local);
//             }
//             _ => continue,
//         }
//     }

//     // todo!()
// }

// for instruction in instructions {
//     match instruction {
//         Instruction {
//             op_code: OpCode::READLOCAL,
//             contents:
//                 Some(SyntaxObject {
//                     ty: TokenType::Identifier(local),
//                     ..
//                 }),
//             ..
//         }
//         | Instruction {
//             op_code: OpCode::READUPVALUE,
//             contents:
//                 Some(SyntaxObject {
//                     ty: TokenType::Identifier(local),
//                     ..
//                 }),
//             ..
//         } => {
//             todo!()
//         }
//         _ => continue,
//     }
// }
// }

pub fn specialize_constants(instructions: &mut [Instruction]) -> Result<()> {
    if instructions.is_empty() {
        return Ok(());
    }

    for i in 0..instructions.len() {

        match instructions.get(i) {
            Some(Instruction {
                op_code: OpCode::PUSHCONST,
                contents:
                    Some(
                        SyntaxObject {
                            ty: TokenType::Identifier(_),
                            ..
                        },
                    ),
                ..
            }) => continue,
            Some(Instruction {
                op_code: OpCode::PUSHCONST,
                contents:
                    Some(
                        syn
                    ),
                ..
            }) => {
                let value = eval_atom(syn)?;
    
                let opcode = match &value {
                    SteelVal::IntV(0) => OpCode::LOADINT0,
                    SteelVal::IntV(1) => OpCode::LOADINT1,
                    SteelVal::IntV(2) => OpCode::LOADINT2,
                    _ => continue,
                };
    
                (*instructions.get_mut(i).unwrap()).op_code = opcode;
            }
            _ => continue,
        }
    }

    Ok(())
}

// fn specialize_constant(&mut self, syn: &SyntaxObject) -> Result<()> {
//     let value = eval_atom(syn)?;

//     let opcode = match &value {
//         // SteelVal::IntV(1) => OpCode::LOADINT1,
//         // SteelVal::IntV(2) => OpCode::LOADINT2,
//         _ => OpCode::PUSHCONST,
//     };

//     let idx = self.constant_map.add_or_get(value);
//     self.push(Instruction::new(opcode, idx, syn.clone(), true));
//     Ok(())
// }

// Use this to flatten calls to globals such that its just one instruction instead of two
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

pub fn inline_num_operations(instructions: &mut [Instruction]) {


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
                    op_code: OpCode::FUNC | OpCode::TAILCALL,
                    contents: Some(RawSyntaxObject { ty: TokenType::Identifier(ident), ..}),
                    payload_size,
                    ..
                }),
            ) => {

                let replaced = match ident.as_ref() {
                    "+" => Some(OpCode::ADD),
                    "-" => Some(OpCode::SUB),
                    "/" => Some(OpCode::DIV),
                    "*" => Some(OpCode::MUL),
                    "equal?" => Some(OpCode::EQUAL),
                    "<=" => Some(OpCode::LTE),
                    _ => None
                };

                if let Some(new_op_code) = replaced {
                    let payload_size = *payload_size;
                    if let Some(x) = instructions.get_mut(i) {
                        x.op_code = new_op_code;
                        x.payload_size = payload_size;
                    }
    
                    if let Some(x) = instructions.get_mut(i + 1) {
                        x.op_code = OpCode::PASS;
                    }
                } 

                
            }
            _ => {}
        }
    }


}

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
            (
                Some(Instruction {
                    op_code: OpCode::MOVEREADLOCAL,
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
                    x.op_code = OpCode::MOVECGLOCALCONST;
                    x.payload_size = ident;
                    x.contents = identifier;
                }

                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::MOVEREADLOCAL;
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

// (define (blagh) (if (vector 1 2 3 4 5) (if (vector) (cons 1 2) (cons 3 4)) 10))

// Note, this should be called AFTER `transform_tail_call`
fn check_and_transform_mutual_recursion(instructions: &mut [Instruction]) -> bool {
    let last_idx = instructions.len() - 1;

    // could panic
    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
            continue;
        }

        // println!("{}", crate::core::instructions::disassemble(instructions));

        // // Iterate until the end of the jumps, include them
        if instruction.op_code == OpCode::JMP {
            // If we found a jump, go to where it said to jump to
            let mut next = &instructions[instruction.payload_size];
            let mut last;

            while next.op_code == OpCode::JMP {
                if next.payload_size == last_idx {
                    indices.push(idx);
                    break;
                } else {
                    last = next;
                    next = &instructions[instruction.payload_size];

                    if last == next {
                        break;
                    }
                }
            }
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
        // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
        TokenType::Keyword(k) => Ok(SteelVal::SymbolV(k.clone().into())),
        what => {
            println!("getting here in the eval_atom");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}
