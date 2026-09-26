//! Lowering of pair, list, vector and box operations.
//!
//! The bodies of `stack_to_ssa`'s larger match arms. The arms themselves, with
//! their guards, stay in `stack_to_ssa`, in order.

use super::*;

impl FunctionTranslator<'_> {
    /// `null?` of a local's register.
    pub(super) fn null_register(&mut self) {
        let last = self.shadow_stack_pop().unwrap().into_index();
        // A cursor answers without being materialised, since index 0
        // means the empty list.
        let result = match self.properties.cached_lookups.cursors.get(&last).copied() {
            Some(cursor) => {
                let index = self.builder.use_var(cursor.index);
                self.builder.ins().icmp_imm_s(IntCC::Equal, index, 0)
            }
            None => {
                let value = self.read_from_vm_stack(last);
                self.check_null_no_drop(value)
            }
        };

        // Okay, we're going to try branch on properties, and assert
        // type checks depending on which branch we take. If the test
        // condition has a property associated with it, then we go ahead
        // and take that on the then branch. Luckily, `and` conditions
        // lower to if statements, so we should really only have an
        // individual check on the condition.
        self.properties.add_property(
            ValueOrRegister::Value(result),
            Properties::CheckedNull(ValueOrRegister::Register(last)),
        );

        self.push(result, InferredType::UnboxedBool);

        self.ip += 2;
    }

    /// `CONS`.
    pub(super) fn lower_cons(&mut self, op: OpCode) {
        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            // Probably could instead just write the value back to the register
            &[MaybeStackValue::Value(_), MaybeStackValue::MutRegister(l)] => {
                let register = self.shadow_stack_pop().unwrap().into_index();
                // Use `shadow_stack_pop` since a spilled operand lives on
                // the vm stack, and taking only its ssa value leaves that
                // copy behind. Everything above it would then shift by a
                // slot, and the self tail call below reads its arguments
                // off the top.
                let value = self
                    .shadow_stack_pop()
                    .unwrap()
                    .into_value(self)
                    .as_steelval(self);

                let register = self.register_index(register);

                // Just... leave it in place if it mutates the register.
                // We can lazily have the register move around.
                self.call_helper_void(CONS_HANDLER_VALUE_REGISTER, [value, register]);

                let result = MaybeStackValue::MutRegister(l);

                // TODO: Make the type as inferred?

                // Check the inferred type, if we know of it
                self.shadow_push(result);

                self.ip += 2;
            }

            &[MaybeStackValue::MutRegister(v), MaybeStackValue::MutRegister(l)] => {
                let register = self.shadow_stack_pop().unwrap().into_index();
                let value = self.shadow_stack_pop().unwrap().into_index();

                let register = self.register_index(register);
                let value = self.register_index(value);

                // Just... leave it in place if it mutates the register.
                // We can lazily have the register move around.
                self.call_helper_void(CONS_HANDLER_REGISTER_REGISTER, [value, register]);

                let result = MaybeStackValue::MutRegister(l);

                self.properties.set_property(
                    ValueOrRegister::Register(v),
                    Properties::InferredType(InferredType::Void),
                );

                // TODO: Make the type as inferred?

                // Check the inferred type, if we know of it
                self.shadow_push(result);

                self.ip += 2;
            }

            // Anything else, we'll move on. The result isn't a `List`
            // here, since consing onto anything but a list makes a pair.
            _ => {
                self.func_ret_val(op, 2, 2, InferredType::ListOrPair);
            }
        }
    }

    /// `CDR`.
    pub(super) fn lower_cdr(&mut self, op: OpCode) {
        if let Some(last) = self.shadow_stack.last().copied() {
            self.shadow_mark_local_type_from_var(last, InferredType::ListOrPair);
        }

        match self.shadow_stack.last().unwrap().clone() {
            // `(cdr l)` where `l` is still read afterwards. Build the
            // rest from an advanced copy of the cursor. This is one
            // helper call like the plain path, but skips writing the
            // cursor back into the slot, and the cursor stays live.
            MaybeStackValue::Register(reg)
                if self.cursors_on
                    && matches!(
                        self.properties.get(&ValueOrRegister::Register(reg)),
                        Some(Properties::ProperNonEmptyList)
                    )
                    && self.properties.cached_lookups.cursors.contains_key(&reg) =>
            {
                let rest_type = self.cdr_result_type(reg);
                self.cursor_copy_reads += 1;
                self.shadow_stack_pop();
                let cursor = self.properties.cached_lookups.cursors[&reg];
                let rest = ListCursor {
                    cell: self.builder.declare_var(types::I64),
                    index: self.builder.declare_var(types::I64),
                };
                let cell = self.builder.use_var(cursor.cell);
                let index = self.builder.use_var(cursor.index);
                self.builder.def_var(rest.cell, cell);
                self.builder.def_var(rest.index, index);
                self.cursor_advance(rest);

                let cell = self.builder.use_var(rest.cell);
                let index = self.builder.use_var(rest.index);
                let data = self.builder.ins().iadd_imm_s(cell, list_cell_base());
                let res = self.call_helper(LIST_CURSOR_MATERIALIZE, [data, index]);
                self.push(res, rest_type);
                self.ip += 2;
            }
            MaybeStackValue::Register(reg) => {
                let can_skip_bounds_check = matches!(
                    self.properties.get(&ValueOrRegister::Register(reg)),
                    Some(Properties::ProperNonEmptyList)
                );
                // The rest of a proper list is a proper list. On an
                // empty list the helper raises, and the deopt check
                // below leaves before the result is used.
                let rest_type = self.cdr_result_type(reg);

                self.properties.add_property(
                    ValueOrRegister::Register(reg),
                    Properties::NonEmptyListOrPair,
                );

                self.shadow_stack_pop();
                let reg = self.register_index(reg);

                if can_skip_bounds_check {
                    let res = self.call_helper(CDR_REG_NO_CHECK, [reg]);
                    self.push(res, rest_type);
                } else {
                    let res = self.call_helper_checked(CDR_REG, [reg]);
                    self.push(res, rest_type);
                };

                self.ip += 2;
            }

            // Can we... avoid checking this fact?
            MaybeStackValue::MutRegister(reg) => {
                let can_skip_bounds_check = matches!(
                    self.properties.get(&ValueOrRegister::Register(reg)),
                    Some(Properties::ProperNonEmptyList)
                );

                // Walk it as a cursor instead of building a list. Only
                // for a header cursor that is live here, and only where
                // emptiness is ruled out, so nothing here can raise
                // while it is live. A cursor must not be live across a
                // deopt.
                if self.cursors_on
                    && can_skip_bounds_check
                    && self.properties.cached_lookups.cursors.contains_key(&reg)
                {
                    self.shadow_stack_pop();
                    let cursor = self.properties.cached_lookups.cursors[&reg];
                    self.cursor_advance(cursor);
                    self.properties
                        .set_property(ValueOrRegister::Register(reg), Properties::ProperList);
                    self.shadow_push(MaybeStackValue::MutRegister(reg));
                    self.ip += 2;
                    return;
                }

                self.shadow_stack_pop();
                let ir_reg = self.register_index(reg);

                if can_skip_bounds_check {
                    self.properties
                        .set_property(ValueOrRegister::Register(reg), Properties::ProperList);

                    self.call_helper_void(CDR_MUT_REG_NO_CHECK, [ir_reg]);
                    self.shadow_push(MaybeStackValue::MutRegister(reg));
                } else {
                    let rest_type = self.cdr_result_type(reg);
                    self.properties.props.insert(
                        ValueOrRegister::Register(reg),
                        vec![Properties::InferredType(InferredType::Void)],
                    );

                    let res = self.call_helper_checked(CDR_MUT_REG, [ir_reg]);
                    self.push(res, rest_type);
                };

                self.ip += 2;
            }

            _ => {
                self.func_ret_val(op, 1, 2, InferredType::Any);
                self.check_deopt();
            }
        }
    }

    /// `LIST`: build a list of the operands.
    pub(super) fn lower_list(&mut self, payload: usize) {
        // Return a list:
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();
        self.ip += 1;

        if let Some(function_name) = ListHandlerDefinitions::arity_to_name(payload) {
            let args = self.split_off(payload);

            let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

            let result = self.call_function_returns_value_args(function_name, &args);

            // Check the inferred type, if we know of it
            self.push(result, InferredType::List);
        } else {
            let args = self.split_off(payload);

            for arg in args {
                self.push_to_vm_stack(arg.0);
            }

            let arity_value = self.i64_const(arity as i64);

            let result = self.call_helper(LIST_HANDLER_SPILLED, [arity_value]);

            // Check the inferred type, if we know of it
            self.push(result, InferredType::List);
        }
    }

    /// `VEC`: build a vector of the operands.
    pub(super) fn lower_vec(&mut self, payload: usize) {
        let arity = payload / 2;

        self.ip += 1;

        let args = self.split_off(arity);

        for arg in args {
            self.push_to_vm_stack(arg.0);
        }

        let arity_value = self.i64_const(payload as i64);

        let result = self.call_helper(VEC_HANDLER_SPILLED, [arity_value]);

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Any);
    }

    /// `CAR`.
    pub(super) fn lower_car(&mut self, op: OpCode) {
        if let Some(last) = self.shadow_stack.last().copied() {
            self.shadow_mark_local_type_from_var(last, InferredType::ListOrPair);
        }

        match self.shadow_stack.last().unwrap().clone() {
            // A cursor-backed list reads its element in place. The
            // cell and position are already in registers, and the
            // head in the slot keeps the cell alive, so the cursor
            // survives the read, even a moving one.
            MaybeStackValue::MutRegister(reg) | MaybeStackValue::Register(reg)
                if self.cursors_on
                    && matches!(
                        self.properties.get(&ValueOrRegister::Register(reg)),
                        Some(Properties::ProperNonEmptyList)
                    )
                    && self.properties.cached_lookups.cursors.contains_key(&reg) =>
            {
                self.shadow_stack_pop();
                let cursor = self.properties.cached_lookups.cursors[&reg];
                let cell = self.builder.use_var(cursor.cell);
                let index = self.builder.use_var(cursor.index);
                let slot_ptr = self.list_slot_ptr(cell, index);
                let res =
                    self.builder
                        .ins()
                        .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);
                self.clone_value(res);
                self.push(res, InferredType::Any);
                self.ip += 2;
            }
            MaybeStackValue::MutRegister(reg) | MaybeStackValue::Register(reg) => {
                // Don't think we can do this. When checking null?, we also want to check
                // that the value is a list - if we assert its a list earlier, we can avoid
                // a lot of checks.

                match self.properties.get(&ValueOrRegister::Register(reg)) {
                    Some(Properties::NonNull) if self.use_lbbv => {
                        let car_moves = matches!(
                            self.shadow_stack.last(),
                            Some(MaybeStackValue::MutRegister(_))
                        );
                        self.shadow_stack_pop();

                        let (tag, value) = self.read_from_vm_stack_split(reg);

                        let typ = self.int;

                        let is_list = self.builder.ins().icmp_imm_s(
                            IntCC::Equal,
                            tag,
                            SteelVal::LIST_TAG as i64,
                        );

                        self.branch_on_condition_and_property(
                            is_list,
                            reg,
                            Properties::ProperNonEmptyList,
                            Properties::NonEmptyListOrPair,
                            |ctx| (ctx.unchecked_car_unboxed(value), InferredType::Any),
                            |ctx| {
                                let is_pair = ctx.builder.ins().icmp_imm_s(
                                    IntCC::Equal,
                                    tag,
                                    SteelVal::PAIR_TAG as i64,
                                );

                                let value = ctx.converging_if_else_cold(
                                    is_pair,
                                    // Inline car for a pair:
                                    // Unboxed variant. This site reads with
                                    // `read_from_vm_stack_split`, so `value` is
                                    // already the payload. Unboxing again isplits
                                    // an i64 into i32 halves and uses one as a
                                    // pointer, which the verifier rejects.
                                    |ctx| ctx.inline_pair_car_unboxed(value),
                                    |ctx| {
                                        let reg = ctx.register_index(reg);
                                        let res = ctx.call_helper_checked(CAR_REG, [reg]);

                                        res
                                    },
                                    typ,
                                );

                                (value, InferredType::Any)
                            },
                            |ctx, res, typ| {
                                // The list stays in its register, so
                                // the element can be left borrowed.
                                if borrow_enabled() && !car_moves {
                                    let owned = ctx.owned_flag(false);
                                    ctx.push_borrowed(res, owned);
                                } else {
                                    ctx.clone_value(res);
                                    ctx.push(res, typ);
                                }
                                ctx.ip += 2;
                            },
                        );
                    }

                    // Known to be a pair or a non-empty list (the arm
                    // where `pair?` held), so there is no error path,
                    // only the two representations.
                    Some(Properties::NonEmptyListOrPair)
                        if crate::config::config().jit_inline_pair_p =>
                    {
                        self.shadow_stack_pop();
                        let (tag, value) = self.read_from_vm_stack_split(reg);

                        let typ = self.int;

                        let is_list = self.builder.ins().icmp_imm_s(
                            IntCC::Equal,
                            tag,
                            SteelVal::LIST_TAG as i64,
                        );

                        let res = self.converging_if(
                            is_list,
                            |ctx| ctx.unchecked_car_unboxed(value),
                            |ctx| ctx.inline_pair_car_unboxed(value),
                            typ,
                        );

                        self.clone_value(res);

                        self.push(res, InferredType::Any);
                        self.ip += 2;
                    }

                    Some(Properties::NonNull) => {
                        self.shadow_stack_pop();
                        let (tag, value) = self.read_from_vm_stack_split(reg);

                        let typ = self.int;

                        let is_list = self.builder.ins().icmp_imm_s(
                            IntCC::Equal,
                            tag,
                            SteelVal::LIST_TAG as i64,
                        );

                        let res = self.converging_if(
                            is_list,
                            |ctx| ctx.unchecked_car_unboxed(value),
                            |ctx| {
                                let is_pair = ctx.is_type(value, SteelVal::PAIR_TAG);

                                ctx.converging_if(
                                    is_pair,
                                    // Inline car for a pair:
                                    |ctx| ctx.inline_pair_car_unboxed(value),
                                    |ctx| {
                                        let reg = ctx.register_index(reg);
                                        ctx.call_helper_checked(CAR_REG, [reg])
                                    },
                                    typ,
                                )
                            },
                            typ,
                        );

                        self.properties.add_property(
                            ValueOrRegister::Register(reg),
                            Properties::NonEmptyListOrPair,
                        );

                        // If this is a
                        self.clone_value(res);

                        self.push(res, InferredType::Any);
                        self.ip += 2;
                    }

                    Some(Properties::ProperNonEmptyList) => {
                        self.shadow_stack_pop();

                        let value = self.read_from_vm_stack_unboxed(reg);
                        let res = self.unchecked_car_unboxed(value);

                        self.clone_value(res);

                        self.push(res, InferredType::Any);
                        self.ip += 2;
                    }

                    _ => {
                        self.shadow_stack_pop();
                        let value = self.read_from_vm_stack(reg);

                        let typ = self.int;

                        let is_list = self.is_type(value, SteelVal::LIST_TAG);

                        let res = self.converging_if(
                            is_list,
                            |ctx| ctx.checked_car(value, reg),
                            |ctx| {
                                let is_pair = ctx.is_type(value, SteelVal::PAIR_TAG);

                                ctx.converging_if(
                                    is_pair,
                                    // Inline car for a pair:
                                    |ctx| ctx.inline_pair_car(value),
                                    |ctx| {
                                        let reg = ctx.register_index(reg);
                                        let res = ctx.call_helper_checked(CAR_REG, [reg]);

                                        res
                                    },
                                    typ,
                                )
                            },
                            typ,
                        );

                        self.properties.add_property(
                            ValueOrRegister::Register(reg),
                            Properties::NonEmptyListOrPair,
                        );

                        // If this is a
                        self.clone_value(res);

                        self.push(res, InferredType::Any);
                        self.ip += 2;
                    }
                }
            }

            _ => {
                self.func_ret_val(op, 1, 2, InferredType::Any);
                self.check_deopt();
            }
        }
    }

    /// `SETBOX`.
    pub(super) fn lower_set_box(&mut self, op: OpCode) {
        if let Some(last) = self.shadow_stack.get(self.shadow_stack.len() - 2).copied() {
            self.shadow_mark_local_type_from_var(last, InferredType::Box);
        }

        // `inline_box_primitive` already lowers `#%set-box!` with
        // the unshared fast path, where a strong count of 1 stores
        // without the spin lock or `Weak::upgrade`. `(set! x ...)` on
        // a captured variable compiles to this opcode.
        //
        // Reuse that lowering for the operand handling. The new value
        // is stored into the box, so it has to be materialised as
        // owned. Letting a borrowed register through would give the
        // box a second owner.
        let inlined = if inline_setbox_enabled() {
            self.inline_box_primitive(
                crate::steel_vm::primitives::steel_set_box_mutable as *const () as usize,
                2,
            )
        } else {
            None
        };

        match inlined {
            Some(res) => {
                self.ip += 2;
                self.push(res, InferredType::Any);
            }
            None => {
                self.func_ret_val(op, 2, 2, InferredType::Any);
            }
        }
    }

    /// `UNBOX`.
    pub(super) fn lower_unbox(&mut self, op: OpCode) {
        let last = self.shadow_stack.last().copied().unwrap();

        self.shadow_mark_local_type_from_var(last, InferredType::Box);

        match last {
            MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i) => {
                self.shadow_stack_pop();
                let value = self.read_from_vm_stack(i);
                let res = self.unbox_value_checked_register(value, false);

                self.ip += 2;
                self.push(res, InferredType::Any);
            }

            MaybeStackValue::Value(StackValue { value, .. }) => {
                self.shadow_stack_pop();
                let res = self.unbox_value_checked_register(value, true);
                self.ip += 2;
                self.push(res, InferredType::Any);
            }

            _ => {
                self.func_ret_val(op, 1, 2, InferredType::Any);
            }
        }
    }

    /// `VECTORREF`.
    pub(super) fn lower_vector_ref(&mut self, op: OpCode) {
        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        // Generate code to handle registers for both reference and setting
        match args {
            &[MaybeStackValue::MutRegister(v) | MaybeStackValue::Register(v), MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i)] =>
            {
                let vector = self.register_index(v);
                let vector_value = self.read_from_vm_stack(v);
                let index_value = self.read_from_vm_stack(i);

                let res = match self.properties.get(&ValueOrRegister::Register(i)) {
                    Some(Properties::PositiveInteger) => {
                        self.shadow_stack_pop();
                        self.shadow_stack_pop();

                        let index = self.read_from_vm_stack_unboxed(i);

                        let fallback = move |ctx: &mut Self| {
                            // An out of range index raises, so leave native code
                            // before anything uses the result.
                            ctx.call_helper_checked(VECTOR_REF_REG_2_UNBOXED_INDEX, [vector, index])
                        };

                        self.inline_vector_ref(vector_value, index_value, fallback)
                    }

                    _ => {
                        let index = self.register_index(i);

                        // If we're indexing with a value
                        self.properties.add_property(
                            ValueOrRegister::Register(i),
                            Properties::PositiveInteger,
                        );

                        // Pop them off
                        self.shadow_stack_pop();
                        self.shadow_stack_pop();

                        let fallback = move |ctx: &mut Self| {
                            ctx.call_helper_checked(VECTOR_REF_REG_2, [vector, index])
                        };

                        self.inline_vector_ref(vector_value, index_value, fallback)
                    }
                };

                self.push(res, InferredType::Any);
                self.ip += 2;
            }
            &[MaybeStackValue::MutRegister(v) | MaybeStackValue::Register(v), MaybeStackValue::Value(_) | MaybeStackValue::Constant(_)] =>
            {
                let index = self.shadow_pop();
                let vector_value = self.read_from_vm_stack(v);
                let vector = self.register_index_small(v);
                self.shadow_stack_pop();

                let index_value = index.0;
                let fallback = move |ctx: &mut Self| {
                    ctx.call_helper_checked(VECTOR_REF_REG_1, [vector, index_value])
                };

                let res = { self.inline_vector_ref(vector_value, index_value, fallback) };

                self.push(res, InferredType::Any);
                self.ip += 2;
            }

            // Neither operand is a register, both are values or
            // constants. The inline sequence only needs the two
            // values, so the bounds-checked fast path still applies.
            _ if generic_inline_enabled() => {
                // `shadow_pop` asserts the operand is not spilled, but
                // in this general arm either operand may be.
                // `shadow_stack_pop` accounts for the stack slot, and
                // the SSA value stays valid to read. Go through
                // `into_value` the way the other inline arms do.
                let index = self.pop_boxed();
                let vector_value = self.pop_boxed();

                let fallback = move |ctx: &mut Self| {
                    let res = ctx.call_helper_checked(VECTOR_REF_VALUE, [vector_value, index]);
                    res
                };

                let res = self.inline_vector_ref(vector_value, index, fallback);

                self.push(res, InferredType::Any);
                self.ip += 2;
            }

            _ => {
                self.func_ret_val(op, 2, 2, InferredType::Any);
            }
        }
    }
}
