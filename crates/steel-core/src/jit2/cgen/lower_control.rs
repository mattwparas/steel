use super::*;

impl FunctionTranslator<'_> {
    /// `POPPURE`: return from the function.
    ///
    /// Ends this function's translation.
    pub(super) fn lower_pop_pure(&mut self) {
        self.maybe_check_last();
        let (value, ty) = self.shadow_pop();

        // Whatever the exit type is, record it, if
        // we have it.
        self.exit_types.insert(ty);

        // Decided before the spill. An empty shadow stack means
        // only the argument and let slots are left above the frame's
        // sp.
        let slots_immediate = self.frame_slots_all_immediate();
        self.spill_cloned_stack();
        let real_res = self.inline_handle_pop_with(value, slots_immediate);

        // if we hit this, the value should not end up getting read?
        let ret = self.builder.ins().return_(&[real_res]);
        self.typed_returns.insert(ret);

        let cold_block = self.builder.create_block();
        self.builder.switch_to_block(cold_block);
        self.properties.dead = true;

        self.finish_tail_position();
    }

    /// `IF`: branch on the value on top of the stack.
    pub(super) fn lower_if(&mut self) {
        // If we can check the local variable on the stack, we should do that
        if matches!(
            self.shadow_stack.last(),
            Some(MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_))
        ) {
            let test = self.shadow_stack_pop().unwrap();

            let false_instr = self.instructions[self.ip].payload_size;
            let true_instr = self.ip + 1;

            let test_bool = self.call_test_handler_register(test.into_index());

            self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());
        } else {
            let last_ref = self
                .shadow_stack
                .last()
                .copied()
                .and_then(|x| x.as_value(self));

            if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
                let false_instr = self.instructions[self.ip].payload_size;
                let true_instr = self.ip + 1;

                // Explicitly want the unboxed value here
                let test_bool = last_ref.unwrap().value;

                self.shadow_stack_pop();

                self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());
            } else {
                // TODO: Type inference here! Change which function is called!
                let (test, typ) = self.shadow_pop();

                let false_instr = self.instructions[self.ip].payload_size;
                let true_instr = self.ip + 1;

                let test_bool = match typ {
                    InferredType::Bool => {
                        let payload = self.unbox_value(test);
                        self.builder.ins().ireduce(types::I8, payload)
                    }
                    InferredType::List => {
                        self.drop_biased_rc(test);

                        self.builder.ins().iconst(types::I8, 1)
                    }
                    _ => {
                        let is_bool = self.is_type(test, SteelVal::BOOL_TAG);
                        let payload = self.unbox_value(test);
                        let small = self.builder.ins().ireduce(types::I8, payload);

                        let is_false = self.builder.ins().icmp_imm_s(IntCC::Equal, small, 0);

                        // Is bool and is false:
                        let overall = self.builder.ins().band(is_bool, is_false);
                        let is_truthy = self.builder.ins().bxor_imm_u(overall, 1);

                        self.drop_tagged_value(test);

                        is_truthy
                    }
                };

                self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());
            }
        }
    }

    /// `NEWSCLOSURE`: make a closure.
    pub(super) fn lower_new_closure(&mut self, payload: usize) {
        let ip = self.ip;
        let offset = payload;
        self.ip += payload + 1;

        assert_eq!(self.instructions[self.ip - 1].op_code, OpCode::ECLOSURE);
        assert!(self.ip < self.instructions.len());

        let ip_value = self.i64_const(ip as i64);
        let offset_value = self.i64_const(offset as i64);

        // code gen the sclosure creation:
        let v = self.call_helper(NEW_CLOSURE, [ip_value, offset_value]);

        self.push(v, InferredType::BytecodeFunction);
    }

    /// `PUREFUNC`: push a closure that captures nothing.
    pub(super) fn lower_pure_func(&mut self, payload: usize) {
        let ip = self.ip;
        let offset = payload;

        self.ip += 1;
        self.ip += 1;
        self.ip += 1;

        let forward_jump = offset - 2;
        let forward_index = self.ip + forward_jump;

        self.ip = forward_index;

        let ip_value = self.i64_const(ip as i64);
        let offset_value = self.i64_const(offset as i64);

        // code gen the sclosure creation:
        let v = self.call_helper(PURE_FUNC, [ip_value, offset_value]);

        self.push(v, InferredType::BytecodeFunction);
    }
}
