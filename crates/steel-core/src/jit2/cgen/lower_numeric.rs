//! Lowering of arithmetic, comparisons and equality.
//!
//! The bodies of `stack_to_ssa`'s larger match arms. The arms themselves, with
//! their guards, stay in `stack_to_ssa`, in order.

use super::*;

impl FunctionTranslator<'_> {
    /// `-` of a local's register and a value.
    pub(super) fn sub_register_value(&mut self) {
        let value = self.shadow_stack_pop().unwrap().into_value(self);
        let register = self.shadow_stack_pop().unwrap().into_index();

        let register = self.register_index(register);

        let args = [register, value.as_steelval(self)];
        // Checked below, once the result is on the stack for the deopt snapshot.
        let result = self.call_helper_unchecked(SUB_BINOP_REG, args);

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.check_deopt();

        self.ip += 2;
    }

    /// `-` with an int on top.
    pub(super) fn sub_int_operand(&mut self, payload: usize) {
        // Call the func

        let args = self.split_off(payload);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        // Checked below, once the result is on the stack for the deopt snapshot.
        let result = self.call_helper_unchecked(SUB_BINOP_INT, args.try_into().unwrap());

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.check_deopt();

        self.ip += 2;
    }

    /// `+` of a local's register and an int constant.
    pub(super) fn add_register_int(&mut self) {
        let rhs_int = self
            .shadow_stack
            .pop()
            .unwrap()
            .into_constant_int(self)
            .unwrap();
        let register_index = self.shadow_stack_pop().unwrap().into_index();

        let local_value = self.read_from_vm_stack(register_index);
        let is_int = self.is_type(local_value, SteelVal::INT_TAG);

        let sp = |ctx: &mut Self| {
            let register = ctx.i64_const(register_index as i64);

            let value = ctx.encode_integer(rhs_int as i64);

            let args = [register, value];
            let result = ctx.call_helper_checked(ADD_BINOP_INT_REG, args);

            // The helper reports a type error by flagging the vm. Without
            // this check the jitted code would keep going and the error
            // would surface after `with-handler` had been unwound.

            result
        };

        let result = self.converging_if(
            is_int,
            |ctx| {
                // If its an int, then we'll do checked subtraction:
                let lhs = ctx.unbox_value_to_pointer(local_value);
                let rhs = ctx.i64_const(rhs_int as i64);

                let (subbed, overflow_flag) = ctx.builder.ins().sadd_overflow(lhs, rhs);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| ctx.encode_value(SteelVal::INT_TAG as _, subbed),
                    types::I128,
                )
            },
            sp,
            types::I128,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.ip += 2;
    }

    /// `+` of a local's register and a value.
    pub(super) fn add_register_value(&mut self) {
        let value = self.shadow_stack_pop().unwrap().into_value(self);

        let value_as_steelval = value.as_steelval(self);

        let register = self.shadow_stack_pop().unwrap().into_index();

        // Lets check the left hand size, handle overflow as necessary:

        let register_value = self.read_from_vm_stack(register);
        let left_is_int = self.is_type(register_value, SteelVal::INT_TAG);

        // If the right hand side is already classified as an int, then
        // we can skip checking if its an integer.
        let both_int = if value.inferred_type == InferredType::Int {
            left_is_int
        } else {
            let right_is_int = self.is_type(value_as_steelval, SteelVal::INT_TAG);
            let both_int = self.builder.ins().band(left_is_int, right_is_int);
            both_int
        };

        let typ = self.int;

        let sp = |ctx: &mut Self| {
            let register = ctx.register_index(register);
            let args = [register, value_as_steelval];
            let result = ctx.call_helper_checked(ADD_BINOP_REG, args);

            // The helper reports a type error by flagging the vm. Without
            // this check the jitted code would keep going and the error
            // would surface after `with-handler` had been unwound.

            result
        };

        let res = self.converging_if(
            both_int,
            |ctx| {
                // This is pointer sized, we're good to shrink it down
                // to a pointer. both have to be int tag, otherwise we fall back to
                // a function, and we'll return the usual
                let left_payload = ctx.unbox_value_to_pointer(register_value);
                let right_payload = ctx.unbox_value_to_pointer(value_as_steelval);

                // Add the values, did they overflow?
                let (added, overflow_flag) =
                    ctx.builder.ins().sadd_overflow(left_payload, right_payload);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| {
                        // Happy path, just return the boxed integer value.
                        ctx.encode_value(SteelVal::INT_TAG as _, added)
                    },
                    typ,
                )
            },
            |ctx| {
                let res = sp(ctx);
                ctx.check_deopt();
                res
            },
            typ,
        );

        self.push(res, InferredType::Number);

        self.ip += 2;
    }

    /// `+` of two values.
    pub(super) fn add_two_values(&mut self) {
        let MaybeStackValue::Value(r) = self.shadow_stack_pop().unwrap() else {
            panic!()
        };
        let MaybeStackValue::Value(l) = self.shadow_stack_pop().unwrap() else {
            panic!()
        };

        // TODO: Might be worth attempting to figure out what the inferred type
        // for function calls are, to propagate downward in the calls
        let (res, t) = self.binop_add_value(l, r);

        self.push(res, t);

        self.ip += 2;
    }

    /// `>=` of a local's register and an operand of unknown type.
    pub(super) fn gte_register_any(&mut self, payload: usize) {
        self.mark_operands(payload, InferredType::Number);

        let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);
        let register_l = self.shadow_stack_pop().unwrap().into_index();

        // Happy path, lets check the type of the lhs. If its an integer tag,
        // then we can go ahead and do the thing.
        // Otherwise, we'll bail and fall through:

        let local_value = self.read_from_vm_stack(register_l);

        // Check both operands. Checking only the register would compare
        // an int or float register against the raw payload of the other
        // operand, e.g. `(>= 1.5 2)` would read 2 as a denormal float.
        let both_int = self.tags_are(&[
            (local_value, SteelVal::INT_TAG),
            (rhs_int.value, SteelVal::INT_TAG),
        ]);

        let result = self.converging_if(
            both_int,
            |ctx| {
                // Just do less than or equal on the value:
                let lhs = ctx.unbox_value_to_pointer(local_value);
                let rhs = ctx.unbox_value_to_pointer(rhs_int.value);

                let res = ctx
                    .builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs);

                res
            },
            |ctx| {
                let both_float = ctx.tags_are(&[
                    (local_value, SteelVal::FLOAT_TAG),
                    (rhs_int.value, SteelVal::FLOAT_TAG),
                ]);

                ctx.converging_if(
                    both_float,
                    |ctx| {
                        // Just do less than or equal on the value:
                        let lhs = ctx.unbox_value_to_float(local_value);
                        let rhs = ctx.unbox_value_to_float(rhs_int.value);

                        let res = ctx
                            .builder
                            .ins()
                            .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs);

                        res
                    },
                    |ctx| {
                        let register_l = ctx.i64_const(register_l as i64);
                        let args = [register_l, rhs_int.value];
                        let result = ctx.call_helper_checked(GTE_REGISTER_UNKNOWN, args);

                        result
                    },
                    types::I8,
                )
            },
            types::I8,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::UnboxedBool);

        self.ip += 2;
    }

    /// `<=` of a local's register and a value.
    pub(super) fn lte_register_value(&mut self, payload: usize) {
        self.mark_operands(payload, InferredType::Number);

        let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);
        let register_l = self.shadow_stack_pop().unwrap().into_index();

        let register_l = self.i64_const(register_l as i64);

        let args = [register_l, rhs_int.value];
        // Checked below, once the result is on the stack for the deopt snapshot.
        let result = self.call_helper_unchecked(LTE_REGISTER, args);

        // Check the inferred type, if we know of it
        self.push(result, InferredType::UnboxedBool);

        self.check_deopt();

        self.ip += 2;
    }

    /// `=` of two values.
    pub(super) fn num_equal_two_values(&mut self) {
        // Lets assume, for now, that we'll happy path the case where they're both
        // ints. We would need to implement something more sophisticated for checking
        // otherwise.

        // This will be our constant value:
        let right = self.pop_boxed();
        let left = self.pop_boxed();

        // Check if the tags are the same, and they're numeric:
        let both_int = self.tags_are(&[(left, SteelVal::INT_TAG), (right, SteelVal::INT_TAG)]);

        // Just check equality of the unboxed values

        let res = self.converging_if(
            both_int,
            |ctx| {
                // We can't rely on the tag matching in this world unfortunately.
                // Tags coming from rust land in debug mode could have garbage in the
                // padding that otherwise isn't there in the release build.
                let left_payload = ctx.unbox_value_to_pointer(left);
                let right_payload = ctx.unbox_value_to_pointer(right);
                ctx.builder
                    .ins()
                    .icmp(IntCC::Equal, left_payload, right_payload)
            },
            |ctx| {
                // Handle the else case here as well

                let res = ctx.call_helper_checked(NUM_EQUAL_VALUE_BOOL, [left, right]);

                // Make sure to check the deopt case here

                res
            },
            types::I8,
        );

        self.push(res, InferredType::UnboxedBool);
        self.ip += 2;
    }

    /// `=` of a local's register and an int value.
    pub(super) fn num_equal_register_int(&mut self) {
        // Lets assume, for now, that we'll happy path the case where they're both
        // ints. We would need to implement something more sophisticated for checking
        // otherwise.

        // This will be our constant value:
        let last = self.shadow_stack_pop().unwrap().into_value(self);

        // This is now our register; this is where the values will live.
        let register = self.shadow_stack_pop().unwrap().into_index();
        let register_value = self.read_from_vm_stack(register);

        // Check if the tags are the same, and they're numeric:
        let is_register_int = self.is_type(register_value, SteelVal::INT_TAG);

        // Just check equality of the unboxed values

        let res = self.converging_if(
            is_register_int,
            |ctx| {
                // We can't rely on the tag matching in this world unfortunately.
                // Tags coming from rust land in debug mode could have garbage in the
                // padding that otherwise isn't there in the release build.
                let register_payload = ctx.unbox_value_to_pointer(register_value);
                let last_steelval = last.as_steelval(ctx);
                let rhs = ctx.unbox_value_to_pointer(last_steelval);
                ctx.builder.ins().icmp(IntCC::Equal, register_payload, rhs)
            },
            |ctx| {
                // Handle the else case here as well
                // todo!();

                let register_int = ctx.register_index(register);

                let last_steelval = last.as_steelval(ctx);
                let res =
                    ctx.call_helper_checked(NUM_EQUAL_INT_REGISTER, [register_int, last_steelval]);

                // Make sure to check the deopt case here

                res
            },
            types::I8,
        );

        self.push(res, InferredType::UnboxedBool);
        self.ip += 2;
    }

    /// `=` of a value and an int constant.
    pub(super) fn num_equal_value_int_constant(&mut self) {
        // Lets assume, for now, that we'll happy path the case where they're both
        // ints. We would need to implement something more sophisticated for checking
        // otherwise.

        let int = self
            .shadow_stack
            .pop()
            .unwrap()
            .into_constant_int(self)
            .unwrap();

        // This is now our register; this is where the values will live.
        let value = self.pop_boxed();

        // Check if the tags are the same, and they're numeric:
        let is_value_int = self.is_type(value, SteelVal::INT_TAG);

        // Just check equality of the unboxed values

        let res = self.converging_if(
            is_value_int,
            |ctx| {
                // We can't rely on the tag matching in this world unfortunately.
                // Tags coming from rust land in debug mode could have garbage in the
                // padding that otherwise isn't there in the release build.
                let register_payload = ctx.unbox_value_to_pointer(value);
                ctx.builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, register_payload, int as i64)
            },
            |ctx| {
                // Handle the else case here as well
                // todo!();

                let last = ctx.encode_integer(int as _);

                let res = ctx.call_helper_checked(NUM_EQUAL_VALUE_BOOL, [value, last]);

                // Make sure to check the deopt case here

                res
            },
            types::I8,
        );

        self.push(res, InferredType::UnboxedBool);
        self.ip += 2;
    }

    /// `<`, `<=`, `>` or `>=` of two values.
    pub(super) fn compare_two_values(&mut self, op: OpCode, payload: usize) {
        self.mark_operands(payload, InferredType::Number);

        let (cc, fallback) = match op {
            OpCode::LT => (IntCC::SignedLessThan, LT_TWO_VALUE_BOOL),
            OpCode::LTE => (IntCC::SignedLessThanOrEqual, LTE_TWO_VALUE_BOOL),
            OpCode::GT => (IntCC::SignedGreaterThan, GT_TWO_VALUE_BOOL),
            OpCode::GTE => (IntCC::SignedGreaterThanOrEqual, GTE_TWO_VALUE_BOOL),
            _ => unreachable!(),
        };

        let res = self.inline_int_compare_two(cc, fallback);
        self.push(res, InferredType::UnboxedBool);
        self.ip += 2;
    }

    /// `EQUAL2`: `equal?` of two operands.
    pub(super) fn lower_equal2(&mut self, op: OpCode, payload: usize) {
        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Value(v)]
            | &[MaybeStackValue::Value(v), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
            {
                self.shadow_stack_pop();
                self.shadow_stack_pop();

                let register_index = self.register_index(i);

                let v_steelval = v.as_steelval(self);
                let res = self.call_helper(EQUAL_BINOP_REGISTER_BOOL, [register_index, v_steelval]);

                self.push(res, InferredType::UnboxedBool);

                self.ip += 2;
            }

            &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Constant(ConstantValue::Symbol(v))]
            | &[MaybeStackValue::Constant(ConstantValue::Symbol(v)), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
            {
                self.shadow_stack_pop();
                self.shadow_stack_pop();

                let constant = self.constants.get(v);
                let SteelVal::SymbolV(s) = constant else {
                    panic!()
                };

                let as_ptr: i64 = unsafe { std::mem::transmute::<SteelString, _>(jit_leak(&s)) };
                let value = self.i64_const(as_ptr);

                // If they're the same type, just compare the bytes. Don't do a lookup.
                let left_value = self.read_from_vm_stack(i);
                let is_symbol = self.is_type(left_value, SteelVal::SYMBOL_TAG);
                // Get the left value
                let lvalue = self.unbox_value_to_pointer(left_value);

                let test = self
                    .builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, lvalue, as_ptr as i64);

                let fast_path = self.builder.ins().band(is_symbol, test);

                let res = self.converging_if(
                    fast_path,
                    |ctx| ctx.builder.ins().iconst(types::I8, 1),
                    |ctx| {
                        ctx.converging_if(
                            is_symbol,
                            |ctx| ctx.call_helper(SYMBOL_EQUAL_P_NO_DROP, [lvalue, value]),
                            |ctx| ctx.builder.ins().iconst(types::I8, 0),
                            types::I8,
                        )
                    },
                    types::I8,
                );

                self.push(res, InferredType::UnboxedBool);

                self.ip += 2;
            }

            // Register + Constant,
            // we might be able to say they're not equal
            // right off the bat based on inferred types, but
            // for now we're going to continue moving on. If the constant
            // is a symbol, we can inline an eq call before falling back to
            // equality with symbols.
            &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Constant(v)]
            | &[MaybeStackValue::Constant(v), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
            {
                self.shadow_stack_pop();
                self.shadow_stack_pop();

                let register_index = self.register_index(i);

                let value = v.to_value(self);

                let res = self.call_helper(EQUAL_BINOP_REGISTER_BOOL, [register_index, value.0]);

                // TODO: Somewhere, I'm not calling unboxed bool to_value properly!
                self.push(res, InferredType::UnboxedBool);

                self.ip += 2;
            }

            _ => {
                self.func_ret_val(op, payload, 2, InferredType::UnboxedBool);
            }
        };
    }

    /// `NOT`.
    pub(super) fn lower_not(&mut self) {
        let last_ref = self
            .shadow_stack
            .last()
            .copied()
            .and_then(|x| x.as_value(self));

        if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
            let test = last_ref.unwrap().value;
            self.shadow_stack_pop();

            let value = self.builder.ins().bxor_imm_u(test, 1);

            // Negating a `null?` result swaps which arm learns what.
            let flipped = match self.properties.get(&ValueOrRegister::Value(test)) {
                Some(Properties::CheckedNull(r)) => Some(Properties::CheckedNotNull(r)),
                Some(Properties::CheckedNotNull(r)) => Some(Properties::CheckedNull(r)),
                _ => None,
            };
            if let Some(prop) = flipped {
                self.properties
                    .add_property(ValueOrRegister::Value(value), prop);
            }

            self.push(value, InferredType::UnboxedBool);
            self.ip += 2;
        } else if last_ref.map(|x| x.inferred_type) == Some(InferredType::Bool) {
            let (test, _) = self.shadow_pop();
            // If this matches SteelVal::BoolV(false)
            // exactly, then we're done.
            let payload = self.unbox_value(test);
            let test_condition = self.builder.ins().ireduce(types::I8, payload);
            let comparison = self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, test_condition, 0);

            self.push(comparison, InferredType::UnboxedBool);
            self.ip += 2;
        } else {
            let (test, _) = self.shadow_pop();
            // If this matches SteelVal::BoolV(false)
            // exactly, then we're done.

            let is_bool = self.is_type(test, SteelVal::BOOL_TAG);
            let payload = self.unbox_value(test);

            let test_condition = self.builder.ins().ireduce(types::I8, payload);
            let comparison = self
                .builder
                .ins()
                .icmp_imm_s(IntCC::Equal, test_condition, 0);

            let comparison = self.builder.ins().band(comparison, is_bool);

            // Make sure to drop this before we're done in the event its not
            // a boolean.
            self.drop_tagged_value(test);

            self.push(comparison, InferredType::UnboxedBool);
            self.ip += 2;
        }
    }
}
