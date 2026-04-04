use super::*;

impl<'a> FunctionTranslator<'a> {
    fn shadow_last_cloned(&self) -> MaybeStackValue {
        self.shadow_stack.last().unwrap().clone()
    }

    pub(super) fn converging_if_no_else_no_value(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        merge: impl Fn(&mut Self),
    ) {
        let then_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], merge_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        then(self);

        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        merge(self);
    }

    pub(super) fn converging_if_no_value(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self),
        else_thunk: impl Fn(&mut Self),
    ) {
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        then(self);

        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        else_thunk(self);
        self.builder.ins().jump(merge_block, &[]);
        self.builder.switch_to_block(merge_block);
    }

    pub(super) fn converging_if(
        &mut self,
        test_condition: Value,
        then: impl Fn(&mut Self) -> Value,
        else_thunk: impl Fn(&mut Self) -> Value,
        typ: Type,
    ) -> Value {
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, typ);

        self.builder
            .ins()
            .brif(test_condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let res = then(self);

        self.builder.ins().jump(merge_block, &[res]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        let then_res = else_thunk(self);
        self.builder.ins().jump(merge_block, &[then_res]);
        self.builder.switch_to_block(merge_block);

        let result = self.builder.block_params(merge_block)[0];

        result
    }

    fn get_tag_and_payload(&mut self, value: Value) -> (Value, Value) {
        let (lo, hi) = self.builder.ins().isplit(value);
        let tag = self.builder.ins().ireduce(types::I8, lo);
        (tag, hi)
    }

    // TODO: Implement binop add directly. Assume naively that the values are probably integers,
    // and then implement the checked addition. We can branch for addition for ints / floats, anything else
    // should fall through to the generic case.
    pub(super) fn binop_add_value(
        &mut self,
        left: StackValue,
        right: StackValue,
    ) -> (Value, InferredType) {
        match (left, right) {
            // If they're both int 64, then we can continue comfortably. No error handling
            // necessary
            // (
            //     StackValue {
            //         inferred_type: InferredType::Int64,
            //         value: lv,
            //         ..
            //     },
            //     StackValue {
            //         inferred_type: InferredType::Int64,
            //         value: rv,
            //         ..
            //     },
            // ) => {
            //     todo!()
            // }
            (StackValue { value: lv, .. }, StackValue { value: rv, .. }) => {
                let left_is_int = self.is_type(lv, SteelVal::INT_TAG);
                let right_is_int = self.is_type(rv, SteelVal::INT_TAG);
                let both_int = self.builder.ins().band(left_is_int, right_is_int);

                let sp = |ctx: &mut Self| {
                    let function_name = op_to_name_payload(OpCode::ADD, 2);
                    let args = [lv, rv];
                    let result = ctx.call_function_returns_value_args(function_name, &args);
                    result
                };

                let typ = self.int;

                let res = self.converging_if(
                    both_int,
                    |ctx| {
                        // This is pointer sized, we're good to shrink it down
                        // to a pointer. both have to be int tag, otherwise we fall back to
                        // a function, and we'll return the usual
                        let left_payload = ctx.unbox_value_to_pointer(lv);
                        let right_payload = ctx.unbox_value_to_pointer(rv);

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

                (res, InferredType::Number)
            }
        }
    }

    // pub(super) fn reverse(&mut self) {
    //     use MaybeStackValue::*;
    // }

    pub(super) fn is_symbol(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_symbol = self.is_type(value, SteelVal::SYMBOL_TAG);
                self.drop_tagged_value(value);
                self.push(is_symbol, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.local_to_value_map.get(&p) {
                    // Elide the call entirely if its a non empty list
                    Some(InferredType::Symbol) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_function_returns_value_args("symbol?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            // TODO: We should probably elide this somehow?
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("symbol?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_void(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_last_cloned();

        match last {
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_void = self.is_type(value, SteelVal::VOID_TAG);
                self.drop_tagged_value(value);
                self.push(is_void, InferredType::UnboxedBool);
                self.ip += 1;
            }

            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                let maybe_typ = self.local_to_value_map.get(&p);

                match (
                    self.properties.get(&ValueOrRegister::Register(p)),
                    maybe_typ,
                ) {
                    // Elide the call entirely if its a non empty list, or if
                    // we know that the value is something else at this point based
                    // on the inferred type.
                    (Some(Properties::NonEmptyListOrPair), _) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 0);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    (_, Some(t)) if *t != InferredType::Void => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 0);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    (_, Some(t)) if *t == InferredType::Void => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_function_returns_value_args("void?", &[register]);
                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("void?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }
}
