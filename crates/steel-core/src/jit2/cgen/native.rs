use crate::values::lock::SpinLock;

use super::*;

impl<'a> FunctionTranslator<'a> {
    fn shadow_last_cloned(&self) -> MaybeStackValue {
        self.shadow_stack.last().unwrap().clone()
    }

    // Check the tags, see if they're equal, etc.
    pub(super) fn value_equals_binop(&mut self) {
        todo!()
    }

    pub(super) fn char_equals(&mut self, arity: usize) {
        let name = CallPrimitiveFixedDefinitions::arity_to_name(arity).unwrap();

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - arity..)
            .unwrap()
            .to_vec();

        // dbg!(args);

        // attempt to move forward with it
        let additional_args = self.split_off(arity);

        // dbg!(&additional_args);

        // let f = crate::primitives::ports::read_char_single
        //     as fn(SteelVal) -> Result<SteelVal, crate::SteelErr>;
        //

        let all_chars = additional_args.iter().all(|x| x.1 == InferredType::Char);

        if all_chars && false {
            // println!("Found all characters, applying equality");
            // Just... compare for equality?

            let left = additional_args[0].0;
            let right = additional_args[1].0;

            let left = self.unbox_value(left);
            let right = self.unbox_value(right);

            let left = self.builder.ins().ireduce(types::I8, left);
            let right = self.builder.ins().ireduce(types::I8, right);

            let comparison = self.builder.ins().icmp(IntCC::Equal, left, right);
            let res = self.builder.ins().uextend(types::I64, comparison);
            let boolean = self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
            self.push(boolean, InferredType::Bool);
            self.ip += 1;

            // No need to check deopt here, we're good.
        } else {
            let function = self.builder.ins().iconst(
                self.module.target_config().pointer_type(),
                crate::primitives::strings::char_equals_binop as *const () as i64,
            );

            let fallback_ip = self
                .builder
                .ins()
                .iconst(Type::int(64).unwrap(), self.ip as i64);

            let mut args = vec![function, fallback_ip];

            args.extend(additional_args.into_iter().map(|x| x.0));

            let result = self.call_function_returns_value_args(name, &args);
            self.push(result, InferredType::Bool);
            self.ip += 1;
            self.check_deopt();
        }
    }

    pub(super) fn vector_set(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 3..)
            .unwrap();

        match args {
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), MutRegister(a) | Register(a)] =>
            {
                let vector = self.register_index(v);
                let index = self.register_index(i);
                let value = self.register_index(a);

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self
                    .call_function_returns_value_args("vector-set-reg-3", &[vector, index, value]);

                self.push(res, InferredType::Any);

                self.ip += 1;
                self.check_deopt();
            }

            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), Value(_)] => {
                let vector = self.register_index(v);
                let index = self.register_index(i);
                let value = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args(
                    "vector-set-reg-2",
                    &[vector, index, value.0],
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
            &[MutRegister(v) | Register(v), Value(_), Value(_)] => {
                let value = self.shadow_pop();
                let index = self.shadow_pop();
                let vector = self.register_index(v);
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args(
                    "vector-set-reg-1",
                    &[vector, index.0, value.0],
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(3)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args("vector-set-args", &args);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
        }
    }

    pub(super) fn eq(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            // Okay, so for constants, we can wait to actually reify them
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i)] => {
                let left = self.register_index(v);
                let right = self.register_index(i);

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args("eq?-reg-2", &[left, right]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(v) | Register(v), Value(_)] => {
                let left = self.register_index(v);
                let right = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args("eq?-reg-1", &[left, right.0]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args_no_context("eq?-args", &args);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_pair(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching - if this is used in the test position
            // of an if statement, we should encode the type checking
            // through.
            Value(stack_value) => {
                // TODO: Still need to invoke drop on this thing though!
                self.shadow_stack.pop();
                // If we've already inferrred this type as a pair,
                // we can skip the code generation for checking the tags
                // and actually invoking the function since we know
                // it will be a pair.
                // match stack_value.inferred_type {
                //     InferredType::List | InferredType::Pair | InferredType::ListOrPair if false => {
                //         let res = self.builder.ins().iconst(types::I64, 1);

                //         let boolean =
                //             self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);

                //         // TODO: Also - we'll need to check the length of the list!
                //         // this should be able to be done inline as well, we just have to load
                //         // the index of the list.
                //         self.push(boolean, InferredType::Bool);
                //         self.ip += 1;

                //         self.drop_tagged_value(stack_value.value);

                //         return;
                //     }

                //     _ => {}
                // }

                let value = stack_value.as_steelval(self);

                // Encode this manually:
                let tag = self.get_tag(value);

                let mut switch = Switch::new();
                let pair_block = self.builder.create_block();
                let list_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I8);

                switch.set_entry(SteelVal::LIST_TAG as _, list_block);
                switch.set_entry(SteelVal::PAIR_TAG as _, pair_block);

                switch.emit(&mut self.builder, tag, else_block);
                {
                    // Is a pair
                    self.builder.switch_to_block(pair_block);
                    self.builder.seal_block(pair_block);
                    let true_val = self.builder.ins().iconst(types::I8, 1);
                    self.builder.ins().jump(merge_block, &[true_val]);
                }

                {
                    // Is a list
                    self.builder.switch_to_block(list_block);
                    self.builder.seal_block(list_block);

                    let value = self.unbox_value_to_pointer(value);

                    // Its not a pair if its an empty list
                    let length = self
                        .builder
                        .ins()
                        .load(types::I32, MemFlags::new(), value, 16);

                    let not_empty = self.builder.ins().icmp_imm(IntCC::NotEqual, length, 0);

                    self.builder.ins().jump(merge_block, &[not_empty]);
                }

                {
                    // Else case, not a list or pair
                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    let false_val = self.builder.ins().iconst(types::I8, 0);
                    self.builder.ins().jump(merge_block, &[false_val]);
                }

                {
                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let result = self.builder.block_params(merge_block)[0];
                    self.push(result, InferredType::UnboxedBool);
                }

                self.drop_tagged_value(value);

                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.properties.get(&ValueOrRegister::Register(p)) {
                    // Elide the call entirely if its a non empty list
                    // Some(Properties::NonEmptyList) if false => {
                    //     println!("Found non empty list!");

                    //     self.shadow_stack.pop();
                    //     let res = self.builder.ins().iconst(types::I8, 1);
                    //     self.push(res, InferredType::UnboxedBool);
                    //     self.ip += 1;
                    // }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_function_returns_value_args("pair?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("pair?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_list(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_list = self.is_type(value, SteelVal::LIST_TAG);
                self.drop_tagged_value(value);
                self.push(is_list, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                // let register = self.register_index(p);

                match self.properties.get(&ValueOrRegister::Register(p)) {
                    // Elide the call entirely if its a non empty list. NOTE: We can't do this here.
                    // Some(Properties::NonEmptyList) if false => {
                    //     self.shadow_stack.pop();
                    //     let res = self.builder.ins().iconst(types::I8, 1);
                    //     self.push(res, InferredType::UnboxedBool);
                    //     self.ip += 1;
                    // }

                    // TODO: Figure out how to align the inferred types and properties
                    // so that there is some semblance of structure
                    Some(Properties::ProperList) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }

                    _ => {
                        self.shadow_stack.pop();
                        // let res = self.call_function_returns_value_args("list?", &[register]);
                        // self.push(res, InferredType::Bool);

                        let value = self.read_from_vm_stack(p);
                        let is_list = self.is_type(value, SteelVal::LIST_TAG);

                        // Mark that this was a list
                        self.properties.add_property(
                            ValueOrRegister::Value(is_list),
                            Properties::CheckedList(ValueOrRegister::Register(p)),
                        );

                        self.push(is_list, InferredType::UnboxedBool);

                        // Now, we also can attach the

                        self.ip += 1;
                    }
                }
            }
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res = self.call_function_returns_value_args_no_context("list?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_string(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            Value(stack_value) => {
                self.shadow_stack.pop();
                let value = stack_value.as_steelval(self);
                let is_list = self.is_type(value, SteelVal::STRING_TAG);
                self.drop_tagged_value(value);
                self.push(is_list, InferredType::UnboxedBool);
                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);

                match self.local_to_value_map.get(&p) {
                    // Elide the call entirely if its a non empty list
                    Some(InferredType::String) => {
                        self.shadow_stack.pop();
                        let res = self.builder.ins().iconst(types::I8, 1);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 1;
                    }
                    _ => {
                        self.shadow_stack.pop();
                        let res = self.call_function_returns_value_args("string?", &[register]);

                        self.push(res, InferredType::Bool);
                        self.ip += 1;
                    }
                }
            }
            Constant(constant_value) => {
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("string?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
    }

    pub(super) fn is_empty(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching - if this is used in the test position
            // of an if statement, we should encode the type checking
            // through.
            Value(stack_value) => {
                self.shadow_stack.pop();
                // If we've already inferrred this type as a pair,
                // we can skip the code generation for checking the tags
                // and actually invoking the function since we know
                // it will be a pair.
                // match stack_value.inferred_type {
                //     InferredType::List | InferredType::Pair | InferredType::ListOrPair if false => {
                //         let res = self.builder.ins().iconst(types::I64, 1);
                //         let boolean =
                //             self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                //         // TODO: Also - we'll need to check the length of the list!
                //         // this should be able to be done inline as well, we just have to load
                //         // the index of the list.
                //         self.push(boolean, InferredType::Bool);
                //         self.ip += 1;
                //         self.drop_tagged_value(stack_value.value);
                //         return;
                //     }
                //     _ => {}
                // }

                let value = stack_value.as_steelval(self);
                let result = self.check_null_no_drop(value);
                self.push(result, InferredType::UnboxedBool);
                self.drop_tagged_value(value);

                self.ip += 1;
            }

            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);
                self.shadow_stack.pop();
                let res = self.call_function_returns_value_args("empty?", &[register]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                // TODO: Check the inferred type here as well, maybe do unboxed bools
                let (value, inferred_type) = self.shadow_pop();
                let res =
                    self.call_function_returns_value_args_no_context("empty?-value", &[value]);
                self.push(res, InferredType::Bool);
                self.ip += 1;
            }
        }
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

    // Given a value, unbox it. This assumes this is coming from a register, so
    // we don't clone it in the happy path case where it is in fact a box.
    pub(super) fn unbox_value_checked_register(&mut self, value: Value) -> Value {
        println!("inline unbox value");
        // TODO: Check that this is actually a box!
        let is_box = self.is_type(value, SteelVal::HEAP_REF_VALUE_TAG);
        let typ = self.int;

        self.converging_if(
            is_box,
            |ctx| {
                // Lets upgrade the pointer first. Otherwise, we're doomed since the
                // value could not be around during the process. We can probably
                // relax this constraint because we won't be in a safepoint during unboxing
                // now. But, considering we could be in an embedded environment, we could
                // have some rogue values around.
                let ptr = ctx.unbox_value_to_pointer(value);

                // The data lives at an offset of 16 from the pointer
                const OFFSET: i64 = 16;

                let lock_pointer = ctx.builder.ins().iadd_imm(ptr, OFFSET);

                let data = ctx.with_spinlock(lock_pointer, |ctx| {
                    let data = ctx.builder.ins().load(
                        types::I128,
                        MemFlags::trusted(),
                        lock_pointer,
                        SpinLock::<SteelVal>::data_offset() as i32,
                    );

                    ctx.clone_value(data);

                    data
                });

                data
            },
            |ctx| {
                // Slow path, check deopt too
                ctx.clone_value(value);
                let res = ctx.call_function_returns_value_args("unbox-handler", &[value]);
                ctx.check_deopt();
                res
            },
            typ,
        )
    }

    // TODO: Replace the spin lock with an actual mutex implementation,
    // eventually.
    pub(super) fn with_spinlock<O>(
        &mut self,
        lock_pointer: Value,
        thunk: impl FnOnce(&mut Self) -> O,
    ) -> O {
        emit_spinlock_inline(&mut self.builder, lock_pointer);
        let res = thunk(self);
        emit_spinlock_unlock_inline(&mut self.builder, lock_pointer);

        res
    }
}

// TODO: Use this now for anything that is boxed:
// i.e. Vectors and boxes
//
// Box get / unbox should now be inlineable.
// Vector ref / set should now be inlineable (once we swap the vector implementation)
// to the custom one.
//
// Struct refs should now be inlineable.
fn emit_spinlock_inline(builder: &mut FunctionBuilder, lock_ptr: Value) {
    let spin_entry = builder.create_block();
    let spin_wait = builder.create_block();
    let acquired = builder.create_block();

    builder.ins().jump(spin_entry, &[]);

    // Attempt CAS
    builder.switch_to_block(spin_entry);
    let zero = builder.ins().iconst(types::I32, 0);
    let one = builder.ins().iconst(types::I32, 1);
    let old = builder
        .ins()
        .atomic_cas(MemFlags::trusted(), lock_ptr, zero, one);
    let cas_ok = builder.ins().icmp(IntCC::Equal, old, zero);
    builder.ins().brif(cas_ok, acquired, &[], spin_wait, &[]);

    // load until zero
    builder.switch_to_block(spin_wait);
    let val = builder
        .ins()
        .atomic_load(types::I32, MemFlags::trusted(), lock_ptr);
    let is_free = builder.ins().icmp(IntCC::Equal, val, zero);
    builder.ins().brif(is_free, spin_entry, &[], spin_wait, &[]);

    // Acquired
    builder.switch_to_block(acquired);
    builder.seal_block(spin_entry);
    builder.seal_block(spin_wait);
    builder.seal_block(acquired);
}

fn emit_spinlock_unlock_inline(builder: &mut FunctionBuilder, lock_ptr: Value) {
    let zero = builder.ins().iconst(types::I32, 0);
    builder
        .ins()
        .atomic_store(MemFlags::trusted(), zero, lock_ptr);
}
