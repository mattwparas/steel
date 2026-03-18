use super::*;

impl<'a> FunctionTranslator<'a> {
    fn shadow_last_cloned(&self) -> MaybeStackValue {
        self.shadow_stack.last().unwrap().clone()
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
                    (Some(Properties::NonEmptyList), _) => {
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
