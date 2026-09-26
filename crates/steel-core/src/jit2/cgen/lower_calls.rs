//! Lowering of calls: to functions on the stack, to globals and to primitives.
//!
//! The bodies of `stack_to_ssa`'s larger match arms. The arms themselves, with
//! their guards, stay in `stack_to_ssa`, in order.

use super::*;

impl FunctionTranslator<'_> {
    /// `FUNC` / `FUNCNOARITY`: call the function under the arguments.
    pub(super) fn lower_func(&mut self, payload: usize) {
        let arity = payload;
        let name = CallFunctionDefinitions::arity_to_name(arity);
        self.ip += 1;

        // TODO: @Matt
        // Lets do a hierarchical check. In the event this is a
        // closure, we can fast path calling the function ourselves
        // with the function calling prelude like what we use
        // otherwise.
        //
        // The only thing we're missing here is how to construct
        // a fat pointer from the closure itself. In this case,
        // I think we just have to pair the pointer to the data
        // + the length, and we'll be good to go.

        match self.shadow_stack.last() {
            // We can clone the value off the register in order
            // to push this value on to the stack. Once we pull
            // it off the register, we then run _clone_ on this,
            // to make sure that its accessible from the child.
            //
            // Then, we'll check that it has a super instruction
            // and that our depth isn't too large.
            Some(MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)) => {
                // Check the type:
                let func = self.read_from_vm_stack(*i);
                self.inline_call_func(arity, name, func, true, true);
            }

            Some(MaybeStackValue::Value(StackValue { value, .. })) => {
                self.inline_call_func(arity, name, *value, false, true);
            }

            _ => {
                if let Some(name) = name {
                    let v = self.call_function(arity, name, false);
                    self.push(v, InferredType::Any);
                } else {
                    todo!("Implement spilled function call (arity {})", arity);
                }
            }
        }

        self.check_deopt();
    }

    /// `TAILCALL` / `TAILCALLNOARITY`: tail call the function under the arguments.
    ///
    /// Ends this function's translation.
    pub(super) fn lower_tail_call(&mut self, payload: usize) {
        let arity = payload;
        let name = CallFunctionTailDefinitions::arity_to_name(arity);
        self.ip += 1;

        match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::MutRegister(i)) => {
                // Remove the register argument
                self.shadow_stack_pop();

                // We don't need to clone, because we've just read it from the stack.
                let value = self.remove_from_vm_stack(i);

                let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                // Capture what the stack is before hand, we'll need this
                let old_stack = self.shadow_stack.clone();

                self.converging_if_no_value(
                    is_closure,
                    |ctx| {
                        let closure = ctx.unbox_value_to_pointer(value);
                        ctx.inline_local_tail_call(arity, closure, value);
                    },
                    |ctx| {
                        ctx.shadow_stack = old_stack.clone();

                        // Lets call the function:
                        if let Some(name) = name {
                            let v = ctx.call_function_with_func(arity, name, true, value);
                            ctx.push(v, InferredType::Any);
                        } else {
                            todo!(
                                "Implement spilled function call bail out case (arity {})",
                                arity
                            );
                        }
                    },
                );

                self.ip = self.instructions.len() + 1;
                self.check_deopt();
                self.depth -= 1;
            }

            Some(MaybeStackValue::Value(StackValue { value, .. })) => {
                // Remove the register argument
                self.shadow_stack_pop();

                let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                // Capture what the stack is before hand, we'll need this
                let old_stack = self.shadow_stack.clone();

                self.converging_if_no_value(
                    is_closure,
                    |ctx| {
                        let closure = ctx.unbox_value_to_pointer(value);
                        ctx.inline_local_tail_call(arity, closure, value);
                    },
                    |ctx| {
                        ctx.shadow_stack = old_stack.clone();

                        // Lets call the function:
                        if let Some(name) = name {
                            let v = ctx.call_function_with_func(arity, name, true, value);
                            ctx.push(v, InferredType::Any);
                        } else {
                            todo!(
                                "Implement spilled function call bail out case (arity {})",
                                arity
                            );
                        }
                    },
                );

                self.ip = self.instructions.len() + 1;
                self.check_deopt();
                self.depth -= 1;
            }

            Some(MaybeStackValue::Register(i)) => {
                // Remove the register argument
                self.shadow_stack_pop();

                // We don't need to clone, because we've just read it from the stack.
                let value = self.read_from_vm_stack(i);

                let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                // Capture what the stack is before hand, we'll need this
                let old_stack = self.shadow_stack.clone();

                self.converging_if_no_value(
                    is_closure,
                    |ctx| {
                        let closure = ctx.unbox_value_to_pointer(value);
                        ctx.increment_ref_count_closure(closure);
                        ctx.inline_local_tail_call(arity, closure, value);
                    },
                    |ctx| {
                        ctx.shadow_stack = old_stack.clone();
                        ctx.clone_value(value);
                        // Lets call the function:
                        if let Some(name) = name {
                            let v = ctx.call_function_with_func(arity, name, true, value);
                            ctx.push(v, InferredType::Any);
                        } else {
                            todo!(
                                "Implement spilled function call bail out case (arity {})",
                                arity
                            );
                        }
                    },
                );

                self.ip = self.instructions.len() + 1;
                self.check_deopt();
                self.depth -= 1;
            }

            _ => {
                if let Some(name) = name {
                    let v = self.call_function(arity, name, true);
                    self.push(v, InferredType::Any);
                } else {
                    todo!("Implement spilled function call (arity {})", arity);
                }

                self.ip = self.instructions.len() + 1;

                self.check_deopt();

                self.depth -= 1;
            }
        }
    }

    /// `CALLGLOBALTAIL` / `CALLGLOBALTAILNOARITY` / `CALLPRIMITIVETAIL`: tail call a global.
    ///
    /// Ends this function's translation.
    pub(super) fn lower_global_tail_call(&mut self, payload: usize) {
        let function_index = payload;
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();

        let func = self._globals.get(function_index).cloned();

        // TODO: For calling struct operations, then we'll
        // just call the thing, and we can move on to the pop
        // I think.
        let maybe_global = self._globals.get(function_index).cloned();
        if let Some(maybe_global) = maybe_global {
            match maybe_global {
                SteelVal::FuncV(f)
                    if f == steel_memq && arity == 2 && self.global_is_closed(function_index) =>
                {
                    let res = self.memq();

                    // Spilling the cloned stack: Coalesce the reads to avoid
                    // repeated lookups on the buf pointer. At this point
                    // the buf pointer is the same, so we can lift it up.
                    self.spill_cloned_stack();

                    self.exit_types.insert(InferredType::List);
                    self.exit_types.insert(InferredType::Bool);

                    self.return_from_tail_call(res);
                    return;
                }

                SteelVal::MutFunc(f) if f == steel_reverse && arity == 1 => {

                    //         todo!()
                }

                _ => {}
            }

            // TODO: @Matt -> This is the issue, something is
            // wrong with the way I'm reading this, or doing something.
            // Only for a closed global, since this bakes the struct
            // function in (`global_is_closed`).
            let spec =
                create_struct_spec(maybe_global).filter(|_| self.global_is_closed(function_index));
            if let Some(spec) = spec {
                // TODO: This is where we inline the calls for struct
                // functions
                if let Some((value, _typ)) =
                    self.inline_struct_call_no_drop(spec, arity, function_index)
                {
                    let value = self.take_struct_result(value);
                    self.spill_cloned_stack();
                    self.return_from_tail_call(value);
                    return;
                }
            }
        }

        // Call direct, by hard coding this, and we're gonna check that the
        // instructions exist already...
        if use_inline_global_tail_call() && matches!(func, Some(SteelVal::Closure(_))) {
            let function = if let Some(SteelVal::Closure(v)) = func {
                v
            } else {
                unreachable!();
            };

            // Take this fast path if the super instructions already exists.
            // Then we can do a direct call.
            //
            // This bakes this `Gc<ByteCodeLambda>` into the generated code, so
            // it is only done when the global can never hold anything else
            // (`global_is_closed`). Whether the closure captures anything
            // doesn't matter. Any other global is read when the call runs.
            //
            // Normally unreachable, since callee globals are usually still unbound
            // when a caller is compiled at construction time.
            // A variadic callee is fine here since the tail call builds
            // its rest list below, unless it was given too few
            // arguments, which has to raise from the helper path.
            let rest_ok = Self::rest_arg_plan(&function, arity).is_some();

            if !self.global_is_closed(function_index) {
                self.generic_global_tail_call(function_index, arity);
            } else if function.super_instructions().is_some() && rest_ok {
                self.inline_global_tail_call(arity, function);
                self.finish_tail_position();
            } else if late_tail_enabled() && rest_ok {
                // Not compiled yet. Mutually recursive functions can't
                // all be compiled before their callers, since the
                // topological sort has to break every cycle somewhere.
                // `inline_global_tail_call` reads the entry point
                // through the lambda at run time, so check there is one
                // by then, and use the deopt helper otherwise.
                // Leaked, like every lambda baked into code.
                let lambda = self.i64_const(unsafe {
                    std::mem::transmute::<Gc<ByteCodeLambda>, i64>(jit_leak(&function))
                });
                let entry = self.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    lambda,
                    closure_field_offset(offset_of!(ByteCodeLambda, super_instructions)),
                );
                let compiled = self.builder.ins().icmp_imm_s(IntCC::NotEqual, entry, 0);

                self.converging_if_no_value(
                    compiled,
                    // `depth` is not part of the branch snapshot, so
                    // only one arm may drop it, the slow path.
                    |ctx| ctx.inline_global_tail_call(arity, function.clone()),
                    |ctx| ctx.slow_path_deopt_tail_call(function_index, arity),
                );
                self.ip = self.instructions.len() + 1;
            } else {
                self.slow_path_deopt_tail_call(function_index, arity);
            }

            return;
        } else {
            // A tail call to a primitive we can emit directly doesn't
            // need the generic deopt helper. Mutable struct fields are
            // boxes, so `(#%unbox (getter ...))` and `#%set-box!` are
            // common here, and the helper would look up the global and
            // match on its kind again, which we already know here.
            //
            // Only for a closed global. Any other goes to the helpers
            // below, which check what it holds when the call runs.
            let inlined = if self.global_is_closed(function_index) {
                self.inline_primitive_tail_call(func.as_ref(), arity)
            } else {
                None
            };
            if let Some(value) = inlined {
                self.spill_cloned_stack();
                self.return_from_tail_call(value);
                return;
            }

            // Unbound while this function is being compiled, e.g. a
            // callee defined further down the file, or the other
            // members of a mutually recursive cycle. There is nothing
            // to bake, so read the global at run time and tail call it
            // the way a local closure is. That path checks for a
            // compiled, fixed arity closure, and falls back to the
            // helper otherwise. It reads the current value every time,
            // so `set!` is not a concern here.
            // Unbound while this function is being compiled, e.g. a
            // callee defined further down the file, or the other
            // members of a mutually recursive cycle. There is nothing
            // to bake, so read the global when the call runs.
            if func.is_none() {
                self.generic_global_tail_call(function_index, arity);
                return;
            }

            // A native function (a builtin, or a host function from
            // `register_fn`) returns its value, so return it from
            // here instead of deopting. The helper checks the global
            // at run time and deopts if it holds something else.
            if matches!(
                func,
                Some(SteelVal::FuncV(_) | SteelVal::BoxedFunction(_) | SteelVal::MutFunc(_))
            ) {
                let result = match &func {
                    Some(SteelVal::BoxedFunction(f)) => {
                        CallBoxedFunctionTailDefinitions::arity_to_name(arity).map(|name| {
                            let f = self.leak_boxed_function(f);
                            self.call_global_function_with(
                                arity,
                                name,
                                function_index,
                                true,
                                Some(f),
                            )
                        })
                    }
                    _ => CallGlobalNativeTailDefinitions::arity_to_name(arity)
                        .map(|name| self.call_global_function(arity, name, function_index, true)),
                };

                if let Some(v) = result {
                    self.check_deopt();
                    self.return_from_tail_call(v);
                    return;
                }
            }

            self.slow_path_deopt_tail_call(function_index, arity);
            return;
        }
    }

    /// `CALLGLOBALNOARITY`: call a global known to take this many arguments.
    pub(super) fn lower_call_global_no_arity(&mut self, payload: usize) {
        // First - find the index that we have to lookup.
        let function_index = payload;
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();
        let frameless_calls_before = self.frameless_calls;

        // Okay, lets do a few things:
        //
        // We should attach some context for whether or not this is a closure.
        //
        // If its not a closure (i.e. has no captured values) then we can actually
        // embed the function directly into the call site. This should in theory,
        // make things a lot faster since now we'll be able to avoid dispatches
        // on the global environment.
        //
        // The biggest issue now, is that the function pointer that we pass to _this_
        // is not necessarily bound yet at the VM level. What we can do though is probably
        // eagerly determine for pure functions what index we're going to bind to.
        //
        // If its bound to a pure function, then we can embed the pointer directly
        // into the value, and then also leak the ref count here and embed it
        // directly into the generated code, so that we can call the function
        // without needing to look it up?
        // Recognise struct constructors, predicates and getters here,
        // like `call_global_impl` does, so they are emitted directly
        // instead of going through the generic deopt helper.
        // Only for a closed global, since this bakes the struct
        // function in (`global_is_closed`).
        if self.global_is_closed(function_index) {
            if let Some(spec) = self
                ._globals
                .get(function_index)
                .cloned()
                .and_then(create_struct_spec)
            {
                if let Some((value, typ)) =
                    self.inline_struct_call_no_drop(spec, arity, function_index)
                {
                    // `inline_struct_call_no_drop` has already advanced past the
                    // FUNC instruction, so this call is done.
                    self.push_struct_result(value, typ);
                    return;
                }
            }
        }

        let name = CallGlobalNoArityFunctionDefinitions::arity_to_name(arity);

        let self_name = CallSelfNoArityFunctionDefinitions::arity_to_name(arity);

        // Okay, lets try to install the self call if we have the ability to.
        //
        // We're also going to commit some very nasty crimes by just arbitrarily passing
        // the value to the function as is, and hope for the best :)
        if self.slot.is_some()
            && self_name.is_some()
            && self._globals.get(payload).is_none()
            && self.function_context == Some(function_index)
        {
            // A variadic callee given too few arguments has to
            // raise, which the helper path does with the right span.
            let arity_ok = self
                .slot
                .as_ref()
                .map(|x| Self::rest_arg_plan(x, arity).is_some())
                .unwrap_or(false);
            if arity_ok {
                let slot = self.slot.unwrap().clone();

                // TODO: Somehow... we'll have to change the calling convention
                // to always be (tag, Value), and handle that accordingly
                // in the function signature wherever possible
                match self.direct_self_call_target(arity) {
                    // Proven to fit, and this function has a
                    // frameless copy, so call that with no frame.
                    Some(_) if self.frameless.is_some() => {
                        let info = self.frameless.unwrap();
                        let result = self.frameless_self_call(arity, slot, info);
                        self.push(result, info.result.unwrap_or(InferredType::Any));
                    }
                    // Arguments proven to fit this specialized copy,
                    // so call it directly, skipping the generic copy's
                    // guard. Check for a deopt before the result is
                    // used, since it may carry an assumed type.
                    Some((target, assumed)) => {
                        let result =
                            self.call_self_function_experimental(arity, slot, Some(target));
                        self.check_deopt();
                        self.direct_self_calls += 1;
                        self.push(result, assumed.unwrap_or(InferredType::Any));
                    }
                    None => {
                        let result = self.call_self_function_experimental(arity, slot, None);
                        self.push(result, InferredType::Any);
                    }
                }
            } else {
                let slot = self.slot.unwrap().clone();
                let result = self.call_self_function(arity, self_name.unwrap(), slot, false);

                // Assuming this worked, we'll want to push this result on to the stack.
                self.push(result, InferredType::Any);
            }
        }
        // TODO: inline the call global here!
        else if matches!(self._globals.get(payload), Some(SteelVal::Closure(_)))
                        // And, we actually have a super instruction. A variadic
                        // callee is fine since the call site builds its rest list,
                        // unless it was given too few arguments, which has to
                        // raise from the helper path.
                        && self._globals.get(payload).map(|x| {
                            if let SteelVal::Closure(c) = x {
                                c.super_instructions().is_some()
                                    && Self::rest_arg_plan(c, arity).is_some()
                            } else {
                                false
                            }
                        }).unwrap_or_default()
                        // This bakes the closure in (`global_is_closed`).
                        && self.global_is_closed(function_index)
        {
            let func = if let SteelVal::Closure(c) = self._globals.get(payload).unwrap() {
                c.clone()
            } else {
                panic!();
            };

            let inferred_type = self.return_type_of(func.id);
            let result = self.call_self_function_experimental(arity, func, None);
            self.push(result, inferred_type);
        } else if CallFunctionDefinitions::arity_to_name(arity).is_some() {
            // Local name here is important
            let name = CallFunctionDefinitions::arity_to_name(arity);

            let value = self.inline_lookup_global(payload);

            self.clone_value(value);

            self.ip += 1;

            // This pushes the value on to the shadow stack for us
            self.inline_call_func(arity, name, value, false, false);
        } else if let Some(name) = name {
            // There is also the case that the function is not implemented yet.
            // For this, unfortunately I think we're going to have to look up the function,
            // and then call it, since it could be dynamically changing.
            //
            // We can use the same functions for calling functions, and just inline
            // the global lookup here as well; we can get rid of that call entirely
            // once global lookups are added to be inlined.

            let result = self.call_global_function(arity, name, function_index, false);

            // Assuming this worked, we'll want to push this result on to the stack.
            self.push(result, InferredType::Any);
        } else {
            let v = self.call_global_function_spilled(
                arity,
                CALL_GLOBAL_NO_ARITY_SPILLED,
                function_index,
                false,
            );

            self.push(v, InferredType::Any)
        }

        // A frameless copy can only make frameless calls. Anything
        // else pushes a real frame, which would sit underneath the
        // frames still waiting to be recorded for its callers.
        if self.is_frameless_callee() && self.frameless_calls == frameless_calls_before {
            self.frameless_bail = true;
        }

        // Then, we're gonna check the result and see if we should deopt
        self.check_deopt();
    }

    /// `CALLPRIMITIVE`: call a primitive through its reserved name.
    pub(super) fn lower_call_primitive(&mut self, payload: usize) {
        // Check the actual value that we're looking up, see if its there
        let function_index = payload;

        let global = self._globals.get(function_index);

        match global.cloned() {
            Some(SteelVal::FuncV(f)) => {
                // Attempt the other call
                self.ip += 1;
                let arity = self.instructions[self.ip].payload_size.to_usize();

                // Primitives with a lowering of their own; see
                // `inline_primitives.rs`.
                if !self.try_inline_primitive(f as usize, arity) {
                    let name = CallPrimitiveDefinitions::arity_to_name(arity);

                    if let Some(name) = name {
                        let origins = self.argument_origins(arity);
                        // attempt to move forward with it
                        let additional_args = self.split_off(arity);

                        let function = self.builder.ins().iconst(
                            self.module.target_config().pointer_type(),
                            // f as *const fn(&[SteelVal]) -> Result<SteelVal, crate::SteelErr>
                            //     as i64,
                            f as i64,
                        );

                        let fallback_ip = self.i64_const(self.ip as i64);

                        let mut args = vec![function, fallback_ip];

                        args.extend(additional_args.into_iter().map(|x| x.0));

                        let result = self.call_function_returns_value_args(name, &args);
                        self.push(result, primitive_result_type(f));
                        self.ip += 1;
                        self.check_deopt();
                        self.narrow_locals_after_call(
                            crate::steel_vm::builtin::get_function_name(f),
                            &origins,
                        );
                    } else {
                        self.ip -= 1;
                        self.call_global_impl(payload);
                    }
                }
            }

            Some(SteelVal::MutFunc(f)) => {
                // Attempt the other call
                self.ip += 1;
                let arity = self.instructions[self.ip].payload_size.to_usize();

                let name = CallPrimitiveMutDefinitions::arity_to_name(arity);

                if let Some(name) = name {
                    let origins = self.argument_origins(arity);
                    // attempt to move forward with it
                    let additional_args = self.split_off(arity);

                    let function = self.builder.ins().iconst(
                        self.module.target_config().pointer_type(),
                        //     -> Result<SteelVal, crate::SteelErr>
                        //     as i64,
                        f as i64,
                    );

                    let fallback_ip = self.i64_const(self.ip as i64);

                    let mut args = vec![function, fallback_ip];

                    args.extend(additional_args.into_iter().map(|x| x.0));

                    let result = self.call_function_returns_value_args(name, &args);
                    self.push(result, InferredType::Any);
                    self.ip += 1;
                    self.check_deopt();
                    self.narrow_locals_after_call(
                        crate::steel_vm::builtin::get_function_metadata(
                            crate::steel_vm::builtin::BuiltInFunctionType::Mutable(f),
                        ),
                        &origins,
                    );
                } else {
                    self.ip -= 1;
                    self.call_global_impl(payload);
                }
            }

            _ => {
                self.call_global_impl(payload);
            }
        }
    }
}
