//! Lowering of locals: constants, let slots, `set!` and captured values.
//!
//! The bodies of `stack_to_ssa`'s larger match arms. The arms themselves, with
//! their guards, stay in `stack_to_ssa`, in order.

use super::*;

impl FunctionTranslator<'_> {
    /// `PUSHCONST`: push a constant.
    pub(super) fn lower_push_const(&mut self) {
        let payload = self.instructions[self.ip].payload_size.to_usize();

        let constant = self.constants.get(payload);

        let encoded = match constant {
            SteelVal::NumV(n) => ConstantValue::Float(n),
            SteelVal::IntV(i) => ConstantValue::Int(i),
            SteelVal::BoolV(b) => ConstantValue::Bool(b),
            SteelVal::CharV(c) => ConstantValue::Char(c),
            SteelVal::ListV(_) => ConstantValue::List(payload),
            SteelVal::SymbolV(_) => ConstantValue::Symbol(payload),
            _ => ConstantValue::Index(payload),
        };

        self.shadow_push(MaybeStackValue::Constant(encoded));
        self.ip += 1;
    }

    /// `LetVar`: bind the value on top of the stack to a let slot.
    pub(super) fn lower_let_var(&mut self) {
        self.ip += 1;

        self.maybe_check_last();

        // A cursor bound by a let moves with its head. The new slot
        // takes the head, the old one is left void (it was a move, so
        // nothing reads it again), and the walk stays in registers.
        let moved_cursor = match self.shadow_stack.last() {
            Some(MaybeStackValue::MutRegister(j)) if self.cursors_on => {
                let j = *j;
                self.properties
                    .cached_lookups
                    .cursors
                    .get(&j)
                    .copied()
                    .map(|c| (j, c))
            }
            _ => None,
        };
        let moved_props = moved_cursor.and_then(|(j, _)| {
            self.properties
                .props
                .get(&ValueOrRegister::Register(j))
                .cloned()
        });

        let (last, typ) = match moved_cursor {
            Some((j, cursor)) => {
                self.shadow_stack.pop();
                let head = self.remove_from_vm_stack_raw(j);
                self.properties.cached_lookups.cursors.remove(&j);
                self.properties.cached_lookups.registers.remove(&j);
                self.properties.props.insert(
                    ValueOrRegister::Register(j),
                    vec![Properties::InferredType(InferredType::Void)],
                );
                let fresh = ListCursor {
                    cell: self.builder.declare_var(types::I64),
                    index: self.builder.declare_var(types::I64),
                };
                let cell = self.builder.use_var(cursor.cell);
                let index = self.builder.use_var(cursor.index);
                self.builder.def_var(fresh.cell, cell);
                self.builder.def_var(fresh.index, index);
                (head, (InferredType::List, Some(fresh)))
            }
            None => {
                let (v, t) = self.shadow_pop();
                (v, (t, None))
            }
        };
        let (typ, fresh_cursor) = typ;

        // Count all enclosing scopes, since `let*` nests and both read
        // paths index with `let_var_stack.iter().sum()`.
        // This is the slot `push_to_vm_stack_let_var_new` puts the value
        // in: after the arguments, the enclosing let bindings, and any
        // pending operands already spilled onto the vm stack below this
        // one. Without the spilled operands, the binding's type would be
        // recorded against the slot of the operand beneath it, e.g. the
        // first half of `(+ (let ...) (let ...))`.
        let spilled_below = self
            .shadow_stack
            .iter()
            .filter(|e| matches!(e, MaybeStackValue::Value(v) if v.spilled))
            .count();
        let local_index =
            self.let_var_stack.iter().sum::<usize>() + self.arity as usize + spilled_below;
        self.value_to_local_map.insert(last, local_index);
        self.local_to_value_map.insert(local_index, typ);

        *self.let_var_stack.last_mut().unwrap() += 1;

        match typ {
            InferredType::List => {
                self.properties.set_property(
                    ValueOrRegister::Register(local_index),
                    Properties::ProperList,
                );
            }
            // Nothing to record, and writing it would mask a better
            // fact established later in the scope.
            InferredType::Any => {
                self.properties
                    .remove(&ValueOrRegister::Register(local_index));
            }
            // Carry the binding's type onto the slot, so
            // `immutable_register_to_value` can report it and the
            // arithmetic fast arms can skip the tag check.
            // `LETENDSCOPE` clears these when the scope ends and
            // `SETLOCAL` clears them on mutation.
            typ => {
                self.properties.set_property(
                    ValueOrRegister::Register(local_index),
                    Properties::InferredType(typ),
                );
            }
        }

        // The let var is not cached here. `local_index` counts let vars
        // only, so it misses operands spilled to the value stack across
        // a scope boundary. An inlined call under a pending `+` lands
        // its bindings one slot higher than this computes. The cache is
        // populated on read instead, where the index comes from the
        // bytecode.

        // TODO: @mparas - in the event we're using a local value,
        // we need to check if this is actually spilled or not.
        //
        // It shouldn't be though
        self.push_to_vm_stack_let_var_new(last);

        if let Some(fresh_cursor) = fresh_cursor {
            // The head in the slot is not the current position, so
            // nothing may cache it as the value.
            self.properties
                .cached_lookups
                .registers
                .remove(&local_index);
            self.properties
                .cached_lookups
                .cursors
                .insert(local_index, fresh_cursor);
            if let Some(props) = moved_props {
                self.properties
                    .props
                    .insert(ValueOrRegister::Register(local_index), props);
            }
        }
    }

    /// `SETLOCAL`: `set!` a local.
    pub(super) fn lower_set_local(&mut self, payload: usize) {
        let index = self.register_index(payload);
        let (value, _) = self.shadow_pop();

        // The slot's type is whatever `LetVar` recorded when it was
        // bound, and this overwrites the slot. Both maps have to be
        // cleared, since `local_to_value_map` feeds the dispatch guards
        // and `properties` feeds `immutable_register_to_value`, and a
        // known type lets a fast arm skip its tag check.
        let slot = payload as usize;
        self.local_to_value_map.remove(&slot);
        self.properties.remove(&ValueOrRegister::Register(slot));
        self.properties.cached_lookups.registers.remove(&slot);
        self.properties.cached_lookups.cursors.remove(&slot);

        let value = self.call_helper(SET_LOCAL_ANY, [index, value]);
        self.push(value, InferredType::Any);
        self.ip += 1;
    }

    /// `READCAPTURED` then `UNBOX`: read a captured box's value.
    pub(super) fn lower_read_captured_unbox(&mut self, payload: usize) {
        // Borrowed. The capture array belongs to the closure being
        // run, which outlives this read, and the unbox below is told
        // not to release it. Cloning here without a drop would leak a
        // reference to the box on every execution.
        let value = self.inline_read_captured(payload, false);
        // Advance for the read captured, but we can elide the unbox since its not
        // going to escape - we're just reading the box value. We can probably inline this
        // even more but it'll be fine for now.
        self.ip += 1;

        let res = self.unbox_value_checked_register(value, false);
        self.ip += 2;

        self.push(res, InferredType::Any);
    }

    /// `LETENDSCOPE`: drop a let scope's slots.
    pub(super) fn lower_let_end_scope(&mut self, payload: usize) {
        self.ip += 1;

        let amt = self.let_var_stack.pop().unwrap();

        let mut properties_to_remove = Vec::new();

        for index in 0..self.shadow_stack.len() {
            let v = self.shadow_stack.get_mut(index).unwrap();
            match v {
                MaybeStackValue::MutRegister(r) if *r >= payload as usize => {
                    let r = *r;
                    let (value, _) = self.mut_register_to_value(r);

                    self.properties.remove(&ValueOrRegister::Register(r));

                    properties_to_remove.push(r);

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    });
                }
                MaybeStackValue::Register(r) if *r >= payload as usize => {
                    let r = *r;
                    let (value, _) = self.immutable_register_to_value(r);

                    self.properties.remove(&ValueOrRegister::Register(r));
                    self.properties.cached_lookups.registers.remove(&r);
                    self.properties.cached_lookups.cursors.remove(&r);

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    });
                }
                _ => {}
            }
        }

        // If we know exactly how many elements we're dropping:

        self.inline_let_end_scope(payload, amt);

        for i in payload..payload + amt {
            self.properties.remove(&ValueOrRegister::Register(i));
            self.local_to_value_map.remove(&i);
            // The loop above only reaches slots still referenced from
            // the shadow stack. Otherwise a cached slot that nothing
            // points at outlives its scope, and the next let to reuse
            // the index reads the previous scope's value. Keyed the same
            // way `LetVar` writes it (let_var_stack + arity), which is
            // what `payload` already counts in.
            self.properties.cached_lookups.registers.remove(&i);
            self.properties.cached_lookups.cursors.remove(&i);
        }
    }
}
