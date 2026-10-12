use super::*;

impl<'a> FunctionTranslator<'a> {
    /// Pop the top operand as a boxed `SteelVal`.
    pub(super) fn pop_boxed(&mut self) -> Value {
        let operand = self.shadow_stack_pop().unwrap().into_value(self);
        operand.as_steelval(self)
    }

    /// Pop the top `N` operands as boxed values, in call order (deepest first).
    /// They come off the stack last argument first.
    pub(super) fn pop_boxed_args<const N: usize>(&mut self) -> [Value; N] {
        let mut args = [Value::from_u32(0); N];
        for slot in args.iter_mut().rev() {
            *slot = self.pop_boxed();
        }
        args
    }

    /// Whether each value carries its tag. Emits every check, then ands them
    /// left to right.
    pub(super) fn tags_are(&mut self, checks: &[(Value, u8)]) -> Value {
        let checks: Vec<Value> = checks
            .iter()
            .map(|&(value, tag)| self.is_type(value, tag))
            .collect();
        let (first, rest) = checks.split_first().expect("at least one tag check");
        rest.iter()
            .fold(*first, |all, &check| self.builder.ins().band(all, check))
    }

    /// A guarded fast path over the top `N` operands, popped as boxed values.
    pub(super) fn guarded_op<const N: usize>(&mut self) -> GuardedOp<'_, 'a, N> {
        let args = self.pop_boxed_args::<N>();
        self.guarded_on(args)
    }

    /// A guarded fast path over operands already in hand.
    pub(super) fn guarded_on<const N: usize>(&mut self, args: [Value; N]) -> GuardedOp<'_, 'a, N> {
        GuardedOp {
            translator: self,
            args,
            guard: None,
        }
    }
}

/// See the module docs. Built by `guarded_op` / `guarded_on`, finished by
/// `emit`.
pub(super) struct GuardedOp<'t, 'a, const N: usize> {
    translator: &'t mut FunctionTranslator<'a>,
    args: [Value; N],
    guard: Option<Value>,
}

impl<'t, 'a, const N: usize> GuardedOp<'t, 'a, N> {
    /// The fast path needs operand `i` to carry `tags[i]`.
    pub(super) fn require(mut self, tags: [u8; N]) -> Self {
        let checks: Vec<(Value, u8)> = self.args.iter().copied().zip(tags).collect();
        let tags = self.translator.tags_are(&checks);
        self.guard = Some(self.and(tags));
        self
    }

    /// And whatever else `condition` checks, emitted after the tag checks.
    pub(super) fn and_also(
        mut self,
        condition: impl FnOnce(&mut FunctionTranslator<'a>, [Value; N]) -> Value,
    ) -> Self {
        let extra = condition(self.translator, self.args);
        self.guard = Some(self.and(extra));
        self
    }

    fn and(&mut self, condition: Value) -> Value {
        match self.guard {
            Some(guard) => self.translator.builder.ins().band(guard, condition),
            None => condition,
        }
    }

    /// `fast` when the guard holds, `fallback` otherwise, merged into one value
    /// of type `typ`. With no guard, only `fast`.
    pub(super) fn emit(
        self,
        fast: impl Fn(&mut FunctionTranslator<'a>, [Value; N]) -> Value,
        fallback: impl Fn(&mut FunctionTranslator<'a>, [Value; N]) -> Value,
        typ: Type,
    ) -> Value {
        let args = self.args;
        match self.guard {
            Some(guard) => {
                self.translator
                    .converging_if(guard, |t| fast(t, args), |t| fallback(t, args), typ)
            }
            None => fast(self.translator, args),
        }
    }
}
