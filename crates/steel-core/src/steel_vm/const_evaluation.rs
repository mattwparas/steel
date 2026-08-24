#![allow(unpredictable_function_pointer_comparisons)]

use crate::compiler::program::PRIM_CONST_LIST;
use crate::primitives::lists::{steel_length, steel_plist_validate_args};
use crate::rvals::{IntoSteelVal, Result, SteelVal};
use crate::{
    compiler::compiler::OptLevel,
    parser::{
        ast::{Atom, Begin, Define, LambdaFunction, List, Quote},
        span_visitor::get_span,
        visitors::VisitorMut,
    },
};
use crate::{
    parser::{
        ast::{ExprKind, If},
        interner::InternedString,
        kernel::Kernel,
        parser::SyntaxObject,
        tokens::TokenType,
        tryfrom_visitor::TryFromExprKindForSteelVal,
    },
    rerrs::ErrorKind,
    SteelErr,
};
use std::{collections::HashSet, convert::TryFrom};

use crate::values::HashMap;
use rustc_hash::{FxBuildHasher, FxHashSet};

use steel_parser::tokens::{IntLiteral, RealLiteral};
use thin_vec::ThinVec;

use super::cache::MemoizationTable;

type EnvId = usize;

const ROOT_ENV: EnvId = 0;

struct EnvArena {
    envs: Vec<ConstantEnv>,
}

impl EnvArena {
    fn new(bindings: HashMap<InternedString, SteelVal, FxBuildHasher>) -> Self {
        Self {
            envs: vec![ConstantEnv::root(bindings)],
        }
    }

    fn truncate(&mut self, len: usize) {
        self.envs.truncate(len);
    }

    fn push(&mut self, env: ConstantEnv) -> EnvId {
        self.envs.push(env);
        self.envs.len() - 1
    }

    fn env(&self, id: EnvId) -> &ConstantEnv {
        &self.envs[id]
    }

    fn env_mut(&mut self, id: EnvId) -> &mut ConstantEnv {
        &mut self.envs[id]
    }

    fn get(&mut self, mut id: EnvId, ident: &InternedString) -> Option<SteelVal> {
        loop {
            let env = &mut self.envs[id];

            if env.non_constant_bound.get(ident).is_some() {
                return None;
            }

            if let Some(value) = env.bindings.get(ident).cloned() {
                env.used_bindings.insert(*ident);
                return Some(value);
            }

            id = env.parent?;
        }
    }

    fn get_constant_list(&self, mut id: EnvId, ident: &InternedString) -> Option<SteelVal> {
        loop {
            let env = &self.envs[id];

            if let Some(value) = env.constant_lists.get(ident) {
                return Some(value.clone());
            }

            id = env.parent?;
        }
    }

    fn unbind(&mut self, mut id: EnvId, ident: &InternedString) -> Option<()> {
        loop {
            let env = &mut self.envs[id];

            if env.bindings.get(ident).is_some() {
                env.bindings.remove(ident);
                env.used_bindings.insert(*ident);
                return Some(());
            }

            id = env.parent?;
        }
    }
}

struct ConstantEnv {
    bindings: HashMap<InternedString, SteelVal, FxBuildHasher>,
    used_bindings: HashSet<InternedString, FxBuildHasher>,
    non_constant_bound: HashSet<InternedString, FxBuildHasher>,

    // We're not suggesting that we can completely eliminate the args,
    // but we could in theory eliminate the values for the
    constant_lists: HashMap<InternedString, SteelVal>,

    parent: Option<EnvId>,
}

impl ConstantEnv {
    fn root(bindings: HashMap<InternedString, SteelVal, FxBuildHasher>) -> Self {
        Self {
            bindings,
            used_bindings: HashSet::default(),
            non_constant_bound: HashSet::default(),
            constant_lists: Default::default(),
            parent: None,
        }
    }

    fn new_subexpression(parent: EnvId) -> Self {
        Self {
            bindings: HashMap::default(),
            used_bindings: HashSet::default(),
            non_constant_bound: HashSet::default(),
            constant_lists: Default::default(),
            parent: Some(parent),
        }
    }

    fn bind(&mut self, ident: &InternedString, value: SteelVal) {
        self.bindings.insert(*ident, value);
    }

    fn bind_non_constant(&mut self, ident: &InternedString) {
        self.non_constant_bound.insert(*ident);
    }

    fn bind_const_list(&mut self, ident: &InternedString, expr: SteelVal) {
        self.constant_lists.insert(*ident, expr);
    }
}

// Holds the global env that will eventually get passed down
// Holds the arena for all environments to eventually be dropped together
pub struct ConstantEvaluatorManager<'a> {
    envs: EnvArena,
    set_idents: FxHashSet<InternedString>,
    pub(crate) changed: bool,
    opt_level: OptLevel,
    _memoization_table: &'a mut MemoizationTable,
    kernel: &'a mut Option<Kernel>,
}

impl<'a> ConstantEvaluatorManager<'a> {
    pub fn new(
        memoization_table: &'a mut MemoizationTable,
        constant_bindings: HashMap<InternedString, SteelVal, FxBuildHasher>,
        opt_level: OptLevel,
        kernel: &'a mut Option<Kernel>,
    ) -> Self {
        Self {
            envs: EnvArena::new(constant_bindings),
            set_idents: HashSet::default(),
            changed: false,
            opt_level,
            _memoization_table: memoization_table,
            kernel,
        }
    }

    pub fn run(&mut self, input: Vec<ExprKind>) -> Result<Vec<ExprKind>> {
        self.changed = false;

        let mut results = Vec::with_capacity(input.len());

        // Collect the set expressions, ignore them for the constant folding
        let mut expr_level_sets = Vec::with_capacity(input.len());
        let mut collector = CollectSet::new(&mut self.set_idents);

        for expr in &input {
            collector.visit(expr);
            expr_level_sets.push(core::mem::take(&mut collector.expr_level_set_idents));
        }

        drop(collector);

        for (expr, expr_level_set_idents) in input.into_iter().zip(expr_level_sets) {
            let mut eval = ConstantEvaluator::new(
                &mut self.envs,
                &self.set_idents,
                &expr_level_set_idents,
                self.opt_level,
                self._memoization_table,
                self.kernel,
            );
            let mut output = expr;
            eval.visit(&mut output)?;
            self.changed = self.changed || eval.changed;

            if !eval.changed && !eval.root_constants_added {
                results.push(output);
                continue;
            }

            eval.changed = false;

            for _ in 0..10 {
                eval.visit(&mut output)?;
                if !eval.changed {
                    break;
                }

                self.changed = true;
                eval.changed = false;
            }

            self.envs.truncate(1);

            results.push(output)
        }

        Ok(results)

        // TODO: Only re-run with the manager on expressions that actually changed.
        // input
        //     .into_iter()
        //     .zip(expr_level_sets)
        //     .map(|(x, set)| {
        //         let mut eval = ConstantEvaluator::new(
        //             Rc::clone(&self.global_env),
        //             &self.set_idents,
        //             &set,
        //             self.opt_level,
        //             self.memoization_table,
        //             self.kernel,
        //         );
        //         let output = eval.visit(x);
        //         self.changed = self.changed || eval.changed;
        //         output
        //     })
        //     .collect()
    }
}

struct ConstantEvaluator<'a> {
    envs: &'a mut EnvArena,
    current: EnvId,
    set_idents: &'a FxHashSet<InternedString>,
    expr_level_set_idents: &'a FxHashSet<InternedString>,
    changed: bool,
    opt_level: OptLevel,
    _memoization_table: &'a mut MemoizationTable,
    kernel: &'a mut Option<Kernel>,
    scope_contains_define: bool,
    root_constants_added: bool,
}

// Converts the atom value into a `TokenType`.
fn steelval_to_atom(value: &SteelVal) -> Option<TokenType<InternedString>> {
    match value {
        SteelVal::BoolV(b) => Some(TokenType::BooleanLiteral(*b)),
        SteelVal::NumV(n) => Some(RealLiteral::Float((*n).into()).into()),
        SteelVal::CharV(c) => Some(TokenType::CharacterLiteral(*c)),
        SteelVal::IntV(i) => Some(IntLiteral::Small(*i).into()),
        SteelVal::StringV(s) => Some(TokenType::StringLiteral(s.as_str().into())),
        _ => None,
    }
}

impl<'a> ConstantEvaluator<'a> {
    fn new(
        envs: &'a mut EnvArena,
        set_idents: &'a FxHashSet<InternedString>,
        expr_level_set_idents: &'a FxHashSet<InternedString>,
        opt_level: OptLevel,
        memoization_table: &'a mut MemoizationTable,
        kernel: &'a mut Option<Kernel>,
    ) -> Self {
        Self {
            envs,
            current: ROOT_ENV,
            set_idents,
            expr_level_set_idents,
            changed: false,
            opt_level,
            _memoization_table: memoization_table,
            kernel,
            scope_contains_define: false,
            root_constants_added: false,
        }
    }

    fn to_constant(&mut self, expr: &ExprKind) -> Option<SteelVal> {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => self.eval_atom(syn),
            ExprKind::Quote(q) => {
                let inner = &q.expr;
                TryFromExprKindForSteelVal::try_from_expr_kind(inner.clone()).ok()
            }
            _ => None,
        }
    }

    fn is_truthy_constant(&mut self, expr: &ExprKind) -> bool {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => match &syn.ty {
                TokenType::BooleanLiteral(f) => return *f,
                TokenType::Identifier(s) => {
                    // If we found a set identifier, skip it
                    if self.set_idents.get(&s).is_some() || self.expr_level_set_idents.contains(&s)
                    {
                        self.envs.unbind(self.current, &s);

                        return false;
                    };
                    self.envs
                        .get(self.current, &s)
                        .map(|x| x.is_truthy())
                        .unwrap_or_default()
                }
                // todo!() figure out if it is ok to expand scope of eval_atom.
                TokenType::Number(_) => true,
                TokenType::StringLiteral(_) => true,
                TokenType::CharacterLiteral(_) => true,
                _ => false,
            },
            ExprKind::Quote(q) => {
                let inner = &q.expr;
                self.is_truthy_constant(inner)
            }
            _ => false,
        }
    }

    fn is_constant(&mut self, expr: &ExprKind) -> bool {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => match &syn.ty {
                TokenType::BooleanLiteral(_) => return true,
                TokenType::Identifier(s) => {
                    // If we found a set identifier, skip it
                    if self.set_idents.get(&s).is_some() || self.expr_level_set_idents.contains(&s)
                    {
                        self.envs.unbind(self.current, &s);

                        return false;
                    };
                    self.envs
                        .get(self.current, &s)
                        .map(|x| x.is_truthy())
                        .unwrap_or_default()
                }
                // todo!() figure out if it is ok to expand scope of eval_atom.
                TokenType::Number(_) => true,
                TokenType::StringLiteral(_) => true,
                TokenType::CharacterLiteral(_) => true,
                _ => true,
            },
            ExprKind::Quote(q) => {
                let inner = &q.expr;
                self.is_truthy_constant(inner)
            }
            _ => false,
        }
    }

    fn eval_atom(&mut self, t: &SyntaxObject) -> Option<SteelVal> {
        match &t.ty {
            TokenType::BooleanLiteral(b) => Some((*b).into()),
            TokenType::Identifier(s) => {
                // If we found a set identifier, skip it
                if self.set_idents.get(s).is_some() || self.expr_level_set_idents.contains(s) {
                    self.envs.unbind(self.current, s);

                    return None;
                };
                self.envs.get(self.current, s)
            }
            // todo!() figure out if it is ok to expand scope of eval_atom.
            TokenType::Number(n) => n.resolve().into_steelval().ok(),
            TokenType::StringLiteral(s) => Some(SteelVal::StringV((s.clone()).into())),
            TokenType::CharacterLiteral(c) => Some(SteelVal::CharV(*c)),
            _ => None,
        }
    }

    fn all_to_constant(&mut self, exprs: &[ExprKind]) -> Option<smallvec::SmallVec<[SteelVal; 8]>> {
        exprs.iter().map(|x| self.to_constant(x)).collect()
    }

    fn is_constant_list(&mut self, expr: Option<&ExprKind>) -> Option<SteelVal> {
        if let Some(arg) = expr.and_then(|x| x.atom_identifier()) {
            return self.envs.get_constant_list(self.current, arg);
        }

        None
    }

    fn eval_kernel_function(
        &mut self,
        ident: InternedString,
        func: &ExprKind,
        args: &[SteelVal],
    ) -> Result<Option<ExprKind>> {
        // TODO: We should just bail immediately if this results in an error
        let output = match self.kernel.as_mut().unwrap().call_function(&ident, args) {
            Ok(v) => v,
            Err(_) => return Ok(None),
        };

        self.constant_output(output, func)
    }

    fn eval_function(
        &mut self,
        evaluated_func: SteelVal,
        func: &ExprKind,
        args: &mut [SteelVal],
    ) -> Result<Option<ExprKind>> {
        // TODO: Eventually, re-enable the memoization table
        let output = match evaluated_func {
            SteelVal::MutFunc(f) => f(args),
            SteelVal::FuncV(f) => f(args),
            // Not a constant evaluatable function, just return the original input
            _ => return Ok(None),
        };

        let output = match output {
            Ok(output) => output,
            Err(_) => return Ok(None),
        };

        self.constant_output(output, func)
    }

    fn constant_output(&mut self, output: SteelVal, func: &ExprKind) -> Result<Option<ExprKind>> {
        if let Some(new_token) = steelval_to_atom(&output) {
            self.changed = true;

            return Ok(Some(ExprKind::Atom(Atom::new(SyntaxObject::new(
                new_token,
                get_span(func),
            )))));
        }

        if let Ok(lst) = ExprKind::try_from(&output) {
            self.changed = true;

            return Ok(Some(ExprKind::Quote(Box::new(Quote::new(
                lst,
                SyntaxObject::new(TokenType::Quote, get_span(func)),
            )))));
        }

        Ok(None)
    }
}

impl<'a> ConstantEvaluator<'a> {
    fn visit(&mut self, expr: &mut ExprKind) -> Result<()> {
        let replacement = match expr {
            ExprKind::If(f) => self.visit_if(f)?,
            ExprKind::Define(d) => self.visit_define(d)?,
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l)?,
            ExprKind::Begin(b) => self.visit_begin(b)?,
            ExprKind::Return(r) => self.visit_return(r)?,
            ExprKind::Quote(_) => None,
            ExprKind::Macro(_) => stop!(Generic => "unexpected macro found in const evaluator"),
            ExprKind::Atom(a) => {
                self.visit_atom(a);
                None
            }
            ExprKind::List(l) => self.visit_list(l)?,
            ExprKind::SyntaxRules(_) => {
                stop!(Generic => "unexpected syntax rules in const evaluator")
            }
            ExprKind::Set(s) => self.visit_set(s)?,
            ExprKind::Require(r) => {
                stop!(Generic => "unexpected require - require is only allowed at the top level"; r.location.span)
            }
            ExprKind::Let(l) => self.visit_let(l)?,
            ExprKind::Vector(_) => None,
        };

        if let Some(replacement) = replacement {
            *expr = replacement;
        }

        Ok(())
    }

    fn visit_if(&mut self, f: &mut Box<crate::parser::ast::If>) -> Result<Option<ExprKind>> {
        self.visit(&mut f.test_expr)?;

        if self.opt_level == OptLevel::Three && self.is_constant(&f.test_expr) {
            let mut branch = if self.is_truthy_constant(&f.test_expr) {
                core::mem::take(&mut f.then_expr)
            } else {
                core::mem::take(&mut f.else_expr)
            };

            self.visit(&mut branch)?;

            return Ok(Some(branch));
        }

        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;

        Ok(None)
    }

    fn visit_define(
        &mut self,
        define: &mut Box<crate::parser::ast::Define>,
    ) -> Result<Option<ExprKind>> {
        let identifier = *define.name.atom_identifier_or_else(
            throw!(BadSyntax => format!("Define expects an identifier, found: {}", define.name); define.location.span),
        )?;

        self.scope_contains_define = true;

        self.visit(&mut define.body)?;

        if let Some(c) = self.to_constant(&define.body) {
            if self.current == ROOT_ENV {
                self.root_constants_added = true;
            }
            self.envs.env_mut(self.current).bind(&identifier, c);
        } else {
            self.envs
                .env_mut(self.current)
                .bind_non_constant(&identifier);
        }

        Ok(None)
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut Box<crate::parser::ast::LambdaFunction>,
    ) -> Result<Option<ExprKind>> {
        let parent = self.current;
        let mut new_env = ConstantEnv::new_subexpression(parent);

        for arg in &lambda_function.args {
            let identifier = arg.atom_identifier_or_else(
                throw!(BadSyntax => format!("lambda expects an identifier for the arguments, found: {arg}"); lambda_function.location.span),
            )?;
            new_env.bind_non_constant(identifier);
        }

        let prev = self.scope_contains_define;
        self.scope_contains_define = false;

        self.current = self.envs.push(new_env);

        self.visit(&mut lambda_function.body)?;

        self.scope_contains_define = prev;
        self.current = parent;

        Ok(None)
    }

    // TODO remove constants from the begins
    fn visit_begin(
        &mut self,
        begin: &mut Box<crate::parser::ast::Begin>,
    ) -> Result<Option<ExprKind>> {
        for expr in begin.exprs.iter_mut() {
            self.visit(expr)?;
        }

        Ok(None)
    }

    fn visit_return(
        &mut self,
        r: &mut Box<crate::parser::ast::Return>,
    ) -> Result<Option<ExprKind>> {
        self.visit(&mut r.expr)?;
        Ok(None)
    }

    fn visit_atom(&mut self, a: &mut crate::parser::ast::Atom) {
        let TokenType::Identifier(s) = &a.syn.ty else {
            return;
        };

        // If we found a set identifier, skip it
        if self.set_idents.get(s).is_some() || self.expr_level_set_idents.contains(s) {
            self.envs.unbind(self.current, s);

            return;
        }

        let replacement = self
            .envs
            .get(self.current, s)
            .and_then(|x| steelval_to_atom(&x));

        if let Some(new_token) = replacement {
            a.syn = SyntaxObject::new(new_token, a.syn.span);
        }
    }

    // Certainly the most complicated case: function application
    // Check if its a function application, and go for it
    fn visit_list(&mut self, l: &mut crate::parser::ast::List) -> Result<Option<ExprKind>> {
        if l.args.is_empty() {
            stop!(BadSyntax => "empty function application"; l.location);
        }

        if l.args.len() == 1 {
            self.visit(&mut l.args[0])?;

            let func = &l.args[0];

            if let Some(evaluated_func) = self.to_constant(func) {
                return self.eval_function(evaluated_func, func, &mut []);
            }

            if let Some(ident) = func.atom_identifier().copied().filter(|x| {
                // TODO: @Matt 4/24/23 - this condition is super ugly and I would prefer if we cleaned it up
                self.kernel.is_some() && self.kernel.as_ref().unwrap().is_constant(x)
            }) {
                return self.eval_kernel_function(ident, func, &[]);
            }

            if let ExprKind::LambdaFunction(f) = func {
                if !f.rest {
                    if !f.args.is_empty() {
                        stop!(ArityMismatch => format!("function expected {} arguments, found 0", f.args.len()); f.location.span)
                    }

                    // If the body is constant we can safely remove the application
                    // Otherwise we can't eliminate the additional scope depth
                    if self.is_constant(&f.body) {
                        if let ExprKind::LambdaFunction(f) = &mut l.args[0] {
                            return Ok(Some(core::mem::take(&mut f.body)));
                        }
                    }
                }
            }

            return Ok(None);
        }

        for arg in l.args.iter_mut().skip(1) {
            self.visit(arg)?;
        }

        // This means we're evaluating a function where the arg is a constant
        // list, AND the rest of the args are constant.
        if let Some(v) = self.is_constant_list(l.args.get(1)) {
            let mut arguments = vec![v];

            for x in l.args.iter().skip(2) {
                if let Some(c) = self.to_constant(x) {
                    arguments.push(c);
                }
            }

            if arguments.len() == l.args.len() - 1 {
                if let Some(SteelVal::FuncV(evaluated_func)) = self.to_constant(&l.args[0]) {
                    if evaluated_func == steel_plist_validate_args || evaluated_func == steel_length
                    {
                        return self.eval_function(
                            SteelVal::FuncV(evaluated_func),
                            &l.args[0],
                            &mut arguments,
                        );
                    }
                }
            }
        }

        // Resolve the arguments - if they're all constants, we have a chance to do constant evaluation
        if let Some(mut arguments) = self.all_to_constant(&l.args[1..]) {
            if let ExprKind::Atom(_) = &l.args[0] {
                // TODO: This shouldn't fail here under normal circumstances! If the end result is an error, we should
                // just return the value that was originally passed in. Otherwise, this signals
                // an error in the dataflow, and it means we're checking a condition that isn't constant
                // before applying a check against a constant value (which probably means we're missing)
                // something in the constant evaluation check. In which case, we should probably
                // just not stop the execution just because we errored
                if let Some(evaluated_func) = self.to_constant(&l.args[0]) {
                    return self.eval_function(evaluated_func, &l.args[0], &mut arguments);
                }

                if let Some(ident) = l.args[0].atom_identifier().copied().filter(|x| {
                    // TODO: @Matt 4/24/23 - this condition is super ugly and I would prefer if we cleaned it up
                    self.kernel.is_some() && self.kernel.as_ref().unwrap().is_constant(x)
                }) {
                    return self.eval_kernel_function(ident, &l.args[0], &arguments);
                }
            }
        }

        if !matches!(&l.args[0], ExprKind::LambdaFunction(_)) {
            self.visit(&mut l.args[0])?;
            return Ok(None);
        }

        let (func_expr, args) = l.args.split_first_mut().unwrap();

        let ExprKind::LambdaFunction(lambda) = func_expr else {
            unreachable!()
        };

        if lambda.args.len() != args.len() && !lambda.rest {
            let m = format!(
                "Anonymous function expected {} arguments, found {}",
                lambda.args.len(),
                args.len()
            );
            stop!(ArityMismatch => m; lambda.location.span);
        }

        let mut new_env = ConstantEnv::new_subexpression(self.current);

        if lambda.rest {
            if let Some((l_last, l_start)) = lambda.args.split_last() {
                let non_list_bindings = &args[0..l_start.len()];

                // If this is a rest arg, bind differently
                for (var, arg) in l_start.iter().zip(non_list_bindings) {
                    let identifier = var.atom_identifier_or_else(
                        throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); lambda.location.span),
                    )?;
                    if let Some(c) = self.to_constant(arg) {
                        new_env.bind(identifier, c);
                    } else {
                        new_env.bind_non_constant(identifier);
                    }
                }

                let last_identifier = l_last.atom_identifier_or_else(
                    throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {l_last}"); lambda.location.span),
                )?;

                let mut rest_args = Vec::new();

                for arg in &args[l_start.len()..] {
                    if let Some(c) = self.to_constant(arg) {
                        rest_args.push(c);
                    } else {
                        new_env.bind_non_constant(last_identifier);
                        break;
                    }
                }

                // If the length is the same, we didn't need to break early, meaning
                // the whole list is constant values
                if rest_args.len() == args[l_start.len()..].len() {
                    let list = SteelVal::ListV(rest_args.into());

                    new_env.bind(last_identifier, list);
                }
            }
        } else {
            for (var, arg) in lambda.args.iter().zip(args.iter()) {
                let identifier = var.atom_identifier_or_else(
                    throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); lambda.location.span),
                )?;
                if let Some(c) = self.to_constant(arg) {
                    new_env.bind(identifier, c);
                } else {
                    new_env.bind_non_constant(identifier);
                }
            }
        }

        let parent = self.current;
        self.current = self.envs.push(new_env);

        self.visit(&mut lambda.body)?;

        // Find which variables and arguments are actually used in the body of the function
        let mut used_arguments = 0;
        let mut non_constant_arguments = 0;

        let span = lambda.location.span;

        for (var, arg) in lambda.args.iter().zip(args.iter()) {
            let identifier = var.atom_identifier_or_else(
                throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); span),
            )?;

            // If the argument/variable is used internally, keep it
            // Also, if the argument is _not_ a constant
            if self
                .envs
                .env(self.current)
                .used_bindings
                .contains(identifier)
            {
                used_arguments += 1;
            } else if self.to_constant(arg).is_none() {
                non_constant_arguments += 1;
            }
        }

        // Found no arguments are there are no non constant arguments
        // TODO: @Matt 12/30/23 - this is causing a miscompilation - actually used
        // arguments is found to be empty.
        if used_arguments == 0 && non_constant_arguments == 0 && !self.scope_contains_define {
            // Unwind the recursion before we bail out
            self.current = parent;

            self.changed = true;
            return Ok(Some(core::mem::take(&mut lambda.body)));
        }

        // TODO only do this if all of the args are constant as well
        // Find a better way to do this
        if let Some(value_output) = self.to_constant(&lambda.body) {
            let remaining_indices: smallvec::SmallVec<[usize; 8]> = args
                .iter()
                .enumerate()
                .filter(|(_, x)| self.to_constant(x).is_none())
                .map(|(index, _)| index)
                .collect();

            self.changed = true;
            self.current = parent;

            let mut remaining: Vec<ExprKind> = remaining_indices
                .into_iter()
                .map(|index| core::mem::take(&mut args[index]))
                .collect();

            if remaining.is_empty() {
                return ExprKind::try_from(&value_output)
                    .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
                    .map(|x| {
                        Some(ExprKind::Quote(Box::new(Quote::new(
                            x,
                            SyntaxObject::default(TokenType::Quote),
                        ))))
                    });
            }

            remaining.push(core::mem::take(&mut lambda.body));

            // TODO come up witih a better location
            return Ok(Some(ExprKind::Begin(Box::new(Begin::new(
                remaining,
                lambda.location.clone(),
            )))));
        }

        // Unwind the 'recursion'
        self.current = parent;

        Ok(None)
    }

    fn visit_set(&mut self, s: &mut Box<crate::parser::ast::Set>) -> Result<Option<ExprKind>> {
        let identifier = *s.variable.atom_identifier_or_else(
            throw!(BadSyntax => "set expects an identifier"; s.location.span),
        )?;

        self.envs.unbind(self.current, &identifier);

        self.visit(&mut s.expr)?;

        Ok(None)
    }

    // TODO come back to this
    fn visit_let(&mut self, l: &mut Box<crate::parser::ast::Let>) -> Result<Option<ExprKind>> {
        let mut new_env = ConstantEnv::new_subexpression(self.current);

        for (_, arg) in l.bindings.iter_mut() {
            self.visit(arg)?;
        }

        for (var, arg) in l.bindings.iter() {
            let identifier = var.atom_identifier_or_else(
                throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); l.location.span),
            )?;

            if let Some(c) = self.to_constant(arg) {
                new_env.bind(identifier, c);
            } else {
                if let Some(maybe_const_list) = arg.list().and_then(|x| x.first_ident()) {
                    if *maybe_const_list == *PRIM_CONST_LIST {
                        let expr: ThinVec<_> = arg.list().unwrap().args.get(1..).unwrap().into();

                        let value = TryFromExprKindForSteelVal::try_from_expr_kind(ExprKind::List(
                            List::new(expr),
                        ))
                        .unwrap();

                        new_env.bind_const_list(identifier, value);
                    }
                }

                new_env.bind_non_constant(identifier);
            }
        }

        let parent = self.current;
        self.current = self.envs.push(new_env);

        self.visit(&mut l.body_expr)?;

        // Find which variables and arguments are actually used in the body of the function
        let mut used_arguments = 0;
        let mut non_constant_arguments = 0;

        let span = l.location.span;
        let mut retain = Vec::with_capacity(l.bindings.len());

        for (var, arg) in l.bindings.iter() {
            let identifier = var.atom_identifier_or_else(
                throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); span),
            )?;

            // If the argument/variable is used internally, keep it
            // Also, if the argument is _not_ a constant
            if self
                .envs
                .env(self.current)
                .used_bindings
                .contains(identifier)
            {
                used_arguments += 1;
                retain.push(true);
            } else if self.to_constant(arg).is_none() {
                non_constant_arguments += 1;
                retain.push(true);
            } else {
                retain.push(false);
            }
        }

        // Found no arguments are there are no non constant arguments
        // TODO: @Matt 12/30/23 - this is causing a miscompilation - actually used
        // arguments is found to be empty.
        if used_arguments == 0 && non_constant_arguments == 0 && !self.scope_contains_define {
            // Unwind the recursion before we bail out
            self.current = parent;

            self.changed = true;
            return Ok(Some(core::mem::take(&mut l.body_expr)));
        }

        self.current = parent;

        // TODO: @Matt
        // The issue here is that the bindings with transformed
        // right hand sides are not actually getting substituted
        // back in. What we need to do is replace the RHS
        // with the now visited constant evaluation, assuming its still
        // remaining.
        if retain.iter().any(|x| !x) {
            let mut index = 0;
            l.bindings.retain(|_| {
                let keep = retain[index];
                index += 1;
                keep
            });
        }

        Ok(None)
    }
}

// TODO: If the value is local, we need to exclude it:
// entering and exiting a scope should push and pop it off.
struct CollectSet<'a> {
    set_idents: &'a mut FxHashSet<InternedString>,
    scopes: quickscope::ScopeSet<InternedString, FxBuildHasher>,
    pub expr_level_set_idents: FxHashSet<InternedString>,
}

impl<'a> CollectSet<'a> {
    fn new(set_idents: &'a mut FxHashSet<InternedString>) -> Self {
        Self {
            set_idents,
            scopes: quickscope::ScopeSet::default(),
            expr_level_set_idents: FxHashSet::default(),
        }
    }
}

impl<'a> VisitorMut for CollectSet<'a> {
    type Output = ();

    fn visit_if(&mut self, f: &If) -> Self::Output {
        self.visit(&f.test_expr);
        self.visit(&f.then_expr);
        self.visit(&f.else_expr);
    }

    fn visit_define(&mut self, define: &Define) -> Self::Output {
        self.visit(&define.name);
        self.visit(&define.body);
    }

    fn visit_lambda_function(&mut self, lambda_function: &LambdaFunction) -> Self::Output {
        self.scopes.push_layer();

        for arg in &lambda_function.args {
            if let Some(ident) = arg.atom_identifier() {
                self.scopes.define(*ident);
            }
        }

        self.visit(&lambda_function.body);

        self.scopes.pop_layer();
    }

    fn visit_begin(&mut self, begin: &Begin) -> Self::Output {
        for expr in &begin.exprs {
            self.visit(expr);
        }
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr);
    }

    fn visit_quote(&mut self, _quote: &Quote) -> Self::Output {}

    fn visit_macro(&mut self, _m: &crate::parser::ast::Macro) -> Self::Output {}

    fn visit_atom(&mut self, _a: &Atom) -> Self::Output {}

    fn visit_list(&mut self, l: &List) -> Self::Output {
        for expr in &l.args {
            self.visit(expr);
        }
    }

    fn visit_syntax_rules(&mut self, _l: &crate::parser::ast::SyntaxRules) -> Self::Output {}

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        if let Ok(identifier) = s.variable.atom_identifier_or_else(
            throw!(BadSyntax => "set expects an identifier"; s.location.span),
        ) {
            if !self.scopes.contains(identifier) {
                // println!("NOT IN SCOPE: {}", identifier.resolve());

                self.set_idents.insert(*identifier);
            } else {
                self.expr_level_set_idents.insert(*identifier);

                // println!("IN SCOPE: {}", identifier.resolve());
            }

            // self.set_idents.insert(*identifier);
        }

        self.visit(&s.expr);
    }

    fn visit_require(&mut self, _s: &crate::parser::ast::Require) -> Self::Output {}

    fn visit_let(&mut self, l: &crate::parser::ast::Let) -> Self::Output {
        self.scopes.push_layer();
        l.bindings.iter().for_each(|x| self.visit(&x.1));

        for (arg, _) in &l.bindings {
            if let Some(ident) = arg.atom_identifier() {
                self.scopes.define(*ident);
            }
        }

        self.visit(&l.body_expr);

        self.scopes.pop_layer();
    }

    fn visit_vector(&mut self, _v: &crate::parser::ast::Vector) -> Self::Output {}
}
