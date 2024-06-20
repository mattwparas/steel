use crate::compiler::program::number_literal_to_steel;
use crate::rvals::{Result, SteelVal};
use crate::{
    compiler::compiler::OptLevel,
    parser::{
        ast::{Atom, Begin, Define, LambdaFunction, List, Quote},
        span_visitor::get_span,
        visitors::{ConsumingVisitor, VisitorMut},
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
use std::{
    cell::RefCell,
    collections::HashSet,
    convert::TryFrom,
    rc::{Rc, Weak},
};

use fxhash::{FxBuildHasher, FxHashSet};
// use fxhash::FxHashSet;
use im_rc::HashMap;

use steel_parser::tokens::{IntLiteral, RealLiteral};

use super::cache::MemoizationTable;

type SharedEnv = Rc<RefCell<ConstantEnv>>;

struct ConstantEnv {
    bindings: HashMap<InternedString, SteelVal, FxBuildHasher>,
    used_bindings: HashSet<InternedString, FxBuildHasher>,
    non_constant_bound: HashSet<InternedString, FxBuildHasher>,
    parent: Option<Weak<RefCell<ConstantEnv>>>,
}

impl ConstantEnv {
    fn root(bindings: HashMap<InternedString, SteelVal, FxBuildHasher>) -> Self {
        Self {
            bindings,
            used_bindings: HashSet::default(),
            non_constant_bound: HashSet::default(),
            parent: None,
        }
    }

    fn new_subexpression(parent: Weak<RefCell<ConstantEnv>>) -> Self {
        Self {
            bindings: HashMap::default(),
            used_bindings: HashSet::default(),
            non_constant_bound: HashSet::default(),
            parent: Some(parent),
        }
    }

    fn bind(&mut self, ident: &InternedString, value: SteelVal) {
        self.bindings.insert(*ident, value);
    }

    fn bind_non_constant(&mut self, ident: &InternedString) {
        self.non_constant_bound.insert(*ident);
    }

    fn get(&mut self, ident: &InternedString) -> Option<SteelVal> {
        if self.non_constant_bound.get(ident).is_some() {
            return None;
        }

        let value = self.bindings.get(ident);
        if value.is_none() {
            self.parent
                .as_ref()?
                .upgrade()
                .expect("Constant environment freed early")
                .borrow_mut()
                .get(ident)
        } else {
            self.used_bindings.insert(*ident);
            value.cloned()
        }
    }

    fn _set(&mut self, ident: &InternedString, value: SteelVal) -> Option<SteelVal> {
        let output = self.bindings.get(ident);
        if output.is_none() {
            self.parent
                .as_ref()?
                .upgrade()
                .expect("Constant environment freed early")
                .borrow_mut()
                ._set(ident, value)
        } else {
            self.bindings.insert(*ident, value)
        }
    }

    fn unbind(&mut self, ident: &InternedString) -> Option<()> {
        if self.bindings.get(ident).is_some() {
            self.bindings.remove(ident);
            self.used_bindings.insert(*ident);
        } else {
            self.parent
                .as_ref()?
                .upgrade()
                .expect("Constant environment freed early")
                .borrow_mut()
                .unbind(ident);
        }
        Some(())
    }
}

// Holds the global env that will eventually get passed down
// Holds the arena for all environments to eventually be dropped together
pub struct ConstantEvaluatorManager<'a> {
    global_env: SharedEnv,
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
            global_env: Rc::new(RefCell::new(ConstantEnv::root(constant_bindings))),
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

        let mut collector = CollectSet::new(&mut self.set_idents);

        for expr in &input {
            collector.visit(&expr);
        }

        // let mut collector = CollectSet::new(&mut self.set_idents);

        // Collect the set expressions, ignore them for the constant folding
        for expr in input {
            let mut collector = CollectSet::new(&mut self.set_idents);

            collector.visit(&expr);

            let expr_level_set_idents = std::mem::take(&mut collector.expr_level_set_idents);

            // println!("Length of expr level sets!: {:?}", expr_level_set_idents);

            drop(collector);

            let mut eval = ConstantEvaluator::new(
                Rc::clone(&self.global_env),
                &self.set_idents,
                &expr_level_set_idents,
                self.opt_level,
                self._memoization_table,
                self.kernel,
            );
            let mut output = eval.visit(expr)?;
            self.changed = self.changed || eval.changed;

            eval.changed = false;

            for _ in 0..10 {
                output = eval.visit(output)?;
                if !eval.changed {
                    break;
                }

                eval.changed = false;
            }

            results.push(output)
        }

        // Only run this on an expr by expr basis
        self.changed = false;

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
    bindings: SharedEnv,
    set_idents: &'a FxHashSet<InternedString>,
    expr_level_set_idents: &'a [InternedString],
    changed: bool,
    opt_level: OptLevel,
    _memoization_table: &'a mut MemoizationTable,
    kernel: &'a mut Option<Kernel>,
    scope_contains_define: bool,
}

// Converts the atom value into a `TokenType`.
fn steelval_to_atom(value: &SteelVal) -> Option<TokenType<InternedString>> {
    match value {
        SteelVal::BoolV(b) => Some(TokenType::BooleanLiteral(*b)),
        SteelVal::NumV(n) => Some(RealLiteral::Float(*n).into()),
        SteelVal::CharV(c) => Some(TokenType::CharacterLiteral(*c)),
        SteelVal::IntV(i) => Some(IntLiteral::Small(*i).into()),
        SteelVal::StringV(s) => Some(TokenType::StringLiteral(s.to_string())),
        _ => None,
    }
}

impl<'a> ConstantEvaluator<'a> {
    fn new(
        bindings: Rc<RefCell<ConstantEnv>>,
        set_idents: &'a FxHashSet<InternedString>,
        expr_level_set_idents: &'a [InternedString],
        opt_level: OptLevel,
        memoization_table: &'a mut MemoizationTable,
        kernel: &'a mut Option<Kernel>,
    ) -> Self {
        Self {
            bindings,
            set_idents,
            expr_level_set_idents,
            changed: false,
            opt_level,
            _memoization_table: memoization_table,
            kernel,
            scope_contains_define: false,
        }
    }

    fn to_constant(&self, expr: &ExprKind) -> Option<SteelVal> {
        match expr {
            ExprKind::Atom(Atom { syn, .. }) => self.eval_atom(syn),
            ExprKind::Quote(q) => {
                let inner = &q.expr;
                TryFromExprKindForSteelVal::try_from_expr_kind(inner.clone()).ok()
            }
            _ => None,
        }
    }

    fn eval_atom(&self, t: &SyntaxObject) -> Option<SteelVal> {
        match &t.ty {
            TokenType::BooleanLiteral(b) => Some((*b).into()),
            TokenType::Identifier(s) => {
                // If we found a set identifier, skip it
                if self.set_idents.get(s).is_some() || self.expr_level_set_idents.contains(s) {
                    self.bindings.borrow_mut().unbind(s);

                    return None;
                };
                self.bindings.borrow_mut().get(s)
            }
            // todo!() figure out if it is ok to expand scope of eval_atom.
            TokenType::Number(n) => number_literal_to_steel(n).ok(),
            TokenType::StringLiteral(s) => Some(SteelVal::StringV(s.clone().into())),
            TokenType::CharacterLiteral(c) => Some(SteelVal::CharV(*c)),
            _ => None,
        }
    }

    fn all_to_constant(&self, exprs: &[ExprKind]) -> Option<smallvec::SmallVec<[SteelVal; 8]>> {
        exprs.iter().map(|x| self.to_constant(x)).collect()
    }

    fn eval_kernel_function(
        &mut self,
        ident: InternedString,
        func: ExprKind,
        mut raw_args: Vec<ExprKind>,
        args: &[SteelVal],
    ) -> Result<ExprKind> {
        // TODO: We should just bail immediately if this results in an error
        let output = match self.kernel.as_mut().unwrap().call_function(&ident, args) {
            Ok(v) => v,
            Err(_) => {
                // log::error!("{:?}", e);
                raw_args.insert(0, func);
                return Ok(ExprKind::List(List::new(raw_args)));
            }
        };

        if let Some(new_token) = steelval_to_atom(&output) {
            let atom = Atom::new(SyntaxObject::new(new_token, get_span(&func)));
            // debug!(
            //     "Const evaluation of a function resulted in an atom: {}",
            //     atom
            // );
            self.changed = true;
            Ok(ExprKind::Atom(atom))
        } else if let Ok(lst) = ExprKind::try_from(&output) {
            self.changed = true;
            let output = ExprKind::Quote(Box::new(Quote::new(
                lst,
                SyntaxObject::new(TokenType::Quote, get_span(&func)),
            )));
            // debug!(
            //     "Const evaluation of a function resulted in a quoted value: {}",
            //     output
            // );
            Ok(output)
        } else {
            // debug!(
            //     "Unable to convert constant-evalutable function output to value: {}",
            //     func
            // );
            // Something went wrong
            raw_args.insert(0, func);
            Ok(ExprKind::List(List::new(raw_args)))
        }

        // todo!()
    }

    fn eval_function(
        &mut self,
        evaluated_func: SteelVal,
        func: ExprKind,
        mut raw_args: Vec<ExprKind>,
        args: &mut [SteelVal],
    ) -> Result<ExprKind> {
        if evaluated_func.is_function() {
            match evaluated_func {
                SteelVal::MutFunc(f) => {
                    let output = f(args)
                        .map_err(|e| e.set_span_if_none(func.atom_syntax_object().unwrap().span))?;

                    self.handle_output(output, func, raw_args)
                }
                // TODO: Eventually, re-enable the memoization table
                SteelVal::FuncV(f) => {
                    // TODO: Clean this up - we shouldn't even enter this section of the code w/o having
                    // the actual atom itself.
                    // let output = if let Some(output) = self
                    // .memoization_table
                    // .get(SteelVal::FuncV(f), args.to_vec())
                    // {
                    // output
                    // } else {
                    let output = f(args)
                        .map_err(|e| e.set_span_if_none(func.atom_syntax_object().unwrap().span))?;

                    // self.memoization_table.insert(
                    //     SteelVal::FuncV(f),
                    //     args.to_vec(),
                    //     output.clone(),
                    // );

                    // output
                    // };

                    self.handle_output(output, func, raw_args)
                }
                _ => {
                    // debug!(
                    //     "Found a non-constant evaluatable function: {}",
                    //     evaluated_func
                    // );
                    raw_args.insert(0, func);
                    // Not a constant evaluatable function, just return the original input
                    Ok(ExprKind::List(List::new(raw_args)))
                }
            }
        } else {
            raw_args.insert(0, func);
            Ok(ExprKind::List(List::new(raw_args)))
        }
    }

    fn handle_output(
        &mut self,
        output: SteelVal,
        func: ExprKind,
        // evaluated_func: &SteelVal,
        mut raw_args: Vec<ExprKind>,
    ) -> std::result::Result<ExprKind, crate::SteelErr> {
        if let Some(new_token) = steelval_to_atom(&output) {
            let atom = Atom::new(SyntaxObject::new(new_token, get_span(&func)));
            // debug!(
            //     "Const evaluation of a function resulted in an atom: {}",
            //     atom
            // );
            self.changed = true;
            Ok(ExprKind::Atom(atom))
        } else if let Ok(lst) = ExprKind::try_from(&output) {
            self.changed = true;
            let output = ExprKind::Quote(Box::new(Quote::new(
                lst,
                SyntaxObject::new(TokenType::Quote, get_span(&func)),
            )));
            // debug!(
            //     "Const evaluation of a function resulted in a quoted value: {}",
            //     output
            // );
            Ok(output)
        } else {
            // debug!(
            //     "Unable to convert constant-evalutable function output to value: {}",
            //     evaluated_func
            // );
            // Something went wrong
            raw_args.insert(0, func);
            Ok(ExprKind::List(List::new(raw_args)))
        }
    }
}

impl<'a> ConsumingVisitor for ConstantEvaluator<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, f: Box<crate::parser::ast::If>) -> Self::Output {
        // Visit the test expression
        let test_expr = self.visit(f.test_expr)?;

        if self.opt_level == OptLevel::Three {
            if let Some(test_expr) = self.to_constant(&test_expr) {
                self.changed = true;
                if test_expr.is_truthy() {
                    // debug!("Const evaluation resulted in taking the then branch");
                    return self.visit(f.then_expr);
                } else {
                    // debug!("Const evaluation resulted in taking the else branch");
                    return self.visit(f.else_expr);
                }
            }
        }

        // If we found a constant, we can elect to only take the truthy path
        // if let Some(test_expr) = self.to_constant(&test_expr) {
        //     self.changed = true;
        //     if test_expr.is_truthy() {
        //         self.visit(f.then_expr)
        //     } else {
        //         self.visit(f.else_expr)
        //     }
        // } else {
        Ok(ExprKind::If(
            If::new(
                test_expr,
                self.visit(f.then_expr)?,
                self.visit(f.else_expr)?,
                f.location,
            )
            .into(),
        ))
        // }
    }

    fn visit_define(&mut self, mut define: Box<crate::parser::ast::Define>) -> Self::Output {
        let identifier = &define.name.atom_identifier_or_else(
            throw!(BadSyntax => format!("Define expects an identifier, found: {}", define.name); define.location.span),
        )?;

        self.scope_contains_define = true;

        define.body = self.visit(define.body)?;

        if let Some(c) = self.to_constant(&define.body) {
            self.bindings.borrow_mut().bind(identifier, c);
        } else {
            self.bindings.borrow_mut().bind_non_constant(identifier);
        }

        Ok(ExprKind::Define(define))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<crate::parser::ast::LambdaFunction>,
    ) -> Self::Output {
        let parent = Rc::clone(&self.bindings);
        let mut new_env = ConstantEnv::new_subexpression(Rc::downgrade(&parent));

        let prev = self.scope_contains_define;
        self.scope_contains_define = false;

        for arg in &lambda_function.args {
            let identifier = arg.atom_identifier_or_else(
                throw!(BadSyntax => format!("lambda expects an identifier for the arguments, found: {arg}"); lambda_function.location.span),
            )?;
            new_env.bind_non_constant(identifier);
        }

        self.bindings = Rc::new(RefCell::new(new_env));

        lambda_function.body = self.visit(lambda_function.body)?;

        self.scope_contains_define = prev;
        self.bindings = parent;

        Ok(ExprKind::LambdaFunction(lambda_function))
    }

    // TODO remove constants from the begins
    fn visit_begin(&mut self, mut begin: crate::parser::ast::Begin) -> Self::Output {
        for expr in begin.exprs.iter_mut() {
            *expr = self.visit(std::mem::take(expr))?;
        }

        Ok(ExprKind::Begin(begin))
    }

    fn visit_return(&mut self, mut r: Box<crate::parser::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        Ok(ExprKind::Return(r))
    }

    fn visit_quote(&mut self, quote: Box<crate::parser::ast::Quote>) -> Self::Output {
        Ok(ExprKind::Quote(quote))
    }

    fn visit_macro(&mut self, _m: Box<crate::parser::ast::Macro>) -> Self::Output {
        stop!(Generic => "unexpected macro found in const evaluator");
    }

    fn visit_atom(&mut self, a: crate::parser::ast::Atom) -> Self::Output {
        // if let Some(inner) = self.eval_atom(&a.syn) {
        //     // TODO Check this part - be able to propagate quoted values
        //     if let Some(new_token) = steelval_to_atom(&inner) {
        //         let atom = Atom::new(SyntaxObject::new(new_token, a.syn.span));
        //         return Ok(ExprKind::Atom(atom));
        //     }
        // }
        // Ok(ExprKind::Atom(a))

        match &a.syn.ty {
            TokenType::Identifier(s) => {
                // If we found a set identifier, skip it
                if self.set_idents.get(&s).is_some() || self.expr_level_set_idents.contains(&s) {
                    self.bindings.borrow_mut().unbind(&s);

                    return Ok(ExprKind::Atom(a));
                };
                if let Some(new_token) = self
                    .bindings
                    .borrow_mut()
                    .get(&s)
                    .and_then(|x| steelval_to_atom(&x))
                {
                    return Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                        new_token, a.syn.span,
                    ))));
                } else {
                    Ok(ExprKind::Atom(a))
                }
            }

            _ => Ok(ExprKind::Atom(a)),
        }
    }

    // Certainly the most complicated case: function application
    // Check if its a function application, and go for it
    fn visit_list(&mut self, l: crate::parser::ast::List) -> Self::Output {
        if l.args.is_empty() {
            stop!(BadSyntax => "empty function application"; get_span(&ExprKind::List(l)));
        }

        if l.args.len() == 1 {
            let mut args_iter = l.args.into_iter();
            let func_expr = args_iter.next().unwrap();
            let func = self.visit(func_expr)?;

            if let Some(evaluated_func) = self.to_constant(&func) {
                // debug!("Attempting to evaluate: {}", &func);
                return self.eval_function(evaluated_func, func, Vec::new(), &mut []);
            } else if let Some(ident) = func.atom_identifier().and_then(|x| {
                // TODO: @Matt 4/24/23 - this condition is super ugly and I would prefer if we cleaned it up
                if self.kernel.is_some() && self.kernel.as_ref().unwrap().is_constant(x) {
                    Some(x)
                } else {
                    None
                }
            }) {
                // log::debug!("Running kernel function!");

                return self.eval_kernel_function(ident.clone(), func, Vec::new(), &[]);
            } else {
                if let ExprKind::LambdaFunction(f) = &func {
                    if !f.rest {
                        if !f.args.is_empty() {
                            stop!(ArityMismatch => format!("function expected {} arguments, found 0", f.args.len()); f.location.span)
                        }

                        // If the body is constant we can safely remove the application
                        // Otherwise we can't eliminate the additional scope depth
                        if self.to_constant(&f.body).is_some() {
                            return Ok(f.body.clone());
                        }
                    }
                }

                let new_expr = vec![func].into_iter().chain(args_iter).collect();

                return Ok(ExprKind::List(List::new(new_expr)));
            }
        }

        let mut args = l.args.into_iter();

        let func_expr = args.next().expect("Function missing");
        let mut args: Vec<_> = args.map(|x| self.visit(x)).collect::<Result<_>>()?;

        // Resolve the arguments - if they're all constants, we have a chance to do constant evaluation
        if let Some(mut arguments) = self.all_to_constant(&args) {
            if let ExprKind::Atom(_) = &func_expr {
                // let span = get_span(&func_expr);

                if let Some(evaluated_func) = self.to_constant(&func_expr) {
                    // println!(
                    //     "Attempting to evaluate: {} with args: {:?}",
                    //     &func_expr, arguments
                    // );
                    // TODO: This shouldn't fail here under normal circumstances! If the end result is an error, we should
                    // just return the value that was originally passed in. Otherwise, this signals
                    // an error in the dataflow, and it means we're checking a condition that isn't constant
                    // before applying a check against a constant value (which probably means we're missing)
                    // something in the constant evaluation check. In which case, we should probably
                    // just not stop the execution just because we errored
                    return self.eval_function(evaluated_func, func_expr, args, &mut arguments);
                } else if let Some(ident) = func_expr.atom_identifier().and_then(|x| {
                    // TODO: @Matt 4/24/23 - this condition is super ugly and I would prefer if we cleaned it up
                    if self.kernel.is_some() && self.kernel.as_ref().unwrap().is_constant(x) {
                        Some(x)
                    } else {
                        None
                    }
                }) {
                    return self.eval_kernel_function(ident.clone(), func_expr, args, &arguments);
                }
                // return self.eval_function(func_expr, span, &arguments);
            }
        }

        match &func_expr {
            ExprKind::LambdaFunction(_) => {}
            _ => {
                let visited_func_expr = self.visit(func_expr)?;
                args.insert(0, visited_func_expr);
                return Ok(ExprKind::List(List::new(args)));
            } // ExprKind::
        }

        if let ExprKind::LambdaFunction(l) = func_expr {
            if l.args.len() != args.len() && !l.rest {
                println!("{}", l);

                let m = format!(
                    "Anonymous function expected {} arguments, found {}",
                    l.args.len(),
                    args.len()
                );
                stop!(ArityMismatch => m; l.location.span);
            }

            let mut new_env = ConstantEnv::new_subexpression(Rc::downgrade(&self.bindings));

            if l.rest {
                if let Some((l_last, l_start)) = l.args.split_last() {
                    let non_list_bindings = &args[0..l_start.len()];
                    // let list_binding = &args[l_start.len()..];

                    // If this is a rest arg, bind differently
                    for (var, arg) in l_start.iter().zip(non_list_bindings) {
                        let identifier = var.atom_identifier_or_else(
                            throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); l.location.span),
                        )?;
                        if let Some(c) = self.to_constant(arg) {
                            new_env.bind(identifier, c);
                        } else {
                            new_env.bind_non_constant(identifier);
                        }
                    }

                    let last_identifier = l_last.atom_identifier_or_else(
                            throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {l_last}"); l.location.span),
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
                // If this is a rest arg, bind differently
                for (var, arg) in l.args.iter().zip(args.iter()) {
                    let identifier = var.atom_identifier_or_else(
                    throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); l.location.span),
                )?;
                    if let Some(c) = self.to_constant(arg) {
                        new_env.bind(identifier, c);
                    } else {
                        new_env.bind_non_constant(identifier);
                    }
                }
            }

            let parent = Rc::clone(&self.bindings);
            self.bindings = Rc::new(RefCell::new(new_env));

            // println!("Visiting body: {}", l.body);

            let output = self.visit(l.body)?;

            // Unwind the 'recursion'
            // self.bindings = parent;

            // Find which variables and arguments are actually used in the body of the function
            let mut actually_used_variables = smallvec::SmallVec::<[_; 8]>::new();
            let mut actually_used_arguments = smallvec::SmallVec::<[_; 8]>::new();

            let mut non_constant_arguments = Vec::new();

            let span = l.location.span;

            for (var, arg) in l.args.iter().zip(args.iter()) {
                let identifier = var.atom_identifier_or_else(
                    throw!(BadSyntax => format!("lambda expects an identifier for the arguments: {var}"); span),
                )?;

                // If the argument/variable is used internally, keep it
                // Also, if the argument is _not_ a constant
                if self.bindings.borrow().used_bindings.contains(identifier) {
                    // if self.to_constant(arg).is_none() {
                    // println!("FOUND ARGUMENT: {}", identifier);
                    actually_used_variables.push(var.clone());
                    actually_used_arguments.push(arg.clone());
                    // }
                } else if self.to_constant(arg).is_none() {
                    // actually_used_variables.push(var.clone());
                    // println!("Found a non constant argument: {}", arg);
                    non_constant_arguments.push(arg.clone());
                }
            }

            // Found no arguments are there are no non constant arguments
            // TODO: @Matt 12/30/23 - this is causing a miscompilation - actually used
            // arguments is found to be empty.
            if actually_used_arguments.is_empty()
                && non_constant_arguments.is_empty()
                && !self.scope_contains_define
            {
                // println!("Found no used arguments or non constant arguments, returning the body");
                // println!("Output: {}", output);

                // Unwind the recursion before we bail out
                self.bindings = parent;

                self.changed = true;
                return Ok(output);
            }

            // if actually_used_arguments.is_empty() {
            //     non_constant_arguments.push(output);
            //     return Ok(ExprKind::Begin(Begin::new(
            //         non_constant_arguments,
            //         l.location,
            //     )));
            // }

            // TODO only do this if all of the args are constant as well
            // Find a better way to do this
            if let Some(value_output) = self.to_constant(&output) {
                let mut non_constant_arguments: Vec<_> = args
                    .into_iter()
                    .filter(|x| self.to_constant(x).is_none())
                    .collect();

                // debug!("Found a constant output from the body");

                self.changed = true;
                self.bindings = parent;
                if non_constant_arguments.is_empty() {
                    // println!("Returning here!");
                    return ExprKind::try_from(&value_output)
                        .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
                        .map(|x| {
                            ExprKind::Quote(Box::new(Quote::new(
                                x,
                                SyntaxObject::default(TokenType::Quote),
                            )))
                        });
                } else {
                    non_constant_arguments.push(output);
                    // TODO come up witih a better location
                    return Ok(ExprKind::Begin(Begin::new(
                        non_constant_arguments,
                        l.location,
                    )));
                }
            }

            // Unwind the 'recursion'
            self.bindings = parent;

            // let constructed_func = ExprKind::LambdaFunction(
            //     LambdaFunction::new(actually_used_variables, output, l.location).into(),
            // );

            let func = if l.rest {
                LambdaFunction::new_with_rest_arg(l.args, output, l.location)
            } else {
                LambdaFunction::new(l.args, output, l.location)
            };

            let constructed_func = ExprKind::LambdaFunction(func.into());

            // Insert the visited function at the beginning of the args
            args.insert(0, constructed_func);
            // actually_used_arguments.insert(0, constructed_func);

            Ok(ExprKind::List(List::new(args)))
            // return Ok(ExprKind::List(List::new(actually_used_arguments)));

            // unimplemented!()
        } else {
            unreachable!();
        }
    }

    fn visit_syntax_rules(&mut self, _l: Box<crate::parser::ast::SyntaxRules>) -> Self::Output {
        stop!(Generic => "unexpected syntax rules in const evaluator");
    }

    fn visit_set(&mut self, s: Box<crate::parser::ast::Set>) -> Self::Output {
        let identifier = &s.variable.atom_identifier_or_else(
            throw!(BadSyntax => "set expects an identifier"; s.location.span),
        )?;

        self.bindings.borrow_mut().unbind(identifier);

        Ok(ExprKind::Set(s))
    }

    fn visit_require(&mut self, s: crate::parser::ast::Require) -> Self::Output {
        stop!(Generic => "unexpected require - require is only allowed at the top level"; s.location.span);
    }

    // TODO come back to this
    fn visit_let(&mut self, l: Box<crate::parser::ast::Let>) -> Self::Output {
        // panic!("---------------------------Visiting let!--------------------");

        // let mut visited_bindings = Vec::new();

        // for (binding, expr) in l.bindings {
        //     visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        // }

        // l.bindings = visited_bindings;
        // l.body_expr = self.visit(l.body_expr)?;

        Ok(ExprKind::Let(l))
    }
}

// TODO: If the value is local, we need to exclude it:
// entering and exiting a scope should push and pop it off.
struct CollectSet<'a> {
    set_idents: &'a mut FxHashSet<InternedString>,
    scopes: quickscope::ScopeSet<InternedString, FxBuildHasher>,
    pub expr_level_set_idents: smallvec::SmallVec<[InternedString; 32]>,
}

impl<'a> CollectSet<'a> {
    fn new(set_idents: &'a mut FxHashSet<InternedString>) -> Self {
        Self {
            set_idents,
            scopes: quickscope::ScopeSet::default(),
            expr_level_set_idents: smallvec::SmallVec::default(),
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
                self.expr_level_set_idents.push(*identifier);

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
}
