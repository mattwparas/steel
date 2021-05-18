use crate::parser::{
    ast::{Atom, Begin, Define, LambdaFunction, List, Quote},
    span::Span,
    span_visitor::get_span,
    visitors::{ConsumingVisitor, VisitorMut},
};
use crate::parser::{
    ast::{ExprKind, If},
    parser::SyntaxObject,
    tokens::TokenType,
    tryfrom_visitor::TryFromExprKindForSteelVal,
};
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use log::debug;

type SharedEnv = Rc<RefCell<ConstantEnv>>;

struct ConstantEnv {
    bindings: HashMap<String, SteelVal>,
    non_constant_bound: HashSet<String>,
    parent: Option<Weak<RefCell<ConstantEnv>>>,
}

impl ConstantEnv {
    fn root(bindings: HashMap<String, SteelVal>) -> Self {
        Self {
            bindings,
            non_constant_bound: HashSet::new(),
            parent: None,
        }
    }

    fn new_subexpression(parent: Weak<RefCell<ConstantEnv>>) -> Self {
        Self {
            bindings: HashMap::new(),
            non_constant_bound: HashSet::new(),
            parent: Some(parent),
        }
    }

    fn bind(&mut self, ident: &str, value: SteelVal) {
        // if self.bindings.get(ident).is_some() {
        //     self.bind_non_constant(ident);
        // } else {
        //     self.bindings.insert(ident.to_owned(), value);
        // }
        self.bindings.insert(ident.to_owned(), value);
    }

    fn bind_non_constant(&mut self, ident: &str) {
        self.non_constant_bound.insert(ident.to_owned());
    }

    fn get(&self, ident: &str) -> Option<SteelVal> {
        if self.non_constant_bound.get(ident).is_some() {
            return None;
        }

        let value = self.bindings.get(ident);
        if value.is_none() {
            self.parent
                .as_ref()?
                .upgrade()
                .expect("Constant environment freed early")
                .borrow()
                .get(ident)
        } else {
            value.cloned()
        }
    }

    fn set(&mut self, ident: &str, value: SteelVal) -> Option<SteelVal> {
        let output = self.bindings.get(ident);
        if output.is_none() {
            self.parent
                .as_ref()?
                .upgrade()
                .expect("Constant environment freed early")
                .borrow_mut()
                .set(ident, value)
        } else {
            self.bindings.insert(ident.to_string(), value)
        }
    }

    fn unbind(&mut self, ident: &str) -> Option<()> {
        if self.bindings.get(ident).is_some() {
            self.bindings.remove(ident);
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
pub struct ConstantEvaluatorManager {
    global_env: SharedEnv,
    set_idents: HashSet<String>,
}

impl ConstantEvaluatorManager {
    pub fn new(constant_bindings: HashMap<String, SteelVal>) -> Self {
        Self {
            global_env: Rc::new(RefCell::new(ConstantEnv::root(constant_bindings))),
            set_idents: HashSet::new(),
        }
    }

    pub fn run(&mut self, input: Vec<ExprKind>) -> Result<Vec<ExprKind>> {
        // Collect the set expressions, ignore them for the constant folding
        for expr in &input {
            CollectSet::new(&mut self.set_idents).visit(expr);
        }

        input
            .into_iter()
            .map(|x| ConstantEvaluator::new(Rc::clone(&self.global_env), &self.set_idents).visit(x))
            .collect()
    }
}

struct ConstantEvaluator<'a> {
    bindings: SharedEnv,
    set_idents: &'a HashSet<String>,
}

fn steelval_to_atom(value: SteelVal) -> Option<TokenType> {
    match value {
        SteelVal::BoolV(b) => Some(TokenType::BooleanLiteral(b)),
        SteelVal::NumV(n) => Some(TokenType::NumberLiteral(n)),
        SteelVal::CharV(c) => Some(TokenType::CharacterLiteral(c)),
        SteelVal::IntV(i) => Some(TokenType::IntegerLiteral(i)),
        SteelVal::StringV(s) => Some(TokenType::StringLiteral(s.unwrap())),
        _ => None,
    }
}

impl<'a> ConstantEvaluator<'a> {
    fn new(bindings: Rc<RefCell<ConstantEnv>>, set_idents: &'a HashSet<String>) -> Self {
        Self {
            bindings,
            set_idents,
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
                if let Some(_) = self.set_idents.get(s) {
                    return None;
                };
                self.bindings.borrow().get(s.as_str())
            }
            TokenType::NumberLiteral(n) => Some(SteelVal::NumV(*n)),
            TokenType::StringLiteral(s) => Some(SteelVal::StringV(s.clone().into())),
            TokenType::CharacterLiteral(c) => Some(SteelVal::CharV(*c)),
            TokenType::IntegerLiteral(n) => Some(SteelVal::IntV(*n)),
            _ => None,
        }
    }

    fn all_to_constant(&self, exprs: &[ExprKind]) -> Option<Vec<SteelVal>> {
        exprs.iter().map(|x| self.to_constant(x)).collect()
    }

    fn eval_function(
        &self,
        evaluated_func: SteelVal,
        func: ExprKind,
        mut raw_args: Vec<ExprKind>,
        args: &[SteelVal],
    ) -> Result<ExprKind> {
        // if let Some(evaluated_func) = self.to_constant(&func) {
        if evaluated_func.is_function() {
            match evaluated_func {
                SteelVal::FuncV(f) => {
                    let output = f(args)?;

                    if let Some(new_token) = steelval_to_atom(output) {
                        let atom = Atom::new(SyntaxObject::new(new_token, get_span(&func)));
                        return Ok(ExprKind::Atom(atom));
                    } else {
                        debug!(
                            "Unable to convert constant-evalutable function output to value: {}",
                            evaluated_func
                        );
                        // Something went wrong
                        raw_args.insert(0, func);
                        return Ok(ExprKind::List(List::new(raw_args)));
                    }
                }
                _ => {
                    debug!(
                        "Found a non-constant evaluatable function: {}",
                        evaluated_func
                    );
                    raw_args.insert(0, func);
                    // Not a constant evaluatable function, just return the original input
                    return Ok(ExprKind::List(List::new(raw_args)));
                }
            }
        } else {
            // let message = format!(
            //     "application not a procedure, expected a function, found {}",
            //     &evaluated_func
            // );
            // stop!(BadSyntax => message; span);
            raw_args.insert(0, func);
            return Ok(ExprKind::List(List::new(raw_args)));
        }
        // }

        // else {
        //     debug!("Found a non-constant evaluatable function: {}", func);
        //     // Not a constant evaluatable function, just return the original input
        //     return Ok(func);
        // }
    }
}

impl<'a> ConsumingVisitor for ConstantEvaluator<'a> {
    type Output = Result<ExprKind>;

    fn visit_if(&mut self, f: Box<crate::parser::ast::If>) -> Self::Output {
        // Visit the test expression
        let test_expr = self.visit(f.test_expr)?;
        // If we found a constant, we can elect to only take the truthy path
        if let Some(test_expr) = self.to_constant(&test_expr) {
            if test_expr.is_truthy() {
                self.visit(f.then_expr)
            } else {
                self.visit(f.else_expr)
            }
        } else {
            Ok(ExprKind::If(
                If::new(
                    test_expr,
                    self.visit(f.then_expr)?,
                    self.visit(f.else_expr)?,
                    f.location,
                )
                .into(),
            ))
        }
    }

    fn visit_define(&mut self, define: Box<crate::parser::ast::Define>) -> Self::Output {
        let identifier = &define.name.atom_identifier_or_else(
            throw!(BadSyntax => "Define expects an identifier"; define.location.span),
        )?;

        let body = self.visit(define.body)?;

        if let Some(c) = self.to_constant(&body) {
            self.bindings.borrow_mut().bind(identifier, c);
        } else {
            self.bindings.borrow_mut().bind_non_constant(identifier);
        }

        Ok(ExprKind::Define(
            Define::new(define.name, body, define.location).into(),
        ))
    }

    fn visit_lambda_function(
        &mut self,
        mut lambda_function: Box<crate::parser::ast::LambdaFunction>,
    ) -> Self::Output {
        let parent = Rc::clone(&self.bindings);
        let mut new_env = ConstantEnv::new_subexpression(Rc::downgrade(&parent));

        for arg in &lambda_function.args {
            let identifier = arg.atom_identifier_or_else(
                throw!(BadSyntax => "lambda expects an identifier for the arguments"; lambda_function.location.span),
            )?;
            new_env.bind_non_constant(identifier);
        }

        self.bindings = Rc::new(RefCell::new(new_env));

        lambda_function.body = self.visit(lambda_function.body)?;

        self.bindings = parent;

        Ok(ExprKind::LambdaFunction(lambda_function))

        // Ok(ExprKind::LambdaFunction(
        //     LambdaFunction::new(
        //         lambda_function.args,
        //         self.visit(lambda_function.body)?,
        //         lambda_function.location,
        //     )
        //     .into(),
        // ))
    }

    fn visit_begin(&mut self, begin: crate::parser::ast::Begin) -> Self::Output {
        Ok(ExprKind::Begin(Begin::new(
            begin
                .exprs
                .into_iter()
                .map(|x| self.visit(x))
                .collect::<Result<Vec<_>>>()?,
            begin.location,
        )))
    }

    fn visit_return(&mut self, mut r: Box<crate::parser::ast::Return>) -> Self::Output {
        r.expr = self.visit(r.expr)?;
        Ok(ExprKind::Return(r))
    }

    fn visit_apply(&mut self, mut apply: Box<crate::parser::ast::Apply>) -> Self::Output {
        apply.func = self.visit(apply.func)?;
        apply.list = self.visit(apply.list)?;
        Ok(ExprKind::Apply(apply))
    }

    fn visit_panic(&mut self, mut p: Box<crate::parser::ast::Panic>) -> Self::Output {
        p.message = self.visit(p.message)?;
        Ok(ExprKind::Panic(p))
    }

    fn visit_transduce(
        &mut self,
        mut transduce: Box<crate::parser::ast::Transduce>,
    ) -> Self::Output {
        transduce.transducer = self.visit(transduce.transducer)?;
        transduce.func = self.visit(transduce.func)?;
        transduce.initial_value = self.visit(transduce.initial_value)?;
        transduce.iterable = self.visit(transduce.iterable)?;
        Ok(ExprKind::Transduce(transduce))
    }

    fn visit_read(&mut self, mut read: Box<crate::parser::ast::Read>) -> Self::Output {
        read.expr = self.visit(read.expr)?;
        Ok(ExprKind::Read(read))
    }

    fn visit_execute(&mut self, mut execute: Box<crate::parser::ast::Execute>) -> Self::Output {
        execute.transducer = self.visit(execute.transducer)?;
        execute.collection = self.visit(execute.collection)?;
        execute.output_type = execute.output_type.map(|x| self.visit(x)).transpose()?;
        Ok(ExprKind::Execute(execute))
    }

    fn visit_quote(&mut self, quote: Box<crate::parser::ast::Quote>) -> Self::Output {
        Ok(ExprKind::Quote(quote))
    }

    fn visit_struct(&mut self, s: Box<crate::parser::ast::Struct>) -> Self::Output {
        Ok(ExprKind::Struct(s))
    }

    fn visit_macro(&mut self, _m: crate::parser::ast::Macro) -> Self::Output {
        stop!(Generic => "unexpected macro found in const evaluator");
    }

    fn visit_eval(&mut self, mut e: Box<crate::parser::ast::Eval>) -> Self::Output {
        e.expr = self.visit(e.expr)?;
        Ok(ExprKind::Eval(e))
    }

    fn visit_atom(&mut self, a: crate::parser::ast::Atom) -> Self::Output {
        if let Some(inner) = self.eval_atom(&a.syn) {
            // TODO Check this part - be able to propagate quoted values
            if let Some(new_token) = steelval_to_atom(inner) {
                let atom = Atom::new(SyntaxObject::new(new_token, a.syn.span));
                return Ok(ExprKind::Atom(atom));
            }
        }
        Ok(ExprKind::Atom(a))
    }

    // Certainly the most complicated case: function application
    // Check if its a function application, and go for it
    fn visit_list(&mut self, l: crate::parser::ast::List) -> Self::Output {
        if l.args.is_empty() {
            stop!(BadSyntax => "empty function application");
        }

        if l.args.len() == 1 {
            let func_expr = l.args.clone().into_iter().next().unwrap();
            let span = get_span(&func_expr);
            let func = self.visit(func_expr)?;

            if let Some(evaluated_func) = self.to_constant(&func) {
                return self.eval_function(evaluated_func, func, Vec::new(), &[]);
            } else {
                return Ok(ExprKind::List(l));
            }
        }

        let mut args = l.args.into_iter();

        let func_expr = args.next().expect("Function missing");
        let mut args: Vec<_> = args.map(|x| self.visit(x)).collect::<Result<_>>()?;

        // Resolve the arguments - if they're all constants, we have a chance to do constant evaluation
        if let Some(arguments) = self.all_to_constant(&args) {
            if let ExprKind::Atom(_) = &func_expr {
                // let span = get_span(&func_expr);

                if let Some(evaluated_func) = self.to_constant(&func_expr) {
                    return self.eval_function(evaluated_func, func_expr, args, &arguments);
                }
                // return self.eval_function(func_expr, span, &arguments);
            }
        }

        match &func_expr {
            ExprKind::LambdaFunction(_) => {}
            _ => {
                let visited_func_expr = self.visit(func_expr)?;

                // if let ExprKind::Atom(Atom { syn }) = &visited_func_expr {
                //     if matches!(
                //         syn.ty,
                //         TokenType::CharacterLiteral(_)
                //             | TokenType::StringLiteral(_)
                //             | TokenType::IntegerLiteral(_)
                //             | TokenType::NumberLiteral(_)
                //             | TokenType::BooleanLiteral(_)
                //     ) {
                //         let message = format!(
                //             "application not a procedure, expected a function, found {}",
                //             &visited_func_expr
                //         );
                //         stop!(BadSyntax => message; get_span(&visited_func_expr));
                //     }
                // }

                args.insert(0, visited_func_expr);
                return Ok(ExprKind::List(List::new(args)));
            } // ExprKind::
        }

        // TODO if the application of the function results in a constant
        // that should also removed
        // For instance
        /*

        (or #f #f #f #f #f #f #t)

            =>

        ((λ (##z)
            ((λ (##z)
                ((λ (##z)
                    ((λ (##z)
                            ((λ (##z)
                                ((λ (##z) #true) #false))
                            #false))
                        #false))
                    #false))
                #false))
        #false)

        */
        if let ExprKind::LambdaFunction(l) = func_expr {
            // unimplemented!()

            if l.args.len() != args.len() {
                let m = format!(
                    "Anonymous function expected {} arguments, found {}",
                    l.args.len(),
                    args.len()
                );
                stop!(ArityMismatch => m; l.location.span);
            }

            let mut new_env = ConstantEnv::new_subexpression(Rc::downgrade(&self.bindings));

            for (var, arg) in l.args.iter().zip(args.iter()) {
                let identifier = var.atom_identifier_or_else(
                    throw!(BadSyntax => "lambda expects an identifier for the arguments"; l.location.span),
                )?;
                if let Some(c) = self.to_constant(arg) {
                    new_env.bind(identifier, c);
                } else {
                    new_env.bind_non_constant(identifier);
                }
            }

            let parent = Rc::clone(&self.bindings);
            self.bindings = Rc::new(RefCell::new(new_env));

            let output = self.visit(l.body)?;

            // Unwind the 'recursion'
            self.bindings = parent;

            let constructed_func =
                ExprKind::LambdaFunction(LambdaFunction::new(l.args, output, l.location).into());

            // Insert the visited function at the beginning of the args
            args.insert(0, constructed_func);

            return Ok(ExprKind::List(List::new(args)));

            // unimplemented!()
        } else {
            unreachable!();
        }
    }

    fn visit_syntax_rules(&mut self, _l: crate::parser::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax rules in const evaluator");
    }

    fn visit_set(&mut self, mut s: Box<crate::parser::ast::Set>) -> Self::Output {
        let identifier = &s.variable.atom_identifier_or_else(
            throw!(BadSyntax => "set expects an identifier"; s.location.span),
        )?;

        // s.expr = self.visit(s.expr)?;

        // if let Some(expr) = self.to_constant(&s.expr) {
        //     self.bindings.borrow_mut().set(identifier, expr);
        // } else {
        //     self.bindings.borrow_mut().unbind(identifier);
        // }

        self.bindings.borrow_mut().unbind(identifier);

        Ok(ExprKind::Set(s.into()))
    }

    fn visit_require(&mut self, _s: crate::parser::ast::Require) -> Self::Output {
        stop!(Generic => "unexpected require in const evaluator");
    }

    fn visit_callcc(&mut self, mut cc: Box<crate::parser::ast::CallCC>) -> Self::Output {
        cc.expr = self.visit(cc.expr)?;
        Ok(ExprKind::CallCC(cc))
    }
}

struct CollectSet<'a> {
    set_idents: &'a mut HashSet<String>,
}

impl<'a> CollectSet<'a> {
    fn new(set_idents: &'a mut HashSet<String>) -> Self {
        Self { set_idents }
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
        self.visit(&lambda_function.body);
    }

    fn visit_begin(&mut self, begin: &Begin) -> Self::Output {
        for expr in &begin.exprs {
            self.visit(&expr);
        }
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr);
    }

    fn visit_apply(&mut self, apply: &crate::parser::ast::Apply) -> Self::Output {
        self.visit(&apply.func);
        self.visit(&apply.list);
    }

    fn visit_panic(&mut self, p: &crate::parser::ast::Panic) -> Self::Output {
        self.visit(&p.message);
    }

    fn visit_transduce(&mut self, transduce: &crate::parser::ast::Transduce) -> Self::Output {
        self.visit(&transduce.transducer);
        self.visit(&transduce.func);
        self.visit(&transduce.initial_value);
        self.visit(&transduce.iterable);
    }

    fn visit_read(&mut self, read: &crate::parser::ast::Read) -> Self::Output {
        self.visit(&read.expr);
    }

    fn visit_execute(&mut self, execute: &crate::parser::ast::Execute) -> Self::Output {
        self.visit(&execute.transducer);
        self.visit(&execute.collection);
        execute.output_type.as_ref().map(|x| self.visit(x));
    }

    fn visit_quote(&mut self, _quote: &Quote) -> Self::Output {}

    fn visit_struct(&mut self, _s: &crate::parser::ast::Struct) -> Self::Output {}

    fn visit_macro(&mut self, _m: &crate::parser::ast::Macro) -> Self::Output {}

    fn visit_eval(&mut self, e: &crate::parser::ast::Eval) -> Self::Output {
        self.visit(&e.expr);
    }

    fn visit_atom(&mut self, _a: &Atom) -> Self::Output {}

    fn visit_list(&mut self, l: &List) -> Self::Output {
        for expr in &l.args {
            self.visit(expr);
        }
    }

    fn visit_syntax_rules(&mut self, _l: &crate::parser::ast::SyntaxRules) -> Self::Output {}

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        if let Ok(identifier) = &s.variable.atom_identifier_or_else(
            throw!(BadSyntax => "set expects an identifier"; s.location.span),
        ) {
            self.set_idents.insert(identifier.to_string());
        }
    }

    fn visit_require(&mut self, _s: &crate::parser::ast::Require) -> Self::Output {}

    fn visit_callcc(&mut self, cc: &crate::parser::ast::CallCC) -> Self::Output {
        self.visit(&cc.expr);
    }
}
