use crate::parser::ast;
use crate::parser::ast::ExprKind;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::VisitorMut;

use std::collections::{HashMap, HashSet};

use super::ir::Expr;

// ((lambda (x y z) (+ x y z)) 10 20 30)
// =>
// (x = 10)
// (y = 20)
// (z = 30)
// (+ x (+ y z))
#[derive(Default)]
struct RenameShadowedVars {
    vars: HashSet<String>,
    name: Option<String>,
    depth: usize,
    in_scope: HashSet<String>,
    shadowed: HashMap<String, String>,
    args: Option<Vec<String>>,
    ret_val: Option<String>,
}

/// Checks that the input is in fact a lowered function
pub fn lower_function(expr: &ExprKind) -> Option<(String, Vec<String>, String, Vec<Expr>)> {
    let mut visitor = RenameShadowedVars::default();

    if let ExprKind::Define(d) = expr {
        if let ExprKind::LambdaFunction(_) = &d.body {
            // let func_name = d.name.atom_identifier_or_else(|| unreachable!()).ok()?;
            let output = visitor.visit_define(&d)?;
            if let Expr::Assign(func_name, stmt) = output {
                // return Some((func_name, visitor.args?, visitor.ret_val?, vec![*stmt]));
                return Some((func_name, visitor.args?, visitor.ret_val?, vec![*stmt]));
            }
        }
    }

    None
}

impl VisitorMut for RenameShadowedVars {
    type Output = Option<Expr>;

    // Not great, but gets the job done
    fn visit_if(&mut self, f: &ast::If) -> Self::Output {
        let test = Box::new(self.visit(&f.test_expr)?);
        let then_expr = self.visit(&f.then_expr)?;
        let else_expr = self.visit(&f.else_expr)?;

        match (&then_expr, &else_expr) {
            (Expr::Block(t), Expr::Block(e)) => Some(Expr::IfElse(test, t.clone(), e.clone())),
            (Expr::Block(t), _) => Some(Expr::IfElse(test, t.clone(), vec![else_expr])),
            (_, Expr::Block(e)) => Some(Expr::IfElse(test, vec![then_expr], e.clone())),
            (_, _) => Some(Expr::IfElse(test, vec![then_expr], vec![else_expr])),
        }
    }

    // Define -> Assignment
    fn visit_define(&mut self, define: &ast::Define) -> Self::Output {
        let body = self.visit(&define.body)?;
        let name = define
            .name
            .atom_identifier_or_else(|| unreachable!())
            .ok()?
            .to_owned();
        self.in_scope.insert(name.clone());
        Some(Expr::Assign(name, Box::new(body)))
    }

    // If done correctly, this _should_ be unreachable
    fn visit_lambda_function(&mut self, lambda_function: &ast::LambdaFunction) -> Self::Output {
        // TODO identify return variables
        // Do I need to specify the return or is the last value just there?
        let args = lambda_function
            .args
            .iter()
            .map(|x| {
                x.atom_identifier_or_else(|| unreachable!())
                    .ok()
                    .map(|x| x.to_string())
            })
            .collect::<Option<Vec<_>>>()?;

        // Make sure these are now in scope
        for arg in &args {
            self.in_scope.insert(arg.clone());
        }

        self.args = Some(args);

        // Badly attach the return value label
        self.ret_val = Some("###__return-value__###".to_string());
        let output = Some(Expr::Assign(
            "###__return-value__###".to_string(),
            Box::new(self.visit(&lambda_function.body)?),
        ));

        output
    }

    fn visit_begin(&mut self, begin: &ast::Begin) -> Self::Output {
        let body = begin
            .exprs
            .iter()
            .map(|e| self.visit(e))
            .collect::<Option<Vec<_>>>()?;
        Some(Expr::Block(body))
    }

    fn visit_return(&mut self, _r: &ast::Return) -> Self::Output {
        None
    }

    fn visit_apply(&mut self, _apply: &ast::Apply) -> Self::Output {
        None
    }

    fn visit_panic(&mut self, _p: &ast::Panic) -> Self::Output {
        None
    }

    fn visit_transduce(&mut self, _transduce: &ast::Transduce) -> Self::Output {
        None
    }

    fn visit_read(&mut self, _read: &ast::Read) -> Self::Output {
        None
    }

    fn visit_execute(&mut self, _execute: &ast::Execute) -> Self::Output {
        None
    }

    fn visit_quote(&mut self, _quote: &ast::Quote) -> Self::Output {
        None
    }

    fn visit_struct(&mut self, _s: &ast::Struct) -> Self::Output {
        None
    }

    fn visit_macro(&mut self, _m: &ast::Macro) -> Self::Output {
        None
    }

    fn visit_eval(&mut self, _e: &ast::Eval) -> Self::Output {
        None
    }

    fn visit_atom(&mut self, a: &ast::Atom) -> Self::Output {
        use TokenType::*;
        match &a.syn.ty {
            Identifier(s) => Some(Expr::Identifier(s.clone())),
            IntegerLiteral(i) => Some(Expr::Literal(i.to_string())),
            _ => None,
        }
    }

    // Check explicit function application with let
    fn visit_list(&mut self, l: &ast::List) -> Self::Output {
        // The first should be the function, and the rest should be the args
        let mut args = l.iter();

        // The corresponds to either a symbol, or a lambda function in this case
        // Anything else we don't care about
        let func = args.next()?;

        match func {
            ExprKind::LambdaFunction(lam) => {
                // These need to be mangled?
                let mut variable_names = lam
                    .args
                    .iter()
                    .map(|x| {
                        x.atom_identifier_or_else(|| unreachable!())
                            .ok()
                            .map(|x| x.to_string())
                    })
                    .collect::<Option<Vec<_>>>()?;

                // Really bad variable mangling
                // TODO fix this
                for variable in &mut variable_names {
                    if self.in_scope.contains(variable) {
                        // Put it in the shadowed
                        // self.shadowed.insert(
                        //     variable.clone(),
                        //     variable.clone() + "###__depth__" + self.depth.to_string().as_str(),
                        // );
                        variable.push_str(
                            &("###__depth__".to_string() + self.depth.to_string().as_str()),
                        );

                        // self.in_scope.insert(variable.clone());
                    } else {
                        self.in_scope.insert(variable.clone());
                    }
                }

                // These are the lowered assignments
                let mut assignments = variable_names
                    .clone()
                    .into_iter()
                    .zip(args)
                    .map(|x| -> Option<Expr> {
                        Some(Expr::Assign(x.0, Box::new(self.visit(x.1)?)))
                    })
                    .collect::<Option<Vec<_>>>()?;

                let body = self.visit(&lam.body)?;

                // They're no longer in scope, take them out
                for variable in &variable_names {
                    self.in_scope.remove(variable);
                }

                assignments.push(body);

                Some(Expr::Block(assignments))
            }
            _ => {
                let ident = func
                    .atom_identifier_or_else(|| "Not a valid function here")
                    .ok()?;
                if let Some(binop) = symbol_to_binop(ident) {
                    self.expr_list_to_bin_op(binop, args)
                } else {
                    Some(Expr::Call(
                        ident.to_owned(),
                        args.map(|x| self.visit(x)).collect::<Option<Vec<_>>>()?,
                    ))
                }
            }
        }
    }

    fn visit_syntax_rules(&mut self, _l: &ast::SyntaxRules) -> Self::Output {
        None
    }

    // This should be assignment
    // but only if the variable being assigned is scoped locally to the function
    fn visit_set(&mut self, _s: &ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, _s: &ast::Require) -> Self::Output {
        None
    }

    fn visit_callcc(&mut self, _cc: &ast::CallCC) -> Self::Output {
        None
    }
}

impl RenameShadowedVars {
    fn expr_list_to_bin_op<'a>(
        &mut self,
        binop: fn(Box<Expr>, Box<Expr>) -> Expr,
        exprs: impl DoubleEndedIterator<Item = &'a ExprKind>,
    ) -> Option<Expr> {
        let mut args_iter = exprs.into_iter();

        let left_initial = Box::new(self.visit(args_iter.next()?)?);
        let right_initial = Box::new(self.visit(args_iter.next()?)?);

        args_iter.try_fold(binop(left_initial, right_initial), |accum, next| {
            Some(binop(Box::new(accum), Box::new(self.visit(next)?)))
        })
    }
}

fn symbol_to_binop(symbol: &str) -> Option<fn(Box<Expr>, Box<Expr>) -> Expr> {
    match symbol {
        "+" => Some(Expr::Add),
        "-" => Some(Expr::Sub),
        "/" => Some(Expr::Div),
        "*" => Some(Expr::Mul),
        "=" | "equal?" | "eq?" => Some(Expr::Eq),
        "<" => Some(Expr::Lt),
        ">" => Some(Expr::Gt),
        "!=" => Some(Expr::Ne),
        ">=" => Some(Expr::Ge),
        "<=" => Some(Expr::Le),
        _ => None,
    }
}
