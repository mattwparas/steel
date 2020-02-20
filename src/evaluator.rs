use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::{Iterator, Peekable};
use std::result;
use std::str::Chars;

use crate::lexer::{Token, TokenError, Tokenizer};
use crate::parser::{Expr, ParseError, Parser, Result};

#[derive(PartialEq)]
pub enum LispVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<LispVal>),
    StringV(String),
}

// impl LispVal

// #[derive(Clone)]
// struct RispEnv {
//   data: HashMap<String, RispExp>,
// }

pub struct Env {
    data: HashMap<String, Expr>,
}

// def eval(x, env=global_env):
//     "Evaluate an expression in an environment."
//     while True:
//         if isa(x, Symbol):       # variable reference
//             return env.find(x)[x]
//         elif not isa(x, list):   # constant literal
//             return x
//         elif x[0] is _quote:     # (quote exp)
//             (_, exp) = x
//             return exp
//         elif x[0] is _if:        # (if test conseq alt)
//             (_, test, conseq, alt) = x
//             x = (conseq if eval(test, env) else alt)
//         elif x[0] is _set:       # (set! var exp)
//             (_, var, exp) = x
//             env.find(var)[var] = eval(exp, env)
//             return None
//         elif x[0] is _define:    # (define var exp)
//             (_, var, exp) = x
//             env[var] = eval(exp, env)
//             return None
//         elif x[0] is _lambda:    # (lambda (var*) exp)
//             (_, vars, exp) = x
//             return Procedure(vars, exp, env)
//         elif x[0] is _begin:     # (begin exp+)
//             for exp in x[1:-1]:
//                 eval(exp, env)
//             x = x[-1]
//         else:                    # (proc exp*)
//             exps = [eval(exp, env) for exp in x]
//             proc = exps.pop(0)
//             if isa(proc, Procedure):
//                 x = proc.exp
//                 env = Env(proc.parms, exps, proc.env)
//             else:
//                 return proc(*exps)

// class Procedure(object):
//     "A user-defined Scheme procedure."
//     def __init__(self, parms, exp, env):
//         self.parms, self.exp, self.env = parms, exp, env
//     def __call__(self, *args):
//         return eval(self.exp, Env(self.parms, args, self.env))

// TODO make this take references to the expression and the environment
// pub fn evaluator(mut expr: Expr, env: Env) -> Option<LispVal> {
//     loop {
//         match expr {
//             Expr::Atom(t) => match t {
//                 Token::BooleanLiteral(b) => {
//                     return Some(LispVal::BoolV(b));
//                 }
//                 Token::Identifier(s) => {
//                     return None;
//                 } // TODO
//                 Token::NumberLiteral(n) => {
//                     return Some(LispVal::NumV(n));
//                 }
//                 Token::StringLiteral(s) => {
//                     return Some(LispVal::StringV(s));
//                 }
//             },
//             Expr::ListVal(vec_tok) => {
//                 // let mut list_val_iter = vec_tok.iter();

//                 if let Some(f) = vec_tok.first() {
//                     match f {
//                         Expr::Atom(Token::Cond) => {
//                             let mut test = vec_tok.get(1)?;
//                             let mut then = vec_tok.get(2)?;
//                             let mut els = vec_tok.get(3)?;

//                             if evaluator(test.clone(), env)? == LispVal::BoolV(true) {
//                                 expr = then;
//                             } else {
//                                 expr = els;
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }

// TODO write macro to destructure vector
