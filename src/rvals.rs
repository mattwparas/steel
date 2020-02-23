use crate::env::EnvRef;
use crate::parser::Expr;
use crate::rerrs::RucketErr;
use std::fmt;

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    Void,
    StringV(String),
    FuncV(fn(&[RucketVal]) -> Result<RucketVal, RucketErr>),
    LambdaV(RucketLambda),
    SyntaxV(Expr),
}

#[derive(Clone)]
pub struct RucketLambda {
    params_exp: Vec<String>,
    body_exp: Expr,
    env: EnvRef,
}

impl RucketLambda {
    pub fn new(params_exp: Vec<String>, body_exp: Expr, env: EnvRef) -> RucketLambda {
        RucketLambda {
            params_exp,
            body_exp,
            env,
        }
    }

    pub fn params_exp(&self) -> &[String] {
        &self.params_exp
    }

    pub fn body_exp(&self) -> Expr {
        self.body_exp.clone()
    }

    pub fn env(&self) -> &EnvRef {
        &self.env
    }
}

impl fmt::Display for RucketVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RucketVal::BoolV(b) => write!(f, "#{}", b),
            RucketVal::NumV(x) => write!(f, "{}", x),
            RucketVal::StringV(s) => write!(f, "\"{}\"", s),
            RucketVal::FuncV(_) => write!(f, "Function"),
            RucketVal::LambdaV(_) => write!(f, "Anonymous Function"),
            RucketVal::SyntaxV(expr) => write!(f, "Expression: '{}", expr),
            RucketVal::Void => write!(f, "Void"),
            lst => {
                let lst_to_str = list_display(lst);
                write!(f, "'{}", lst_to_str)
            }
        }
    }
}

fn list_display(lst: &RucketVal) -> String {
    match lst {
        RucketVal::ListV(lst) => {
            let s = lst
                .iter()
                .map(list_display)
                .collect::<Vec<String>>()
                .join(" ");
            format!("({})", s)
        }
        atom => atom.to_string(),
    }
}
