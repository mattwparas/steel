use crate::env::EnvRef;
use crate::parser::Expr;
use crate::rerrs::RucketErr;
use std::fmt;

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    StringV(String),
    FuncV(fn(&[RucketVal]) -> Result<Expr, RucketErr>),
    LambdaV(RucketLambda),
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
            RucketVal::StringV(s) => write!(f, "{}", s),
            RucketVal::ListV(lst) => {
                let lst = lst
                    .iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>();
                write!(f, "{:?}", lst)
            }
            RucketVal::FuncV(_) => write!(f, "Function"),
            RucketVal::LambdaV(_) => write!(f, "Anonymous Function"),
            _ => write!(f, "display not implemented"), // RucketVal::ListV(x) => write!(f, "()")
        }
    }
}
