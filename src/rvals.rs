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
            RucketVal::ListV(lst) => {
                let lst = lst
                    .iter()
                    .map(|item| item.to_string() + " ")
                    .collect::<String>();
                write!(f, "'({})", lst.trim())
            }
            RucketVal::FuncV(_) => write!(f, "Function"),
            RucketVal::LambdaV(_) => write!(f, "Anonymous Function"),
            RucketVal::SyntaxV(expr) => write!(f, "'{}", expr.to_string()),
            RucketVal::Void => write!(f, "#<void>"),
            _ => write!(f, "display not implemented"), // RucketVal::ListV(x) => write!(f, "()")
        }
    }
}
