use crate::env::EnvRef;
use crate::parser::Expr;
use crate::rerrs::RucketErr;
use std::fmt;
use RucketVal::*;

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
            BoolV(b) => write!(f, "#{}", b),
            NumV(x) => write!(f, "{}", x),
            StringV(s) => write!(f, "\"{}\"", s),
            FuncV(_) => write!(f, "Function"),
            LambdaV(_) => write!(f, "Lambda Function"),
            SyntaxV(expr) => write!(f, "'{}", expr),
            Void => write!(f, "Void"),
            lst => {
                write!(f, "'")?;
                list_display(lst, f)
            }
        }
    }
}

/// this function recursively prints lists without prepending the `'`
/// at the beginning
fn list_display(lst: &RucketVal, f: &mut fmt::Formatter) -> fmt::Result {
    match lst {
        RucketVal::ListV(lst) => {
            let mut iter = lst.iter();
            write!(f, "(")?;
            if let Some(last) = iter.next_back() {
                for item in iter {
                    list_display(item, f)?;
                    write!(f, " ")?;
                }
                list_display(last, f)?;
            }
            write!(f, ")")
        }
        RucketVal::SyntaxV(expr) => write!(f, "{}", expr),
        atom => write!(f, "{}", atom),
    }
}

#[test]
fn display_test() {
    use crate::tokens::Token;
    assert_eq!(RucketVal::BoolV(false).to_string(), "#false");
    assert_eq!(RucketVal::NumV(1.0).to_string(), "1");
    assert_eq!(
        RucketVal::FuncV(|args: &[RucketVal]| -> Result<RucketVal, RucketErr> {
            Ok(RucketVal::ListV(Vec::from(args)))
        })
        .to_string(),
        "Function"
    );
    assert_eq!(
        RucketVal::LambdaV(RucketLambda::new(
            vec!["arg1".to_owned()],
            Expr::Atom(Token::NumberLiteral(1.0)),
            crate::env::EnvRef::null(),
        ))
        .to_string(),
        "Lambda Function"
    );
    assert_eq!(
        RucketVal::SyntaxV(Expr::Atom(Token::BooleanLiteral(false))).to_string(),
        "'#false"
    );
}

#[test]
fn display_list_test() {
    use crate::tokens::Token;
    assert_eq!(ListV(vec![]).to_string(), "'()");
    assert_eq!(
        ListV(vec![
            BoolV(false),
            NumV(1.0),
            LambdaV(RucketLambda::new(
                vec!["arg1".to_owned()],
                Expr::Atom(Token::NumberLiteral(1.0)),
                crate::env::EnvRef::null(),
            ))
        ])
        .to_string(),
        "'(#false 1 Lambda Function)"
    );
    assert_eq!(
        ListV(vec![
            ListV(vec![NumV(1.0), ListV(vec!(NumV(2.0), NumV(3.0)))]),
            ListV(vec![NumV(4.0), NumV(5.0)]),
            NumV(6.0),
            ListV(vec![NumV(7.0)])
        ])
        .to_string(),
        "'((1 (2 3)) (4 5) 6 (7))"
    );
    assert_eq!(
        ListV(vec![
            NumV(1.0),
            SyntaxV(Expr::Atom(Token::BooleanLiteral(false)))
        ])
        .to_string(),
        "'(1 #false)"
    );
}
