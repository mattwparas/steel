use crate::env::Env;
use crate::parser::Expr;
use crate::rerrs::RucketErr;
use crate::tokens::Token::*;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;
use RucketVal::*;

use std::convert::TryFrom;

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    Void,
    StringV(String),
    FuncV(fn(&[&RucketVal]) -> Result<RucketVal, RucketErr>),
    LambdaV(RucketLambda),
    SymbolV(String),
}

// sometimes you want to just
// return an expression
impl TryFrom<Expr> for RucketVal {
    type Error = RucketErr;
    fn try_from(e: Expr) -> Result<Self, Self::Error> {
        match e {
            Expr::Atom(a) => match a {
                OpenParen => Err(RucketErr::UnexpectedToken("(".to_string())),
                CloseParen => Err(RucketErr::UnexpectedToken(")".to_string())),
                If => Ok(SymbolV("if".to_string())),
                Let => Ok(SymbolV("let".to_string())),
                Define => Ok(SymbolV("define".to_string())),
                Lambda => Ok(SymbolV("lambda".to_string())),
                Quote => Ok(SymbolV("quote".to_string())),
                QuoteTick => Err(RucketErr::UnexpectedToken("'".to_string())),
                BooleanLiteral(x) => Ok(BoolV(x)),
                Identifier(x) => Ok(SymbolV(x)),
                NumberLiteral(x) => Ok(NumV(x)),
                StringLiteral(x) => Ok(StringV(x)),
            },
            Expr::ListVal(lst) => {
                let items: Result<Vec<Self>, Self::Error> =
                    lst.into_iter().map(|item| Self::try_from(item)).collect();
                Ok(ListV(items?))
            }
        }
    }
}

// TODO add tests
impl PartialEq for RucketVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BoolV(l), BoolV(r)) => l == r,
            (NumV(l), NumV(r)) => l == r,
            (StringV(l), StringV(r)) => l == r,
            (ListV(l), ListV(r)) => l == r,
            (_, _) => false,
        }
    }
}

// TODO add tests
impl PartialOrd for RucketVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (NumV(n), NumV(o)) => n.partial_cmp(o),
            (StringV(s), StringV(o)) => s.partial_cmp(o),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone)]
pub struct RucketLambda {
    params_exp: Vec<String>,
    body_exp: Expr,
    env: Rc<RefCell<Env>>,
}

impl RucketLambda {
    pub fn new(params_exp: Vec<String>, body_exp: Expr, env: Rc<RefCell<Env>>) -> RucketLambda {
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

    pub fn env(&self) -> &Rc<RefCell<Env>> {
        &self.env
    }
}

impl fmt::Display for RucketVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | ListV(_) => write!(f, "'")?,
            _ => (),
        };
        display_helper(self, f)
    }
}

/// this function recursively prints lists without prepending the `'`
/// at the beginning
fn display_helper(val: &RucketVal, f: &mut fmt::Formatter) -> fmt::Result {
    match val {
        BoolV(b) => write!(f, "#{}", b),
        NumV(x) => write!(f, "{}", x),
        StringV(s) => write!(f, "\"{}\"", s),
        FuncV(_) => write!(f, "Function"),
        LambdaV(_) => write!(f, "Lambda Function"),
        Void => write!(f, "Void"),
        SymbolV(s) => write!(f, "{}", s),
        ListV(lst) => {
            let mut iter = lst.iter();
            write!(f, "(")?;
            if let Some(last) = iter.next_back() {
                for item in iter {
                    display_helper(item, f)?;
                    write!(f, " ")?;
                }
                display_helper(last, f)?;
            }
            write!(f, ")")
        }
    }
}

#[test]
fn display_test() {
    use crate::tokens::Token;
    assert_eq!(RucketVal::BoolV(false).to_string(), "#false");
    assert_eq!(RucketVal::NumV(1.0).to_string(), "1");
    assert_eq!(
        RucketVal::FuncV(|_args: &[&RucketVal]| -> Result<RucketVal, RucketErr> {
            Ok(RucketVal::ListV(vec![]))
        })
        .to_string(),
        "Function"
    );
    assert_eq!(
        RucketVal::LambdaV(RucketLambda::new(
            vec!["arg1".to_owned()],
            Expr::Atom(Token::NumberLiteral(1.0)),
            Rc::new(RefCell::new(crate::env::default_env())),
        ))
        .to_string(),
        "Lambda Function"
    );
    assert_eq!(RucketVal::SymbolV("foo".to_string()).to_string(), "'foo");
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
                Rc::new(RefCell::new(crate::env::default_env())),
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
}
