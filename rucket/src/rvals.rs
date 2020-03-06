use crate::env::Env;
use crate::parser::Expr;
use crate::rerrs::RucketErr;
use crate::tokens::Token::*;
// use std::any::Any;
use std::any::Any;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;
use RucketVal::*;

use std::convert::TryFrom;
use std::result;

pub trait StructFunctions {
    fn generate_bindings() -> Vec<(&'static str, RucketVal)>;
}

pub trait CustomType {
    fn box_clone(&self) -> Box<dyn CustomType>;
    fn as_any(&self) -> Box<dyn Any>;
    fn name(&self) -> String {
        (std::any::type_name::<Self>()).to_string()
    }
    fn new_rucket_val(&self) -> RucketVal;
}

impl Clone for Box<dyn CustomType> {
    fn clone(&self) -> Box<dyn CustomType> {
        self.box_clone()
    }
}

impl From<Box<dyn CustomType>> for RucketVal {
    fn from(val: Box<dyn CustomType>) -> RucketVal {
        val.new_rucket_val()
    }
}

#[macro_export]
macro_rules! implement {
    ($($type:ty),*) => {
        $(
            as_item! {
                impl crate::rvals::CustomType for $type {
                    fn box_clone(&self) -> Box<dyn CustomType> {
                        Box::new((*self).clone())
                    }
                    fn as_any(&self) -> Box<dyn Any> {
                        Box::new((*self).clone())
                    }
                    fn new_rucket_val(&self) -> RucketVal {
                        RucketVal::Custom(Box::new(self.clone()))
                    }
                }
            }
            as_item! {
                impl From<$type> for RucketVal {
                    fn from(val: $type) -> RucketVal {
                        val.new_rucket_val()
                    }
                }
            }
        ) *
    };

    ($type:ident, $($e:ident, $t: ty),*) => {
        as_item! {
            impl crate::rvals::CustomType for $type {
                fn box_clone(&self) -> Box<dyn CustomType> {
                    Box::new((*self).clone())
                }
                fn as_any(&self) -> Box<dyn Any> {
                    Box::new((*self).clone())
                }
                fn new_rucket_val(&self) -> RucketVal {
                    RucketVal::Custom(Box::new(self.clone()))
                }
            }
        }

        as_item! {
            impl From<$type> for RucketVal {
                fn from(val: $type) -> RucketVal {
                    val.new_rucket_val()
                }
            }
        }

        as_item! {
            impl From<RucketVal> for $type {
                fn from(val: RucketVal) -> $type {
                    println!("inside from rucketval to {}", stringify!($type));
                    unwrap!(val, $type).unwrap()
                }
            }
        }

        as_item! {
            impl $type {
                pub fn generate_bindings() -> Vec<(&'static str, RucketVal)> {
                    use std::convert::TryFrom;
                    use crate::rvals::RucketVal;
                    use crate::rerrs::RucketErr;
                    use crate::unwrap;
                    use crate::stop;
                    let mut vec_binding = vec![];

                    // generate predicate
                    let name = concat!(stringify!($type), "?");
                    println!("{}", name);
                    let func =
                         RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                return Ok(RucketVal::BoolV(unwrap!(first, $type).is_ok()));
                            }
                            stop!(ArityMismatch => "set! expected 2 arguments");
                        });
                    vec_binding.push((name, func));
                    $(
                        // generate setters
                        let name = concat!("set-", stringify!($type), "-", stringify!($e), "!");
                        let func =
                             RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    if let Some(second) = args_iter.next() {
                                        println!("{}", stringify!($type));
                                        let my_struct = unwrap!(first, $type)?;
                                        println!("We got after the unwrap!");
                                        println!("{:?}", my_struct);
                                        let new_struct = $type {
                                            $e : match second {
                                                RucketVal::Custom(_) => {
                                                    println!("Inside custom: {}", stringify!($t));
                                                    unwrap!(second, $t)?
                                                },
                                                _ => {
                                                    print!("Inside else: {}", second);
                                                    <$t>::try_from(second)?
                                                 }
                                            },
                                            ..my_struct
                                        };
                                        return Ok(new_struct.new_rucket_val());
                                    }
                                    stop!(ArityMismatch => "set! expected 2 arguments");
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            });
                        vec_binding.push((name, func));

                        // generate getters
                        let name = concat!(stringify!($type), "-", stringify!($e));
                        println!("{}", name);
                        let func =
                             RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    let my_struct = unwrap!(first, $type)?;
                                    return Ok(my_struct.$e.into());
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            });
                        vec_binding.push((name, func));
                    ) *
                    vec_binding
                }
            }
        }
    };
}

#[macro_export]
macro_rules! as_item {
    ($i:item) => {
        $i
    };
}

// implement!(f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);

#[macro_export]
macro_rules! unwrap {
    ($x:expr, $body:ty) => {{
        if let crate::rvals::RucketVal::Custom(v) = $x {
            let left_type = (*v).as_any();
            let left = left_type.downcast_ref::<$body>();
            left.map(|x| x.clone()).ok_or_else(|| {
                crate::rerrs::RucketErr::ConversionError(
                    "Type Mismatch: Type of RucketVal did not match the given type".to_string(),
                )
            })
        } else {
            Err(crate::rerrs::RucketErr::ConversionError(
                "Type Mismatch: Type of RucketVal did not match the given type".to_string(),
            ))
        }
    }};
}

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    Void,
    StringV(String),
    FuncV(fn(Vec<RucketVal>) -> Result<RucketVal, RucketErr>),
    LambdaV(RucketLambda),
    SymbolV(String),
    Custom(Box<dyn CustomType>),
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
                QuoteTick => Err(RucketErr::UnexpectedToken("'".to_string())),
                BooleanLiteral(x) => Ok(BoolV(x)),
                Identifier(x) => Ok(SymbolV(x)),
                NumberLiteral(x) => Ok(NumV(x)),
                StringLiteral(x) => Ok(StringV(x)),
            },
            Expr::ListVal(lst) => {
                let items: Result<Vec<Self>, Self::Error> =
                    lst.into_iter().map(Self::try_from).collect();
                Ok(ListV(items?))
            }
        }
    }
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<RucketVal> for Expr {
    type Error = &'static str;
    fn try_from(r: RucketVal) -> result::Result<Self, Self::Error> {
        match r {
            BoolV(x) => Ok(Expr::Atom(BooleanLiteral(x))),
            NumV(x) => Ok(Expr::Atom(NumberLiteral(x))),
            ListV(lst) => {
                let items: result::Result<Vec<Self>, Self::Error> =
                    lst.into_iter().map(Self::try_from).collect();
                Ok(Expr::ListVal(items?))
            }
            Void => Err("Can't convert from Void to expression!"),
            StringV(x) => Ok(Expr::Atom(StringLiteral(x))),
            FuncV(_) => Err("Can't convert from Function to expression!"),
            LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            SymbolV(x) => Ok(Expr::Atom(Identifier(x))),
            Custom(_) => Err("Can't convert from Custom Type to expression!"),
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
            (SymbolV(l), SymbolV(r)) => l == r,
            //TODO
            (_, _) => false, // (l, r) => {
                             //     let left = unwrap!(l, usize);
                             //     let right = unwrap!(r, usize);
                             //     match (left, right) {
                             //         (Ok(l), Ok(r)) => l == r,
                             //         (_, _) => false,
                             //     }
                             // }
        }
    }
}

// TODO add tests
impl PartialOrd for RucketVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (NumV(n), NumV(o)) => n.partial_cmp(o),
            (StringV(s), StringV(o)) => s.partial_cmp(o),
            _ => None, // unimplemented for other types
        }
    }
}

#[derive(Clone)]
/// struct representing data required to describe a scheme function
pub struct RucketLambda {
    /// symbols representing the arguments to the function
    params_exp: Vec<String>,
    /// body of the function with identifiers yet to be bound
    body_exp: Expr,
    /// parent environment that created this Lambda.
    /// the actual environment with correct bindingsis built at runtime
    /// once the function is called
    parent_env: Rc<RefCell<Env>>,
}
impl RucketLambda {
    pub fn new(
        params_exp: Vec<String>,
        body_exp: Expr,
        parent_env: Rc<RefCell<Env>>,
    ) -> RucketLambda {
        RucketLambda {
            params_exp,
            body_exp,
            parent_env,
        }
    }
    /// symbols representing the arguments to the function
    pub fn params_exp(&self) -> &[String] {
        &self.params_exp
    }
    /// body of the function with identifiers yet to be bound
    pub fn body_exp(&self) -> Expr {
        self.body_exp.clone()
    }
    /// parent environment that created this Lambda.
    ///
    /// The actual environment with correct bindings is built at runtime
    /// once the function is called
    pub fn parent_env(&self) -> &Rc<RefCell<Env>> {
        &self.parent_env
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
        Custom(x) => write!(f, "Custom Type: {}", x.name()),
    }
}

#[test]
fn display_test() {
    use crate::tokens::Token;
    assert_eq!(RucketVal::BoolV(false).to_string(), "#false");
    assert_eq!(RucketVal::NumV(1.0).to_string(), "1");
    assert_eq!(
        RucketVal::FuncV(|_args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
            Ok(RucketVal::ListV(vec![]))
        })
        .to_string(),
        "Function"
    );
    assert_eq!(
        RucketVal::LambdaV(RucketLambda::new(
            vec!["arg1".to_owned()],
            Expr::Atom(Token::NumberLiteral(1.0)),
            Rc::new(RefCell::new(crate::env::Env::default_env())),
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
                Rc::new(RefCell::new(crate::env::Env::default_env())),
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
