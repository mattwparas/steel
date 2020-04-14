use crate::env::Env;
use crate::parser::tokens::Token::*;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
// use std::any::Any;
use std::any::Any;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;
use SteelVal::*;

use im_rc::Vector;
use std::convert::TryFrom;
use std::result;

pub type RcRefSteelVal = Rc<RefCell<SteelVal>>;
pub fn new_rc_ref_cell(x: SteelVal) -> RcRefSteelVal {
    Rc::new(RefCell::new(x))
}

pub type Result<T> = result::Result<T, SteelErr>;
pub type FunctionSignature = fn(Vec<Rc<SteelVal>>) -> Result<Rc<SteelVal>>;

pub trait StructFunctions {
    fn generate_bindings() -> Vec<(String, SteelVal)>;
}

pub trait CustomType {
    fn box_clone(&self) -> Box<dyn CustomType>;
    fn as_any(&self) -> Box<dyn Any>;
    fn name(&self) -> String {
        (std::any::type_name::<Self>()).to_string()
    }
    fn new_steel_val(&self) -> SteelVal;
    fn display(&self) -> std::result::Result<String, std::fmt::Error>;
}

impl Clone for Box<dyn CustomType> {
    fn clone(&self) -> Box<dyn CustomType> {
        self.box_clone()
    }
}

impl From<Box<dyn CustomType>> for SteelVal {
    fn from(val: Box<dyn CustomType>) -> SteelVal {
        val.new_steel_val()
    }
}

/// Unwraps the `SteelVal::Custom` with the given type. The type must implement the `CustomType` trait.
/// If the type does not match, then
/// the macro returns a `SteelErr::ConverstionError`. If the type does match, return the
/// underlying value.
///
/// # Example
/// ```rust
///
///
///
/// ```
///
#[macro_export]
macro_rules! unwrap {
    ($x:expr, $body:ty) => {{
        if let crate::rvals::SteelVal::Custom(ref v) = $x {
            let left_type = (*v).as_any();
            let left = left_type.downcast_ref::<$body>();
            left.map(|x| x.clone()).ok_or_else(|| {
                crate::rerrs::SteelErr::ConversionError(
                    "Type Mismatch: Type of SteelVal did not match the given type".to_string(),
                )
            })
        } else {
            Err(crate::rerrs::SteelErr::ConversionError(
                "Type Mismatch: Type of SteelVal did not match the given type".to_string(),
            ))
        }
    }};
}

#[derive(Clone)]
pub enum SteelVal {
    /// Represents a boolean value
    BoolV(bool),
    /// Represents a number, currently only f64 numbers are supported
    NumV(f64),
    /// Represents a character type
    CharV(char),
    /// Represents a cons cell
    /// cons, cdr, optional parent pointer
    Pair(Rc<SteelVal>, Option<Rc<SteelVal>>),
    /// Vectors are represented as `im_rc::Vector`'s, which are immutable
    /// data structures
    VectorV(Vector<SteelVal>),
    /// Void return value
    Void,
    /// Represents strings
    StringV(String),
    /// Represents built in rust functions
    FuncV(FunctionSignature),
    /// Represents Steel Lambda functions or closures defined inside the environment
    LambdaV(SteelLambda),
    /// Represents a symbol, internally represented as `String`s
    SymbolV(String),
    /// Container for a type that implements the `Custom Type` trait. (trait object)
    Custom(Box<dyn CustomType>),
}

impl Drop for SteelVal {
    // don't want to blow the stack with destructors,
    // but also don't want to walk the whole list.
    // So walk the list until we find a non-uniquely owned item
    fn drop(&mut self) {
        let mut curr = match *self {
            Pair(_, ref mut next) => next.take(),
            _ => return,
        };
        loop {
            match curr {
                Some(r) => match Rc::try_unwrap(r) {
                    Ok(Pair(_, ref mut next)) => curr = next.take(),
                    _ => return,
                },
                _ => return,
            }
        }
    }
}

// sometimes you want to just
// return an expression
impl TryFrom<Rc<Expr>> for SteelVal {
    type Error = SteelErr;
    fn try_from(e: Rc<Expr>) -> std::result::Result<Self, Self::Error> {
        match &*e {
            Expr::Atom(a) => match a {
                OpenParen => Err(SteelErr::UnexpectedToken("(".to_string())),
                CloseParen => Err(SteelErr::UnexpectedToken(")".to_string())),
                QuoteTick => Err(SteelErr::UnexpectedToken("'".to_string())),
                BooleanLiteral(x) => Ok(BoolV(*x)),
                Identifier(x) => Ok(SymbolV(x.clone())),
                NumberLiteral(x) => Ok(NumV(*x)),
                StringLiteral(x) => Ok(StringV(x.clone())),
                CharacterLiteral(x) => Ok(CharV(*x)),
            },
            Expr::VectorVal(lst) => {
                let items: std::result::Result<Vector<Self>, Self::Error> =
                    lst.iter().map(|x| Self::try_from(x.clone())).collect();
                Ok(VectorV(items?))
            }
        }
    }
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<&SteelVal> for Rc<Expr> {
    type Error = &'static str;
    fn try_from(r: &SteelVal) -> result::Result<Self, Self::Error> {
        match r {
            BoolV(x) => Ok(Rc::new(Expr::Atom(BooleanLiteral(*x)))),
            NumV(x) => Ok(Rc::new(Expr::Atom(NumberLiteral(*x)))),
            VectorV(lst) => {
                let items: result::Result<Vec<Self>, Self::Error> =
                    lst.into_iter().map(Self::try_from).collect();
                Ok(Rc::new(Expr::VectorVal(items?)))
            }
            Void => Err("Can't convert from Void to expression!"),
            StringV(x) => Ok(Rc::new(Expr::Atom(StringLiteral(x.clone())))),
            FuncV(_) => Err("Can't convert from Function to expression!"),
            LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            SymbolV(x) => Ok(Rc::new(Expr::Atom(Identifier(x.clone())))),
            Custom(_) => Err("Can't convert from Custom Type to expression!"),
            Pair(_, _) => Err("Can't convert from pair"), // TODO
            CharV(x) => Ok(Rc::new(Expr::Atom(CharacterLiteral(*x)))),
        }
    }
}

// TODO add tests
impl PartialEq for SteelVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BoolV(l), BoolV(r)) => l == r,
            (NumV(l), NumV(r)) => l == r,
            (StringV(l), StringV(r)) => l == r,
            (VectorV(l), VectorV(r)) => l == r,
            (SymbolV(l), SymbolV(r)) => l == r,
            (CharV(l), CharV(r)) => l == r,
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
impl PartialOrd for SteelVal {
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
pub struct SteelLambda {
    /// symbols representing the arguments to the function
    params_exp: Vec<String>,
    /// body of the function with identifiers yet to be bound
    body_exp: Rc<Expr>,
    /// parent environment that created this Lambda.
    /// the actual environment with correct bindingsis built at runtime
    /// once the function is called
    parent_env: Rc<RefCell<Env>>,
}
impl SteelLambda {
    pub fn new(
        params_exp: Vec<String>,
        body_exp: Rc<Expr>,
        parent_env: Rc<RefCell<Env>>,
    ) -> SteelLambda {
        SteelLambda {
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
    pub fn body_exp(&self) -> Rc<Expr> {
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

impl fmt::Display for SteelVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | Pair(_, _) => write!(f, "'")?,
            VectorV(_) => write!(f, "'#")?,
            _ => (),
        };
        display_helper(self, f)
    }
}

impl fmt::Debug for SteelVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | Pair(_, _) => write!(f, "'")?,
            VectorV(_) => write!(f, "'#")?,
            _ => (),
        };
        display_helper(self, f)
    }
}

/// this function recursively prints lists without prepending the `'`
/// at the beginning
fn display_helper(val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
    match val {
        BoolV(b) => write!(f, "#{}", b),
        NumV(x) => write!(f, "{}", x),
        StringV(s) => write!(f, "\"{}\"", s),
        CharV(c) => write!(f, "#\\{}", c),
        FuncV(_) => write!(f, "#<function>"),
        LambdaV(_) => write!(f, "#<lambda-function>"),
        Void => write!(f, "#<void>"),
        SymbolV(s) => write!(f, "{}", s),
        VectorV(lst) => {
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
        // Pair(_, _) => {
        //     collect_pair_into_vector(mut p: &SteelVal)
        // }
        Custom(x) => write!(f, "#<{}>", x.display()?),
        // write!(f, "#<Custom-Type: {}>", x.name()),
        Pair(_, _) => {
            let v = collect_pair_into_vector(val);
            // println!("collected v");
            // write!(f, "'")?;
            display_helper(&v, f)
        }
    }
}

fn collect_pair_into_vector(mut p: &SteelVal) -> SteelVal {
    let mut lst = Vector::new();

    loop {
        if let Pair(cons, cdr) = p {
            lst.push_back((**cons).clone());
            match cdr.as_ref() {
                Some(rest) => match rest.as_ref() {
                    Pair(_, _) => p = rest,
                    _ => {
                        lst.push_back((**rest).clone());
                        return VectorV(lst);
                    }
                },
                None => {
                    return VectorV(lst);
                }
            }
        }
    }
}

/*

#[test]
fn display_test() {
    use crate::parser::tokens::Token;
    use im_rc::vector;
    assert_eq!(SteelVal::BoolV(false).to_string(), "#false");
    assert_eq!(SteelVal::NumV(1.0).to_string(), "1");
    assert_eq!(
        SteelVal::FuncV(|_args: Vec<SteelVal>| -> Result<SteelVal, SteelErr> {
            Ok(SteelVal::VectorV(vector![]))
        })
        .to_string(),
        "Function"
    );
    assert_eq!(
        SteelVal::LambdaV(SteelLambda::new(
            vec!["arg1".to_owned()],
            Rc::new(Expr::Atom(Token::NumberLiteral(1.0))),
            Rc::new(RefCell::new(crate::env::Env::default_env())),
        ))
        .to_string(),
        "Lambda Function"
    );
    assert_eq!(SteelVal::SymbolV("foo".to_string()).to_string(), "'foo");
}

#[test]
fn display_list_test() {
    use crate::parser::tokens::Token;
    use im_rc::vector;
    assert_eq!(VectorV(vector![]).to_string(), "'()");
    assert_eq!(
        VectorV(vector![
            BoolV(false),
            NumV(1.0),
            LambdaV(SteelLambda::new(
                vec!["arg1".to_owned()],
                Rc::new(Expr::Atom(Token::NumberLiteral(1.0))),
                Rc::new(RefCell::new(crate::env::Env::default_env())),
            ))
        ])
        .to_string(),
        "'(#false 1 Lambda Function)"
    );
    assert_eq!(
        VectorV(vector![
            VectorV(vector![NumV(1.0), VectorV(vector!(NumV(2.0), NumV(3.0)))]),
            VectorV(vector![NumV(4.0), NumV(5.0)]),
            NumV(6.0),
            VectorV(vector![NumV(7.0)])
        ])
        .to_string(),
        "'((1 (2 3)) (4 5) 6 (7))"
    );
}

*/
