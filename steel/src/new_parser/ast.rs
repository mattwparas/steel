// use crate::parser::{tokens::TokenType, Expr, SyntaxObject};
use crate::new_parser::parser::SyntaxObject;
use crate::new_parser::tokens::TokenType;
use crate::rvals::Result;

// use super::{VisitChildren, Visitor};

use super::visitors::VisitorMutResult;

use crate::new_parser::parser::ParseError;

use std::convert::TryFrom;

use itertools::Itertools;
use std::fmt;

pub trait VisitChildrenMutResult<T> {
    fn accept(&self, visitor: &mut impl VisitorMutResult<Output = T>) -> Result<T>;
}

impl<T> VisitChildrenMutResult<T> for If {
    fn accept(&self, visitor: &mut impl VisitorMutResult<Output = T>) -> Result<T> {
        visitor.visit(&self.test_expr)?;
        visitor.visit(&self.then_expr)?;
        visitor.visit(&self.else_expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Atom(Atom),
    If(Box<If>),
    Define(Box<Define>),
    DefineFunction(Box<DefineFunction>),
    LambdaFunction(Box<LambdaFunction>),
    Begin(Begin),
    Return(Box<Return>),
    Apply(Box<Apply>),
    Panic(Box<Panic>),
    Transduce(Box<Transduce>),
    Read(Box<Read>),
    Execute(Box<Execute>),
    Quote(Box<Quote>),
    Struct(Box<Struct>),
    Macro(Box<Macro>),
    Eval(Box<Eval>),
    List(List),
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!();
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Atom {
    pub syn: SyntaxObject,
}

impl Atom {
    pub fn new(syn: SyntaxObject) -> Self {
        Atom { syn }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.syn.ty.to_string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub test_expr: ExprKind,
    pub then_expr: ExprKind,
    pub else_expr: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.test_expr, self.then_expr, self.else_expr
        )
    }
}

impl If {
    pub fn new(
        test_expr: ExprKind,
        then_expr: ExprKind,
        else_expr: ExprKind,
        location: SyntaxObject,
    ) -> Self {
        If {
            test_expr,
            then_expr,
            else_expr,
            location,
        }
    }
}

// Define normal
#[derive(Clone, Debug, PartialEq)]
pub struct Define {
    // This could either be name + args
    pub name: ExprKind,
    pub body: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for Define {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(define {} {})", self.name, self.body)
    }
}

impl Define {
    pub fn new(name: ExprKind, body: ExprKind, location: SyntaxObject) -> Self {
        Define {
            name,
            body,
            location,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DefineFunction {
    pub name: ExprKind,
    pub args: Vec<ExprKind>,
    // Insert a begin implicitly?
    pub body: Vec<ExprKind>,
    pub location: SyntaxObject,
}

impl DefineFunction {
    pub fn new(
        name: ExprKind,
        args: Vec<ExprKind>,
        body: Vec<ExprKind>,
        location: SyntaxObject,
    ) -> Self {
        DefineFunction {
            name,
            args,
            body,
            location,
        }
    }
}

// Immediately parse into a begin
// otherwise its relatively
#[derive(Clone, Debug, PartialEq)]
pub struct LambdaFunction {
    pub args: Vec<ExprKind>,
    pub body: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for LambdaFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(lambda ({}) {})",
            self.args.iter().map(|x| x.to_string()).join(" "),
            self.body
        )
    }
}

impl LambdaFunction {
    pub fn new(args: Vec<ExprKind>, body: ExprKind, location: SyntaxObject) -> Self {
        LambdaFunction {
            args,
            body,
            location,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Begin {
    pub exprs: Vec<ExprKind>,
    pub location: SyntaxObject,
}

impl fmt::Display for Begin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(begin {})", self.exprs.iter().join(" "))
    }
}

impl Begin {
    pub fn new(exprs: Vec<ExprKind>, location: SyntaxObject) -> Self {
        Begin { exprs, location }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return {
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl Return {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Return { expr, location }
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(return {})", self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    pub args: Vec<ExprKind>,
}

impl List {
    pub fn new(args: Vec<ExprKind>) -> Self {
        List { args }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.args.iter().join(" "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Apply {
    pub func: ExprKind,
    pub list: ExprKind,
    pub location: SyntaxObject,
}

impl Apply {
    pub fn new(func: ExprKind, list: ExprKind, location: SyntaxObject) -> Self {
        Apply {
            func,
            list,
            location,
        }
    }
}

impl fmt::Display for Apply {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(apply {} {})", self.func, self.list)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Panic {
    pub message: ExprKind,
    pub location: SyntaxObject,
}

impl Panic {
    pub fn new(message: ExprKind, location: SyntaxObject) -> Self {
        Panic { message, location }
    }
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(panic {})", self.message)
    }
}

// transducer func initial_value iterable
#[derive(Clone, Debug, PartialEq)]
pub struct Transduce {
    pub transducer: ExprKind,
    pub func: ExprKind,
    pub initial_value: ExprKind,
    pub iterable: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for Transduce {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(transduce {} {} {} {})",
            self.transducer, self.func, self.initial_value, self.iterable
        )
    }
}

impl Transduce {
    pub fn new(
        transducer: ExprKind,
        func: ExprKind,
        initial_value: ExprKind,
        iterable: ExprKind,
        location: SyntaxObject,
    ) -> Self {
        Transduce {
            transducer,
            func,
            initial_value,
            iterable,
            location,
        }
    }
}

// impl Transduce {
//     fn accept(visitor_mut: &mut impl VisitorMut) {
//         visitor_mut.visit(expr)
//     }
// }
#[derive(Clone, Debug, PartialEq)]
pub struct Read {
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for Read {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(read {})", self.expr)
    }
}

impl Read {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Read { expr, location }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Execute {
    pub transducer: ExprKind,
    pub collection: ExprKind,
    pub output_type: Option<ExprKind>,
    pub location: SyntaxObject,
}

impl fmt::Display for Execute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(o) = &self.output_type {
            write!(f, "(execute {} {} {})", self.transducer, self.collection, o)
        } else {
            write!(f, "(execute {} {})", self.transducer, self.collection)
        }
    }
}

impl Execute {
    pub fn new(
        transducer: ExprKind,
        collection: ExprKind,
        output_type: Option<ExprKind>,
        location: SyntaxObject,
    ) -> Self {
        Execute {
            transducer,
            collection,
            output_type,
            location,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: ExprKind,
    pub fields: Vec<ExprKind>,
    pub location: SyntaxObject,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(struct {} ({})",
            self.name,
            self.fields.iter().map(|x| x.to_string()).join(" ")
        )
    }
}

impl Struct {
    pub fn new(name: ExprKind, fields: Vec<ExprKind>, location: SyntaxObject) -> Self {
        Struct {
            name,
            fields,
            location,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Quote {
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl Quote {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Quote { expr, location }
    }
}

impl fmt::Display for Quote {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(quote {})", self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Eval {
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl fmt::Display for Eval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(eval {})", self.expr)
    }
}

impl Eval {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Eval { expr, location }
    }
}

// TODO figure out how many fields a macro has
// put it into here nicely
#[derive(Clone, Debug, PartialEq)]
pub struct Macro {
    pub name: Atom,
    pub syntax_rules: SyntaxRules,
}

// TODO figure out a good mapping immediately to a macro that can be interpreted
// by the expander
#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxRules {
    pub syntax: Vec<Atom>,
    pub patterns: Vec<ExprKind>,
}

impl TryFrom<Vec<ExprKind>> for ExprKind {
    type Error = ParseError;
    fn try_from(value: Vec<ExprKind>) -> std::result::Result<Self, Self::Error> {
        if let Some(f) = value.first() {
            match f {
                ExprKind::Atom(a) => {
                    match &a.syn.ty {
                        // TokenType::CharacterLiteral(_) => {}
                        // TokenType::Comment => {}
                        // TokenType::BooleanLiteral(_) => {}
                        // TokenType::Identifier(_) => {}
                        // TokenType::NumberLiteral(_) => {}
                        // TokenType::IntegerLiteral(_) => {}
                        // TokenType::StringLiteral(_) => {}
                        TokenType::If => {
                            if value.len() != 4 {
                                return Err(ParseError::UnexpectedEOF);
                            }

                            let syn = a.syn.clone();

                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            return Ok(ExprKind::If(Box::new(If::new(
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                syn,
                            ))));
                        }
                        TokenType::Define => {
                            let length = value.len();
                            let syn = a.syn.clone();

                            if length < 3 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "Define requires at least 2 arguments, found {}",
                                        length
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            match value_iter.next().unwrap() {
                                // TODO maybe add implicit begin here
                                // maybe do it later, not sure
                                ExprKind::List(l) => {
                                    let mut args = l.args.into_iter();

                                    let name = args.next().unwrap();
                                    let args = args.collect();

                                    let body_exprs: Vec<_> = value_iter.collect();

                                    let body = if body_exprs.len() == 1 {
                                        body_exprs[0].clone()
                                    } else {
                                        ExprKind::Begin(Begin::new(
                                            body_exprs,
                                            SyntaxObject::default(TokenType::Begin),
                                        ))
                                    };

                                    let lambda =
                                        ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                            args,
                                            body,
                                            SyntaxObject::default(TokenType::Lambda),
                                        )));

                                    return Ok(ExprKind::Define(Box::new(Define::new(
                                        name, lambda, syn,
                                    ))));
                                }
                                ExprKind::Atom(a) => {
                                    return Ok(ExprKind::Define(Box::new(Define::new(
                                        ExprKind::Atom(a),
                                        value_iter.next().unwrap(),
                                        syn,
                                    ))));
                                }

                                _ => {
                                    if length < 3 {
                                        return Err(ParseError::SyntaxError(
                                            format!(
                                                "Define expects either an identifier or a list with the function name and arguments"
                                            ),
                                            syn.span,
                                        ));
                                    }
                                }
                            }
                        }
                        TokenType::Let => {
                            if value.len() < 3 {
                                return Err(ParseError::UnexpectedEOF);
                            }
                            let syn = a.syn.clone();
                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            let let_pairs = if let ExprKind::List(l) = value_iter.next().unwrap() {
                                l.args
                            } else {
                                return Err(ParseError::SyntaxError(
                                    "let expects a list of variable bindings pairs in the second position".to_string(),
                                    syn.span,
                                ));
                            };

                            let body_exprs: Vec<_> = value_iter.collect();
                            let body = if body_exprs.len() == 1 {
                                body_exprs[0].clone()
                            } else {
                                ExprKind::Begin(Begin::new(
                                    body_exprs,
                                    SyntaxObject::default(TokenType::Begin),
                                ))
                            };

                            let mut arguments = Vec::with_capacity(let_pairs.len());

                            // insert args at the end
                            // put the function in the inside
                            let mut application_args = Vec::with_capacity(let_pairs.len());

                            for pair in let_pairs {
                                if let ExprKind::List(l) = pair {
                                    let pair = l.args;

                                    if pair.len() != 2 {
                                        println!("getting in here!");
                                        return Err(ParseError::UnexpectedEOF);
                                    }

                                    if pair.len() != 2 {
                                        return Err(ParseError::SyntaxError(
                                            format!("let expected a list of variable binding pairs, found a pair with length {}",
                                            pair.len()),
                                            syn.span
                                        ));
                                    }

                                    let identifier = pair[0].clone();
                                    let application_arg = pair[1].clone();

                                    arguments.push(identifier);
                                    application_args.push(application_arg);
                                } else {
                                    return Err(ParseError::SyntaxError(
                                        "let expected a list of variable binding pairs".to_string(),
                                        syn.span,
                                    ));
                                }
                            }

                            let mut function = vec![];

                            function.push(ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                arguments,
                                body,
                                syn.clone(),
                            ))));

                            function.append(&mut application_args);

                            let return_value = ExprKind::List(List::new(function));

                            return Ok(return_value);
                        }
                        TokenType::Transduce => {
                            let syn = a.syn.clone();

                            if value.len() != 5 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "Transduce expected 4 arguments, found {}",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Transduce(Box::new(Transduce::new(
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                syn,
                            ))));
                        }
                        TokenType::Quote => {
                            let syn = a.syn.clone();

                            if value.len() != 2 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "quote expected 1 argument, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Quote(Box::new(Quote::new(
                                value_iter.next().unwrap(),
                                syn,
                            ))));
                        }
                        TokenType::Execute => {
                            let syn = a.syn.clone();

                            if value.len() < 2 || value.len() > 4 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "execute expected 3 (or possibly 4) arguments, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Execute(Box::new(Execute::new(
                                value_iter.next().unwrap(),
                                value_iter.next().unwrap(),
                                value_iter.next(),
                                syn,
                            ))));
                        }
                        TokenType::Return => {
                            let syn = a.syn.clone();

                            if value.len() != 2 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "return expected 1 argument, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Return(Box::new(Return::new(
                                value_iter.next().unwrap(),
                                syn,
                            ))));
                        }
                        TokenType::Begin => {
                            let syn = a.syn.clone();

                            if value.len() < 2 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "begin expected at least one expression, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Begin(Begin::new(value_iter.collect(), syn)));
                        }
                        TokenType::Panic => {
                            let syn = a.syn.clone();

                            if value.len() != 2 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "panic expected 1 argument, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            return Ok(ExprKind::Panic(Box::new(Panic::new(
                                value_iter.next().unwrap(),
                                syn,
                            ))));
                        }
                        TokenType::Lambda => {
                            let syn = a.syn.clone();

                            if value.len() < 3 {
                                return Err(ParseError::SyntaxError(
                                    format!(
                                        "lambda expected at least 2 arguments - the bindings list and one or more expressions, found {} instead",
                                        value.len()
                                    ),
                                    syn.span,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            let arguments = value_iter.next();

                            if let Some(ExprKind::List(l)) = arguments {
                                let args = l.args;

                                for arg in &args {
                                    if let ExprKind::Atom(_) = arg {
                                        continue;
                                    } else {
                                        return Err(ParseError::SyntaxError(
                                            "lambda function expects a list of identifiers"
                                                .to_string(),
                                            syn.span,
                                        ));
                                    }
                                }

                                let body_exprs: Vec<_> = value_iter.collect();

                                let body = if body_exprs.len() == 1 {
                                    body_exprs[0].clone()
                                } else {
                                    ExprKind::Begin(Begin::new(
                                        body_exprs,
                                        SyntaxObject::default(TokenType::Begin),
                                    ))
                                };

                                return Ok(ExprKind::LambdaFunction(Box::new(
                                    LambdaFunction::new(args, body, syn),
                                )));
                            } else {
                                return Err(ParseError::SyntaxError(
                                    "lambda function expected a list of identifiers".to_string(),
                                    syn.span,
                                ));
                            }
                        }
                        TokenType::Identifier(_) => {
                            return Ok(ExprKind::List(List::new(value)));
                            // unimplemented!("Pass through the value as list")
                        }
                        // Application not a procedure, can catch this here for constants
                        _ => {
                            return Err(ParseError::SyntaxError(
                                "illegal function application - application not a procedure"
                                    .to_string(),
                                a.syn.span.clone(),
                            ))
                        }
                    }
                }
                _ => return Ok(ExprKind::List(List::new(value))),
            }
        } else {
            return Ok(ExprKind::List(List::new(vec![])));
        }

        unimplemented!("Missing a case")
    }
}

// impl Quote {
//     pub fn new(expr: ExprKind) -> Self {
//         Quote { expr }
//     }
// }
