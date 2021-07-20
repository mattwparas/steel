use crate::parser::parser::ParseError;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::tokens::TokenType::*;

use std::convert::TryFrom;

use itertools::Itertools;
use pretty::RcDoc;
use std::fmt;
use std::ops::Deref;

use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;

use crate::parser::tryfrom_visitor::TryFromExprKindForSteelVal;

use crate::rvals::collect_pair_into_vector;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Atom(Atom),
    If(Box<If>),
    Define(Box<Define>),
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
    Macro(Macro),
    SyntaxRules(SyntaxRules),
    Eval(Box<Eval>),
    List(List),
    Set(Box<Set>),
    Require(Require),
    CallCC(Box<CallCC>),
}

impl ExprKind {
    pub fn atom_identifier_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&str, E> {
        match self {
            Self::Atom(Atom {
                syn: SyntaxObject { ty: t, .. },
            }) => match t {
                TokenType::Identifier(s) => Ok(s),
                _ => Err(err()),
            },
            _ => Err(err()),
        }
    }

    pub fn list_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&List, E> {
        match self {
            Self::List(l) => Ok(l),
            _ => Err(err()),
        }
    }
}

impl TryFrom<ExprKind> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: ExprKind) -> std::result::Result<Self, Self::Error> {
        TryFromExprKindForSteelVal::try_from_expr_kind(e)
    }
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<&SteelVal> for ExprKind {
    type Error = &'static str;
    fn try_from(r: &SteelVal) -> std::result::Result<Self, Self::Error> {
        match r {
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                BooleanLiteral(*x),
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                NumberLiteral(*x),
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                IntegerLiteral(*x),
            )))),
            VectorV(lst) => {
                let items: std::result::Result<Vec<Self>, Self::Error> =
                    lst.iter().map(|x| Self::try_from(x)).collect();
                Ok(ExprKind::List(List::new(items?)))
            }
            Void => Err("Can't convert from Void to expression!"),
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                StringLiteral(x.unwrap()),
            )))),
            FuncV(_) => Err("Can't convert from Function to expression!"),
            // LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            // MacroV(_) => Err("Can't convert from Macro to expression!"),
            SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier(x.unwrap()),
            )))),
            Custom(_) => Err("Can't convert from Custom Type to expression!"),
            // Pair(_, _) => Err("Can't convert from pair"), // TODO
            Pair(_) => {
                if let VectorV(ref lst) = collect_pair_into_vector(r) {
                    let items: std::result::Result<Vec<Self>, Self::Error> =
                        lst.iter().map(|x| Self::try_from(x)).collect();
                    Ok(ExprKind::List(List::new(items?)))
                } else {
                    Err("Couldn't convert from list to expression")
                }
            }
            CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                CharacterLiteral(*x),
            )))),
            StructV(_) => Err("Can't convert from Struct to expression!"),
            // StructClosureV(_) => Err("Can't convert from struct-function to expression!"),
            PortV(_) => Err("Can't convert from port to expression!"),
            Closure(_) => Err("Can't convert from bytecode closure to expression"),
            HashMapV(_) => Err("Can't convert from hashmap to expression!"),
            HashSetV(_) => Err("Can't convert from hashset to expression!"),
            IterV(_) => Err("Can't convert from iterator to expression!"),
            FutureFunc(_) => Err("Can't convert from future function to expression!"),
            FutureV(_) => Err("Can't convert future to expression!"),
            // Promise(_) => Err("Can't convert from promise to expression!"),
            StreamV(_) => Err("Can't convert from stream to expression!"),
            BoxV(_) => Err("Can't convert from box to expression!"),
            Contract(_) => Err("Can't convert from contract to expression!"),
            ContractedFunction(_) => Err("Can't convert from contracted function to expression!"),
            BoxedFunction(_) => Err("Can't convert from boxed function to expression!"),
            ContinuationFunction(_) => Err("Can't convert from continuation to expression!"),
            CompiledFunction(_) => Err("Can't convert from function to expression!"),
        }
    }
}

pub trait ToDoc {
    fn to_doc(&self) -> RcDoc<()>;
}

impl ToDoc for ExprKind {
    fn to_doc(&self) -> RcDoc<()> {
        // unimplemented!()
        match self {
            ExprKind::Atom(a) => a.to_doc(),
            ExprKind::If(i) => i.to_doc(),
            ExprKind::Define(d) => d.to_doc(),
            ExprKind::LambdaFunction(l) => l.to_doc(),
            ExprKind::Begin(b) => b.to_doc(),
            ExprKind::Return(r) => r.to_doc(),
            ExprKind::Apply(a) => a.to_doc(),
            ExprKind::Panic(p) => p.to_doc(),
            ExprKind::Transduce(t) => t.to_doc(),
            ExprKind::Read(r) => r.to_doc(),
            ExprKind::Execute(e) => e.to_doc(),
            ExprKind::Quote(q) => q.to_doc(),
            ExprKind::Struct(s) => s.to_doc(),
            ExprKind::Macro(m) => m.to_doc(),
            ExprKind::SyntaxRules(s) => s.to_doc(),
            ExprKind::Eval(e) => e.to_doc(),
            ExprKind::List(l) => l.to_doc(),
            ExprKind::Set(s) => s.to_doc(),
            ExprKind::Require(r) => r.to_doc(),
            ExprKind::CallCC(c) => c.to_doc(),
        }
    }
}

impl ExprKind {
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprKind::Atom(a) => write!(f, "{}", a),
            ExprKind::If(i) => write!(f, "{}", i),
            ExprKind::Define(d) => write!(f, "{}", d),
            ExprKind::LambdaFunction(l) => write!(f, "{}", l),
            ExprKind::Begin(b) => write!(f, "{}", b),
            ExprKind::Return(r) => write!(f, "{}", r),
            ExprKind::Apply(a) => write!(f, "{}", a),
            ExprKind::Panic(p) => write!(f, "{}", p),
            ExprKind::Transduce(t) => write!(f, "{}", t),
            ExprKind::Read(r) => write!(f, "{}", r),
            ExprKind::Execute(e) => write!(f, "{}", e),
            ExprKind::Quote(q) => write!(f, "{}", q),
            ExprKind::Struct(s) => write!(f, "{}", s),
            ExprKind::Macro(m) => write!(f, "{}", m),
            ExprKind::SyntaxRules(s) => write!(f, "{}", s),
            ExprKind::Eval(e) => write!(f, "{}", e),
            ExprKind::List(l) => write!(f, "{}", l),
            ExprKind::Set(s) => write!(f, "{}", s),
            ExprKind::Require(r) => write!(f, "{}", r),
            ExprKind::CallCC(cc) => write!(f, "{}", cc),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallCC {
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl CallCC {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        CallCC { expr, location }
    }
}

impl fmt::Display for CallCC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(call/cc {})", self.expr)
    }
}

// impl SExp {
//     /// Return a pretty printed format of self.
//     pub fn to_doc(&self) -> RcDoc<()> {
//         match *self {
//             Atom(ref x) => RcDoc::as_string(x),
//             List(ref xs) =>
//                 RcDoc::text("(")
//                     .append(RcDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
//                     .append(RcDoc::text(")"))
//         }
//     }
// }

impl ToDoc for CallCC {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(call/cc")
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
    }
}

impl From<CallCC> for ExprKind {
    fn from(val: CallCC) -> Self {
        ExprKind::CallCC(Box::new(val))
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

impl ToDoc for Atom {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.syn.ty.to_string())
    }
}

impl From<Atom> for ExprKind {
    fn from(val: Atom) -> Self {
        ExprKind::Atom(val)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Set {
    pub variable: ExprKind,
    pub expr: ExprKind,
    pub location: SyntaxObject,
}

impl Set {
    pub fn new(variable: ExprKind, expr: ExprKind, location: SyntaxObject) -> Self {
        Set {
            variable,
            expr,
            location,
        }
    }
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(set! {} {})", self.variable, self.expr)
    }
}

impl ToDoc for Set {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(set!")
            .append(RcDoc::line())
            .append(self.variable.to_doc())
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
    }
}

impl From<Set> for ExprKind {
    fn from(val: Set) -> Self {
        ExprKind::Set(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub test_expr: ExprKind,
    pub then_expr: ExprKind,
    pub else_expr: ExprKind,
    pub location: SyntaxObject,
}

impl ToDoc for If {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(if")
            .append(RcDoc::space())
            .append(self.test_expr.to_doc())
            .append(RcDoc::line())
            .append(self.then_expr.to_doc())
            .append(RcDoc::line())
            .append(self.else_expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(if {} {} {})",
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

impl From<If> for ExprKind {
    fn from(val: If) -> Self {
        ExprKind::If(Box::new(val))
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

impl ToDoc for Define {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(define")
            .append(RcDoc::space())
            .append(self.name.to_doc())
            .append(RcDoc::line())
            .append(self.body.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
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

impl From<Define> for ExprKind {
    fn from(val: Define) -> Self {
        ExprKind::Define(Box::new(val))
    }
}

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

impl ToDoc for LambdaFunction {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(λ")
            .append(RcDoc::space())
            .append(RcDoc::text("("))
            .append(
                RcDoc::intersperse(self.args.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(2)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .append(RcDoc::line())
            .append(self.body.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
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

impl From<LambdaFunction> for ExprKind {
    fn from(val: LambdaFunction) -> Self {
        ExprKind::LambdaFunction(Box::new(val))
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

impl ToDoc for Begin {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(begin")
            .append(RcDoc::line())
            .append(
                RcDoc::intersperse(self.exprs.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(1)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .nest(1)
            .group()
    }
}

impl Begin {
    pub fn new(exprs: Vec<ExprKind>, location: SyntaxObject) -> Self {
        Begin { exprs, location }
    }
}

impl From<Begin> for ExprKind {
    fn from(val: Begin) -> Self {
        ExprKind::Begin(val)
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

impl ToDoc for Return {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(return")
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(return! {})", self.expr)
    }
}

impl From<Return> for ExprKind {
    fn from(val: Return) -> Self {
        ExprKind::Return(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Require {
    pub modules: Vec<Atom>,
    pub location: SyntaxObject,
}

impl Require {
    pub fn new(modules: Vec<Atom>, location: SyntaxObject) -> Self {
        Require { modules, location }
    }
}

impl ToDoc for Require {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(require")
            .append(RcDoc::line())
            .append(
                RcDoc::intersperse(self.modules.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(2)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl fmt::Display for Require {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(require {})", self.modules.iter().join(" "))
    }
}

impl From<Require> for ExprKind {
    fn from(val: Require) -> Self {
        ExprKind::Require(val)
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

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn first_ident(&self) -> Option<&str> {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                },
        })) = self.args.first()
        {
            Some(s)
        } else {
            None
        }
    }
}

impl ToDoc for List {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(")
            .append(
                RcDoc::intersperse(self.args.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(1)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.args.iter().join(" "))
    }
}

impl From<List> for ExprKind {
    fn from(val: List) -> Self {
        ExprKind::List(val)
    }
}

impl Deref for List {
    type Target = [ExprKind];

    fn deref(&self) -> &[ExprKind] {
        &self.args
    }
}

// and we'll implement IntoIterator
impl IntoIterator for List {
    type Item = ExprKind;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
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

impl ToDoc for Apply {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(apply")
            .append(RcDoc::line())
            .append(self.func.to_doc())
            .append(RcDoc::line())
            .append(self.list.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl From<Apply> for ExprKind {
    fn from(val: Apply) -> Self {
        ExprKind::Apply(Box::new(val))
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

impl ToDoc for Panic {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(panic")
            .append(RcDoc::line())
            .append(self.message.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(panic! {})", self.message)
    }
}

impl From<Panic> for ExprKind {
    fn from(val: Panic) -> Self {
        ExprKind::Panic(Box::new(val))
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

impl ToDoc for Transduce {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(transduce")
            .append(RcDoc::space())
            .append(self.transducer.to_doc())
            .append(RcDoc::line())
            .append(self.func.to_doc())
            .append(RcDoc::line())
            .append(self.initial_value.to_doc())
            .append(RcDoc::line())
            .append(self.iterable.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
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

impl From<Transduce> for ExprKind {
    fn from(val: Transduce) -> Self {
        ExprKind::Transduce(Box::new(val))
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

impl ToDoc for Read {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(read")
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl Read {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Read { expr, location }
    }
}

impl From<Read> for ExprKind {
    fn from(val: Read) -> Self {
        ExprKind::Read(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Execute {
    pub transducer: ExprKind,
    pub collection: ExprKind,
    pub output_type: Option<ExprKind>,
    pub location: SyntaxObject,
}

impl ToDoc for Execute {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = RcDoc::text("(execute")
            .append(RcDoc::space())
            .append(self.transducer.to_doc())
            .append(RcDoc::line())
            .append(self.collection.to_doc());

        let doc = if let Some(output_type) = &self.output_type {
            doc.append(RcDoc::line()).append(output_type.to_doc())
        } else {
            doc
        };

        doc.append(RcDoc::text(")")).nest(2).group()
    }
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

impl From<Execute> for ExprKind {
    fn from(val: Execute) -> Self {
        ExprKind::Execute(Box::new(val))
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
            "(struct {} ({}))",
            self.name,
            self.fields.iter().map(|x| x.to_string()).join(" ")
        )
    }
}

impl ToDoc for Struct {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(struct")
            .append(RcDoc::space())
            .append(self.name.to_doc())
            .append(RcDoc::line())
            .append(RcDoc::text("("))
            .append(
                RcDoc::intersperse(self.fields.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(2)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .append(RcDoc::text(")"))
            .nest(2)
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

impl From<Struct> for ExprKind {
    fn from(val: Struct) -> Self {
        ExprKind::Struct(Box::new(val))
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

impl ToDoc for Quote {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(quote")
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl fmt::Display for Quote {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(quote {})", self.expr)
    }
}

impl From<Quote> for ExprKind {
    fn from(val: Quote) -> Self {
        ExprKind::Quote(Box::new(val))
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

impl ToDoc for Eval {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(eval")
            .append(RcDoc::line())
            .append(self.expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl Eval {
    pub fn new(expr: ExprKind, location: SyntaxObject) -> Self {
        Eval { expr, location }
    }
}

impl From<Eval> for ExprKind {
    fn from(val: Eval) -> Self {
        ExprKind::Eval(Box::new(val))
    }
}

// TODO figure out how many fields a macro has
// put it into here nicely
#[derive(Clone, Debug, PartialEq)]
pub struct Macro {
    pub name: Box<ExprKind>,
    pub syntax_rules: SyntaxRules,
    pub location: SyntaxObject,
}

impl fmt::Display for Macro {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(define-syntax {} {})", self.name, self.syntax_rules)
    }
}

impl ToDoc for Macro {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(define-syntax")
            .append(RcDoc::line())
            .append(self.name.to_doc())
            .append(RcDoc::line())
            .append(self.syntax_rules.to_doc())
            .append(RcDoc::text(")"))
            .nest(1)
            .group()
    }
}

impl Macro {
    pub fn new(name: ExprKind, syntax_rules: SyntaxRules, location: SyntaxObject) -> Self {
        Macro {
            name: Box::new(name),
            syntax_rules,
            location,
        }
    }
}

impl From<Macro> for ExprKind {
    fn from(val: Macro) -> Self {
        ExprKind::Macro(val)
    }
}

// TODO figure out a good mapping immediately to a macro that can be interpreted
// by the expander
#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxRules {
    pub syntax: Vec<ExprKind>,
    pub patterns: Vec<PatternPair>,
    pub location: SyntaxObject,
}

impl SyntaxRules {
    pub fn new(syntax: Vec<ExprKind>, patterns: Vec<PatternPair>, location: SyntaxObject) -> Self {
        SyntaxRules {
            syntax,
            patterns,
            location,
        }
    }
}

impl fmt::Display for SyntaxRules {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(syntax-rules ({}) {})",
            self.syntax.iter().map(|x| x.to_string()).join(" "),
            self.patterns.iter().map(|x| x.to_string()).join("\n")
        )
    }
}

impl ToDoc for SyntaxRules {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(syntax-rules")
            .append(RcDoc::line())
            .append(RcDoc::text("("))
            .append(
                RcDoc::intersperse(self.syntax.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(1)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .append(RcDoc::line())
            .append(
                RcDoc::intersperse(self.patterns.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(2)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl From<SyntaxRules> for ExprKind {
    fn from(val: SyntaxRules) -> Self {
        ExprKind::SyntaxRules(val)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternPair {
    pub pattern: ExprKind,
    pub body: ExprKind,
}

impl PatternPair {
    pub fn new(pattern: ExprKind, body: ExprKind) -> Self {
        PatternPair { pattern, body }
    }
}

impl ToDoc for PatternPair {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("[")
            .append(self.pattern.to_doc())
            .append(RcDoc::line())
            .append(self.body.to_doc())
            .append(RcDoc::text("]"))
            .nest(1)
            .group()
    }
}

impl fmt::Display for PatternPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}\n{}]", self.pattern, self.body)
    }
}

#[inline]
fn parse_if<I>(mut value_iter: I, syn: SyntaxObject) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    // let mut value_iter = value.into_iter();
    value_iter.next();

    let ret_value = If::new(
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "if expects a test condition, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "if expects a then condition, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "if expects an else condition, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        syn.clone(),
    )
    .into();

    if value_iter.next().is_some() {
        Err(ParseError::SyntaxError(
            "if takes only 3 expressions".to_string(),
            syn.span,
            None,
        ))
    } else {
        Ok(ret_value)
    }
}

#[inline]
fn parse_define<I>(
    mut value_iter: I,
    syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    match value_iter.next().ok_or_else(|| {
        ParseError::SyntaxError(
            "define expects an identifier, found none".to_string(),
            syn.span,
            None,
        )
    })? {
        // TODO maybe add implicit begin here
        // maybe do it later, not sure
        ExprKind::List(l) => {
            let name_ref = l.args.first().ok_or_else(|| {
                ParseError::SyntaxError(
                    "define expected a function name, found none".to_string(),
                    syn.span,
                    None,
                )
            })?;

            if let ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(datum_syntax),
                        ..
                    },
            }) = name_ref
            {
                if datum_syntax == "datum->syntax" {
                    return Ok(ExprKind::Define(Box::new(Define::new(
                        ExprKind::List(List::new(l.args)),
                        {
                            let v = value_iter.next().ok_or_else(|| {
                                ParseError::SyntaxError(
                                    "define statement expected a body, found none".to_string(),
                                    syn.span,
                                    None,
                                )
                            })?;
                            if value_iter.next().is_some() {
                                return Err(ParseError::SyntaxError(
                                    "Define expected only one expression after the identifier"
                                        .to_string(),
                                    syn.span,
                                    None,
                                ));
                            }
                            v
                        },
                        syn,
                    ))));
                }
            }

            let mut args = l.args.into_iter();

            let name = args.next().ok_or_else(|| {
                ParseError::SyntaxError(
                    "define expected a function name, found none".to_string(),
                    syn.span,
                    None,
                )
            })?;

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

            let lambda = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                args,
                body,
                SyntaxObject::new(TokenType::Lambda, syn.span),
            )));

            Ok(ExprKind::Define(Box::new(Define::new(name, lambda, syn))))
        }
        ExprKind::Atom(a) => Ok(ExprKind::Define(Box::new(Define::new(
            ExprKind::Atom(a),
            {
                let v = value_iter.next().ok_or_else(|| {
                    ParseError::SyntaxError(
                        "define statement expected a body, found none".to_string(),
                        syn.span,
                        None,
                    )
                })?;
                if value_iter.next().is_some() {
                    return Err(ParseError::SyntaxError(
                        "Define expected only one expression after the identifier".to_string(),
                        syn.span,
                        None,
                    ));
                }
                v
            },
            syn,
        )))),

        _ => Err(ParseError::SyntaxError(
            "Define expects either an identifier or a list with the function name and arguments"
                .to_string(),
            syn.span,
            None,
        )),
    }
}

#[inline]
fn parse_let<I>(mut value_iter: I, syn: SyntaxObject) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    let let_pairs = if let ExprKind::List(l) = value_iter.next().ok_or_else(|| {
        ParseError::SyntaxError(
            "let expected a list of variable bindings pairs in the second position, found none"
                .to_string(),
            syn.span,
            None,
        )
    })? {
        l.args
    } else {
        return Err(ParseError::SyntaxError(
            "let expects a list of variable bindings pairs in the second position".to_string(),
            syn.span,
            None,
        ));
    };

    let body_exprs: Vec<_> = value_iter.collect();

    if body_exprs.is_empty() {
        return Err(ParseError::SyntaxError(
            "let expects an expression, found none".to_string(),
            syn.span,
            None,
        ));
    }

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
                return Err(ParseError::SyntaxError(
                    format!("let expected a list of variable binding pairs, found a pair with length {}",
                    pair.len()),
                    syn.span, None
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
                None,
            ));
        }
    }

    let mut function: Vec<ExprKind> = vec![LambdaFunction::new(arguments, body, syn).into()];

    function.append(&mut application_args);

    Ok(ExprKind::List(List::new(function)))
}

#[inline]
fn parse_transduce<I>(
    mut value_iter: I,
    syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    let t = Transduce::new(
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "transducer expected a transducer, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "transducer expected a function, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "transducer expected a initial value, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        value_iter.next().ok_or_else(|| {
            ParseError::SyntaxError(
                "transducer expected an iterable, found none".to_string(),
                syn.span,
                None,
            )
        })?,
        syn.clone(),
    );

    if value_iter.next().is_some() {
        Err(ParseError::ArityMismatch(
            "Transduce expected 4 arguments".to_string(),
            syn.span,
            None,
        ))
    } else {
        Ok(t.into())
    }
}

#[inline]
fn parse_execute<I>(
    mut value_iter: I,
    syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    let transducer = value_iter.next().ok_or_else(|| ParseError::ArityMismatch("execute expects 2 (or possibly 3) arguments, and a transducer in the first position, found none".to_string(), syn.span, None))?;

    let collection = value_iter.next().ok_or_else(|| ParseError::ArityMismatch("execute expects 2 (or possibly 3) arguments, and a collection in the second position, found none".to_string(), syn.span, None))?;

    let output_type = value_iter.next();

    if value_iter.next().is_some() {
        Err(ParseError::SyntaxError(
            "execute takes at most 3 arguments".to_string(),
            syn.span,
            None,
        ))
    } else {
        Ok(Execute::new(transducer, collection, output_type, syn).into())
    }
}

#[inline]
fn parse_single_argument<I>(
    mut value_iter: I,
    syn: SyntaxObject,
    name: &'static str,
    constructor: fn(ExprKind, SyntaxObject) -> ExprKind,
) -> Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    let func = value_iter.next().ok_or_else(|| {
        ParseError::ArityMismatch(
            format!("{} expected one argument, found none", name),
            syn.span,
            None,
        )
    })?;

    if value_iter.next().is_some() {
        Err(ParseError::SyntaxError(
            format!("{} expects only one argument", name),
            syn.span,
            None,
        ))
    } else {
        Ok(constructor(func, syn))
    }
}

impl TryFrom<Vec<ExprKind>> for ExprKind {
    type Error = ParseError;
    fn try_from(value: Vec<ExprKind>) -> std::result::Result<Self, Self::Error> {
        // let mut value = value.into_iter().peekable();
        if let Some(f) = value.first().map(|x| x.clone()) {
            match f {
                ExprKind::Atom(a) => {
                    // let value = value.into_iter();
                    match &a.syn.ty {
                        TokenType::If => parse_if(value.into_iter(), a.syn.clone()),
                        TokenType::Define => parse_define(value.into_iter(), a.syn.clone()),
                        TokenType::Let => parse_let(value.into_iter(), a.syn.clone()),
                        TokenType::Transduce => parse_transduce(value.into_iter(), a.syn.clone()),
                        TokenType::Quote => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "quote",
                            |expr, syn| Quote::new(expr, syn).into(),
                        ),
                        TokenType::Execute => parse_execute(value.into_iter(), a.syn.clone()),
                        TokenType::Return => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "return",
                            |expr, syn| Return::new(expr, syn).into(),
                        ),
                        TokenType::CallCC => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "call/cc",
                            |expr, syn| CallCC::new(expr, syn).into(),
                        ),
                        TokenType::Require => {
                            let syn = a.syn.clone();
                            if value.len() < 2 {
                                return Err(ParseError::ArityMismatch(
                                    "require expects at least one identifier or string".to_string(),
                                    syn.span,
                                    a.syn.source,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            let expressions = value_iter
                                .map(|x| {
                                    if let ExprKind::Atom(a) = x {
                                        Ok(a)
                                    } else {
                                        Err(ParseError::SyntaxError(
                                            "require expects atoms".to_string(),
                                            syn.span,
                                            a.syn.source.clone(),
                                        ))
                                    }
                                })
                                .collect::<Result<Vec<_>, Self::Error>>()?;

                            Ok(ExprKind::Require(Require::new(expressions, syn)))
                        }
                        TokenType::Eval => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "eval",
                            |expr, syn| Eval::new(expr, syn).into(),
                        ),
                        TokenType::Read => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "read",
                            |expr, syn| Read::new(expr, syn).into(),
                        ),
                        TokenType::Set => {
                            let syn = a.syn.clone();
                            if value.len() != 3 {
                                return Err(ParseError::ArityMismatch(
                                    "set! expects an identifier and an expression".to_string(),
                                    syn.span,
                                    None,
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            let identifier = value_iter.next().unwrap();
                            let expression = value_iter.next().unwrap();

                            Ok(ExprKind::Set(Box::new(Set::new(
                                identifier, expression, syn,
                            ))))
                        }
                        TokenType::Apply => {
                            let syn = a.syn.clone();
                            if value.len() != 3 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "apply expects a symbol (for a function) and a list of fields, found {} arguments instead",value.len()
                                    ), syn.span, None
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            let function = value_iter.next().unwrap();
                            let list = value_iter.next().unwrap();

                            Ok(ExprKind::Apply(Box::new(Apply::new(function, list, syn))))
                        }
                        TokenType::Struct => {
                            let syn = a.syn.clone();

                            if value.len() != 3 {
                                return Err(ParseError::ArityMismatch(
                                    format!(
                                        "struct expects a name and a list of fields, found {} arguments instead", value.len()
                                    ), syn.span, None
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            let name = value_iter.next().unwrap();
                            let args = value_iter.next().unwrap();

                            if let ExprKind::List(l) = args {
                                Ok(ExprKind::Struct(Box::new(Struct::new(name, l.args, syn))))
                            } else {
                                Err(ParseError::SyntaxError(
                                    "struct expected a list of field names".to_string(),
                                    syn.span,
                                    None,
                                ))
                            }
                        }
                        TokenType::Begin => {
                            let syn = a.syn.clone();
                            let mut value_iter = value.into_iter();
                            value_iter.next();
                            Ok(ExprKind::Begin(Begin::new(value_iter.collect(), syn)))
                        }
                        TokenType::Panic => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "panic!",
                            |expr, syn| Panic::new(expr, syn).into(),
                        ),
                        TokenType::Lambda => {
                            let syn = a.syn.clone();

                            if value.len() < 3 {
                                return Err(ParseError::SyntaxError(
                                    format!(
                                        "lambda expected at least 2 arguments - the bindings list and one or more expressions, found {} instead",
                                        value.len()
                                    ),
                                    syn.span, None
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
                                            None,
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

                                Ok(ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
                                    args, body, syn,
                                ))))
                            } else {
                                Err(ParseError::SyntaxError(
                                    "lambda function expected a list of identifiers".to_string(),
                                    syn.span,
                                    None,
                                ))
                            }
                        }
                        TokenType::DefineSyntax => {
                            let syn = a.syn.clone();

                            if value.len() < 3 {
                                return Err(ParseError::SyntaxError(
                                    format!("define-syntax expects 2 arguments - the name of the macro and the syntax-rules, found {}", value.len()), syn.span, None
                                ));
                            }

                            // println!("{}", value.iter().map(|x| x.to_pretty(60)).join("\n\n"));

                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            let name = value_iter.next().unwrap();

                            let syntax = value_iter.next();

                            // println!("{:?}", syntax);

                            let syntax_rules = if let Some(ExprKind::SyntaxRules(s)) = syntax {
                                s
                            } else {
                                return Err(ParseError::SyntaxError(
                                    "define-syntax expected a syntax-rules object".to_string(),
                                    syn.span,
                                    None,
                                ));
                            };

                            Ok(ExprKind::Macro(Macro::new(name, syntax_rules, syn)))
                        }
                        TokenType::SyntaxRules => {
                            let syn = a.syn.clone();

                            if value.len() < 3 {
                                return Err(ParseError::SyntaxError(
                                    format!("syntax-rules expects a list of introduced syntax, and at least one pattern-body pair, found {} arguments", value.len()), syn.span, None
                                ));
                            }

                            let mut value_iter = value.into_iter();
                            value_iter.next();

                            let syntax_vec = if let Some(ExprKind::List(l)) = value_iter.next() {
                                l.args
                            } else {
                                return Err(ParseError::SyntaxError(
                                    "syntax-rules expects a list of new syntax forms used in the macro".to_string(), syn.span, None));
                            };

                            let mut pairs = Vec::new();
                            let rest: Vec<_> = value_iter.collect();

                            for pair in rest {
                                if let ExprKind::List(l) = pair {
                                    if l.args.len() != 2 {
                                        return Err(ParseError::SyntaxError(
                                            "syntax-rules requires only one pattern to one body"
                                                .to_string(),
                                            syn.span,
                                            None,
                                        ));
                                    }

                                    let mut pair_iter = l.args.into_iter();
                                    let pair_object = PatternPair::new(
                                        pair_iter.next().unwrap(),
                                        pair_iter.next().unwrap(),
                                    );
                                    pairs.push(pair_object);
                                } else {
                                    return Err(ParseError::SyntaxError(
                                        "syntax-rules requires pattern to expressions to be in a list".to_string(), syn.span, None
                                    ));
                                }
                            }

                            Ok(ExprKind::SyntaxRules(SyntaxRules::new(
                                syntax_vec, pairs, syn,
                            )))
                        }
                        _ => Ok(ExprKind::List(List::new(value))),
                    }
                }
                _ => Ok(ExprKind::List(List::new(value))),
            }
        } else {
            Ok(ExprKind::List(List::new(vec![])))
        }
    }
}

#[cfg(test)]
mod display_tests {

    use super::*;
    use crate::parser::parser::{Parser, Result};
    use std::collections::HashMap;
    use std::rc::Rc;

    fn parse(expr: &str) -> ExprKind {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<ExprKind>> = Parser::new(expr, &mut cache).collect();
        let a = a.unwrap()[0].clone();
        a
    }

    #[test]
    fn display_lambda_quote() {
        let expression = "(lambda (x) (quote x))";
        let parsed_expr = parse(expression);
        let expected = "(lambda (x) (quote x))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_list() {
        let expression = "(list 1 2 3 4)";
        let parsed_expr = parse(expression);
        let expected = "(list 1 2 3 4)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_lambda() {
        let expression = "(lambda (x) (+ x 10))";
        let parsed_expr = parse(expression);
        let expected = "(lambda (x) (+ x 10))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_set() {
        let expression = "(set! x 10)";
        let parsed_expr = parse(expression);
        let expected = "(set! x 10)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_panic() {
        let expression = "(panic! 12345)";
        let parsed_expr = parse(expression);
        let expected = "(panic! 12345)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_begin() {
        let expression = "(begin 1 2 3 4 5)";
        let parsed_expr = parse(expression);
        let expected = "(begin 1 2 3 4 5)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_define_normal() {
        let expression = "(define a 10)";
        let parsed_expr = parse(expression);
        let expected = "(define a 10)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_define_function() {
        let expression = "(define (applesauce x y z) (+ x y z))";
        let parsed_expr = parse(expression);
        let expected = "(define applesauce (lambda (x y z) (+ x y z)))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_let() {
        let expression = "(let ((x 10)) (+ x 10))";
        let parsed_expr = parse(expression);
        let expected = "((lambda (x) (+ x 10)) 10)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_apply() {
        let expression = "(apply + (list 1 2 3 4))";
        let parsed_expr = parse(expression);
        let expected = "(apply + (list 1 2 3 4))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_transduce() {
        let expression = "(transduce 1 2 3 4)";
        let parsed_expr = parse(expression);
        let expected = "(transduce 1 2 3 4)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_execute_two_args() {
        let expression = "(execute 1 2)";
        let parsed_expr = parse(expression);
        let expected = "(execute 1 2)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_execute_three_args() {
        let expression = "(execute 1 2 3)";
        let parsed_expr = parse(expression);
        let expected = "(execute 1 2 3)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_if() {
        let expression = "(if 1 2 3)";
        let parsed_expr = parse(expression);
        let expected = "(if 1 2 3)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_quote() {
        let expression = "'(1 2 3 4)";
        let parsed_expr = parse(expression);
        let expected = "(quote (1 2 3 4))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_read() {
        let expression = "(read '(1 2 3 4))";
        let parsed_expr = parse(expression);
        let expected = "(read (quote (1 2 3 4)))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_return() {
        let expression = "(return! 10)";
        let parsed_expr = parse(expression);
        let expected = "(return! 10)";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_struct() {
        let expression = "(struct Apple (a b c))";
        let parsed_expr = parse(expression);
        let expected = "(struct Apple (a b c))";
        assert_eq!(parsed_expr.to_string(), expected);
    }

    #[test]
    fn display_eval() {
        let expression = "(eval 'a)";
        let parsed_expr = parse(expression);
        let expected = "(eval (quote a))";
        assert_eq!(parsed_expr.to_string(), expected);
    }
}

#[cfg(test)]
mod pretty_print_tests {
    use super::*;
    use crate::parser::parser::{Parser, Result};
    use std::collections::HashMap;
    use std::rc::Rc;

    // pub fn to_pretty(&self, width: usize) -> String {
    //     let mut w = Vec::new();
    //     self.to_doc().render(width, &mut w).unwrap();
    //     String::from_utf8(w).unwrap()
    // }

    fn parse(expr: &str) -> ExprKind {
        let mut cache: HashMap<String, Rc<TokenType>> = HashMap::new();
        let a: Result<Vec<ExprKind>> = Parser::new(expr, &mut cache).collect();
        let a = a.unwrap()[0].clone();
        a
    }

    #[test]
    fn pretty_set() {
        let expression = r#"
            (define test-function 
                (lambda (a b c) 
                    (begin 
                        (set! bananas 10) 
                        (if applesauce 100 #f)
                        (if applesauce 100 (if applesauce 100 #f)))))"#;
        let parsed_expr = parse(expression);
        let _output = parsed_expr.to_pretty(45);

        assert!(true)
    }
}
