use crate::{
    parser::{ParseError, SyntaxObject},
    tokens::{NumberLiteral, ParenMod, RealLiteral, TokenType},
};

use std::{convert::TryFrom, fmt::Write};

use crate::tokens::IntLiteral;
use pretty::RcDoc;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Deref;

use super::{interner::InternedString, parser::SyntaxObjectId, span::Span};

// TODO: Have this macro share the implementation crate wide
macro_rules! define_symbols {
    ($($name:tt => $str:expr,) * ) => {
        $(
            pub static $name: once_cell::sync::Lazy<InternedString> = once_cell::sync::Lazy::new(|| $str.into());
        )*
    };
}

define_symbols! {
    UNREADABLE_MODULE_GET => "##__module-get",
    STANDARD_MODULE_GET => "%module-get%",
    PROTO_HASH_GET => "%proto-hash-get%",
    PROVIDE => "provide",
    DATUM_SYNTAX => "datum->syntax",
    SYNTAX_SPAN => "#%syntax-span",
    IF => "if",
    DEFINE => "define",
    LET => "let",
    QUOTE =>"quote",
    RETURN => "return!",
    REQUIRE => "require",
    SET => "set!",
    PLAIN_LET => "%plain-let",
    LAMBDA => "lambda",
    LAMBDA_SYMBOL => "λ",
    LAMBDA_FN => "fn",
    BEGIN => "begin",
    DOC_MACRO => "@doc",
    REQUIRE_BUILTIN => "require-builtin",
    REQUIRE_DYLIB => "#%require-dylib",
    STRUCT_KEYWORD => "struct",
    UNQUOTE => "unquote",
    UNQUOTE_COMMA => "#%unquote-comma",
    RAW_UNQUOTE => "#%unquote",
    UNQUOTE_SPLICING => "unquote-splicing",
    RAW_UNQUOTE_SPLICING => "#%unquote-splicing",
    QUASIQUOTE => "quasiquote",
    RAW_QUOTE => "#%quote",
    QUASISYNTAX => "quasisyntax",
    UNSYNTAX => "unsyntax",
    RAW_UNSYNTAX => "#%unsyntax",
    UNSYNTAX_SPLICING => "unsyntax-splicing",
    RAW_UNSYNTAX_SPLICING => "#%unsyntax-splicing",
    SYNTAX_QUOTE => "syntax",
}

pub trait AstTools {
    fn pretty_print(&self);
}

impl AstTools for Vec<ExprKind> {
    fn pretty_print(&self) {
        println!("{}", self.iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }
}

impl AstTools for Vec<&ExprKind> {
    fn pretty_print(&self) {
        println!("{}", self.iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }
}

impl AstTools for &mut Vec<ExprKind> {
    fn pretty_print(&self) {
        println!("{}", self.iter().map(|x| x.to_pretty(60)).join("\n\n"))
    }
}

pub trait IteratorExtensions: Iterator {
    fn join(&mut self, sep: &str) -> String
    where
        Self::Item: std::fmt::Display,
    {
        match self.next() {
            None => String::new(),
            Some(first_elt) => {
                // estimate lower bound of capacity needed
                let (lower, _) = self.size_hint();
                let mut result = String::with_capacity(sep.len() * lower);
                write!(&mut result, "{}", first_elt).unwrap();
                self.for_each(|elt| {
                    result.push_str(sep);
                    write!(&mut result, "{}", elt).unwrap();
                });
                result
            }
        }
    }
}

impl<T> IteratorExtensions for T where T: Iterator {}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ExprKind {
    Atom(Atom),
    If(Box<If>),
    Let(Box<Let>),
    Define(Box<Define>),
    LambdaFunction(Box<LambdaFunction>),
    Begin(Box<Begin>),
    Return(Box<Return>),
    Quote(Box<Quote>),
    Macro(Box<Macro>),
    SyntaxRules(Box<SyntaxRules>),
    List(List),
    Set(Box<Set>),
    Require(Box<Require>),
    Vector(Vector),
}

impl Default for ExprKind {
    fn default() -> Self {
        ExprKind::List(List::new(Vec::new()))
    }
}

#[test]
fn check_size() {
    println!("ExprKind: {}", std::mem::size_of::<ExprKind>());
    println!("SyntaxRules: {}", std::mem::size_of::<SyntaxRules>());
    println!("Macro: {}", std::mem::size_of::<Macro>());
    println!("List: {}", std::mem::size_of::<List>());
    println!("Atom: {}", std::mem::size_of::<Atom>());
    println!("Require: {}", std::mem::size_of::<Require>());
    println!("Vector: {}", std::mem::size_of::<Vector>());
}

#[macro_export]
macro_rules! expr_list {
    () => { $crate::ast::ExprKind::List($crate::ast::List::new(vec![])) };

    ( $($x:expr),* ) => {{
        $crate::ast::ExprKind::List($crate::ast::List::new(vec![$(
            $x,
        ) *]))
    }};

    ( $($x:expr ,)* ) => {{
        $crate::ast::ExprKind::List($crate::ast::List::new(vec![$($x, )*]))
    }};
}

impl ExprKind {
    pub fn span(&self) -> Option<Span> {
        match self {
            ExprKind::Atom(expr) => Some(expr.syn.span),
            ExprKind::If(expr) => Some(expr.location.span),
            ExprKind::Let(expr) => Some(expr.location.span),
            ExprKind::Define(expr) => Some(expr.location.span),
            ExprKind::LambdaFunction(expr) => Some(expr.location.span),
            ExprKind::Begin(expr) => Some(expr.location.span),
            ExprKind::Return(expr) => Some(expr.location.span),
            ExprKind::Quote(expr) => Some(expr.location.span),
            ExprKind::Macro(expr) => Some(expr.location.span),
            ExprKind::SyntaxRules(expr) => Some(expr.location.span),
            ExprKind::List(expr) => Some(expr.location),
            ExprKind::Set(expr) => Some(expr.location.span),
            ExprKind::Require(expr) => Some(expr.location.span),
            ExprKind::Vector(vec) => Some(vec.span),
        }
    }

    pub fn to_string_literal(&self) -> Option<&String> {
        if let ExprKind::Atom(a) = self {
            if let TokenType::StringLiteral(s) = &a.syn.ty {
                Some(s)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn into_lambda_function(self) -> Option<Box<LambdaFunction>> {
        if let ExprKind::LambdaFunction(func) = self {
            Some(func)
        } else {
            None
        }
    }

    pub fn quoted_list() -> ExprKind {
        ExprKind::Quote(Box::new(Quote::new(
            Self::empty(),
            SyntaxObject::default(TokenType::QuoteTick),
        )))
    }

    pub fn empty() -> ExprKind {
        ExprKind::List(List::new(Vec::new()))
    }

    pub fn integer_literal(value: isize, span: Span) -> ExprKind {
        ExprKind::Atom(crate::ast::Atom::new(SyntaxObject::new(
            IntLiteral::Small(value).into(),
            span,
        )))
    }

    pub fn atom<T: Into<InternedString>>(name: T) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            name.into(),
        ))))
    }

    pub fn ident(name: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            name.into(),
        ))))
    }

    pub fn string_lit(input: String) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::StringLiteral(
            Box::new(input),
        ))))
    }

    pub fn bool_lit(b: bool) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::BooleanLiteral(
            b,
        ))))
    }

    pub fn default_if(test: ExprKind, then: ExprKind, els: ExprKind) -> ExprKind {
        ExprKind::If(Box::new(If::new(
            test,
            then,
            els,
            SyntaxObject::default(TokenType::If),
        )))
    }

    pub fn into_atom_syntax_object(self) -> Option<SyntaxObject> {
        match self {
            Self::Atom(Atom { syn }) => Some(syn),
            _ => None,
        }
    }

    pub fn atom_syntax_object(&self) -> Option<&SyntaxObject> {
        match self {
            Self::Atom(Atom { syn }) => Some(syn),
            _ => None,
        }
    }

    pub fn atom_syntax_object_mut(&mut self) -> Option<&mut SyntaxObject> {
        match self {
            Self::Atom(Atom { syn }) => Some(syn),
            _ => None,
        }
    }

    pub fn define_syntax_ident(&self) -> bool {
        match self {
            Self::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::DefineSyntax,
                        ..
                    },
            }) => true,
            _ => false,
        }
    }

    pub fn atom_identifier_mut(&mut self) -> Option<&mut InternedString> {
        match self {
            Self::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) => Some(s),
            _ => None,
        }
    }

    pub fn lambda_function(&self) -> Option<&LambdaFunction> {
        match self {
            Self::LambdaFunction(l) => Some(l),
            _ => None,
        }
    }

    pub fn atom_identifier_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&InternedString, E> {
        match self {
            Self::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) => Ok(s),
            _ => Err(err()),
        }
    }

    pub fn atom_identifier(&self) -> Option<&InternedString> {
        match self {
            Self::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    },
            }) => Some(s),
            _ => None,
        }
    }

    pub fn string_literal(&self) -> Option<&str> {
        match self {
            Self::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::StringLiteral(s),
                        ..
                    },
            }) => Some(s),
            _ => None,
        }
    }

    pub fn list(&self) -> Option<&List> {
        if let ExprKind::List(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn list_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&List, E> {
        match self {
            Self::List(l) => Ok(l),
            _ => Err(err()),
        }
    }

    pub fn list_mut_or_else<E, F: FnOnce() -> E>(
        &mut self,
        err: F,
    ) -> std::result::Result<&mut List, E> {
        match self {
            Self::List(l) => Ok(l),
            _ => Err(err()),
        }
    }

    pub fn into_list_or_else<E, F: FnOnce() -> E>(self, err: F) -> std::result::Result<List, E> {
        match self {
            Self::List(l) => Ok(l),
            _ => Err(err()),
        }
    }

    pub fn into_list(self) -> List {
        if let ExprKind::List(l) = self {
            l
        } else {
            panic!("Attempted to coerce a non list to a list");
        }
    }

    pub fn unwrap_function(self) -> Option<Box<LambdaFunction>> {
        if let ExprKind::LambdaFunction(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn get_list(&self) -> Option<&List> {
        if let ExprKind::List(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn update_string_in_atom(&mut self, ident: InternedString) {
        if let ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(ref mut s),
                    ..
                },
        }) = self
        {
            *s = ident;
        }
    }
}

pub trait ToDoc {
    fn to_doc(&self) -> RcDoc<()>;
}

impl ToDoc for ExprKind {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            ExprKind::Atom(a) => a.to_doc(),
            ExprKind::If(i) => i.to_doc(),
            ExprKind::Define(d) => d.to_doc(),
            ExprKind::LambdaFunction(l) => l.to_doc(),
            ExprKind::Begin(b) => b.to_doc(),
            ExprKind::Return(r) => r.to_doc(),
            ExprKind::Let(l) => l.to_doc(),
            ExprKind::Quote(q) => q.to_doc(),
            ExprKind::Macro(m) => m.to_doc(),
            ExprKind::SyntaxRules(s) => s.to_doc(),
            ExprKind::List(l) => l.to_doc(),
            ExprKind::Set(s) => s.to_doc(),
            ExprKind::Require(r) => r.to_doc(),
            ExprKind::Vector(v) => v.to_doc(),
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
            ExprKind::Atom(a) => write!(f, "{a}"),
            ExprKind::If(i) => write!(f, "{i}"),
            ExprKind::Define(d) => write!(f, "{d}"),
            ExprKind::LambdaFunction(l) => write!(f, "{l}"),
            ExprKind::Begin(b) => write!(f, "{b}"),
            ExprKind::Return(r) => write!(f, "{r}"),
            ExprKind::Let(l) => write!(f, "{l}"),
            ExprKind::Quote(q) => write!(f, "{q}"),
            ExprKind::Macro(m) => write!(f, "{m}"),
            ExprKind::SyntaxRules(s) => write!(f, "{s}"),
            ExprKind::List(l) => write!(f, "{l}"),
            ExprKind::Set(s) => write!(f, "{s}"),
            ExprKind::Require(r) => write!(f, "{r}"),
            ExprKind::Vector(v) => write!(f, "{v}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Atom {
    pub syn: SyntaxObject,
}

impl Atom {
    pub fn new(syn: SyntaxObject) -> Self {
        Atom { syn }
    }

    pub fn ident(&self) -> Option<&InternedString> {
        if let TokenType::Identifier(ref ident) = self.syn.ty {
            Some(ident)
        } else {
            None
        }
    }

    pub fn ident_mut(&mut self) -> Option<&mut InternedString> {
        if let TokenType::Identifier(ref mut ident) = self.syn.ty {
            Some(ident)
        } else {
            None
        }
    }

    pub fn byte(&self) -> Option<u8> {
        // TODO: accept more literals e.g. `3+0i`, `3/3`, etc.
        let TokenType::Number(number) = &self.syn.ty else {
            return None;
        };

        match &**number {
            NumberLiteral::Real(RealLiteral::Int(int)) => match int {
                IntLiteral::Small(int) => u8::try_from(*int).ok(),
                IntLiteral::Big(bigint) => u8::try_from(&**bigint).ok(),
            },
            _ => None,
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.syn.ty)
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Let {
    pub bindings: Vec<(ExprKind, ExprKind)>,
    pub body_expr: ExprKind,
    pub location: SyntaxObject,
    pub syntax_object_id: u32,
}

impl Let {
    pub fn new(
        bindings: Vec<(ExprKind, ExprKind)>,
        body_expr: ExprKind,
        location: SyntaxObject,
    ) -> Self {
        Let {
            bindings,
            body_expr,
            location,
            syntax_object_id: SyntaxObjectId::fresh().0,
        }
    }

    pub fn local_bindings(&self) -> impl Iterator<Item = &'_ ExprKind> {
        self.bindings.iter().map(|x| &x.0)
    }

    pub fn expression_arguments(&self) -> impl Iterator<Item = &'_ ExprKind> {
        self.bindings.iter().map(|x| &x.1)
    }
}

impl fmt::Display for Let {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(%plain-let ({}) {})",
            self.bindings
                .iter()
                .map(|x| format!("({} {})", x.0, x.1))
                .join(" "),
            self.body_expr
        )
    }
}

impl ToDoc for Let {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("(%plain-let")
            .append(RcDoc::space())
            .append(RcDoc::text("("))
            .append(
                RcDoc::intersperse(
                    self.bindings.iter().map(|x| {
                        RcDoc::text("(")
                            .append(x.0.to_doc())
                            .append(RcDoc::space())
                            .append(x.1.to_doc())
                            .append(RcDoc::text(")"))
                    }),
                    RcDoc::line(),
                )
                .nest(2)
                .group(),
            )
            .append(RcDoc::text(")"))
            .append(RcDoc::line())
            .append(self.body_expr.to_doc())
            .append(RcDoc::text(")"))
            .nest(2)
    }
}

impl From<Let> for ExprKind {
    fn from(val: Let) -> Self {
        ExprKind::Let(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
            .append(RcDoc::space())
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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

    pub fn is_an_alias_definition(&self) -> Option<SyntaxObjectId> {
        if let Some(atom) = self.body.atom_syntax_object() {
            if let TokenType::Identifier(_) = atom.ty {
                return Some(atom.syntax_object_id);
            }
        }

        None
    }

    pub fn name_id(&self) -> Option<SyntaxObjectId> {
        self.name.atom_syntax_object().map(|x| x.syntax_object_id)
    }
}

impl From<Define> for ExprKind {
    fn from(val: Define) -> Self {
        ExprKind::Define(Box::new(val))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct LambdaFunction {
    pub args: Vec<ExprKind>,
    pub body: ExprKind,
    pub location: SyntaxObject,
    pub rest: bool,
    pub syntax_object_id: u32,
}

impl Clone for LambdaFunction {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            body: self.body.clone(),
            location: self.location.clone(),
            rest: self.rest,
            syntax_object_id: SyntaxObjectId::fresh().0,
        }
    }
}

impl PartialEq for LambdaFunction {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args
            && self.body == other.body
            && self.location == other.location
            && self.rest == other.rest
    }
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
        if self.rest && self.args.len() == 1 {
            RcDoc::text("(λ")
                .append(RcDoc::space())
                .append(self.args.first().unwrap().to_doc())
                .append(RcDoc::line())
                .append(self.body.to_doc())
                .append(RcDoc::text(")"))
                .nest(2)
        } else {
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
}

impl LambdaFunction {
    pub fn new(args: Vec<ExprKind>, body: ExprKind, location: SyntaxObject) -> Self {
        LambdaFunction {
            args,
            body,
            location,
            rest: false,
            syntax_object_id: SyntaxObjectId::fresh().0,
        }
    }

    pub fn new_with_rest_arg(args: Vec<ExprKind>, body: ExprKind, location: SyntaxObject) -> Self {
        LambdaFunction {
            args,
            body,
            location,
            rest: true,
            syntax_object_id: SyntaxObjectId::fresh().0,
        }
    }

    pub fn new_maybe_rest(
        args: Vec<ExprKind>,
        body: ExprKind,
        location: SyntaxObject,
        rest: bool,
    ) -> Self {
        LambdaFunction {
            args,
            body,
            location,
            rest,
            syntax_object_id: SyntaxObjectId::fresh().0,
        }
    }

    pub fn arguments(&self) -> Option<Vec<&InternedString>> {
        self.args.iter().map(|x| x.atom_identifier()).collect()
    }

    pub fn arguments_mut(&mut self) -> impl Iterator<Item = &mut InternedString> {
        self.args.iter_mut().filter_map(|x| x.atom_identifier_mut())
    }

    pub fn syntax_objects_arguments_mut(&mut self) -> impl Iterator<Item = &mut SyntaxObject> {
        self.args
            .iter_mut()
            .filter_map(|x| x.atom_syntax_object_mut())
    }
}

impl From<LambdaFunction> for ExprKind {
    fn from(val: LambdaFunction) -> Self {
        ExprKind::LambdaFunction(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
            .nest(5)
            .append(
                RcDoc::intersperse(self.exprs.iter().map(|x| x.to_doc()), RcDoc::line())
                    .nest(5)
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
        ExprKind::Begin(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Require {
    pub modules: Vec<ExprKind>,
    pub location: SyntaxObject,
}

impl Require {
    pub fn new(modules: Vec<ExprKind>, location: SyntaxObject) -> Self {
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
        ExprKind::Require(Box::new(val))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Vector {
    pub args: Vec<ExprKind>,
    pub bytes: bool,
    pub span: Span,
}

impl Vector {
    fn prefix(&self) -> ParenMod {
        if self.bytes {
            ParenMod::Bytes
        } else {
            ParenMod::Vector
        }
    }

    pub fn as_bytes(&self) -> impl Iterator<Item = u8> + '_ {
        self.args.iter().flat_map(move |expr| {
            let byte = if let ExprKind::Atom(atom) = expr {
                atom.byte()
            } else {
                None
            };

            debug_assert!(!(self.bytes && byte.is_none()));

            byte
        })
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.prefix())?;

        if let Some((first, rest)) = self.args.split_first() {
            write!(f, "{first}")?;

            for arg in rest {
                write!(f, " {arg}")?;
            }
        }

        write!(f, ")")
    }
}

impl ToDoc for Vector {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.prefix().as_str())
            .append("(")
            .append(
                RcDoc::intersperse(self.args.iter().map(ToDoc::to_doc), RcDoc::line())
                    .nest(1)
                    .group(),
            )
            .append(RcDoc::text(")"))
            .nest(2)
            .group()
    }
}

impl From<Vector> for ExprKind {
    fn from(value: Vector) -> Self {
        ExprKind::Vector(value)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct List {
    pub args: Vec<ExprKind>,
    pub syntax_object_id: u32,
    pub improper: bool,
    // TODO: Attach the span from the parser - just the offset
    // of the open and close parens
    pub location: Span,
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args
            && self.improper == other.improper
            && self.location == other.location
    }
}

impl List {
    pub fn new(args: Vec<ExprKind>) -> Self {
        List {
            args,
            syntax_object_id: SyntaxObjectId::fresh().0 as _,
            improper: false,
            location: Span::default(),
        }
    }

    pub fn new_maybe_improper(args: Vec<ExprKind>, improper: bool) -> Self {
        List {
            args,
            syntax_object_id: SyntaxObjectId::fresh().0 as _,
            improper,
            location: Span::default(),
        }
    }

    pub fn with_spans(args: Vec<ExprKind>, open: Span, close: Span) -> Self {
        List {
            args,
            improper: false,
            location: Span::merge(open, close),
            syntax_object_id: SyntaxObjectId::fresh().0 as _,
        }
    }

    pub fn make_improper(mut self) -> Self {
        let Some(last) = self.args.pop() else {
            debug_assert!(false);

            self.improper = true;
            return self;
        };

        let ExprKind::List(l) = last else {
            self.args.push(last);

            self.improper = true;
            return self;
        };

        self.args.extend(l.args);
        self.improper = l.improper;
        self
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn rest_mut(&mut self) -> Option<&mut [ExprKind]> {
        self.args.split_first_mut().map(|x| x.1)
    }

    pub fn args_proper(self, ty: TokenType<&str>) -> Result<Vec<ExprKind>, ParseError> {
        if self.improper {
            return Err(ParseError::SyntaxError(
                format!("{} expression requires a proper list", ty),
                self.location,
                None,
            ));
        }

        Ok(self.args)
    }

    pub fn first_ident_mut(&mut self) -> Option<&mut InternedString> {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                },
        })) = self.args.first_mut()
        {
            Some(s)
        } else {
            None
        }
    }

    pub fn is_require(&self) -> bool {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Require,
                    ..
                },
        })) = self.args.first()
        {
            true
        } else {
            false
        }
    }

    pub fn is_define_syntax(&self) -> bool {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::DefineSyntax,
                    ..
                },
        })) = self.args.first()
        {
            self.args.len() == 3
        } else {
            false
        }
    }

    pub fn is_syntax_rules(&self) -> bool {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::SyntaxRules,
                    ..
                },
        })) = self.args.first()
        {
            self.args.len() > 2
        } else {
            false
        }
    }

    pub fn is_quote(&self) -> bool {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Quote,
                    ..
                },
        })) = self.args.first()
        {
            true
        } else {
            false
        }
    }

    pub fn first_ident(&self) -> Option<&InternedString> {
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

    pub fn second_ident(&self) -> Option<&InternedString> {
        if let Some(ExprKind::Atom(Atom {
            syn:
                SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                },
        })) = self.args.get(1)
        {
            Some(s)
        } else {
            None
        }
    }

    pub fn is_anonymous_function_call(&self) -> bool {
        matches!(self.args.get(0), Some(ExprKind::LambdaFunction(_)))
    }

    pub fn is_a_builtin_expr(&self) -> bool {
        matches!(self.first_ident(), Some(func) if *func == *UNREADABLE_MODULE_GET || *func == *STANDARD_MODULE_GET)
    }

    pub fn first_func_mut(&mut self) -> Option<&mut LambdaFunction> {
        if let Some(ExprKind::LambdaFunction(l)) = self.args.get_mut(0) {
            Some(l)
        } else {
            None
        }
    }

    pub fn first_func(&self) -> Option<&LambdaFunction> {
        if let Some(ExprKind::LambdaFunction(l)) = self.args.get(0) {
            Some(l)
        } else {
            None
        }
    }

    pub fn split_improper(&self) -> Option<(&[ExprKind], &ExprKind)> {
        if self.improper {
            self.args.split_last().map(|(last, rest)| (rest, last))
        } else {
            None
        }
    }
}

impl ToDoc for List {
    fn to_doc(&self) -> RcDoc<()> {
        if let Some(func) = self.first_func().filter(|_| !self.improper) {
            let mut args_iter = self.args.iter();
            args_iter.next();

            let bindings = func.args.iter().zip(args_iter);

            RcDoc::text("(let")
                .append(RcDoc::space())
                .append(RcDoc::text("("))
                .append(
                    RcDoc::intersperse(
                        bindings.map(|x| {
                            RcDoc::text("(")
                                .append(x.0.to_doc())
                                .append(RcDoc::space())
                                .append(x.1.to_doc())
                                .append(RcDoc::text(")"))
                        }),
                        RcDoc::line(),
                    )
                    .nest(2)
                    .group(),
                )
                .append(RcDoc::text(")"))
                .append(RcDoc::line())
                .append(func.body.to_doc())
                .append(RcDoc::text(")"))
                .nest(2)
        } else {
            let args = if let Some((car, cdr)) = self.split_improper() {
                let iter = car
                    .iter()
                    .map(ToDoc::to_doc)
                    .chain(std::iter::once(RcDoc::text(".")))
                    .chain(std::iter::once(cdr.to_doc()));

                RcDoc::intersperse(iter, RcDoc::line())
            } else {
                RcDoc::intersperse(self.args.iter().map(ToDoc::to_doc), RcDoc::line())
            };

            RcDoc::text("(")
                .append(args.nest(1).group())
                .append(RcDoc::text(")"))
                .nest(2)
                .group()
        }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Some((car, cdr)) = self.split_improper() else {
            return write!(f, "({})", self.args.iter().join(" "));
        };

        write!(f, "(")?;

        for arg in car {
            write!(f, "{} ", arg)?;
        }

        write!(f, ". {})", cdr)
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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

// TODO figure out how many fields a macro has
// put it into here nicely
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Macro {
    pub name: Box<ExprKind>,
    pub syntax_rules: Box<SyntaxRules>,
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
    pub fn new(name: ExprKind, syntax_rules: Box<SyntaxRules>, location: SyntaxObject) -> Self {
        Macro {
            name: Box::new(name),
            syntax_rules,
            location,
        }
    }
}

impl From<Macro> for ExprKind {
    fn from(val: Macro) -> Self {
        ExprKind::Macro(Box::new(val))
    }
}

// TODO figure out a good mapping immediately to a macro that can be interpreted
// by the expander
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
        ExprKind::SyntaxRules(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
pub(crate) fn parse_if<I>(
    mut value_iter: I,
    syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
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
        // Replace else condition with just a void if its not found!
        value_iter
            .next()
            .unwrap_or_else(|| ExprKind::ident("#%prim.void")),
        //     ok_or_else(|| {
        //     ParseError::SyntaxError(
        //         "if expects an else condition, found none".to_string(),
        //         syn.span,
        //         None,
        //     )
        // })?,
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
pub(crate) fn parse_define<I>(
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
                if *datum_syntax == *DATUM_SYNTAX {
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

            if body_exprs.is_empty() {
                return Err(ParseError::SyntaxError(
                    "Function body cannot be empty".to_string(),
                    syn.span,
                    None,
                ));
            }

            let body = if body_exprs.len() == 1 {
                body_exprs[0].clone()
            } else {
                ExprKind::Begin(Box::new(Begin::new(
                    body_exprs,
                    SyntaxObject::default(TokenType::Begin),
                )))
            };

            let rest = l.improper;
            let lambda = ExprKind::LambdaFunction(Box::new(LambdaFunction::new_maybe_rest(
                args,
                body,
                SyntaxObject::new(TokenType::Lambda, syn.span),
                rest,
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
pub(crate) fn parse_new_let<I>(
    mut value_iter: I,
    syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
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
        ExprKind::Begin(Box::new(Begin::new(
            body_exprs,
            SyntaxObject::default(TokenType::Begin),
        )))
    };

    let mut pairs = Vec::with_capacity(let_pairs.len());

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

            let mut iter = pair.into_iter();

            let identifier = iter.next().unwrap();
            let application_arg = iter.next().unwrap();
            pairs.push((identifier, application_arg))
        } else {
            return Err(ParseError::SyntaxError(
                "let expected a list of variable binding pairs".to_string(),
                syn.span,
                None,
            ));
        }
    }

    Ok(ExprKind::Let(Let::new(pairs, body, syn).into()))
}

#[inline]
fn parse_named_let<I>(
    mut value_iter: I,
    syn: SyntaxObject,
    name: ExprKind,
) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    let pairs = if let ExprKind::List(l) = value_iter.next().ok_or_else(|| {
        ParseError::SyntaxError(
            "named let expects a list of argument id and init expr pairs, found none".to_string(),
            syn.span,
            None,
        )
    })? {
        l.args
    } else {
        return Err(ParseError::SyntaxError(
            "named let expects a list of variable bindings pairs in the second position"
                .to_string(),
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
        ExprKind::Begin(Box::new(Begin::new(
            body_exprs,
            SyntaxObject::default(TokenType::Begin),
        )))
    };

    let mut arguments = Vec::with_capacity(pairs.len());

    // insert args at the end
    // put the function in the inside
    let mut application_args = Vec::with_capacity(pairs.len());

    for pair in pairs {
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

    // This is the body of the define
    let function: ExprKind = LambdaFunction::new(arguments, body, syn.clone()).into();

    let define: ExprKind = Define::new(name.clone(), function, syn.clone()).into();

    let application: ExprKind = {
        let mut application = vec![name];
        application.append(&mut application_args);
        List::new(application).into()
    };

    let begin = ExprKind::Begin(Box::new(Begin::new(vec![define, application], syn.clone())));

    // Wrap the whole thing inside of an empty function application, to create a new scope

    Ok(List::new(vec![LambdaFunction::new(vec![], begin, syn).into()]).into())
}

#[inline]
pub(crate) fn parse_let<I>(
    mut value_iter: I,
    mut syn: SyntaxObject,
) -> std::result::Result<ExprKind, ParseError>
where
    I: Iterator<Item = ExprKind>,
{
    value_iter.next();

    let let_pairs = match value_iter.next().ok_or_else(|| {
        ParseError::SyntaxError(
            "let expected a list of variable bindings pairs in the second position, found none"
                .to_string(),
            syn.span,
            None,
        )
    })? {
        // Standard let
        ExprKind::List(l) => l.args,
        // Named let
        name @ ExprKind::Atom(_) => return parse_named_let(value_iter, syn, name),
        _ => {
            return Err(ParseError::SyntaxError(
                "let expects a list of variable bindings pairs in the second position".to_string(),
                syn.span,
                None,
            ));
        }
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
        ExprKind::Begin(Box::new(Begin::new(
            body_exprs,
            SyntaxObject::default(TokenType::Begin),
        )))
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

    // Since we've converted it, we should turn it into a lambda
    syn.ty = TokenType::Lambda;

    let mut function: Vec<ExprKind> = vec![LambdaFunction::new(arguments, body, syn).into()];

    function.append(&mut application_args);

    Ok(ExprKind::List(List::new(function)))
}

#[inline]
pub(crate) fn parse_single_argument<I>(
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
            format!("{name} expected one argument, found none"),
            syn.span,
            None,
        )
    })?;

    if value_iter.next().is_some() {
        Err(ParseError::SyntaxError(
            format!("{name} expects only one argument"),
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

        // TODO -> get rid of this clone on the first value
        if let Some(f) = value.first().cloned() {
            match f {
                ExprKind::Atom(a) => {
                    // let value = value.into_iter();

                    match &a.syn.ty {
                        // Have this also match on the first argument being a TokenType::Identifier("if")
                        // Do the same for the rest of the arguments
                        TokenType::If => parse_if(value.into_iter(), a.syn.clone()),
                        TokenType::Identifier(expr) if *expr == *IF => {
                            parse_if(value.into_iter(), a.syn.clone())
                        }

                        TokenType::Define => parse_define(value.into_iter(), a.syn.clone()),
                        TokenType::Identifier(expr) if *expr == *DEFINE => {
                            parse_define(value.into_iter(), a.syn.clone())
                        }

                        TokenType::Let => parse_let(value.into_iter(), a.syn.clone()),
                        TokenType::Identifier(expr) if *expr == *LET => {
                            parse_let(value.into_iter(), a.syn.clone())
                        }

                        // TODO: Deprecate
                        TokenType::TestLet => parse_new_let(value.into_iter(), a.syn.clone()),
                        TokenType::Identifier(expr) if *expr == *PLAIN_LET => {
                            parse_new_let(value.into_iter(), a.syn.clone())
                        }

                        TokenType::Quote => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "quote",
                            |expr, syn| Quote::new(expr, syn).into(),
                        ),
                        TokenType::Identifier(expr) if *expr == *QUOTE => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "quote",
                            |expr, syn| Quote::new(expr, syn).into(),
                        ),

                        TokenType::Return => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "return!",
                            |expr, syn| Return::new(expr, syn).into(),
                        ),
                        TokenType::Identifier(expr) if *expr == *RETURN => parse_single_argument(
                            value.into_iter(),
                            a.syn.clone(),
                            "return!",
                            |expr, syn| Return::new(expr, syn).into(),
                        ),

                        TokenType::Require => parse_require(&a, value),
                        TokenType::Identifier(expr) if *expr == *REQUIRE => {
                            parse_require(&a, value)
                        }

                        TokenType::Set => parse_set(&a, value),
                        TokenType::Identifier(expr) if *expr == *SET => parse_set(&a, value),

                        TokenType::Begin => parse_begin(a, value),
                        TokenType::Identifier(expr) if *expr == *BEGIN => parse_begin(a, value),

                        TokenType::Lambda => parse_lambda(a, value),
                        TokenType::Identifier(expr)
                            if *expr == *LAMBDA
                                || *expr == *LAMBDA_FN
                                || *expr == *LAMBDA_SYMBOL =>
                        {
                            parse_lambda(a, value)
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

                            Ok(ExprKind::Macro(Box::new(Macro::new(
                                name,
                                syntax_rules,
                                syn,
                            ))))
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

                            Ok(ExprKind::SyntaxRules(Box::new(SyntaxRules::new(
                                syntax_vec, pairs, syn,
                            ))))
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

pub fn parse_lambda(a: Atom, value: Vec<ExprKind>) -> Result<ExprKind, ParseError> {
    let syn = a.syn;
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
    match arguments {
        Some(ExprKind::List(l)) => {
            let args = l.args;

            for arg in &args {
                if let ExprKind::Atom(_) = arg {
                    continue;
                } else {
                    return Err(ParseError::SyntaxError(
                        format!(
                            "lambda function expects a list of identifiers, found: {}",
                            List::new(args)
                        ),
                        syn.span,
                        None,
                    ));
                }
            }

            let body_exprs: Vec<_> = value_iter.collect();

            let body = if body_exprs.len() == 1 {
                body_exprs.into_iter().next().unwrap()
            } else {
                ExprKind::Begin(Box::new(Begin::new(
                    body_exprs,
                    SyntaxObject::default(TokenType::Begin),
                )))
            };

            let rest = l.improper;

            Ok(ExprKind::LambdaFunction(Box::new(
                LambdaFunction::new_maybe_rest(args, body, syn, rest),
            )))
        }
        Some(ExprKind::Atom(a)) => {
            let body_exprs: Vec<_> = value_iter.collect();

            let body = if body_exprs.len() == 1 {
                body_exprs.into_iter().next().unwrap()
            } else {
                ExprKind::Begin(Box::new(Begin::new(
                    body_exprs,
                    SyntaxObject::default(TokenType::Begin),
                )))
            };

            // (lambda x ...) => x is a rest arg, becomes a list at run time
            Ok(ExprKind::LambdaFunction(Box::new(
                LambdaFunction::new_with_rest_arg(vec![ExprKind::Atom(a)], body, syn),
            )))
        }
        _ => {
            // TODO -> handle case like
            // (lambda x 10) <- where x is immediately bound to be a rest arg
            // This should be fairly trivial in this case since we can just put the
            // first thing into a vec for the lambda node
            Err(ParseError::SyntaxError(
                format!("lambda function expected a list of identifiers, found: {arguments:?}"),
                syn.span,
                None,
            ))
        }
    }
}

pub(crate) fn parse_set(a: &Atom, value: Vec<ExprKind>) -> Result<ExprKind, ParseError> {
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

pub(crate) fn parse_require(a: &Atom, value: Vec<ExprKind>) -> Result<ExprKind, ParseError> {
    let syn = a.syn.clone();
    if value.len() < 2 {
        return Err(ParseError::ArityMismatch(
            "require expects at least one identifier or string".to_string(),
            syn.span,
            None,
        ));
    }
    let mut value_iter = value.into_iter();
    value_iter.next();
    let expressions = value_iter
        .map(|x| {
            match &x {
                ExprKind::Atom(_) | ExprKind::List(_) => Ok(x),
                _ => Err(ParseError::SyntaxError(
                    "require expects atoms".to_string(),
                    syn.span,
                    None,
                )),
            }

            // if let ExprKind::Atom(a) = x {
            //     Ok(a)
            // } else {

            // }
        })
        .collect::<Result<Vec<_>, ParseError>>()?;
    Ok(ExprKind::Require(Box::new(Require::new(expressions, syn))))
}

pub(crate) fn parse_begin(a: Atom, value: Vec<ExprKind>) -> Result<ExprKind, ParseError> {
    let syn = a.syn;
    let mut value_iter = value.into_iter();
    value_iter.next();
    Ok(ExprKind::Begin(Box::new(Begin::new(
        value_iter.collect(),
        syn,
    ))))
}

#[cfg(test)]
mod display_tests {

    use super::*;
    use crate::parser::{Parser, Result, SourceId};

    fn parse(expr: &str) -> ExprKind {
        let a: Result<Vec<ExprKind>> = Parser::new(expr, SourceId::none()).collect();

        a.unwrap()[0].clone()
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
    use crate::parser::{Parser, Result, SourceId};

    fn parse(expr: &str) -> ExprKind {
        let a: Result<Vec<ExprKind>> = Parser::new(expr, SourceId::none()).collect();

        a.unwrap()[0].clone()
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
