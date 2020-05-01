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

use std::collections::HashMap;

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
    /// Represents built in macros,
    MacroV(SteelMacro),
    /// Represents a symbol, internally represented as `String`s
    SymbolV(String),
    /// Container for a type that implements the `Custom Type` trait. (trait object)
    Custom(Box<dyn CustomType>),
}

impl SteelVal {
    pub fn bool_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<bool, E> {
        match self {
            Self::BoolV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    pub fn num_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<f64, E> {
        match self {
            Self::NumV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    pub fn char_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<char, E> {
        match self {
            Self::CharV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    /// Vector does copy on the value to return
    pub fn vector_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<Vector<SteelVal>, E> {
        match self {
            Self::VectorV(v) => Ok(v.clone()),
            _ => Err(err()),
        }
    }

    pub fn void_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<(), E> {
        match self {
            Self::Void => Ok(()),
            _ => Err(err()),
        }
    }

    pub fn string_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&str, E> {
        match self {
            Self::StringV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&FunctionSignature, E> {
        match self {
            Self::FuncV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn lambda_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&SteelLambda, E> {
        match self {
            Self::LambdaV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn macro_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&SteelMacro, E> {
        match self {
            Self::MacroV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn symbol_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&str, E> {
        match self {
            Self::SymbolV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn custom_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&Box<dyn CustomType>, E> {
        match self {
            Self::Custom(v) => Ok(&v),
            _ => Err(err()),
        }
    }
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
            MacroV(_) => Err("Can't convert from Macro to expression!"),
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
            (Pair(_, _), Pair(_, _)) => {
                collect_pair_into_vector(self) == collect_pair_into_vector(other)
            }
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

#[derive(Clone, Debug)]
pub enum MacroPattern {
    Single(String),
    Syntax(String),
    Many(String),
    Nested(Vec<MacroPattern>),
}

impl MacroPattern {
    // TODO make this not so trash
    pub fn deconstruct(&self) -> Option<Vec<String>> {
        match self {
            Self::Syntax(s) => Some(vec![s.clone()]),
            Self::Single(s) => Some(vec![s.clone()]),
            Self::Many(s) => Some(vec![s.clone()]),
            Self::Nested(v) => Some(
                v.into_iter()
                    .filter_map(|x| x.deconstruct())
                    .flatten()
                    .collect(),
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MacroCase {
    args: Vec<MacroPattern>,
    body: Rc<Expr>,
}

impl MacroCase {
    pub fn new(args: Vec<MacroPattern>, body: Rc<Expr>) -> MacroCase {
        MacroCase { args, body }
    }

    pub fn expand(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        // unimplemented!()
        self.replace_exprs_in_body(list_of_tokens)
    }

    // this is a half-baked attempt to avoid namespace clashing inside of a macro body
    // not particularly sure if this is the answer, but recursively explore the body of the macro
    // and rename identifiers with a reserved keyword that could clash with the rest of the program
    // otherwise leave the same
    // TODO
    // fix this kinda junky function
    pub fn rename_identifiers(
        expr: Rc<Expr>,
        env: &Rc<RefCell<Env>>,
        args: &[MacroPattern],
    ) -> Rc<Expr> {
        // unimplemented!()
        let env = Rc::clone(env);
        let args_str: Vec<String> = args
            .iter()
            .filter_map(|x| x.deconstruct())
            .flatten()
            .collect();
        match expr.as_ref() {
            Expr::Atom(t) => {
                if let Identifier(s) = t {
                    if args_str.contains(s) || SteelMacro::is_reserved_keyword(&s) {
                        return expr;
                    } else if env.borrow().lookup(&s).is_err() {
                        return Rc::new(Expr::Atom(Identifier("##".to_string() + s)));
                    }
                }
                expr
            }
            Expr::VectorVal(vec_exprs) => Rc::new(Expr::VectorVal(
                vec_exprs
                    .into_iter()
                    .map(|x| Self::rename_identifiers(Rc::clone(x), &env, args))
                    .collect(),
            )),
        }
    }

    fn recursive_match(&self, list_of_tokens: &[Rc<Expr>]) -> bool {
        Self::match_vec_pattern_to_list_of_tokens(&self.args, list_of_tokens)
    }

    fn match_vec_pattern_to_list_of_tokens(
        args: &[MacroPattern],
        list_of_tokens: &[Rc<Expr>],
    ) -> bool {
        let mut token_iter = list_of_tokens.iter();
        for pat in args {
            // println!("Matching pattern: {:?}", pat);
            if let Some(val) = token_iter.next() {
                match pat {
                    MacroPattern::Single(_) | MacroPattern::Many(_) => {
                        continue;
                    }
                    MacroPattern::Syntax(v) => {
                        if let Expr::Atom(Identifier(s)) = val.as_ref() {
                            if s == v || v == "_" {
                                continue;
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    MacroPattern::Nested(vec) => {
                        if let Expr::VectorVal(l) = val.as_ref() {
                            if Self::match_vec_pattern_to_list_of_tokens(&vec, l) {
                                continue;
                            } else {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            } else {
                true;
            }
        }
        true
    }

    // be able to recognize deep patterns like let
    fn parse_pattern_into_vec(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Rc<Expr>],
    ) -> Result<Vec<MacroPattern>> {
        let mut pattern_vec: Vec<MacroPattern> = Vec::new();
        let mut peek_token_iter = list_of_tokens.iter().peekable();

        while let Some(token) = peek_token_iter.next() {
            match token.as_ref() {
                Expr::Atom(Identifier(t)) => {
                    if t == macro_name || special_forms.contains(t) {
                        pattern_vec.push(MacroPattern::Syntax(t.clone()))
                    } else {
                        if let Some(nxt) = peek_token_iter.peek() {
                            if let Expr::Atom(Identifier(n)) = nxt.as_ref() {
                                if n == "..." {
                                    peek_token_iter.next();
                                    pattern_vec.push(MacroPattern::Many(t.clone()))
                                } else {
                                    pattern_vec.push(MacroPattern::Single(t.clone()))
                                }
                            }
                        } else {
                            pattern_vec.push(MacroPattern::Single(t.clone()));
                        }
                    }
                }
                Expr::VectorVal(l) => pattern_vec.push(MacroPattern::Nested(
                    Self::parse_pattern_into_vec(macro_name, special_forms, l)?,
                )),
                _ => stop!(BadSyntax => "syntax-rules requires identifiers in the pattern"),
            }
        }
        Ok(pattern_vec)
    }

    pub fn parse_from_tokens(
        macro_name: &str,
        special_forms: &[String],
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<MacroCase> {
        if let [pattern_expr, body_expr] = list_of_tokens {
            let pattern_expr_vec = pattern_expr.as_ref().vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a pattern in the case argument"),
            )?;

            let args = Self::parse_pattern_into_vec(macro_name, special_forms, pattern_expr_vec)?;
            let renamed_body = Self::rename_identifiers(Rc::clone(body_expr), env, &args);
            Ok(MacroCase::new(args, renamed_body))
        } else {
            stop!(ArityMismatch => "syntax-rules cases have 2 arguments")
        }
    }

    // count up the "..." and increment them?
    // TODO
    fn collect_bindings(
        args: &[MacroPattern],
        list_of_tokens: &[Rc<Expr>],
        bindings: &mut HashMap<String, Rc<Expr>>,
    ) -> Result<()> {
        let mut token_iter = list_of_tokens.into_iter().map(|x| Rc::clone(x));

        for arg in args {
            match arg {
                // bind the expression to the variable
                MacroPattern::Single(s) => {
                    if let Some(e) = token_iter.next() {
                        bindings.insert(s.to_string(), e);
                    } else {
                        stop!(ArityMismatch => "macro expansion failed")
                    }
                }
                // actually check if the syntax matches
                MacroPattern::Syntax(s) => {
                    let e = token_iter
                        .next()
                        .ok_or_else(throw!(BadSyntax => "macro expansion expected keyword"))?;
                    let syn = e.as_ref().atom_identifier_or_else(
                        throw!(BadSyntax => "macro expansion expected keyword"),
                    )?;
                    if s != syn {
                        stop!(BadSyntax => "macro expansion expected keyword")
                    }
                }
                // TODO
                // bind the ellipses to the rest of the statement
                MacroPattern::Many(ident) => {
                    let rest: Vec<Rc<Expr>> = token_iter.collect();
                    bindings.insert(ident.to_string(), Rc::new(Expr::VectorVal(rest)));
                    break;
                }
                MacroPattern::Nested(children) => {
                    let child = token_iter
                        .next()
                        .ok_or_else(throw!(ArityMismatch => "Macro expected a pattern"))?;

                    let child_vec = child.as_ref().vector_val_or_else(
                        throw!(BadSyntax => "macro expected a vector of values"),
                    )?;

                    Self::collect_bindings(&children, child_vec, bindings)?;
                }
            }
        }

        Ok(())
    }

    // let recursive macros handle themselves - only expand the case that it can and then move on
    fn replace_exprs_in_body(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        let mut bindings: HashMap<String, Rc<Expr>> = HashMap::new();
        Self::collect_bindings(&self.args, list_of_tokens, &mut bindings)?;
        Self::recursive_replace(Rc::clone(&self.body), &bindings)
    }

    pub fn arity(&self) -> usize {
        self.args
            .iter()
            .map(|x| if let MacroPattern::Many(_) = x { 2 } else { 1 })
            .sum()
        // self.args.len()
    }

    // TODO also fix this
    pub fn has_ellipses(&self) -> bool {
        self.args
            .iter()
            .find(|x| {
                if let MacroPattern::Many(_) = x {
                    true
                } else {
                    false
                }
            })
            .is_some()
    }

    fn check_ellipses(expr: &Rc<Expr>) -> bool {
        let expr = Rc::clone(expr);
        if let Expr::Atom(t) = expr.as_ref() {
            if let Identifier(s) = t {
                s == "..."
            } else {
                false
            }
        } else {
            false
        }
    }

    // walk through the expression and replace all of the bindings with the expressions
    fn recursive_replace(expr: Rc<Expr>, bindings: &HashMap<String, Rc<Expr>>) -> Result<Rc<Expr>> {
        match expr.as_ref() {
            Expr::Atom(t) => {
                if let Identifier(s) = t {
                    if let Some(body) = bindings.get(s) {
                        Ok(Rc::clone(body))
                    } else {
                        Ok(expr)
                    }
                } else {
                    Ok(expr)
                }
            }
            Expr::VectorVal(vec_exprs) => {
                let mut vec_exprs = vec_exprs.clone();

                // TODO find this issue
                // Go to the position before the ellipses, look up that variable, insert all the expressions
                // you can there
                // find where the "..." is and insert all of the expressions there first
                if let Some(ellipses_pos) = vec_exprs.iter().position(Self::check_ellipses) {
                    let variable_to_lookup = vec_exprs.get(ellipses_pos - 1).ok_or_else(
                        throw!(BadSyntax => "macro expansion failed, could not find variable"),
                    )?; // TODO

                    let rest = bindings
                        .get(variable_to_lookup.as_ref().atom_identifier_or_else(
                            throw!(BadSyntax => "macro expansion failed"),
                        )?)
                        .ok_or_else(throw!(BadSyntax => "macro expansion failed"))?;

                    let list_of_exprs = rest.as_ref().vector_val_or_else(
                        throw!(BadSyntax => "macro expansion failed, expected list of expressions"),
                    )?;

                    let mut first_chunk = vec_exprs[0..ellipses_pos - 1].to_vec();
                    first_chunk.extend_from_slice(list_of_exprs);
                    first_chunk.extend_from_slice(&vec_exprs[(ellipses_pos + 1)..]);
                    vec_exprs = first_chunk;
                }

                Ok(Rc::new(Expr::VectorVal(
                    vec_exprs
                        .into_iter()
                        .map(|x| Self::recursive_replace(x, &bindings))
                        .collect::<Result<Vec<Rc<Expr>>>>()?,
                )))
            }
        }
    }
}

// rename identifiers used inside the macro as ##identifier
#[derive(Clone, Debug)]
pub struct SteelMacro {
    name: String,
    special_forms: Vec<String>,
    cases: Vec<MacroCase>,
}

impl SteelMacro {
    pub fn new(name: String, special_forms: Vec<String>, cases: Vec<MacroCase>) -> SteelMacro {
        SteelMacro {
            name,
            special_forms,
            cases,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    fn parse_syntax_rules(
        macro_name: String,
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<SteelMacro> {
        // unimplemented!()
        // cannot check arity, only minimum
        if list_of_tokens.len() < 2 {
            stop!(ArityMismatch => "syntax-rules expected at least 3 arguments")
        }

        let mut token_iter = list_of_tokens.iter();
        let mut special_forms_vec: Vec<String> = Vec::new();
        let mut cases_vec: Vec<MacroCase> = Vec::new();

        let name = token_iter.next().ok_or_else(
            throw!(ArityMismatch => "syntax-rules expected an identifier in the first argument"),
        )?.as_ref().atom_identifier_or_else(
            throw!(TypeMismatch => "syntax-rules expected an identifier in the first argument"),
        )?;

        // make sure that we have found "syntax-rules" at the start of this
        if name != "syntax-rules" {
            stop!(BadSyntax => "macro-expansion failed: expected syntax-rules")
        }

        let list_of_idents = token_iter
            .next()
            .ok_or_else(throw!(ArityMismatch => "syntax-rules expected a list of identifiers"))?
            .as_ref()
            .vector_val_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;

        // parse each special form inside the syntax-rules
        for special_form in list_of_idents {
            let name = special_form.as_ref().atom_identifier_or_else(
                throw!(TypeMismatch => "syntax-rules expected a list of identifiers"),
            )?;
            special_forms_vec.push(name.to_string())
        }

        // walk through cases and parse each individually
        while let Some(next_case) = token_iter.next() {
            cases_vec.push(MacroCase::parse_from_tokens(
                &macro_name,
                &special_forms_vec,
                next_case
                    .as_ref()
                    .vector_val_or_else(throw!(BadSyntax => "syntax-rules expected a pattern"))?,
                env,
            )?);
        }

        Ok(SteelMacro::new(macro_name, special_forms_vec, cases_vec))
    }

    // TODO
    pub fn parse_from_tokens(
        list_of_tokens: &[Rc<Expr>],
        env: &Rc<RefCell<Env>>,
    ) -> Result<SteelMacro> {
        if list_of_tokens.len() != 2 {
            stop!(ArityMismatch => "define-syntax takes 2 arguments, the name and the syntax-rules")
        }

        if let [name_expr, syntax_rules_expr] = list_of_tokens {
            let name = name_expr.as_ref().atom_identifier_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            let syntax_rules_tokens = syntax_rules_expr.as_ref().vector_val_or_else(
                throw!(TypeMismatch => "define-syntax expected a syntax-rules in the second position"),
            )?;

            Self::parse_syntax_rules(name.to_string(), syntax_rules_tokens, env)
        } else {
            stop!(ArityMismatch => "define-syntax expected 2 arguments")
        }
    }

    // TODO
    // its worth a shot
    fn match_case(&self, list_of_tokens: &[Rc<Expr>]) -> Result<&MacroCase> {
        for case in &self.cases {
            // println!("{}, {}", list_of_tokens.len(), case.arity());
            // TODO this should actually be `case.arity() - num_ellipses_in_top_level`
            if (case.has_ellipses() && list_of_tokens.len() >= (case.arity() - 1))
                || case.arity() == list_of_tokens.len()
            {
                // println!("got inside the if");
                if case.recursive_match(list_of_tokens) {
                    return Ok(case);
                }
            }
        }
        stop!(ArityMismatch => "macro expansion could not match case")
    }

    fn is_reserved_keyword(word: &str) -> bool {
        // unimplemented!()
        match word {
            "lambda" | "define" | "map'" | "filter'" | "and" | "or" | "define-syntax-rule"
            | "eval" | "set!" | "let" | "begin" | "if" | "quote" | "..." => true,
            _ => false,
        }
    }

    pub fn expand(&self, list_of_tokens: &[Rc<Expr>]) -> Result<Rc<Expr>> {
        let case_to_expand = self.match_case(list_of_tokens)?;
        case_to_expand.expand(list_of_tokens)
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
        MacroV(_) => write!(f, "#<macro>"),
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

#[test]
fn display_test() {
    use crate::parser::tokens::Token;
    use im_rc::vector;
    assert_eq!(SteelVal::BoolV(false).to_string(), "#false");
    assert_eq!(SteelVal::NumV(1.0).to_string(), "1");
    assert_eq!(
        SteelVal::FuncV(|_args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            Ok(Rc::new(SteelVal::VectorV(vector![])))
        })
        .to_string(),
        "#<function>"
    );
    assert_eq!(
        SteelVal::LambdaV(SteelLambda::new(
            vec!["arg1".to_owned()],
            Rc::new(Expr::Atom(Token::NumberLiteral(1.0))),
            Rc::new(RefCell::new(crate::env::Env::default_env())),
        ))
        .to_string(),
        "#<lambda-function>"
    );
    assert_eq!(SteelVal::SymbolV("foo".to_string()).to_string(), "'foo");
}

#[test]
fn display_list_test() {
    use crate::parser::tokens::Token;
    use im_rc::vector;
    assert_eq!(VectorV(vector![]).to_string(), "'#()");
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
        "'#(#false 1 #<lambda-function>)"
    );
    assert_eq!(
        VectorV(vector![
            VectorV(vector![NumV(1.0), VectorV(vector!(NumV(2.0), NumV(3.0)))]),
            VectorV(vector![NumV(4.0), NumV(5.0)]),
            NumV(6.0),
            VectorV(vector![NumV(7.0)])
        ])
        .to_string(),
        "'#((1 (2 3)) (4 5) 6 (7))"
    );
}
