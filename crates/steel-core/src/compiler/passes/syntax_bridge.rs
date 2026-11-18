//! The program as Steel syntax objects and back, for compiler passes written in
//! Steel (STEEL_PASSES_IN_STEEL_PLAN.md).
//!
//! Going to syntax, every node becomes a syntax object carrying an id, and
//! the bridge remembers what the syntax can't show: an atom's exact token and
//! flags, whether a list is improper, a lambda's rest and keyword flags, and
//! nodes passes don't look into (`require`, macros), which travel as opaque
//! atoms. Coming back, syntax that still has its id gets its node back
//! exactly; syntax a pass made, or changed, becomes new nodes with fresh ids.
//!
//! Unlike the macro converters (`parser/tryfrom_visitor.rs`) this loses
//! nothing and copies nothing per level: the datum of a form is made lazily.

use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use rustc_hash::{FxHashMap, FxHashSet};
use steel_parser::ast::{
    Atom, Begin, Define, ExprKind, If, LambdaFunction, Let, List, Quote, Return, Set, Vector,
};
use steel_parser::parser::{SyntaxObject, SyntaxObjectId};
use steel_parser::span::Span;
use steel_parser::tokens::{IntLiteral, RealLiteral, TokenType};
use thin_vec::ThinVec;

use crate::gc::Gc;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal, Syntax};
use steel_parser::interner::InternedString;

#[derive(Clone)]
enum Form {
    List { improper: bool },
    Vector { bytes: bool },
    Let,
    Lambda { rest: bool, kwargs: bool },
    // if, define, begin, set!, quote, return!, by which one it was: generated
    // code can give a node another form's keyword as its location
    Keyword(TokenType<InternedString>),
    // Passed through untouched: require, and macro definitions
    Opaque(ExprKind),
}

// A form's syntax carries a key of the bridge's own, not the node's id: the AST
// can give different nodes the same id (a `let` made from an applied `lambda`
// keeps the lambda's), so keying by it would mix them up.
#[derive(Clone)]
struct FormEntry {
    form: Form,
    // The `let`, `lambda` or list node's own id
    node_id: u32,
    // A core form's keyword as the program had it. Passes see the form's kind
    // as its head instead (`let`, never the `lambda` a let can carry).
    location: Option<SyntaxObject>,
}

#[derive(Default)]
pub(crate) struct SyntaxBridge {
    // Atoms by id: the syntax object, and its datum as Steel code sees it.
    // Copies made by inlining can share an id, hence more than one.
    atoms: FxHashMap<u32, Vec<(SyntaxObject, SteelVal)>>,
    forms: FxHashMap<u32, FormEntry>,
    // How often each form's key has come back: a copy a pass made gets a
    // fresh node id
    form_uses: FxHashMap<u32, u32>,
    // How often each id occurred in the program, and how often it has been
    // handed out again: a pass that copies a node gets fresh ids for the
    // copies, while copies the program already had keep theirs
    occurrences: FxHashMap<u32, u32>,
    handed_out: FxHashMap<u32, u32>,
}

fn fresh() -> u32 {
    SyntaxObjectId::fresh().0
}

// A copy that keeps the id (`clone` gives a syntax object a fresh one)
fn same_id(s: &SyntaxObject) -> SyntaxObject {
    let mut copy = s.clone();
    copy.syntax_object_id = s.syntax_object_id;
    copy
}

fn conversion_error(message: String) -> SteelErr {
    SteelErr::new(ErrorKind::ConversionError, message)
}

fn syntax(value: SteelVal, span: Span) -> Syntax {
    Syntax::new(value, span)
}

fn spans_of(items: &[SteelVal]) -> Span {
    let spans: Vec<Span> = items
        .iter()
        .filter_map(|item| match item {
            SteelVal::SyntaxObject(s) => Some(s.syntax_loc()),
            _ => None,
        })
        .collect();
    if spans.is_empty() {
        Span::default()
    } else {
        Span::coalesce_span(&spans)
    }
}

impl SyntaxBridge {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// The program as a list of syntax objects
    pub(crate) fn encode_program(&mut self, exprs: &[ExprKind]) -> Result<SteelVal> {
        let items: Result<crate::values::lists::List<SteelVal>> =
            exprs.iter().map(|expr| self.encode(expr)).collect();
        Ok(SteelVal::ListV(items?))
    }

    /// Back from what `to_syntax` made, or what a pass made of it
    pub(crate) fn decode_program(&mut self, value: &SteelVal) -> Result<Vec<ExprKind>> {
        let SteelVal::ListV(items) = value else {
            return Err(conversion_error(format!(
                "a compiler pass has to return a list of syntax objects, not {value}"
            )));
        };
        items.iter().map(|item| self.decode(item)).collect()
    }

    /// A form key's node id (`let`, `lambda` and application nodes have one,
    /// 0 for others) and, for a lambda, whether it takes a rest argument
    pub(crate) fn form_node(&self, key: u32) -> Option<(u32, Option<bool>)> {
        self.forms.get(&key).map(|entry| {
            let rest = match entry.form {
                Form::Lambda { rest, .. } => Some(rest),
                _ => None,
            };
            (entry.node_id, rest)
        })
    }

    /// `decode_program`, leaving the bridge as it was: for a look at what a
    /// pass in the middle of a chain was given, before the chain's real decode
    pub(crate) fn decode_detached(&mut self, value: &SteelVal) -> Result<Vec<ExprKind>> {
        let form_uses = self.form_uses.clone();
        let handed_out = self.handed_out.clone();
        let decoded = self.decode_program(value);
        self.form_uses = form_uses;
        self.handed_out = handed_out;
        decoded
    }

    fn count(&mut self, id: u32) {
        *self.occurrences.entry(id).or_insert(0) += 1;
    }

    fn hand_out(&mut self, id: u32) -> u32 {
        let handed_out = self.handed_out.entry(id).or_insert(0);
        *handed_out += 1;
        if *handed_out > self.occurrences.get(&id).copied().unwrap_or(0) {
            fresh()
        } else {
            id
        }
    }

    fn form(&mut self, form: Form, node_id: u32, value: SteelVal, span: Span) -> Syntax {
        self.core_form(form, node_id, None, value, span)
    }

    fn core_form(
        &mut self,
        form: Form,
        node_id: u32,
        location: Option<SyntaxObject>,
        value: SteelVal,
        span: Span,
    ) -> Syntax {
        let key = fresh();
        self.forms.insert(
            key,
            FormEntry {
                form,
                node_id,
                location,
            },
        );
        syntax(value, span).with_id(key)
    }

    // A core form's head: its kind's name, at its keyword's place
    fn head(&mut self, kind: &TokenType<InternedString>, location: &SyntaxObject) -> SteelVal {
        let id = location.syntax_object_id.0;
        self.count(id);
        syntax(SteelVal::SymbolV(kind_name(kind).into()), location.span)
            .with_scopes(location.scopes.clone())
            .with_id(id)
            .into()
    }

    // The node id for a form coming back: its own the first time, fresh for copies
    fn node_id(&mut self, key: Option<u32>) -> u32 {
        let Some(key) = key else {
            return fresh();
        };
        let uses = self.form_uses.entry(key).or_insert(0);
        *uses += 1;
        match self.forms.get(&key) {
            Some(entry) if *uses == 1 => entry.node_id,
            _ => fresh(),
        }
    }

    // A form made of a keyword and its parts: `(if test then else)`
    fn keyword_form(
        &mut self,
        kind: TokenType<InternedString>,
        keyword: &SyntaxObject,
        parts: &[&ExprKind],
    ) -> Result<SteelVal> {
        let mut items = Vec::with_capacity(parts.len() + 1);
        items.push(self.head(&kind, keyword));
        for part in parts {
            items.push(self.encode(part)?);
        }
        Ok(self
            .core_form(
                Form::Keyword(kind),
                0,
                Some(same_id(keyword)),
                SteelVal::ListV(items.into()),
                keyword.span,
            )
            .into())
    }

    fn atom(&mut self, s: &SyntaxObject) -> Result<SteelVal> {
        let datum = SteelVal::try_from(s.ty.clone()).map_err(|err| err.with_span(s.span))?;
        let id = s.syntax_object_id.0;
        self.count(id);
        let seen = self.atoms.entry(id).or_default();
        if !seen.iter().any(|(original, _)| same_atom(original, s)) {
            seen.push((s.clone(), datum.clone()));
        }
        Ok(syntax(datum, s.span)
            .with_scopes(s.scopes.clone())
            .with_id(id)
            .into())
    }

    fn encode_all<'a>(
        &mut self,
        exprs: impl Iterator<Item = &'a ExprKind>,
    ) -> Result<Vec<SteelVal>> {
        exprs.map(|expr| self.encode(expr)).collect()
    }

    fn encode(&mut self, expr: &ExprKind) -> Result<SteelVal> {
        steel_parser::visitors::with_stack(|| self.encode_expr(expr))
    }

    fn encode_expr(&mut self, expr: &ExprKind) -> Result<SteelVal> {
        match expr {
            ExprKind::Atom(a) => self.atom(&a.syn),
            ExprKind::If(f) => self.keyword_form(
                TokenType::If,
                &f.location,
                &[&f.test_expr, &f.then_expr, &f.else_expr],
            ),
            ExprKind::Define(d) => {
                self.keyword_form(TokenType::Define, &d.location, &[&d.name, &d.body])
            }
            ExprKind::Begin(b) => {
                let parts: Vec<&ExprKind> = b.exprs.iter().collect();
                self.keyword_form(TokenType::Begin, &b.location, &parts)
            }
            ExprKind::Set(s) => {
                self.keyword_form(TokenType::Set, &s.location, &[&s.variable, &s.expr])
            }
            ExprKind::Quote(q) => self.keyword_form(TokenType::Quote, &q.location, &[&q.expr]),
            ExprKind::Return(r) => self.keyword_form(TokenType::Return, &r.location, &[&r.expr]),
            ExprKind::Let(l) => {
                let keyword = self.head(&TokenType::Let, &l.location);
                let mut bindings = Vec::with_capacity(l.bindings.len());
                for (name, value) in &l.bindings {
                    let pair = vec![self.encode(name)?, self.encode(value)?];
                    let span = spans_of(&pair);
                    bindings.push(syntax(SteelVal::ListV(pair.into()), span).into());
                }
                let span = spans_of(&bindings);
                let bindings = syntax(SteelVal::ListV(bindings.into()), span).into();
                let body = self.encode(&l.body_expr)?;
                Ok(self
                    .core_form(
                        Form::Let,
                        l.syntax_object_id,
                        Some(same_id(&l.location)),
                        SteelVal::ListV(vec![keyword, bindings, body].into()),
                        l.location.span,
                    )
                    .into())
            }
            ExprKind::LambdaFunction(l) => {
                let keyword = self.head(&TokenType::Lambda, &l.location);
                let args = self.encode_all(l.args.iter())?;
                let span = spans_of(&args);
                let args = syntax(SteelVal::ListV(args.into()), span).into();
                let body = self.encode(&l.body)?;
                Ok(self
                    .core_form(
                        Form::Lambda {
                            rest: l.rest,
                            kwargs: l.kwargs,
                        },
                        l.syntax_object_id,
                        Some(same_id(&l.location)),
                        SteelVal::ListV(vec![keyword, args, body].into()),
                        l.location.span,
                    )
                    .into())
            }
            ExprKind::List(l) => {
                let items = self.encode_all(l.args.iter())?;
                Ok(self
                    .form(
                        Form::List {
                            improper: l.improper,
                        },
                        l.syntax_object_id,
                        SteelVal::ListV(items.into()),
                        l.location,
                    )
                    .with_scopes(l.scopes.clone())
                    .into())
            }
            ExprKind::Vector(v) => {
                let items = self.encode_all(v.args.iter())?;
                let value = SteelVal::VectorV(
                    Gc::new(items.into_iter().collect::<crate::values::Vector<_>>()).into(),
                );
                Ok(self
                    .form(Form::Vector { bytes: v.bytes }, 0, value, v.span)
                    .into())
            }
            ExprKind::Require(_) | ExprKind::Macro(_) | ExprKind::SyntaxRules(_) => {
                // An atom to a pass, which can move it but not look inside
                Ok(self
                    .form(
                        Form::Opaque(expr.clone()),
                        0,
                        SteelVal::SymbolV("#%opaque-ast".into()),
                        Span::default(),
                    )
                    .into())
            }
        }
    }

    fn decode(&mut self, value: &SteelVal) -> Result<ExprKind> {
        steel_parser::visitors::with_stack(|| self.decode_value(value))
    }

    fn decode_value(&mut self, value: &SteelVal) -> Result<ExprKind> {
        match value {
            SteelVal::SyntaxObject(s) => self.decode_syntax(s),
            // A plain datum a pass put in
            SteelVal::ListV(items) => {
                let args: Result<ThinVec<ExprKind>> =
                    items.iter().map(|item| self.decode(item)).collect();
                Ok(ExprKind::List(List::new(args?)))
            }
            other => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                token_for(other)?,
                Span::default(),
            )))),
        }
    }

    fn decode_syntax(&mut self, s: &Syntax) -> Result<ExprKind> {
        let span = s.syntax_loc();
        let form = s
            .id()
            .and_then(|key| self.forms.get(&key).map(|entry| entry.form.clone()));

        if let Some(Form::Opaque(expr)) = form {
            return Ok(expr);
        }

        match &s.syntax {
            SteelVal::SyntaxObject(inner) => self.decode_syntax(inner),
            SteelVal::ListV(items) => self.decode_list(s, items, form),
            SteelVal::VectorV(items) => {
                let args: Result<ThinVec<ExprKind>> =
                    items.iter().map(|item| self.decode(item)).collect();
                Ok(ExprKind::Vector(Vector {
                    args: args?,
                    bytes: matches!(form, Some(Form::Vector { bytes: true })),
                    span,
                }))
            }
            datum => Ok(ExprKind::Atom(Atom::new(self.decode_atom(
                s.id(),
                datum,
                span,
                s.scopes(),
            )?))),
        }
    }

    fn decode_atom(
        &mut self,
        id: Option<u32>,
        datum: &SteelVal,
        span: Span,
        scopes: steel_parser::scopes::ScopeSetId,
    ) -> Result<SyntaxObject> {
        if let Some(id) = id {
            let original = self.atoms.get(&id).and_then(|seen| {
                seen.iter()
                    .find(|(original, original_datum)| {
                        same_datum(original_datum, datum) && original.span == span
                    })
                    .map(|(original, _)| original.clone())
            });
            if let Some(mut original) = original {
                original.scopes = scopes;
                original.syntax_object_id = SyntaxObjectId(self.hand_out(id));
                return Ok(original);
            }
        }
        // New, or changed by a pass
        let mut syn = SyntaxObject::new(token_for(datum)?, span);
        syn.scopes = scopes;
        Ok(syn)
    }

    fn decode_list(
        &mut self,
        s: &Syntax,
        items: &crate::values::lists::List<SteelVal>,
        form: Option<Form>,
    ) -> Result<ExprKind> {
        match form {
            Some(Form::Let) | Some(Form::Lambda { .. }) | Some(Form::Keyword(_)) => {
                self.decode_core(s, items, form)
            }
            // A core form when its head names one and a pass wrote it: the
            // program's own heads are tokens, not symbols
            _ if items.iter().next().is_some_and(|head| self.names_core_form(head)) => {
                self.decode_core(s, items, None)
            }
            Some(Form::List { improper }) => {
                let args: Result<ThinVec<ExprKind>> =
                    items.iter().map(|item| self.decode(item)).collect();
                let id = self.node_id(s.id());
                Ok(ExprKind::List(List {
                    args: args?,
                    syntax_object_id: id,
                    improper,
                    location: s.syntax_loc(),
                    scopes: s.scopes(),
                }))
            }
            Some(Form::Vector { .. }) | Some(Form::Opaque(_)) | None => {
                let args: Result<ThinVec<ExprKind>> =
                    items.iter().map(|item| self.decode(item)).collect();
                let mut list = List::new(args?);
                list.location = s.syntax_loc();
                list.scopes = s.scopes();
                Ok(ExprKind::List(list))
            }
        }
    }

    // A symbol naming a core form that isn't one of the program's atoms
    fn names_core_form(&self, head: &SteelVal) -> bool {
        let from_program = match head {
            SteelVal::SyntaxObject(h) => h.id().is_some_and(|id| self.atoms.contains_key(&id)),
            _ => false,
        };
        !from_program && syntax_symbol(head).is_some_and(|name| core_keyword(&name).is_some())
    }

    fn decode_core(
        &mut self,
        s: &Syntax,
        items: &crate::values::lists::List<SteelVal>,
        form: Option<Form>,
    ) -> Result<ExprKind> {
        let items: Vec<&SteelVal> = items.iter().collect();
        let Some(head) = items.first() else {
            return Err(conversion_error("an empty form".into()));
        };

        let entry = s.id().and_then(|key| self.forms.get(&key));
        let entry_kind = entry.and_then(|entry| match &entry.form {
            Form::Let => Some(TokenType::Let),
            Form::Lambda { .. } => Some(TokenType::Lambda),
            Form::Keyword(kind) => Some(kind.clone()),
            _ => None,
        });
        let entry_location = entry.and_then(|entry| entry.location.as_ref());
        let head_name = syntax_symbol(head);
        let head_kind = head_name.as_deref().and_then(core_keyword);

        // The head the bridge made, still naming the form's kind: the node's
        // own keyword comes back, whatever token it was
        let original = match (head, entry_location, &entry_kind) {
            (SteelVal::SyntaxObject(h), Some(location), Some(kind))
                if head_kind.as_ref() == Some(kind)
                    && Some(location.syntax_object_id.0) == h.id()
                    && location.span == h.syntax_loc() =>
            {
                let mut location = same_id(location);
                location.scopes = h.scopes();
                Some(location)
            }
            _ => None,
        };
        let original = original.map(|mut location| {
            location.syntax_object_id = SyntaxObjectId(self.hand_out(location.syntax_object_id.0));
            location
        });

        let (kind, location) = match original {
            Some(location) => (entry_kind.clone().unwrap(), location),
            None => {
                let Some(kind) = head_kind else {
                    let head = match head {
                        SteelVal::SyntaxObject(h) => h.syntax_datum(),
                        other => (*other).clone(),
                    };
                    return Err(conversion_error(format!(
                        "{head} isn't a form a pass can write"
                    )));
                };
                let (span, scopes) = match head {
                    SteelVal::SyntaxObject(h) => (h.syntax_loc(), h.scopes()),
                    _ => (s.syntax_loc(), s.scopes()),
                };
                let mut location = SyntaxObject::new(kind.clone(), span);
                location.scopes = scopes;
                (kind, location)
            }
        };
        // The form's flags and id only if it's still the same kind of form
        let same_kind = entry_kind.as_ref() == Some(&kind);
        let form = if same_kind { form } else { None };

        let arity = |wanted: usize| -> Result<()> {
            if items.len() == wanted {
                Ok(())
            } else {
                let mut form = s.syntax_datum().to_string();
                if form.chars().count() > 120 {
                    form = form.chars().take(120).collect::<String>() + " ...";
                }
                Err(conversion_error(format!(
                    "{} takes {} parts, a pass gave it {}: {}",
                    kind,
                    wanted - 1,
                    items.len() - 1,
                    form
                )))
            }
        };

        let node_id = |bridge: &mut Self| {
            if same_kind {
                bridge.node_id(s.id())
            } else {
                fresh()
            }
        };

        match &kind {
            TokenType::If => {
                arity(4)?;
                Ok(ExprKind::If(Box::new(If {
                    test_expr: self.decode(items[1])?,
                    then_expr: self.decode(items[2])?,
                    else_expr: self.decode(items[3])?,
                    location,
                })))
            }
            TokenType::Define => {
                arity(3)?;
                Ok(ExprKind::Define(Box::new(Define {
                    name: self.decode(items[1])?,
                    body: self.decode(items[2])?,
                    location,
                })))
            }
            TokenType::Begin => {
                let exprs: Result<Vec<ExprKind>> =
                    items[1..].iter().map(|item| self.decode(item)).collect();
                Ok(ExprKind::Begin(Box::new(Begin {
                    exprs: exprs?,
                    location,
                })))
            }
            TokenType::Set => {
                arity(3)?;
                Ok(ExprKind::Set(Box::new(Set {
                    variable: self.decode(items[1])?,
                    expr: self.decode(items[2])?,
                    location,
                })))
            }
            TokenType::Quote => {
                arity(2)?;
                Ok(ExprKind::Quote(Box::new(Quote {
                    expr: self.decode(items[1])?,
                    location,
                })))
            }
            TokenType::Return => {
                arity(2)?;
                Ok(ExprKind::Return(Box::new(Return {
                    expr: self.decode(items[1])?,
                    location,
                })))
            }
            TokenType::Let => {
                arity(3)?;
                let mut bindings = Vec::new();
                for pair in list_items(items[1])? {
                    let pair = list_items(&pair)?;
                    if pair.len() != 2 {
                        return Err(conversion_error(
                            "a let binding takes a name and a value".into(),
                        ));
                    }
                    bindings.push((self.decode(&pair[0])?, self.decode(&pair[1])?));
                }
                let body_expr = self.decode(items[2])?;
                Ok(ExprKind::Let(Box::new(Let {
                    bindings,
                    body_expr,
                    location,
                    syntax_object_id: node_id(self),
                })))
            }
            TokenType::Lambda => {
                arity(3)?;
                let args: Result<ThinVec<ExprKind>> = list_items(items[1])?
                    .iter()
                    .map(|arg| self.decode(arg))
                    .collect();
                let (rest, kwargs) = match form {
                    Some(Form::Lambda { rest, kwargs }) => (rest, kwargs),
                    _ => (false, false),
                };
                let body = self.decode(items[2])?;
                Ok(ExprKind::LambdaFunction(Box::new(LambdaFunction {
                    args: args?,
                    body,
                    location,
                    rest,
                    syntax_object_id: node_id(self),
                    kwargs,
                })))
            }
            other => Err(conversion_error(format!(
                "{other} isn't a form a pass can write"
            ))),
        }
    }
}

// The items of a list, whether syntax or a plain list
fn list_items(value: &SteelVal) -> Result<Vec<SteelVal>> {
    match value {
        SteelVal::SyntaxObject(s) => list_items(&s.syntax),
        SteelVal::ListV(items) => Ok(items.iter().cloned().collect()),
        other => Err(conversion_error(format!("expected a list, found {other}"))),
    }
}

fn syntax_symbol(value: &SteelVal) -> Option<String> {
    match value {
        SteelVal::SyntaxObject(s) => syntax_symbol(&s.syntax),
        SteelVal::SymbolV(name) => Some(name.as_str().into()),
        _ => None,
    }
}

fn kind_name(kind: &TokenType<InternedString>) -> &'static str {
    match kind {
        TokenType::If => "if",
        TokenType::Define => "define",
        TokenType::Let => "let",
        TokenType::Lambda => "lambda",
        TokenType::Begin => "begin",
        TokenType::Set => "set!",
        TokenType::Quote => "quote",
        TokenType::Return => "return!",
        other => unreachable!("{other} isn't a core form"),
    }
}

fn core_keyword(name: &str) -> Option<TokenType<InternedString>> {
    Some(match name {
        "if" => TokenType::If,
        "define" => TokenType::Define,
        "let" => TokenType::Let,
        "lambda" | "λ" => TokenType::Lambda,
        "begin" => TokenType::Begin,
        "set!" => TokenType::Set,
        "quote" => TokenType::Quote,
        "return!" => TokenType::Return,
        _ => return None,
    })
}

// The token for a datum a pass made
fn token_for(datum: &SteelVal) -> Result<TokenType<InternedString>> {
    Ok(match datum {
        SteelVal::BoolV(b) => TokenType::BooleanLiteral(*b),
        SteelVal::IntV(i) => RealLiteral::Int(IntLiteral::Small(*i)).into(),
        SteelVal::NumV(n) => RealLiteral::Float((*n).into()).into(),
        SteelVal::StringV(s) => TokenType::StringLiteral(s.as_str().into()),
        SteelVal::CharV(c) => TokenType::CharacterLiteral(*c),
        // As the reader gives them: a keyword is a symbol starting with `#:`
        SteelVal::SymbolV(s) if s.starts_with("#:") => TokenType::Keyword(s.as_str().into()),
        SteelVal::SymbolV(s) => TokenType::Identifier(s.as_str().into()),
        other => {
            return Err(conversion_error(format!(
                "a compiler pass can't put {other} in the program"
            )))
        }
    })
}

// `==`, except a float is the same as itself even when it's NaN, in a
// complex number too
fn same_datum(a: &SteelVal, b: &SteelVal) -> bool {
    match (a, b) {
        (SteelVal::NumV(x), SteelVal::NumV(y)) => x.to_bits() == y.to_bits(),
        (SteelVal::Complex(x), SteelVal::Complex(y)) => {
            same_datum(&x.re, &y.re) && same_datum(&x.im, &y.im)
        }
        _ => a == b,
    }
}

fn same_atom(a: &SyntaxObject, b: &SyntaxObject) -> bool {
    a.ty == b.ty
        && a.span == b.span
        && a.unresolved == b.unresolved
        && a.introduced_via_macro == b.introduced_via_macro
        && a.scopes == b.scopes
}

/// The first place `a` and `b` differ, counting ids, spans, flags and scopes,
/// which `ExprKind`'s `==` mostly doesn't
pub(crate) fn first_difference(a: &[ExprKind], b: &[ExprKind]) -> Option<String> {
    program_difference(a, b, &Compare::exact())
}

/// The first place `a` and `b` differ as programs: tokens and structure, not
/// ids, spans, scopes or keyword tokens. How a Rust pass and its Steel twin
/// are compared. Identifiers `input` doesn't have are names the passes made
/// up: they only have to correspond one to one (`##%car-hoist7` in one,
/// `##%car-hoist912` in the other).
pub(crate) fn shape_difference(input: &[ExprKind], a: &[ExprKind], b: &[ExprKind]) -> Option<String> {
    program_difference(a, b, &Compare::shape(identifier_names(input)))
}

pub(crate) struct Compare {
    exact: bool,
    // Identifiers in the input, for shape comparisons
    input: FxHashSet<InternedString>,
    // Made up names, a's to b's and back
    renames: core::cell::RefCell<(
        FxHashMap<InternedString, InternedString>,
        FxHashMap<InternedString, InternedString>,
    )>,
}

impl Compare {
    fn exact() -> Self {
        Compare {
            exact: true,
            input: FxHashSet::default(),
            renames: Default::default(),
        }
    }

    fn shape(input: FxHashSet<InternedString>) -> Self {
        Compare {
            exact: false,
            input,
            renames: Default::default(),
        }
    }

    fn same_token(&self, a: &TokenType<InternedString>, b: &TokenType<InternedString>) -> bool {
        if self.exact {
            return a == b;
        }
        match (a, b) {
            (TokenType::Identifier(x), TokenType::Identifier(y))
                if !self.input.contains(x) && !self.input.contains(y) =>
            {
                let mut renames = self.renames.borrow_mut();
                let (forward, back) = &mut *renames;
                match (forward.get(x), back.get(y)) {
                    (None, None) => {
                        forward.insert(*x, *y);
                        back.insert(*y, *x);
                        true
                    }
                    (Some(to), Some(from)) => to == y && from == x,
                    _ => false,
                }
            }
            _ => a == b,
        }
    }
}

/// Every identifier in `exprs`
pub(crate) fn identifier_names(exprs: &[ExprKind]) -> FxHashSet<InternedString> {
    fn walk(expr: &ExprKind, out: &mut FxHashSet<InternedString>) {
        steel_parser::visitors::with_stack(|| match expr {
            ExprKind::Atom(a) => {
                if let TokenType::Identifier(name) = &a.syn.ty {
                    out.insert(*name);
                }
            }
            ExprKind::If(f) => {
                walk(&f.test_expr, out);
                walk(&f.then_expr, out);
                walk(&f.else_expr, out);
            }
            ExprKind::Define(d) => {
                walk(&d.name, out);
                walk(&d.body, out);
            }
            ExprKind::Begin(b) => b.exprs.iter().for_each(|e| walk(e, out)),
            ExprKind::Set(t) => {
                walk(&t.variable, out);
                walk(&t.expr, out);
            }
            ExprKind::Quote(q) => walk(&q.expr, out),
            ExprKind::Return(r) => walk(&r.expr, out),
            ExprKind::Let(l) => {
                for (name, value) in &l.bindings {
                    walk(name, out);
                    walk(value, out);
                }
                walk(&l.body_expr, out);
            }
            ExprKind::LambdaFunction(l) => {
                l.args.iter().for_each(|e| walk(e, out));
                walk(&l.body, out);
            }
            ExprKind::List(l) => l.args.iter().for_each(|e| walk(e, out)),
            ExprKind::Vector(v) => v.args.iter().for_each(|e| walk(e, out)),
            ExprKind::Require(_) | ExprKind::Macro(_) | ExprKind::SyntaxRules(_) => {}
        })
    }
    let mut out = FxHashSet::default();
    exprs.iter().for_each(|e| walk(e, &mut out));
    out
}

fn program_difference(a: &[ExprKind], b: &[ExprKind], cx: &Compare) -> Option<String> {
    if a.len() != b.len() {
        return Some(format!("{} top level forms vs {}", a.len(), b.len()));
    }
    a.iter()
        .zip(b)
        .enumerate()
        .find_map(|(i, (a, b))| difference(a, b, &format!("[{i}]"), cx))
}

fn atom_difference(a: &SyntaxObject, b: &SyntaxObject, path: &str, cx: &Compare) -> Option<String> {
    if !cx.same_token(&a.ty, &b.ty) {
        return Some(format!("{path}: token {} vs {}", a.ty, b.ty));
    }
    if !cx.exact {
        return None;
    }
    if a.syntax_object_id != b.syntax_object_id {
        return Some(format!(
            "{path} ({}): id {:?} vs {:?}",
            a.ty, a.syntax_object_id, b.syntax_object_id
        ));
    }
    if a.span != b.span {
        return Some(format!(
            "{path} ({}): span {:?} vs {:?}",
            a.ty, a.span, b.span
        ));
    }
    if a.unresolved != b.unresolved || a.introduced_via_macro != b.introduced_via_macro {
        return Some(format!("{path} ({}): flags differ", a.ty));
    }
    if a.scopes != b.scopes {
        return Some(format!(
            "{path} ({}): scopes {:?} vs {:?}",
            a.ty, a.scopes, b.scopes
        ));
    }
    None
}

fn all_difference(a: &[ExprKind], b: &[ExprKind], path: &str, cx: &Compare) -> Option<String> {
    if a.len() != b.len() {
        return Some(format!("{path}: {} parts vs {}", a.len(), b.len()));
    }
    a.iter()
        .zip(b)
        .enumerate()
        .find_map(|(i, (a, b))| difference(a, b, &format!("{path}[{i}]"), cx))
}

fn difference(a: &ExprKind, b: &ExprKind, path: &str, cx: &Compare) -> Option<String> {
    steel_parser::visitors::with_stack(|| difference_inner(a, b, path, cx))
}

fn id_difference(a: u32, b: u32, path: &str, cx: &Compare) -> Option<String> {
    (cx.exact && a != b).then(|| format!("{path}: id {a} vs {b}"))
}

fn difference_inner(a: &ExprKind, b: &ExprKind, path: &str, cx: &Compare) -> Option<String> {
    let atom = |a: &SyntaxObject, b: &SyntaxObject, path: &str| atom_difference(a, b, path, cx);
    // A form's keyword token is where it came from, not what it does
    let location = |a: &SyntaxObject, b: &SyntaxObject, path: &str| {
        if cx.exact {
            atom_difference(a, b, path, cx)
        } else {
            None
        }
    };
    let one = |a: &ExprKind, b: &ExprKind, path: &str| difference(a, b, path, cx);
    let all = |a: &[ExprKind], b: &[ExprKind], path: &str| all_difference(a, b, path, cx);
    match (a, b) {
        (ExprKind::Atom(a), ExprKind::Atom(b)) => atom(&a.syn, &b.syn, path),
        (ExprKind::If(a), ExprKind::If(b)) => {
            let path = format!("{path}.if");
            location(&a.location, &b.location, &path)
                .or_else(|| one(&a.test_expr, &b.test_expr, &format!("{path}.test")))
                .or_else(|| one(&a.then_expr, &b.then_expr, &format!("{path}.then")))
                .or_else(|| one(&a.else_expr, &b.else_expr, &format!("{path}.else")))
        }
        (ExprKind::Define(a), ExprKind::Define(b)) => {
            let path = format!("{path}.define");
            location(&a.location, &b.location, &path)
                .or_else(|| one(&a.name, &b.name, &format!("{path}.name")))
                .or_else(|| one(&a.body, &b.body, &format!("{path}.body")))
        }
        (ExprKind::Begin(a), ExprKind::Begin(b)) => {
            let path = format!("{path}.begin");
            location(&a.location, &b.location, &path).or_else(|| all(&a.exprs, &b.exprs, &path))
        }
        (ExprKind::Set(a), ExprKind::Set(b)) => {
            let path = format!("{path}.set!");
            location(&a.location, &b.location, &path)
                .or_else(|| one(&a.variable, &b.variable, &format!("{path}.variable")))
                .or_else(|| one(&a.expr, &b.expr, &format!("{path}.value")))
        }
        (ExprKind::Quote(a), ExprKind::Quote(b)) => {
            let path = format!("{path}.quote");
            location(&a.location, &b.location, &path).or_else(|| one(&a.expr, &b.expr, &path))
        }
        (ExprKind::Return(a), ExprKind::Return(b)) => {
            let path = format!("{path}.return");
            location(&a.location, &b.location, &path).or_else(|| one(&a.expr, &b.expr, &path))
        }
        (ExprKind::Let(a), ExprKind::Let(b)) => {
            let path = format!("{path}.let");
            if a.bindings.len() != b.bindings.len() {
                return Some(format!(
                    "{path}: {} bindings vs {}\n  {}\n  {}",
                    a.bindings.len(),
                    b.bindings.len(),
                    excerpt(&ExprKind::Let(a.clone())),
                    excerpt(&ExprKind::Let(b.clone()))
                ));
            }
            id_difference(a.syntax_object_id, b.syntax_object_id, &path, cx)
                .or_else(|| location(&a.location, &b.location, &path))
                .or_else(|| {
                    a.bindings
                        .iter()
                        .zip(&b.bindings)
                        .enumerate()
                        .find_map(|(i, (x, y))| {
                            one(&x.0, &y.0, &format!("{path}.binding[{i}].name"))
                                .or_else(|| one(&x.1, &y.1, &format!("{path}.binding[{i}].value")))
                        })
                })
                .or_else(|| one(&a.body_expr, &b.body_expr, &format!("{path}.body")))
        }
        (ExprKind::LambdaFunction(a), ExprKind::LambdaFunction(b)) => {
            let path = format!("{path}.lambda");
            if a.rest != b.rest || a.kwargs != b.kwargs {
                return Some(format!("{path}: rest/kwargs differ"));
            }
            id_difference(a.syntax_object_id, b.syntax_object_id, &path, cx)
                .or_else(|| location(&a.location, &b.location, &path))
                .or_else(|| all(&a.args, &b.args, &format!("{path}.args")))
                .or_else(|| one(&a.body, &b.body, &format!("{path}.body")))
        }
        (ExprKind::List(a), ExprKind::List(b)) => {
            let path = format!("{path}.list");
            if a.improper != b.improper {
                return Some(format!("{path}: improper {} vs {}", a.improper, b.improper));
            }
            if cx.exact && a.location != b.location {
                return Some(format!("{path}: span {:?} vs {:?}", a.location, b.location));
            }
            if cx.exact && a.scopes != b.scopes {
                return Some(format!("{path}: scopes differ"));
            }
            id_difference(a.syntax_object_id, b.syntax_object_id, &path, cx)
                .or_else(|| all(&a.args, &b.args, &path))
        }
        (ExprKind::Vector(a), ExprKind::Vector(b)) => {
            let path = format!("{path}.vector");
            if a.bytes != b.bytes || (cx.exact && a.span != b.span) {
                return Some(format!("{path}: bytes/span differ"));
            }
            all(&a.args, &b.args, &path)
        }
        (ExprKind::Require(a), ExprKind::Require(b)) => {
            (a != b).then(|| format!("{path}: require differs"))
        }
        (ExprKind::Macro(a), ExprKind::Macro(b)) => {
            (a != b).then(|| format!("{path}: macro differs"))
        }
        (ExprKind::SyntaxRules(a), ExprKind::SyntaxRules(b)) => {
            (a != b).then(|| format!("{path}: syntax-rules differs"))
        }
        (a, b) => Some(format!(
            "{path}: {} vs {}\n  {}\n  {}",
            node_name(a),
            node_name(b),
            excerpt(a),
            excerpt(b)
        )),
    }
}

// The start of a node's text, for messages
fn excerpt(expr: &ExprKind) -> String {
    let text = expr.to_string();
    if text.chars().count() > 300 {
        text.chars().take(300).collect::<String>() + " ..."
    } else {
        text
    }
}

fn node_name(expr: &ExprKind) -> &'static str {
    match expr {
        ExprKind::Atom(_) => "atom",
        ExprKind::If(_) => "if",
        ExprKind::Let(_) => "let",
        ExprKind::Define(_) => "define",
        ExprKind::LambdaFunction(_) => "lambda",
        ExprKind::Begin(_) => "begin",
        ExprKind::Return(_) => "return",
        ExprKind::Quote(_) => "quote",
        ExprKind::Macro(_) => "macro",
        ExprKind::SyntaxRules(_) => "syntax-rules",
        ExprKind::List(_) => "list",
        ExprKind::Set(_) => "set!",
        ExprKind::Require(_) => "require",
        ExprKind::Vector(_) => "vector",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parser::Parser;

    // Syntax a pass would write: no id
    fn stx(value: SteelVal) -> SteelVal {
        Syntax::new(value, Span::default()).into()
    }

    fn sym(name: &str) -> SteelVal {
        stx(SteelVal::SymbolV(name.into()))
    }

    fn list(items: Vec<SteelVal>) -> SteelVal {
        stx(SteelVal::ListV(items.into()))
    }

    fn decode_one(form: SteelVal) -> Result<ExprKind> {
        let mut bridge = SyntaxBridge::new();
        let mut decoded = bridge.decode_program(&SteelVal::ListV(vec![form].into()))?;
        Ok(decoded.remove(0))
    }

    #[test]
    fn round_trip_is_exact() {
        let program = r#"
            (require "some/module")
            (define (f a #:key [key 1] . rest)
              (let ([b (+ a 1.5)] [c "text"])
                (if (> b 0)
                    (begin (set! a #\x) (vector-ref #(1 2 3) 0))
                    (quote (x y . z)))))
            (define big 123456789012345678901234567890)
            (define nan +nan.0)
            (define complex-nan 0.0+nan.0i)
            (define keyword #:kw)
            ((lambda (x) x) 1)
        "#;
        let exprs = Parser::parse(program).unwrap();
        let mut bridge = SyntaxBridge::new();
        let syntax = bridge.encode_program(&exprs).unwrap();
        let back = bridge.decode_program(&syntax).unwrap();
        assert_eq!(first_difference(&exprs, &back), None);
    }

    #[test]
    fn forms_show_their_kind() {
        // A let made from an applied lambda keeps the lambda's keyword
        let mut exprs = Parser::parse("(define x ((lambda (a) a) 1))").unwrap();
        crate::compiler::passes::analysis::anonymous_calls_to_lets(&mut exprs);
        let ExprKind::Define(define) = &exprs[0] else {
            unreachable!()
        };
        let ExprKind::Let(l) = &define.body else {
            panic!("{}", define.body)
        };
        assert_eq!(l.location.ty, TokenType::Lambda);

        let mut bridge = SyntaxBridge::new();
        let syntax = bridge.encode_program(&exprs).unwrap();
        let SteelVal::ListV(forms) = &syntax else {
            unreachable!()
        };
        let SteelVal::SyntaxObject(form) = forms.iter().next().unwrap() else {
            unreachable!()
        };
        assert_eq!(form.syntax_datum().to_string(), "(define x (let ((a 1)) a))");
        let back = bridge.decode_program(&syntax).unwrap();
        assert_eq!(first_difference(&exprs, &back), None);
    }

    #[test]
    fn forms_a_pass_writes() {
        let int = |i| stx(SteelVal::IntV(i));
        let cases = [
            (
                list(vec![sym("if"), stx(SteelVal::BoolV(true)), int(1), int(2)]),
                "if",
            ),
            (
                list(vec![sym("lambda"), list(vec![sym("x")]), sym("x")]),
                "lambda",
            ),
            (
                list(vec![
                    sym("let"),
                    list(vec![list(vec![sym("a"), int(1)])]),
                    sym("a"),
                ]),
                "let",
            ),
            (list(vec![sym("define"), sym("y"), int(2)]), "define"),
            (list(vec![sym("begin"), int(1), int(2)]), "begin"),
            (list(vec![sym("set!"), sym("y"), int(3)]), "set!"),
            (list(vec![sym("quote"), sym("z")]), "quote"),
            (list(vec![sym("f"), int(1)]), "list"),
        ];
        for (form, expected) in cases {
            let decoded = decode_one(form).unwrap();
            assert_eq!(node_name(&decoded), expected, "{decoded}");
        }
    }

    #[test]
    fn copies_get_fresh_ids() {
        let exprs = Parser::parse("(define (f x) x)").unwrap();
        let mut bridge = SyntaxBridge::new();
        let SteelVal::ListV(syntax) = bridge.encode_program(&exprs).unwrap() else {
            unreachable!()
        };
        let form = syntax.iter().next().unwrap().clone();
        // A pass that put the same form in twice
        let back = bridge
            .decode_program(&SteelVal::ListV(vec![form.clone(), form].into()))
            .unwrap();

        let lambda_id = |expr: &ExprKind| match expr {
            ExprKind::Define(d) => match &d.body {
                ExprKind::LambdaFunction(l) => l.syntax_object_id,
                other => panic!("{other}"),
            },
            other => panic!("{other}"),
        };
        let ExprKind::Define(original) = &exprs[0] else {
            unreachable!()
        };
        let ExprKind::LambdaFunction(original) = &original.body else {
            unreachable!()
        };
        assert_eq!(lambda_id(&back[0]), original.syntax_object_id);
        assert_ne!(lambda_id(&back[1]), original.syntax_object_id);
        // The first copy is the original, the second only looks like it
        assert_eq!(first_difference(&exprs, &back[..1]), None);
        assert!(first_difference(&exprs, &back[1..]).is_some());
        assert_eq!(back[0].to_string(), back[1].to_string());
    }

    #[test]
    fn malformed_forms() {
        let err = decode_one(list(vec![
            sym("if"),
            stx(SteelVal::IntV(1)),
            stx(SteelVal::IntV(2)),
        ]))
        .unwrap_err();
        assert!(err.to_string().contains("if takes 3 parts"), "{err}");

        let err = decode_one(stx(SteelVal::VectorV(
            Gc::new(crate::values::Vector::new()).into(),
        )))
        .map(|_| ());
        assert!(err.is_ok(), "an empty vector is fine");

        let mut bridge = SyntaxBridge::new();
        let err = bridge.decode_program(&SteelVal::IntV(1)).unwrap_err();
        assert!(err.to_string().contains("list of syntax objects"), "{err}");
    }
}
