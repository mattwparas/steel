use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use thin_vec::ThinVec;

use crate::compiler::passes::{VisitorMutControlFlow, VisitorMutRefUnit};
use crate::compiler::program::{ELLIPSES_SYMBOL, QUASISYNTAX_LOWERED, SYNTAX_SPAN};
use crate::parser::parser::{ExpansionMark, SyntaxObject};
use crate::parser::span::Span;
use crate::parser::tokens::TokenType;
use crate::{
    compiler::program::{DATUM_SYNTAX, SYNTAX_CONST_IF},
    parser::ast::ExprKind,
};

use crate::rvals::Result;

use super::expander::BindingKind;
use super::visitors::VisitorMutRef;
use super::{ast::Atom, interner::InternedString};

use core::ops::ControlFlow;

use thin_vec::thin_vec;

// `(#%quasisyntax template)`, which the quasisyntax macro expands to, becomes a syntax
// template with a pattern variable in place of each `#,e` and `#,@e`, so the rest of the
// template, ellipses included, means what it means in `syntax`
fn lower_quasisyntax(l: &mut super::ast::List) -> Option<ExprKind> {
    if l.args.len() != 2 || l.first_ident() != Some(&*QUASISYNTAX_LOWERED) {
        return None;
    }

    let mut template = l.args.pop().unwrap();
    let span = crate::parser::span_visitor::get_span(&template);

    let mut lift = LiftUnsyntax {
        depth: 0,
        holes: Vec::new(),
    };

    lift.visit(&mut template);

    let ident = |name: &str| ExprKind::ident_with_span(name, span);
    let quoted = |expr: ExprKind| {
        ExprKind::Quote(Box::new(super::ast::Quote::new(
            expr,
            SyntaxObject::new(TokenType::Quote, span),
        )))
    };
    let call = |args: ThinVec<ExprKind>| ExprKind::List(super::ast::List::new(args));

    // One hash of the holes, which nest no deeper as there are more of them
    let mut holes = thin_vec![ident("#%prim.hash")];
    let mut kinds = thin_vec![ident("#%prim.hash")];

    for (name, expr, splicing) in lift.holes {
        holes.push(quoted(ExprKind::ident_with_span(name.resolve(), span)));
        holes.push(expr);
        kinds.push(quoted(ExprKind::ident_with_span(name.resolve(), span)));
        kinds.push(ExprKind::integer_literal(splicing as isize, span));
    }

    let bindings = call(thin_vec![
        ident("#%prim.hash-union"),
        call(thin_vec![ident("#%syntax-bindings")]),
        call(holes),
    ]);
    let kinds = call(thin_vec![
        ident("#%prim.hash-union"),
        call(thin_vec![ident("#%syntax-binding-kind")]),
        call(kinds),
    ]);

    Some(call(thin_vec![
        ident("#%expand-syntax-case"),
        quoted(template),
        bindings,
        kinds,
    ]))
}

struct LiftUnsyntax {
    // How many quasisyntax forms inside the template this is in
    depth: usize,
    holes: Vec<(InternedString, ExprKind, bool)>,
}

enum Hole {
    No,
    // Its parts were visited already
    Visited,
    Lifted { splicing: bool },
}

impl LiftUnsyntax {
    fn hole(&mut self, expr: &mut ExprKind) -> Hole {
        let ExprKind::List(l) = expr else {
            return Hole::No;
        };

        let splicing = match crate::parser::kernel::form_name(l) {
            Some("#%unsyntax") if l.args.len() == 2 => false,
            Some("#%unsyntax-splicing") if l.args.len() == 2 => true,
            Some("quasisyntax") => {
                self.depth += 1;
                self.visit_elements(&mut l.args);
                self.depth -= 1;
                return Hole::Visited;
            }
            _ => return Hole::No,
        };

        if self.depth > 0 {
            self.depth -= 1;
            self.visit(&mut l.args[1]);
            self.depth += 1;
            return Hole::Visited;
        }

        let name = format!("#%quasisyntax-hole{}", self.holes.len());
        let value = l.args.pop().unwrap();
        let span = crate::parser::span_visitor::get_span(&value);

        *expr = ExprKind::ident_with_span(&name, span);
        self.holes.push((name.into(), value, splicing));

        Hole::Lifted { splicing }
    }

    // `(a . #,e)` reads as `(a #%unsyntax e)`, whose tail is the hole
    fn unsyntax_tail(&mut self, l: &mut super::ast::List) {
        if self.depth > 0 || l.improper || l.args.len() < 3 {
            return;
        }

        let unsyntax = l.args[l.args.len() - 2]
            .atom_syntax_object()
            .is_some_and(|x| match &x.ty {
                TokenType::UnquoteSyntax => true,
                TokenType::Identifier(name) => name.resolve() == "#%unsyntax",
                _ => false,
            });

        if !unsyntax {
            return;
        }

        let value = l.args.pop().unwrap();
        l.args.pop();

        let name = format!("#%quasisyntax-hole{}", self.holes.len());
        let span = crate::parser::span_visitor::get_span(&value);

        l.args.push(ExprKind::ident_with_span(&name, span));
        l.improper = true;
        self.holes.push((name.into(), value, false));
    }

    // `#,@e` splices into the sequence it is in
    fn visit_elements(&mut self, exprs: &mut ThinVec<ExprKind>) {
        let mut i = 0;

        while i < exprs.len() {
            match self.hole(&mut exprs[i]) {
                Hole::Lifted { splicing: true } => {
                    let span = crate::parser::span_visitor::get_span(&exprs[i]);
                    exprs.insert(i + 1, ExprKind::ident_with_span("...", span));
                    i += 1;
                }
                Hole::No => self.visit(&mut exprs[i]),
                _ => {}
            }

            i += 1;
        }
    }
}

impl VisitorMutRefUnit for LiftUnsyntax {
    fn visit(&mut self, expr: &mut ExprKind) {
        crate::compiler::passes::with_stack(|| {
            match self.hole(expr) {
                Hole::No => {}
                _ => return,
            }

            match expr {
                ExprKind::List(l) => {
                    self.unsyntax_tail(l);
                    self.visit_elements(&mut l.args)
                }
                ExprKind::Begin(b) => {
                    let mut exprs: ThinVec<_> = core::mem::take(&mut b.exprs).into_iter().collect();
                    self.visit_elements(&mut exprs);
                    b.exprs = exprs.into_iter().collect();
                }
                ExprKind::LambdaFunction(f) => {
                    self.visit_elements(&mut f.args);
                    self.visit(&mut f.body);
                }
                ExprKind::Vector(v) => self.visit_elements(&mut v.args),
                ExprKind::If(f) => self.visit_if(f),
                ExprKind::Let(l) => self.visit_let(l),
                ExprKind::Define(d) => self.visit_define(d),
                ExprKind::Return(r) => self.visit_return(r),
                ExprKind::Quote(q) => self.visit_quote(q),
                ExprKind::Set(s) => self.visit_set(s),
                _ => {}
            }
        })
    }
}

pub fn replace_identifiers(
    expr: &mut ExprKind,
    bindings: &mut FxHashMap<InternedString, ExprKind>,
    binding_kind: &mut FxHashMap<InternedString, BindingKind>,
    fallback_bindings: &mut FxHashMap<InternedString, ExprKind>,
    span: Span,
) -> Result<()> {
    // let mut rewrite_spans = expr;

    RewriteSpan::new(span).visit(expr);

    // TODO: Replace this here!
    ReplaceExpressions::new(bindings, binding_kind, fallback_bindings).visit(expr)
}

pub fn expand_template_pair(
    mut exprs: Vec<ExprKind>,
    bindings_list: impl IntoIterator<Item = (InternedString, (BindingKind, ExprKind))>,
) -> Result<ExprKind> {
    let mut bindings = FxHashMap::default();
    let mut binding_kind = FxHashMap::default();

    for (key, (kind, expr)) in bindings_list.into_iter() {
        bindings.insert(key, expr);
        binding_kind.insert(key, kind);
    }

    for expr in &mut exprs {
        expand_template(expr, &mut bindings, &mut binding_kind)?;
    }

    if exprs.len() == 1 {
        Ok(exprs.into_iter().next().unwrap())
    } else {
        Ok(ExprKind::Begin(Box::new(steel_parser::ast::Begin::new(
            exprs,
            SyntaxObject::default(TokenType::Begin),
        ))))
    }
}

// TODO: I think this will need to know the values that are in scope
// at the time the kernel macro is expanded, as well as what the globals
// are. These will need to get passed down; otherwise it won't be able
// to respect the scope properly.
pub fn expand_template(
    expr: &mut ExprKind,
    bindings: &mut FxHashMap<InternedString, ExprKind>,
    binding_kind: &mut FxHashMap<InternedString, BindingKind>,
) -> Result<()> {
    ReplaceExpressions::new(bindings, binding_kind, &mut Default::default()).visit(expr)
}

// struct ConstExprKindTransformers {
//     functions: HashMap<&'static str, fn(&ReplaceExpressions<'_>, ExprKind) -> Result<ExprKind>>,
// }

pub struct ReplaceExpressions<'a> {
    bindings: &'a mut FxHashMap<InternedString, ExprKind>,
    fallback_bindings: &'a mut FxHashMap<InternedString, ExprKind>,
    binding_kind: &'a mut FxHashMap<InternedString, BindingKind>,
    wildcard: InternedString,
    escaped: bool,
}

fn check_ellipses(expr: &ExprKind) -> bool {
    matches!(
        expr,
        ExprKind::Atom(Atom {
            syn: SyntaxObject {
                ty: TokenType::Ellipses,
                ..
            },
        })
    ) || matches!(expr, ExprKind::Atom(Atom {
            syn: SyntaxObject {
                ty: TokenType::Identifier(t),
                ..
            }
        }) if *t == *ELLIPSES_SYMBOL)
}

struct EllipsesExpanderVisitor<'a> {
    bindings: &'a mut FxHashMap<InternedString, ExprKind>,
    binding_kind: &'a mut FxHashMap<InternedString, BindingKind>,
    found_length: Option<usize>,
    collected: SmallVec<[InternedString; 8]>,
    error: Option<String>,
}

impl<'a> EllipsesExpanderVisitor<'a> {
    fn find_expansion_width_and_collect_ellipses_expanders(
        bindings: &'a mut FxHashMap<InternedString, ExprKind>,
        binding_kind: &'a mut FxHashMap<InternedString, BindingKind>,
        expr: &ExprKind,
    ) -> Self {
        let mut visitor = Self {
            bindings,
            binding_kind,
            found_length: None,
            collected: SmallVec::default(),
            error: None,
        };

        let _ = VisitorMutControlFlow::visit(&mut visitor, expr);

        visitor
    }
}

impl<'a> VisitorMutControlFlow for EllipsesExpanderVisitor<'a> {
    fn visit_atom(&mut self, a: &Atom) -> ControlFlow<()> {
        // A splice from a quasisyntax repeats with its own ellipsis only
        if a.ident()
            .is_some_and(|x| x.resolve().starts_with("#%quasisyntax-hole"))
        {
            return ControlFlow::Continue(());
        }

        let expansion = a.ident().and_then(|x| self.bindings.get(x));

        if let Some(ExprKind::List(found_list)) = expansion {
            if let Some(BindingKind::Many) = a.ident().and_then(|x| self.binding_kind.get(x)) {
                if let Some(previously_seen_length) = self.found_length {
                    // Check that the length is the same
                    if previously_seen_length != found_list.len() {
                        self.error =
                            Some("Mismatched lengths found in ellipses expansion".to_owned());
                        return ControlFlow::Break(());
                    }

                    // Found this one, use it
                    self.collected.push(*(a.ident().unwrap()));
                } else {
                    self.found_length = Some(found_list.len());
                    self.collected.push(*(a.ident().unwrap()));
                }
            }
        }
        ControlFlow::Continue(())
    }
}

impl<'a> ReplaceExpressions<'a> {
    pub fn new(
        bindings: &'a mut FxHashMap<InternedString, ExprKind>,
        binding_kind: &'a mut FxHashMap<InternedString, BindingKind>,
        fallback_bindings: &'a mut FxHashMap<InternedString, ExprKind>,
    ) -> Self {
        ReplaceExpressions {
            bindings,
            binding_kind,
            fallback_bindings,
            wildcard: InternedString::from_static("_"),
            escaped: false,
        }
    }

    // fn expand_atom(&self, &mut expr: Atom) -> ExprKind {
    //     // Overwrite the span on any atoms
    //     // expr.syn.set_span(self.span);

    //     if let TokenType::Identifier(s) = &expr.syn.ty {
    //         if let Some(body) = self.bindings.get(s) {
    //             return body.clone();
    //         }
    //     }

    //     ExprKind::Atom(expr)
    // }

    // Expands each `x ...` in a list of template elements and visits the rest. What an
    // ellipsis expands to isn't visited again, it holds the macro's input.
    fn visit_elements(&mut self, exprs: &mut ThinVec<ExprKind>) -> Result<bool> {
        if !exprs.iter().any(check_ellipses) {
            for expr in exprs.iter_mut() {
                self.visit(expr)?;
            }

            return Ok(false);
        }

        let mut improper = false;
        let mut output = ThinVec::with_capacity(exprs.len());
        let mut rest = core::mem::take(exprs).into_iter().peekable();

        while let Some(mut expr) = rest.next() {
            // A `#,@` still splices inside `(... template)`
            let expands = !self.escaped
                || expr
                    .atom_identifier()
                    .is_some_and(|x| x.resolve().starts_with("#%quasisyntax-hole"));

            if expands && !check_ellipses(&expr) && rest.peek().is_some_and(check_ellipses) {
                let ellipsis = rest.next().unwrap();

                // `x ... ...` is `(x ...) ...` with each copy spliced in
                let mut more = ThinVec::new();
                while !self.escaped && rest.peek().is_some_and(check_ellipses) {
                    more.push(rest.next().unwrap());
                }

                let nested = !more.is_empty();
                if nested {
                    more.insert(0, expr);
                    expr = ExprKind::List(super::ast::List::new(more));
                }

                let mut pair = thin_vec![expr, ellipsis];
                let escaped = core::mem::replace(&mut self.escaped, false);
                let result = self.expand_ellipses(&mut pair);
                self.escaped = escaped;
                improper |= result?;

                if nested {
                    for copy in pair {
                        match copy {
                            ExprKind::List(l) => output.extend(l.args),
                            other => output.push(other),
                        }
                    }
                } else {
                    output.extend(pair);
                }
            } else {
                self.visit(&mut expr)?;
                output.push(expr);
            }
        }

        *exprs = output;

        Ok(improper)
    }

    // Note: Returns a bool indicating if this should be made improper/rest or not
    fn expand_ellipses(&mut self, vec_exprs: &mut ThinVec<ExprKind>) -> Result<bool> {
        if self.escaped {
            return Ok(false);
        }
        if let Some(ellipses_pos) = vec_exprs.iter().position(check_ellipses) {
            if ellipses_pos == 0 {
                return Ok(false);
            }

            let variable_to_lookup = vec_exprs.get(ellipses_pos - 1).ok_or_else(
                throw!(BadSyntax => "macro expansion failed, could not find variable when expanding ellipses")
            )?;

            match variable_to_lookup {
                ExprKind::Atom(Atom {
                    syn:
                        SyntaxObject {
                            ty: TokenType::Identifier(var),
                            introduced_via_macro,
                            ..
                        },
                }) => {
                    let improper;

                    // let rest = self.bindings.get(var).ok_or_else(throw!(BadSyntax => format!("macro expansion failed at finding the variable when expanding ellipses: {var}")))?;

                    let rest = if let Some(rest) = self.bindings.get(var) {
                        rest
                    } else {
                        return Ok(false);
                    };

                    // A list of a quote and its datum comes back from a syntax object as a quote
                    let quoted;

                    let list_of_exprs = if let ExprKind::List(list_of_exprs) = rest {
                        improper = list_of_exprs.improper;

                        list_of_exprs
                    } else if let ExprKind::Quote(q) = rest {
                        improper = false;
                        quoted = super::ast::List::new(thin_vec![
                            ExprKind::ident_with_span("quote", q.location.span),
                            q.expr.clone()
                        ]);

                        &quoted
                    } else {
                        let res = if let Some(res) = self.fallback_bindings.get(var) {
                            res.list_or_else(
                        throw!(BadSyntax => "macro expansion failed, expected list of expressions, found: {}, within {}", rest, super::ast::List::new(vec_exprs.clone())))?
                        } else if var.resolve().starts_with("#%quasisyntax-hole") {
                            stop!(BadSyntax => "an unsyntax before an ellipsis has to be a list, found: {}", rest)
                        } else if let Some(BindingKind::Many) = self.binding_kind.get(var) {
                            stop!(BadSyntax => "expected a list for {} before an ellipsis, found: {}", var, rest)
                        } else {
                            return Ok(false);
                        };

                        improper = res.improper;

                        //     let res = self.fallback_bindings.get(var).ok_or_else(throw!(BadSyntax => format!("macro expansion failed at finding the variable when expanding ellipses: {var}")))?.list_or_else(
                        //     throw!(BadSyntax => "macro expansion failed, expected list of expressions, found: {}, within {}", rest, super::ast::List::new(vec_exprs.clone()))
                        // )?;

                        res
                    };

                    let mut list_of_exprs: ThinVec<_> = list_of_exprs.args.clone();

                    for expr in list_of_exprs.iter_mut() {
                        if let ExprKind::Atom(a) = expr {
                            a.syn.introduced_via_macro = *introduced_via_macro;
                            a.syn.unresolved = false;
                        }
                    }

                    let back_chunk = vec_exprs
                        .drain(ellipses_pos - 1..)
                        .collect::<SmallVec<[_; 8]>>();

                    vec_exprs.reserve(list_of_exprs.len() + back_chunk[2..].len());

                    vec_exprs.append(&mut list_of_exprs);

                    vec_exprs.extend_from_slice(&back_chunk[2..]);

                    Ok(improper)
                }

                // A template before the ellipsis
                ExprKind::List(_) | ExprKind::Quote(_) | ExprKind::Vector(_) => {
                    // Each copy of an improper template is an improper element, not a tail
                    let improper = false;

                    let visitor = EllipsesExpanderVisitor::find_expansion_width_and_collect_ellipses_expanders(self.bindings, self.binding_kind, variable_to_lookup);

                    if let Some(error) = visitor.error {
                        stop!(BadSyntax => error);
                    }

                    let width = visitor.found_length.ok_or_else(throw!(BadSyntax => "No pattern variables before ellipses in template: at {} in {}", variable_to_lookup, ExprKind::List(super::ast::List::new(vec_exprs.clone()))))?;

                    let mut original_bindings: FxHashMap<_, _> = visitor
                        .collected
                        .iter()
                        .flat_map(|x| self.bindings.get(x).map(|value| (*x, value.clone())))
                        .collect();

                    core::mem::swap(self.fallback_bindings, &mut original_bindings);

                    let mut expanded_expressions = SmallVec::<[ExprKind; 6]>::with_capacity(width);

                    for i in 0..width {
                        let mut template = variable_to_lookup.clone();

                        for (key, value) in self.fallback_bindings.iter() {
                            if let ExprKind::List(expansion) = value {
                                let new_binding = expansion
                                    .get(i)
                                    .ok_or_else(throw!(BadSyntax => "Unreachable"))?;

                                self.bindings.insert(*key, new_binding.clone());
                            } else {
                                stop!(BadSyntax => "Unexpected value found in ellipses expansion")
                            }
                        }

                        self.visit(&mut template)?;

                        expanded_expressions.push(template);
                    }

                    core::mem::swap(self.fallback_bindings, &mut original_bindings);

                    // Move the original bindings back in
                    for (key, value) in original_bindings {
                        self.bindings.insert(key, value);
                    }

                    let back_chunk = vec_exprs
                        .drain(ellipses_pos - 1..)
                        .collect::<SmallVec<[_; 8]>>();

                    // TODO: We might need to mimic the above, where we
                    // set if the resulting expression was introduced via macro.
                    vec_exprs.reserve(expanded_expressions.len() + back_chunk[2..].len());
                    vec_exprs.extend(expanded_expressions);
                    vec_exprs.extend_from_slice(&back_chunk[2..]);

                    Ok(improper)
                }

                _ => {
                    stop!(BadSyntax => "macro expansion failed at lookup!: {}", variable_to_lookup)
                }
            }
        } else {
            Ok(false)
        }
    }

    fn vec_expr_syntax_const_if(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        match vec_exprs.first() {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *SYNTAX_CONST_IF => {
                if vec_exprs.len() != 4 {
                    stop!(BadSyntax => "syntax-const-if expects a const test condition, a then and an else case");
                }

                let test_expr = vec_exprs.get(1).unwrap();
                let then_expr = vec_exprs.get(2).unwrap();
                let else_expr = vec_exprs.get(3).unwrap();

                if let ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty, .. },
                }) = test_expr
                {
                    // TODO -> what happens if reserved tokens are in here
                    match ty {
                        TokenType::BooleanLiteral(_)
                        | TokenType::CharacterLiteral(_)
                        | TokenType::Number(_)
                        | TokenType::StringLiteral(_) => return Ok(Some(then_expr.clone())),
                        TokenType::Identifier(s) => {
                            if let Some(ExprKind::Atom(Atom {
                                syn: SyntaxObject { ty, .. },
                            })) = self.bindings.get(s)
                            {
                                log::debug!("Syntax const if resolved to: {:?}", ty);

                                if matches!(
                                    ty,
                                    TokenType::BooleanLiteral(_)
                                        | TokenType::Number(_)
                                        | TokenType::CharacterLiteral(_)
                                        | TokenType::StringLiteral(_)
                                ) {
                                    return Ok(Some(then_expr.clone()));
                                }
                            }
                        }
                        _ => {}
                    }
                }

                Ok(Some(else_expr.clone()))
            }
            _ => Ok(None),
        }
    }

    fn vec_expr_datum_to_syntax(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        match vec_exprs.first() {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *DATUM_SYNTAX
                && vec_exprs
                    .iter()
                    .skip(1)
                    .all(|x| x.atom_identifier().is_some())
                && vec_exprs.iter().skip(1).any(|x| {
                    x.atom_identifier().is_some_and(|x| {
                        x.resolve().starts_with("##")
                            || self.bindings.contains_key(x)
                            || self
                                .bindings
                                .contains_key(&format!("##{}", x.resolve()).into())
                    })
                }) =>
            {
                // Joining identifiers, one of them the pattern's. Otherwise it's Racket's,
                // called at run time.
                let mut buffer = String::new();
                if let Some((_, rest)) = vec_exprs.split_first() {
                    for syntax in rest {
                        let transformer = syntax.atom_identifier_or_else(
                            throw!(BadSyntax => "datum->syntax requires an identifier"),
                        )?;

                        let resolved = transformer.resolve();

                        // TODO this is no longer correct
                        // Should actually just visit the variable in the define name part
                        // TODO
                        if resolved.starts_with("##") {
                            if let Some(body) = self.bindings.get(transformer) {
                                buffer.push_str(body.to_string().as_str());
                            } else {
                                let (_, cdr) = resolved.split_at(2);
                                buffer.push_str(cdr);
                            }
                        } else {
                            // Try to get the prepended variable
                            if let Some(body) =
                                self.bindings.get(&("##".to_string() + resolved).into())
                            {
                                // println!("Found datum: {}", transformer);
                                buffer.push_str(body.to_string().as_str());
                            } else {
                                // println!("Unable to find datum: {}", transformer);
                                buffer.push_str(resolved);
                            }
                        }
                    }

                    // This is a new identifier, so it doesn't have a known mark
                    let mut syn = SyntaxObject::default(TokenType::Identifier(buffer.into()));
                    syn.mark = ExpansionMark::UNKNOWN;

                    Ok(Some(ExprKind::Atom(Atom::new(syn))))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    // TODO: @Matt - Come back here
    fn vec_syntax_span_object(&self, vec_exprs: &[ExprKind]) -> Result<Option<ExprKind>> {
        if vec_exprs.len() != 2 {
            return Ok(None);
            // stop!(ArityMismatch => "#%syntax-span requires 2 arguments, found: {}", vec_exprs.len());
        }

        match vec_exprs.first() {
            Some(ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::Identifier(check),
                        ..
                    },
            })) if *check == *SYNTAX_SPAN => {
                let expr_to_extract_span = vec_exprs.get(1).unwrap();

                // dbg!(&expr_to_extract_span);

                let span = crate::parser::span_visitor::get_span(expr_to_extract_span);

                let start = ExprKind::integer_literal(span.start as isize, span);
                let end = ExprKind::integer_literal(span.end as isize, span);

                // TODO: Fix this unwrap
                let source_id = ExprKind::integer_literal(
                    span.source_id().map(|x| x.0).unwrap_or(0) as isize,
                    span,
                );

                Ok(Some(ExprKind::Quote(Box::new(super::ast::Quote::new(
                    ExprKind::List(super::ast::List::new(thin_vec![start, end, source_id])),
                    SyntaxObject::default(TokenType::Quote),
                )))))

                // Ok(Some(ExprKind::List(List::new(vec![

                // ]))))
            }
            _ => Ok(None),
        }
    }
}

// TODO replace spans on all of the nodes and atoms
impl<'a> VisitorMutRef for ReplaceExpressions<'a> {
    type Output = Result<()>;

    fn visit(&mut self, expr: &mut ExprKind) -> Self::Output {
        crate::compiler::passes::with_stack(|| {
            // println!("Visiting: {}", expr);
            match expr {
                ExprKind::If(f) => self.visit_if(f),
                ExprKind::Define(d) => self.visit_define(d),
                ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
                ExprKind::Begin(b) => self.visit_begin(b),
                ExprKind::Return(r) => self.visit_return(r),
                ExprKind::Let(l) => self.visit_let(l),
                ExprKind::Quote(q) => self.visit_quote(q),
                ExprKind::Macro(m) => self.visit_macro(m),
                ExprKind::Atom(a) => {
                    if let TokenType::Identifier(s) = &mut a.syn.ty {
                        // A quasisyntax template holds the input's identifiers, which aren't
                        // pattern variables even when they share a name
                        if *s != self.wildcard && !crate::parser::kernel::is_macro_input(a.syn.mark)
                        {
                            if let Some(body) = self.bindings.get(s) {
                                let introduced_via_macro = a.syn.introduced_via_macro;

                                *expr = body.clone();

                                if let ExprKind::Atom(a) = expr {
                                    a.syn.introduced_via_macro = introduced_via_macro;
                                    a.syn.unresolved = false;
                                }
                            }
                        }
                    }

                    Ok(())
                }
                ExprKind::List(l) => {
                    if !self.escaped
                        && !l.improper
                        && l.args.len() == 2
                        && check_ellipses(&l.args[0])
                    {
                        let mut template = l.args[1].clone();
                        self.escaped = true;
                        let result = self.visit(&mut template);
                        self.escaped = false;
                        result?;
                        *expr = template;
                        return Ok(());
                    }

                    if let Some(expanded) = self.vec_expr_datum_to_syntax(&l.args)? {
                        // return Ok(expanded);
                        *expr = expanded;
                        return Ok(());
                    }

                    if let Some(mut expanded) = self.vec_expr_syntax_const_if(&l.args)? {
                        self.visit(&mut expanded)?;

                        *expr = expanded;

                        return Ok(());

                        // return self.visit(expanded);
                    }

                    // An improper list spliced in at the end makes this one improper
                    if self.visit_elements(&mut l.args)? {
                        l.improper = true;
                    }

                    if l.improper {
                        l.make_improper();
                    }

                    if let Some(expanded) = self.vec_syntax_span_object(&l.args)? {
                        *expr = expanded;
                        return Ok(());
                    }

                    if let Some(expanded) = lower_quasisyntax(l) {
                        *expr = expanded;
                        return Ok(());
                    }

                    Ok(())
                }
                ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
                ExprKind::Set(s) => self.visit_set(s),
                ExprKind::Require(r) => self.visit_require(r),
                ExprKind::Vector(v) => self.visit_vector(v),
            }
        })
    }

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr)?;
        self.visit(&mut f.then_expr)?;
        self.visit(&mut f.else_expr)?;
        Ok(())
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        if let ExprKind::List(l) = &define.name {
            if let Some(expanded) = self.vec_expr_datum_to_syntax(&l.args)? {
                define.name = expanded
            }
        }
        self.visit(&mut define.name)?;
        self.visit(&mut define.body)?;

        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        let improper = self.visit_elements(&mut lambda_function.args)?;

        self.visit(&mut lambda_function.body)?;

        if improper {
            lambda_function.rest = true;
        }

        // TODO: @Matt - 2/28/12 -> clean up this
        // This mangles the values
        // lambda_function.args.iter_mut().for_each(|x| {
        //     if let ExprKind::Atom(Atom {
        //         syn: SyntaxObject { ty: t, .. },
        //     }) = x
        //     {
        //         // log::debug!("Checking if expression needs to be rewritten: {:?}", t);
        //         reserved_token_type_to_ident(t);
        //     }

        //     // if let ExprKind::Define(d) = x {
        //     //     log::debug!("Found a define to be rewritten: {:?}", d);
        //     // }
        // });

        Ok(())
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        let mut exprs: ThinVec<_> = core::mem::take(&mut begin.exprs).into_iter().collect();
        self.visit_elements(&mut exprs)?;
        begin.exprs = exprs.into_iter().collect();

        Ok(())
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) -> Self::Output {
        self.visit(&mut quote.expr)
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        stop!(BadSyntax => format!("unexpected macro definition: {}", m); m.location.span)
    }

    // Lift this up to the visit function
    fn visit_atom(&mut self, _a: &mut Atom) -> Self::Output {
        // self.expand_atom(a)

        Ok(())

        // todo!()
    }

    // Lift this up to the visit function
    fn visit_list(&mut self, _l: &mut super::ast::List) -> Self::Output {
        Ok(())

        // if let Some(expanded) = self.vec_expr_datum_to_syntax(&l.args)? {
        //     return Ok(expanded);
        // }

        // if let Some(expanded) = self.vec_expr_syntax_const_if(&l.args)? {
        //     return self.visit(expanded);
        // }

        // l.args = self.expand_ellipses(l.args)?;
        // l.args = l
        //     .args
        //     .into_iter()
        //     .map(|e| self.visit(e))
        //     .collect::<Result<Vec<_>>>()?;

        // if let Some(expanded) = self.vec_syntax_span_object(&l.args)? {
        //     return Ok(expanded);
        // }

        // todo!()

        // Ok(ExprKind::List(l))
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        stop!(Generic => "unexpected syntax-rules definition"; l.location.span)
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable)?;
        self.visit(&mut s.expr)?;

        Ok(())
    }

    fn visit_require(&mut self, s: &mut super::ast::Require) -> Self::Output {
        for expr in s.modules.iter_mut() {
            self.visit(expr)?;
        }

        Ok(())

        // stop!(Generic => "unexpected require statement in replace idents"; s.location.span)
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        // let mut visited_bindings = Vec::new();

        // let (bindings, exprs): (Vec<_>, Vec<_>) = l.bindings.iter().cloned().unzip();

        for (binding, expr) in l.bindings.iter_mut() {
            // self.expand_ellipses(binding);

            self.visit(binding)?;
            self.visit(expr)?;
        }

        // let bindings = self.expand_ellipses(bindings)?;

        // for (binding, expr) in bindings.into_iter().zip(exprs) {
        //     visited_bindings.push((self.visit(binding)?, self.visit(expr)?));
        // }

        // l.bindings = visited_bindings;
        // l.body_expr = self.visit(l.body_expr)?;

        self.visit(&mut l.body_expr)?;

        Ok(())
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Self::Output {
        if v.bytes {
            return Ok(());
        }

        self.visit_elements(&mut v.args)?;

        Ok(())
    }
}

pub struct RewriteSpan {
    span: Span,
}

impl RewriteSpan {
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

// TODO replace spans on all of the nodes and atoms
impl VisitorMutRefUnit for RewriteSpan {
    fn visit_if(&mut self, f: &mut super::ast::If) {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
        f.location.set_span(self.span);
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) {
        self.visit(&mut define.name);
        self.visit(&mut define.body);
        define.location.set_span(self.span);
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut super::ast::LambdaFunction) {
        lambda_function.args.iter_mut().for_each(|e| self.visit(e));
        self.visit(&mut lambda_function.body);
        lambda_function.location.set_span(self.span);
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) {
        begin.exprs.iter_mut().for_each(|e| self.visit(e));
        begin.location.set_span(self.span);
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) {
        self.visit(&mut r.expr);
        r.location.set_span(self.span);
    }

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) {
        self.visit(&mut quote.expr);
        quote.location.set_span(self.span);
    }

    fn visit_macro(&mut self, _m: &mut super::ast::Macro) {}

    fn visit_atom(&mut self, a: &mut Atom) {
        // Overwrite the span on any atoms
        a.syn.set_span(self.span);
    }

    fn visit_list(&mut self, l: &mut super::ast::List) {
        // if let Some(first) = l.first_ident() {
        //     if *first == *QUASISYNTAX || *first == *SYNTAX_QUOTE {
        //         return Ok(ExprKind::List(l));
        //     }
        // }

        l.args.iter_mut().for_each(|e| self.visit(e));
    }

    fn visit_syntax_rules(&mut self, _l: &mut super::ast::SyntaxRules) {}

    fn visit_set(&mut self, s: &mut super::ast::Set) {
        self.visit(&mut s.variable);
        self.visit(&mut s.expr);
    }

    fn visit_require(&mut self, s: &mut super::ast::Require) {
        s.modules.iter_mut().for_each(|x| self.visit(x));

        // stop!(Generic => "unexpected require statement in replace idents"; s.location.span)
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) {
        for (binding, expr) in l.bindings.iter_mut() {
            self.visit(binding);
            self.visit(expr);
        }

        self.visit(&mut l.body_expr);
    }
}

#[cfg(test)]
mod replace_expressions_tests {
    use crate::parser::ast::{LambdaFunction, List};

    use super::*;

    macro_rules! map {
        ($ ( $key:expr => $value:expr ), *,) => {{
            let mut hm: FxHashMap<InternedString, ExprKind> = FxHashMap::default();
            $ (hm.insert($key.into(), $value); ) *
            hm
        }};
    }

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn ellipses() -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Ellipses)))
    }

    // TODO -> move this to ExprKind
    // fn atom_int(n: isize) -> ExprKind {
    //     ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
    //         n,
    //     ))))
    // }

    // TODO replace this test with something that doesn't use transduce
    // #[test]
    // fn test_expand_atom() {
    //     let bindings = map! {
    //         "apples" => atom_identifier("x"),
    //         "bananas" => atom_identifier("y"),
    //         "number" => atom_int(1),
    //     };

    //     let expr = ExprKind::If(Box::new(If::new(
    //         atom_identifier("test-condition"),
    //         ExprKind::Transduce(Box::new(Transduce::new(
    //             atom_identifier("apples"),
    //             atom_identifier("bananas"),
    //             atom_identifier("number"),
    //             atom_identifier("z"),
    //             SyntaxObject::default(TokenType::Transduce),
    //         ))),
    //         atom_identifier("else-condition"),
    //         SyntaxObject::default(TokenType::If),
    //     )));

    //     let post_condition = ExprKind::If(Box::new(If::new(
    //         atom_identifier("test-condition"),
    //         ExprKind::Transduce(Box::new(Transduce::new(
    //             atom_identifier("x"),
    //             atom_identifier("y"),
    //             atom_int(1),
    //             atom_identifier("z"),
    //             SyntaxObject::default(TokenType::Transduce),
    //         ))),
    //         atom_identifier("else-condition"),
    //         SyntaxObject::default(TokenType::If),
    //     )));

    //     let output = ReplaceExpressions::new(&bindings).visit(expr).unwrap();

    //     assert_eq!(output, post_condition);
    // }

    #[test]
    fn test_expand_datum_syntax() {
        let mut bindings = map! {
            "##struct-name" => atom_identifier("apple"),
        };

        let mut expr = ExprKind::List(List::new(thin_vec![
            atom_identifier("datum->syntax"),
            atom_identifier("struct-name"),
            atom_identifier("?"),
        ]));

        let post_condition = atom_identifier("apple?");

        ReplaceExpressions::new(
            &mut bindings,
            &mut FxHashMap::default(),
            &mut FxHashMap::default(),
        )
        .visit(&mut expr)
        .unwrap();

        assert_eq!(expr, post_condition);
    }

    #[test]
    fn test_expand_ellipses() {
        let mut bindings = map! {
            "apple" => atom_identifier("x"),
            "many" => ExprKind::List(List::new(thin_vec![
                atom_identifier("first"),
                atom_identifier("second")
            ])),
        };

        let mut expr = ExprKind::List(List::new(thin_vec![
            atom_identifier("apple"),
            atom_identifier("many"),
            ellipses(),
        ]));

        let post_condition = ExprKind::List(List::new(thin_vec![
            atom_identifier("x"),
            atom_identifier("first"),
            atom_identifier("second"),
        ]));

        ReplaceExpressions::new(
            &mut bindings,
            &mut FxHashMap::default(),
            &mut FxHashMap::default(),
        )
        .visit(&mut expr)
        .unwrap();

        assert_eq!(expr, post_condition);
    }

    #[test]
    fn test_lambda_expression() {
        let mut bindings = map! {
            "apple" => atom_identifier("x"),
            "many" => ExprKind::List(List::new(thin_vec![
                atom_identifier("first-arg"),
                atom_identifier("second-arg")
            ])),
        };

        let mut expr: ExprKind = LambdaFunction::new(
            thin_vec![atom_identifier("many"), ellipses()],
            atom_identifier("apple"),
            SyntaxObject::default(TokenType::Lambda),
        )
        .into();

        let post_condition = LambdaFunction::new(
            thin_vec![atom_identifier("first-arg"), atom_identifier("second-arg")],
            atom_identifier("x"),
            SyntaxObject::default(TokenType::Lambda),
        )
        .into();

        ReplaceExpressions::new(
            &mut bindings,
            &mut FxHashMap::default(),
            &mut FxHashMap::default(),
        )
        .visit(&mut expr)
        .unwrap();

        assert_eq!(expr, post_condition);
    }
}
