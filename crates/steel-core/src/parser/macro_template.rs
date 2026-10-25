use core::ops::ControlFlow;

use rustc_hash::FxHashMap;
use steel_parser::{
    ast::{Atom, Begin, ExprKind, List, Vector},
    interner::InternedString,
    tokens::TokenType,
};

use crate::{
    compiler::{passes::VisitorMutControlFlow, program::ELLIPSES_SYMBOL},
    SteelErr,
};

pub struct MacroTemplate {
    bindings: FxHashMap<InternedString, u8>,
    depth: u8,
    result: Result<(), SteelErr>,
    escaped: bool,
    // The clauses of a syntax-case transformer, which are code. In a template that's
    // output, a syntax-case is data the template fills in.
    transformer_code: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ListType {
    Improper,
    Vec,
    Normal,
    Syntax,
}

impl MacroTemplate {
    pub fn new(bindings: FxHashMap<InternedString, u8>) -> Self {
        Self {
            bindings,
            depth: 0,
            result: Ok(()),
            escaped: false,
            transformer_code: false,
        }
    }

    pub fn of_transformer_code(mut self, transformer_code: bool) -> Self {
        self.transformer_code = transformer_code;
        self
    }

    pub fn verify(mut self, expr: &ExprKind) -> Result<(), SteelErr> {
        let _ = self.visit(expr);

        self.result
    }

    fn visit_list_elements(&mut self, elements: &[ExprKind], list_ty: ListType) -> ControlFlow<()> {
        let mut iter = elements.iter().enumerate().peekable();
        let len = elements.len();

        while let Some((i, expr)) = iter.next() {
            // Note: `(... template)` treats the ellipses in `template` as identifiers, and we have this
            // silly business with the TokenType::Ellipses we have to deal with. So while we still have that,
            // we have to hack around it.
            let ellipsis =
                expr.atom_syntax_object().map(|syn| syn.ty.clone()) == Some(TokenType::Ellipses);

            if !self.escaped && i == 0 && len == 2 && ellipsis && list_ty == ListType::Normal {
                let (_, next) = iter.next().unwrap();

                self.escaped = true;
                let result = self.visit(next);
                self.escaped = false;

                return result;
            }

            if self.escaped {
                self.visit(expr)?;
                continue;
            }

            // `x ... ...` repeats `x` at one more depth for each ellipsis
            let ellipses = elements[i + 1..]
                .iter()
                .take_while(|expr| {
                    expr.atom_syntax_object().is_some_and(|syn| match syn.ty {
                        TokenType::Ellipses => true,
                        TokenType::Identifier(ident) => ident == *ELLIPSES_SYMBOL,
                        _ => false,
                    })
                })
                .count();

            if ellipses > 0 && list_ty == ListType::Improper && i + 1 + ellipses == len {
                let span = elements[len - 1].atom_syntax_object().unwrap().span;
                self.result = steelerr![BadSyntax => "ellipsis cannot appear as list tail"; span];
            }

            if ellipses > 1 {
                if let Some(deepest) = self.deepest(expr) {
                    if deepest < self.depth + ellipses as u8 {
                        let span = elements[i + ellipses].atom_syntax_object().unwrap().span;
                        self.result = steelerr![BadSyntax => "too many ellipses in template"; span];
                        return ControlFlow::Break(());
                    }
                }
            }

            self.depth += ellipses as u8;

            self.visit(expr)?;

            self.depth -= ellipses as u8;

            for _ in 0..ellipses {
                let _ = iter.next();
            }
        }

        ControlFlow::Continue(())
    }
}

impl MacroTemplate {
    // The deepest pattern variable in `expr`, if every identifier in it is one
    fn deepest(&self, expr: &ExprKind) -> Option<u8> {
        struct Deepest<'a> {
            bindings: &'a FxHashMap<InternedString, u8>,
            deepest: Option<u8>,
            transformer_code: bool,
        }

        impl VisitorMutControlFlow for Deepest<'_> {
            fn visit_atom(&mut self, atom: &Atom) -> ControlFlow<()> {
                let Some(ident) = atom.ident() else {
                    return ControlFlow::Continue(());
                };

                if *ident == *ELLIPSES_SYMBOL {
                    return ControlFlow::Continue(());
                }

                match self.bindings.get(ident) {
                    Some(depth) => {
                        self.deepest = Some(self.deepest.unwrap_or(0).max(*depth));
                        ControlFlow::Continue(())
                    }
                    // In a syntax-rules template every pattern variable is known, so another
                    // identifier is a literal. Transformer code can bind ones with with-syntax.
                    None if !self.transformer_code => ControlFlow::Continue(()),
                    None => {
                        self.deepest = None;
                        ControlFlow::Break(())
                    }
                }
            }
        }

        let mut visitor = Deepest {
            bindings: &self.bindings,
            deepest: None,
            transformer_code: self.transformer_code,
        };

        match visitor.visit(expr) {
            ControlFlow::Continue(()) => visitor.deepest,
            ControlFlow::Break(()) => None,
        }
    }
}

// The pattern variables of a pattern, with how many ellipses each is under
fn pattern_variables(
    pattern: &ExprKind,
    literals: &[InternedString],
    depth: u8,
    out: &mut Vec<(InternedString, u8)>,
) {
    let is_ellipsis = |expr: &ExprKind| {
        expr.atom_syntax_object().is_some_and(|syn| match syn.ty {
            TokenType::Ellipses => true,
            TokenType::Identifier(ident) => ident == *ELLIPSES_SYMBOL,
            _ => false,
        })
    };

    match pattern {
        ExprKind::Atom(a) => {
            if let Some(name) = a.ident() {
                if name.resolve() != "_" && *name != *ELLIPSES_SYMBOL && !literals.contains(name) {
                    out.push((*name, depth));
                }
            }
        }
        ExprKind::List(l) => {
            let args = &l.args;
            for (i, arg) in args.iter().enumerate() {
                if is_ellipsis(arg) {
                    continue;
                }
                let repeated = args.get(i + 1).is_some_and(is_ellipsis);
                pattern_variables(arg, literals, depth + repeated as u8, out);
            }
        }
        ExprKind::Vector(v) => {
            for (i, arg) in v.args.iter().enumerate() {
                if is_ellipsis(arg) {
                    continue;
                }
                let repeated = v.args.get(i + 1).is_some_and(is_ellipsis);
                pattern_variables(arg, literals, depth + repeated as u8, out);
            }
        }
        _ => {}
    }
}

impl MacroTemplate {
    // Visits `body` with `variables` bound as pattern variables, over the ones they shadow
    fn shadowed(
        &mut self,
        variables: Vec<(InternedString, u8)>,
        body: &[ExprKind],
    ) -> ControlFlow<()> {
        let saved: Vec<_> = variables
            .iter()
            .map(|(name, _)| (*name, self.bindings.get(name).copied()))
            .collect();

        for (name, depth) in variables {
            self.bindings.insert(name, depth);
        }

        let result = body.iter().try_for_each(|x| self.visit(x));

        for (name, old) in saved {
            match old {
                Some(depth) => self.bindings.insert(name, depth),
                None => self.bindings.remove(&name),
            };
        }

        result
    }

    // A syntax-case or with-syntax in the template's code, whose patterns aren't templates
    // and bind pattern variables in their bodies
    fn binding_form(&mut self, l: &List) -> Option<ControlFlow<()>> {
        if !self.transformer_code || self.escaped || l.improper {
            return None;
        }

        match l.first_ident().map(|x| x.resolve()) {
            Some("syntax-case" | "#%syntax-case") if l.args.len() >= 3 => {
                let literals: Vec<InternedString> = l.args[2]
                    .list()
                    .map(|x| {
                        x.args
                            .iter()
                            .filter_map(|x| x.atom_identifier().copied())
                            .collect()
                    })
                    .unwrap_or_default();

                Some((|| {
                    self.visit(&l.args[1])?;

                    for clause in &l.args[3..] {
                        let Some(clause) = clause.list() else {
                            self.visit(clause)?;
                            continue;
                        };

                        let mut variables = Vec::new();
                        if let Some(pattern) = clause.args.first() {
                            pattern_variables(pattern, &literals, self.depth, &mut variables);
                        }

                        self.shadowed(variables, clause.args.get(1..).unwrap_or_default())?;
                    }

                    ControlFlow::Continue(())
                })())
            }
            Some("with-syntax") if l.args.len() >= 2 => {
                let bindings = l.args[1].list()?;

                Some((|| {
                    let mut variables = Vec::new();

                    for binding in &bindings.args {
                        match binding.list() {
                            Some(binding) => {
                                if let Some(pattern) = binding.args.first() {
                                    pattern_variables(pattern, &[], self.depth, &mut variables);
                                }
                                binding
                                    .args
                                    .iter()
                                    .skip(1)
                                    .try_for_each(|x| self.visit(x))?;
                            }
                            None => self.visit(binding)?,
                        }
                    }

                    self.shadowed(variables, &l.args[2..])
                })())
            }
            _ => None,
        }
    }
}

impl VisitorMutControlFlow for MacroTemplate {
    #[inline]
    fn visit_list(&mut self, l: &List) -> ControlFlow<()> {
        if let Some(result) = self.binding_form(l) {
            return result;
        }

        self.visit_list_elements(
            &l.args,
            if l.improper {
                ListType::Improper
            } else {
                ListType::Normal
            },
        )
    }

    #[inline]
    fn visit_vector(&mut self, v: &Vector) -> ControlFlow<()> {
        if v.bytes {
            return ControlFlow::Continue(());
        }

        self.visit_list_elements(&v.args, ListType::Vec)
    }

    #[inline]
    fn visit_begin(&mut self, begin: &Begin) -> ControlFlow<()> {
        self.visit_list_elements(&begin.exprs, ListType::Syntax)
    }

    #[inline]
    fn visit_atom(&mut self, atom: &Atom) -> ControlFlow<()> {
        if atom.syn.ty == TokenType::Ellipses && !self.escaped {
            self.result = steelerr![BadSyntax => "ellipses are not a valid identifier in templates"; atom.syn.span];
            return ControlFlow::Break(());
        }

        let Some(ident) = atom.ident() else {
            return ControlFlow::Continue(());
        };

        let Some(pattern_depth) = self.bindings.get(ident).copied() else {
            return ControlFlow::Continue(());
        };

        if pattern_depth > self.depth {
            let missing = pattern_depth - self.depth;
            let name = if missing > 1 { "ellipses" } else { "ellipsis" };

            self.result = steelerr![BadSyntax => format!("missing {}: pattern variable needs at least {} levels of repetition, found {}", name, pattern_depth, self.depth);  atom.syn.span];

            return ControlFlow::Break(());
        }

        ControlFlow::Continue(())
    }
}
