use std::ops::ControlFlow;

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
        }
    }

    pub fn verify(mut self, expr: &ExprKind) -> Result<(), SteelErr> {
        let _ = self.visit(expr);

        self.result
    }

    fn visit_list_elements(&mut self, elements: &[ExprKind], list_ty: ListType) -> ControlFlow<()> {
        let mut iter = elements.iter().enumerate().peekable();
        let len = elements.len();

        while let Some((i, expr)) = iter.next() {
            let ellipsis =
                expr.atom_syntax_object().map(|syn| syn.ty.clone()) == Some(TokenType::Ellipses);

            // special case: (... expr)
            if i == 0 && len == 2 && ellipsis && list_ty == ListType::Normal {
                let (_, next) = iter.next().unwrap();

                self.visit(next)?;

                return ControlFlow::Continue(());
            }

            let ellipsis = iter
                .peek()
                .and_then(|(_, expr)| expr.atom_syntax_object())
                .filter(|syn| match syn.ty {
                    TokenType::Ellipses => true,
                    TokenType::Identifier(ident) if &ident == &*ELLIPSES_SYMBOL => true,
                    _ => false,
                });

            let is_many = if let Some(ellipsis) = ellipsis {
                if list_ty == ListType::Improper && i + 2 == len {
                    self.result = steelerr![BadSyntax => "ellipsis cannot appear as list tail"; ellipsis.span];
                }

                true
            } else {
                false
            };

            if is_many {
                self.depth += 1;
            }

            self.visit(expr)?;

            if is_many {
                self.depth -= 1;

                let _ = iter.next();
            }
        }

        ControlFlow::Continue(())
    }
}

impl VisitorMutControlFlow for MacroTemplate {
    #[inline]
    fn visit_list(&mut self, l: &List) -> ControlFlow<()> {
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
        if atom.syn.ty == TokenType::Ellipses {
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
