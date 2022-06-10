use std::collections::HashMap;

use quickscope::ScopeMap;

use crate::parser::{
    parser::{IdentifierMetadata, SyntaxObject},
    visitors::VisitorMutRef,
};

use super::VisitorMutRefUnit;

enum IdentifierStatus {
    Global,
    Local,
    Captured,
}

pub struct LexicalInformation {
    kind: IdentifierStatus,
    set_bang: bool,
}

// Populate the metadata about individual
pub struct Analysis {
    info: HashMap<SyntaxObject, LexicalInformation>,
}

#[derive(Default)]
pub struct MetaDataPopulator {
    depth: usize,
}

impl VisitorMutRefUnit for MetaDataPopulator {
    fn visit_if(&mut self, f: &mut crate::parser::ast::If) {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
    }

    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) {
        self.depth += 1;
        l.bindings.iter_mut().for_each(|x| self.visit(&mut x.1));
        self.depth -= 1;
        self.visit(&mut l.body_expr);
    }

    fn visit_define(&mut self, define: &mut crate::parser::ast::Define) {
        self.visit(&mut define.name);
        self.visit(&mut define.body);
    }

    fn visit_lambda_function(&mut self, lambda_function: &mut crate::parser::ast::LambdaFunction) {
        for var in &mut lambda_function.args {
            self.visit(var);
        }
        self.depth += 1;
        self.visit(&mut lambda_function.body);
        self.depth -= 1;
    }

    fn visit_begin(&mut self, begin: &mut crate::parser::ast::Begin) {
        for expr in &mut begin.exprs {
            self.visit(expr);
        }
    }

    fn visit_return(&mut self, r: &mut crate::parser::ast::Return) {
        self.visit(&mut r.expr);
    }

    fn visit_quote(&mut self, quote: &mut crate::parser::ast::Quote) {
        self.visit(&mut quote.expr);
    }

    fn visit_struct(&mut self, _s: &mut crate::parser::ast::Struct) {}

    fn visit_macro(&mut self, _m: &mut crate::parser::ast::Macro) {}

    fn visit_atom(&mut self, a: &mut crate::parser::ast::Atom) {
        // Populate metadata on the identifier if its not there
        match &mut a.syn.metadata {
            Some(m) => m.depth = self.depth,
            m @ None => *m = Some(IdentifierMetadata { depth: self.depth }),
        }
    }

    fn visit_list(&mut self, l: &mut crate::parser::ast::List) {
        for expr in &mut l.args {
            self.visit(expr);
        }
    }

    fn visit_syntax_rules(&mut self, _l: &mut crate::parser::ast::SyntaxRules) {}

    fn visit_set(&mut self, s: &mut crate::parser::ast::Set) {
        self.visit(&mut s.variable);
        self.visit(&mut s.expr);
    }

    fn visit_require(&mut self, _s: &mut crate::parser::ast::Require) {}

    fn visit_callcc(&mut self, cc: &mut crate::parser::ast::CallCC) {
        self.visit(&mut cc.expr);
    }
}

struct AnalysisPass<'a> {
    info: &'a mut LexicalInformation,
    scope: ScopeMap<String, bool>,
}

impl<'a> VisitorMutRef for AnalysisPass<'a> {
    type Output = ();

    fn visit_if(&mut self, f: &mut crate::parser::ast::If) -> Self::Output {
        todo!()
    }

    fn visit_define(&mut self, define: &mut crate::parser::ast::Define) -> Self::Output {
        todo!()
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        todo!()
    }

    fn visit_begin(&mut self, begin: &mut crate::parser::ast::Begin) -> Self::Output {
        todo!()
    }

    fn visit_return(&mut self, r: &mut crate::parser::ast::Return) -> Self::Output {
        todo!()
    }

    fn visit_quote(&mut self, quote: &mut crate::parser::ast::Quote) -> Self::Output {
        todo!()
    }

    fn visit_struct(&mut self, s: &mut crate::parser::ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &mut crate::parser::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &mut crate::parser::ast::Atom) -> Self::Output {
        let name = a.ident();

        if let Some(ident) = name {
            // If this contains a key at the top, then it shouldn't be marked as captured by this scope
            if self.scope.contains_key_at_top(ident) {
                // Set it to not be captured if its contained at the top level
                *(self.scope.get_mut(ident).unwrap()) = false;
                return;
            }

            // Otherwise, go ahead and mark it as captured if we can find a reference to it
            if let Some(is_captured) = self.scope.get_mut(ident) {
                *is_captured = true;
                return;
            }
        }
    }

    fn visit_list(&mut self, l: &mut crate::parser::ast::List) -> Self::Output {
        todo!()
    }

    fn visit_syntax_rules(&mut self, l: &mut crate::parser::ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    fn visit_set(&mut self, s: &mut crate::parser::ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, s: &mut crate::parser::ast::Require) -> Self::Output {
        todo!()
    }

    fn visit_callcc(&mut self, cc: &mut crate::parser::ast::CallCC) -> Self::Output {
        todo!()
    }

    fn visit_let(&mut self, l: &mut crate::parser::ast::Let) -> Self::Output {
        todo!()
    }
}
