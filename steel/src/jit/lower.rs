use crate::parser::ast;
use crate::parser::ast::ExprKind;
use crate::parser::visitors::VisitorMut;

use im_rc::HashMap;

// ((lambda (x y z) (+ x y z)) 10 20 30)
// =>
// (x = 10)
// (y = 20)
// (z = 30)
// (+ x (+ y z))
struct RenameScopedVars {
    vars: HashMap<String, String>,
}

impl VisitorMut for RenameScopedVars {
    type Output = Option<ExprKind>;

    fn visit_if(&mut self, f: &ast::If) -> Self::Output {
        todo!()
    }

    fn visit_define(&mut self, define: &ast::Define) -> Self::Output {
        todo!()
    }

    fn visit_lambda_function(&mut self, lambda_function: &ast::LambdaFunction) -> Self::Output {
        todo!()
    }

    fn visit_begin(&mut self, begin: &ast::Begin) -> Self::Output {
        todo!()
    }

    fn visit_return(&mut self, r: &ast::Return) -> Self::Output {
        todo!()
    }

    fn visit_apply(&mut self, apply: &ast::Apply) -> Self::Output {
        todo!()
    }

    fn visit_panic(&mut self, p: &ast::Panic) -> Self::Output {
        todo!()
    }

    fn visit_transduce(&mut self, transduce: &ast::Transduce) -> Self::Output {
        todo!()
    }

    fn visit_read(&mut self, read: &ast::Read) -> Self::Output {
        todo!()
    }

    fn visit_execute(&mut self, execute: &ast::Execute) -> Self::Output {
        todo!()
    }

    fn visit_quote(&mut self, quote: &ast::Quote) -> Self::Output {
        todo!()
    }

    fn visit_struct(&mut self, s: &ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_eval(&mut self, e: &ast::Eval) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &ast::Atom) -> Self::Output {
        todo!()
    }

    fn visit_list(&mut self, l: &ast::List) -> Self::Output {
        todo!()
    }

    fn visit_syntax_rules(&mut self, l: &ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    fn visit_set(&mut self, s: &ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, s: &ast::Require) -> Self::Output {
        todo!()
    }

    fn visit_callcc(&mut self, cc: &ast::CallCC) -> Self::Output {
        todo!()
    }
}
