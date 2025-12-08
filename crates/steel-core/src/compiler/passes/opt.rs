use steel_parser::{
    ast::{Atom, ExprKind},
    parser::SyntaxObject,
    tokens::TokenType,
};

use crate::compiler::program::PRIM_NOT;

use super::VisitorMutRefUnit;

pub struct SingleExprOptimizer;

impl SingleExprOptimizer {
    pub fn run(exprs: &mut Vec<ExprKind>) {
        // Might be able to parallelize this? If it takes long enough?
        // TODO: Have some heuristic for how big the expressions would have to
        // be to justify a parallelization. Same thing for module expansion.
        for expr in exprs {
            FlipNotCondition.visit(expr);
            PruneConstantIfBranches.visit(expr);
        }
    }
}

pub struct FlipNotCondition;

impl FlipNotCondition {
    pub fn run(exprs: &mut Vec<ExprKind>) {
        for expr in exprs {
            Self.visit(expr);
        }
    }
}

fn not_expr_target(expr: &mut ExprKind) -> Option<&mut ExprKind> {
    if let ExprKind::List(l) = expr {
        if l.len() == 2 && l.first_ident().copied() == Some(*PRIM_NOT) {
            return l.args.get_mut(1);
        }
    }
    None
}

impl VisitorMutRefUnit for FlipNotCondition {
    // Check if the test condition is equal to `(not <expr>)`, and if so, replace the condition
    // with the inner and re order the then/else statement
    fn visit_if(&mut self, f: &mut steel_parser::ast::If) {
        if let Some(not_target_expr) = not_expr_target(&mut f.test_expr) {
            let inner = core::mem::replace(not_target_expr, ExprKind::empty());
            f.test_expr = inner;
            core::mem::swap(&mut f.then_expr, &mut f.else_expr);
        }

        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.then_expr);
    }
}

pub struct PruneConstantIfBranches;

impl PruneConstantIfBranches {
    pub fn run(exprs: &mut Vec<ExprKind>) {
        for expr in exprs {
            Self.visit(expr);
        }
    }
}

fn expr_is_truthy(expr: &ExprKind) -> bool {
    match expr {
        ExprKind::Atom(Atom { syn, .. }) => is_truthy(syn),
        ExprKind::Quote(_) => true,
        _ => false,
    }
}

fn is_truthy(t: &SyntaxObject) -> bool {
    match &t.ty {
        TokenType::BooleanLiteral(b) => *b,
        TokenType::Number(_) | TokenType::StringLiteral(_) | TokenType::CharacterLiteral(_) => true,
        _ => false,
    }
}

impl VisitorMutRefUnit for PruneConstantIfBranches {
    fn visit(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::If(f) => {
                if expr_is_truthy(&f.test_expr) {
                    *expr = core::mem::replace(&mut f.then_expr, ExprKind::empty());
                    self.visit(expr);
                } else {
                    self.visit_if(f)
                }
            }
            ExprKind::Define(d) => self.visit_define(d),
            ExprKind::LambdaFunction(l) => self.visit_lambda_function(l),
            ExprKind::Begin(b) => self.visit_begin(b),
            ExprKind::Return(r) => self.visit_return(r),
            ExprKind::Quote(q) => self.visit_quote(q),
            ExprKind::Macro(m) => self.visit_macro(m),
            ExprKind::Atom(a) => self.visit_atom(a),
            ExprKind::List(l) => self.visit_list(l),
            ExprKind::SyntaxRules(s) => self.visit_syntax_rules(s),
            ExprKind::Set(s) => self.visit_set(s),
            ExprKind::Require(r) => self.visit_require(r),
            ExprKind::Let(l) => self.visit_let(l),
            ExprKind::Vector(v) => self.visit_vector(v),
        }
    }
}
