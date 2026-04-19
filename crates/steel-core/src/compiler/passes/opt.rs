use steel_parser::{
    ast::{Atom, ExprKind},
    interner::InternedString,
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
            RemoveLetsBoundToOtherLocalVars {
                scope: Vec::new(),
                args: Vec::new(),
            }
            .visit(expr);
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

pub struct RemoveLetsBoundToOtherLocalVars {
    // Variables that are in scope. The RHS has
    // to be something that is local, otherwise
    // we're capturing a global reference and we
    // need that to be constant.
    args: Vec<InternedString>,
    scope: Vec<Vec<(InternedString, Option<InternedString>)>>,
}

impl VisitorMutRefUnit for RemoveLetsBoundToOtherLocalVars {
    fn visit_lambda_function(&mut self, lambda_function: &mut steel_parser::ast::LambdaFunction) {
        for var in &mut lambda_function.args {
            if let Some(ident) = var.atom_identifier() {
                self.args.push(*ident);
            }
        }

        // println!(
        //     "Bound: {:#?}",
        //     self.args.iter().map(|x| x.resolve()).collect::<Vec<_>>()
        // );

        self.visit(&mut lambda_function.body);

        for _ in &lambda_function.args {
            self.args.pop();
        }
    }

    fn visit_atom(&mut self, a: &mut Atom) {
        if let Some(ident) = a.ident().copied() {
            // Go through each scope
            for scope in self.scope.iter().rev() {
                if let Some(r) = scope
                    .iter()
                    .find_map(|x| if x.0 == ident { Some(x.1) } else { None })
                {
                    if let Some(r) = r {
                        println!("Replacing {} with {}", a, r);
                        *a.ident_mut().unwrap() = r;
                    }

                    break;
                } else {
                    continue;
                }
            }
        }
    }

    fn visit_let(&mut self, l: &mut steel_parser::ast::Let) {
        let mut bound = Vec::new();

        for (ident, expr) in l.bindings.iter_mut() {
            // (let ((a b))
            //     ;; Replace all usages of a with b?
            //     ;; Remove it afterwards
            // )
            self.visit(expr);
            if let Some(left) = ident.atom_identifier() {
                let rhs = expr.atom_identifier().copied();

                if let Some(rhs) = rhs {
                    if !self.args.contains(&rhs) {
                        // We'll allow these to move forward - we need to readjust
                        // this optimization to also contains the analysis so that
                        // we can check that these aren't mutable.
                        //
                        // Then, we can move stuff around intra module, and then also
                        // run another small inlining pass
                        if !rhs.resolve().starts_with("##__lifted_pure_function")
                            && !rhs.resolve().starts_with("#%prim.")
                        {
                            println!("Found unbound ref: {} -> {}", left, rhs);
                            bound.push((*left, None));
                            continue;
                        }
                    }
                }

                bound.push((*left, rhs));
            }
        }

        // Bind the locals here as well
        for (ident, _) in l.bindings.iter() {
            if let Some(ident) = ident.atom_identifier() {
                self.args.push(*ident);
            }
        }

        self.scope.push(bound);
        self.visit(&mut l.body_expr);

        // Unbind the local values here
        for (ident, _) in l.bindings.iter() {
            if let Some(_) = ident.atom_identifier() {
                self.args.pop();
            }
        }

        let bound = self.scope.pop().unwrap();

        if bound.is_empty() {
            return;
        }

        l.bindings.retain(|(ident, expr)| {
            if ident.atom_identifier().is_some() && expr.atom_identifier().is_some() {
                let rhs = expr.atom_identifier().unwrap();
                if self.args.contains(rhs) {
                    return false;
                }

                if rhs.resolve().starts_with("##__lifted_pure_function")
                    || rhs.resolve().starts_with("#%prim.")
                {
                    return false;
                }
            }

            true
        });
    }
}

#[cfg(test)]
mod let_var_tests {
    use steel_parser::{
        ast::AstTools,
        parser::{ParseError, Parser},
    };

    use super::*;

    #[test]
    fn plain_lets_remove_local_bind_values() {
        let expr = r#"
(lambda (baz)
    (%plain-let ((bar baz))
        (%plain-let ((bananas bar))
            (displayln bananas))))
        "#;

        let parsed: core::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr.as_ref(), None).collect();

        let mut parsed = parsed.unwrap();

        for expr in &mut parsed {
            RemoveLetsBoundToOtherLocalVars {
                scope: Vec::new(),
                args: Vec::new(),
            }
            .visit(expr);
        }

        parsed.pretty_print();
    }

    #[test]
    fn complex_let_var_aliasing() {
        let expr = r#"

(define mm5609__%#__maps-rest
        (λ (source2
            target2
            pas2
            rest2
            to-12
            to-collect2)
          (if (#%prim.null? rest2)
            (to-12 pas2)
            (%plain-let ((next3 (#%prim.car rest2))
                (rest3 (#%prim.cdr rest2)))
              (to-collect2
                 (%plain-let ((function2 (λ (x4)
                       (%plain-let ((pas25 (#%prim.cons
                              (#%prim.cons next3 x4)
                              pas2))
                           (rest25 rest3))
                         (if (#%prim.null? rest25)
                           (to-12 pas25)
                           (%plain-let ((next36 (#%prim.car
                                  rest25))
                               (rest36 (#%prim.cdr
                                  rest25)))
                             (to-collect2
                                (mm5609__%#__map
                                   (λ (x47)
                                     (mm5609__%#__maps-rest
                                        source2
                                        target2
                                        (#%prim.cons
                                           (#%prim.cons
                                              next36
                                              x47)
                                           pas25)
                                        rest36
                                        to-12
                                        to-collect2))
                                   (mm5609__%#__maps-1
                                      source2
                                      target2
                                      pas25
                                      next36))))))))
                     (list12 (%plain-let ((new2 next3))
                       (%plain-let ((scmp3 (#%prim.cdr
                              source2))
                           (tcmp3 (#%prim.cdr target2)))
                         (%plain-let ((less4 (mm5609__%#__select-map
                                (λ (p4)
                                  (#%prim.eq?
                                     (quote
                                       less)
                                     (scmp3
                                        (#%prim.car p4)
                                        new2)))
                                #%prim.cdr
                                pas2))
                             (more4 (mm5609__%#__select-map
                                (λ (p4)
                                  (#%prim.eq?
                                     (quote
                                       more)
                                     (scmp3
                                        (#%prim.car p4)
                                        new2)))
                                #%prim.cdr
                                pas2)))
                           (mm5609__%#__zulu-select
                              (λ (t5)
                                (if (mm5609__%#__map-and
                                     (λ (t26)
                                       (#%prim.memq
                                          (tcmp3
                                             t26
                                             t5)
                                          (quote
                                            (less equal))))
                                     less4)
                                  (mm5609__%#__map-and
                                     (λ (t26)
                                       (#%prim.memq
                                          (tcmp3
                                             t26
                                             t5)
                                          (quote
                                            (more equal))))
                                     more4)
                                  #false))
                              (#%prim.car target2)))))))
                   (begin
                    (closure-lifting-985229
                            function2
                            (quote
                              ())
                            list12
                            source2
                            target2
                            pas2
                            to-12
                            to-collect2
                            next3
                            rest3))))))))
            
        "#;

        let parsed: core::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr.as_ref(), None).collect();

        let mut parsed = parsed.unwrap();

        for expr in &mut parsed {
            RemoveLetsBoundToOtherLocalVars {
                scope: Vec::new(),
                args: Vec::new(),
            }
            .visit(expr);
        }

        parsed.pretty_print();
    }
}
