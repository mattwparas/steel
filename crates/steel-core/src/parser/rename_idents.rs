use steel_parser::ast::{
    Atom, LAMBDA, LAMBDA_FN, PLAIN_LET, QUASIQUOTE, RAW_QUOTE, RAW_UNQUOTE, RAW_UNQUOTE_SPLICING,
    UNQUOTE, UNQUOTE_COMMA, UNQUOTE_SPLICING,
};

use crate::compiler::program::{DATUM_SYNTAX, LAMBDA_SYMBOL};
use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::VisitorMutRef;

use std::collections::HashSet;

use super::interner::InternedString;

pub struct RenameIdentifiersVisitor<'a> {
    introduced_identifiers: HashSet<InternedString>,
    // The binders in scope where the visit is, which references there are renamed to
    scope: Vec<InternedString>,
    // In a nested syntax-rules template, where code in quoted data is visited as one
    in_nested: bool,
    pattern_variables: &'a [InternedString],
    syntax: &'a [InternedString],
}

impl<'a> RenameIdentifiersVisitor<'a> {
    pub fn new(pattern_variables: &'a [InternedString], syntax: &'a [InternedString]) -> Self {
        RenameIdentifiersVisitor {
            introduced_identifiers: HashSet::new(),
            scope: Vec::new(),
            in_nested: false,
            pattern_variables,
            syntax,
        }
    }

    pub fn add(&mut self, ident: InternedString) {
        self.introduced_identifiers.insert(ident);
        self.scope.push(ident);
    }

    pub fn is_gensym(&self, ident: &InternedString) -> bool {
        self.scope.contains(ident) || self.pattern_variables.contains(ident)
    }

    // The defines of a body are in scope in all of it, before them too
    fn bind_defines(&mut self, forms: &[ExprKind]) {
        for form in forms {
            match form {
                ExprKind::Define(d) => self.bind_name(&d.name),
                ExprKind::Begin(b) => self.bind_defines(&b.exprs),
                ExprKind::List(l) => match l.first() {
                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Define,
                                ..
                            },
                    })) => match l.args.get(1) {
                        Some(ExprKind::List(signature)) => {
                            if let Some(name) = signature.args.first() {
                                self.bind_name(name);
                            }
                        }
                        Some(name) => self.bind_name(name),
                        None => {}
                    },
                    Some(ExprKind::Atom(Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::Begin,
                                ..
                            },
                    })) => self.bind_defines(&l.args[1..]),
                    _ if l.first_ident().is_some_and(|x| x.resolve() == "begin") => {
                        self.bind_defines(&l.args[1..])
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    // A parameter with a default, `[name default]`. The default is outside the parameter.
    fn bind_default_parameter(&mut self, parameter: &mut super::ast::List) {
        for expr in parameter.args.iter_mut().skip(1) {
            self.visit(expr);
        }

        if let Some(ExprKind::Atom(a)) = parameter.args.first_mut() {
            if let TokenType::Identifier(s) = a.syn.ty {
                if !self.pattern_variables.contains(&s) {
                    self.add(s);
                }

                a.syn = renamed_binder(a.syn.mark, s);
                a.syn.introduced_via_macro = true;
            }
        }
    }

    fn bind_name(&mut self, name: &ExprKind) {
        if let Some(s) = name.atom_identifier() {
            if !self.pattern_variables.contains(s) && *s != *DATUM_SYNTAX {
                self.add(*s);
            }
        }
    }

    // The names the template binds, as they are after renaming
    pub fn introduced_binders(&self) -> Vec<InternedString> {
        self.introduced_identifiers
            .iter()
            .map(|x| ("##".to_string() + x.resolve()).into())
            .collect()
    }

    pub fn rename_identifiers(&mut self, expr: &mut ExprKind) {
        self.bind_defines(core::slice::from_ref(expr));
        self.visit(expr);
    }

    // Okay so this is annoying: we'll have to handle quoted data in the template.
    // A pattern variable is still replaced by its argument, but a name the template introduces stays
    // whatever it was before.
    fn visit_data(&mut self, expr: &mut ExprKind, depth: Option<usize>) {
        match expr {
            ExprKind::Atom(a) => {
                if let TokenType::Identifier(s) = a.syn.ty {
                    if self.pattern_variables.contains(&s) {
                        a.syn.ty = TokenType::Identifier(("##".to_string() + s.resolve()).into());
                    }
                }
            }
            ExprKind::List(l) => {
                let depth = match (depth, l.first_ident()) {
                    (Some(depth), Some(head)) if *head == *QUASIQUOTE => Some(depth + 1),
                    (Some(depth), Some(head)) if is_unquote(head) => Some(depth - 1),
                    (depth, _) => depth,
                };
                for expr in &mut l.args {
                    if depth == Some(0) {
                        self.visit_code(expr);
                    } else {
                        self.visit_data(expr, depth);
                    }
                }
            }
            ExprKind::Vector(v) => {
                for expr in &mut v.args {
                    self.visit_data(expr, depth);
                }
            }
            ExprKind::Quote(q) => self.visit_data(&mut q.expr, depth),
            _ => self.visit_code(expr),
        }
    }

    fn visit_code(&mut self, expr: &mut ExprKind) {
        if self.in_nested {
            self.visit_nested_template(expr);
        } else {
            self.visit(expr);
        }
    }
}

impl RenameIdentifiersVisitor<'_> {
    // A nested syntax-rules, whose binders are its own
    fn visit_nested_template(&mut self, expr: &mut ExprKind) {
        match expr {
            ExprKind::Atom(a) => {
                if let TokenType::Identifier(s) = a.syn.ty {
                    if self.syntax.contains(&s) || s == *DATUM_SYNTAX {
                        return;
                    }

                    if self.is_gensym(&s) {
                        a.syn.ty = TokenType::Identifier(("##".to_string() + s.resolve()).into());
                    } else {
                        a.syn.unresolved = true;
                    }
                }
            }
            // Quoted data keeps its names, only pattern variables are replaced in it
            ExprKind::List(l) => match l.first() {
                Some(ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty, .. },
                })) if *ty == TokenType::Quote || *ty == TokenType::Identifier(*RAW_QUOTE) => {
                    for expr in &mut l.args[1..] {
                        self.visit_data(expr, None);
                    }
                }
                Some(ExprKind::Atom(Atom {
                    syn: SyntaxObject { ty, .. },
                })) if *ty == TokenType::Identifier(*QUASIQUOTE) => {
                    for expr in &mut l.args[1..] {
                        self.visit_data(expr, Some(1));
                    }
                }
                _ => l
                    .args
                    .iter_mut()
                    .for_each(|x| self.visit_nested_template(x)),
            },
            ExprKind::Vector(v) => v
                .args
                .iter_mut()
                .for_each(|x| self.visit_nested_template(x)),
            ExprKind::Quote(q) => self.visit_data(&mut q.expr, None),
            ExprKind::Define(d) => {
                self.visit_nested_template(&mut d.name);
                self.visit_nested_template(&mut d.body);
            }
            ExprKind::LambdaFunction(f) => {
                f.args
                    .iter_mut()
                    .for_each(|x| self.visit_nested_template(x));
                self.visit_nested_template(&mut f.body);
            }
            ExprKind::If(f) => {
                self.visit_nested_template(&mut f.test_expr);
                self.visit_nested_template(&mut f.then_expr);
                self.visit_nested_template(&mut f.else_expr);
            }
            ExprKind::Begin(b) => b
                .exprs
                .iter_mut()
                .for_each(|x| self.visit_nested_template(x)),
            ExprKind::Let(l) => {
                for (name, value) in l.bindings.iter_mut() {
                    self.visit_nested_template(name);
                    self.visit_nested_template(value);
                }
                self.visit_nested_template(&mut l.body_expr);
            }
            ExprKind::Set(set) => {
                self.visit_nested_template(&mut set.variable);
                self.visit_nested_template(&mut set.expr);
            }
            ExprKind::Return(r) => self.visit_nested_template(&mut r.expr),
            _ => {}
        }
    }
}

// A binder the template introduces, keeping the mark of the template that wrote it
fn renamed_binder(
    mark: crate::parser::parser::ExpansionMark,
    s: impl core::borrow::Borrow<InternedString>,
) -> SyntaxObject {
    let mut syn = SyntaxObject::default(TokenType::Identifier(
        ("##".to_string() + s.borrow().resolve()).into(),
    ));
    syn.mark = mark;
    syn
}

fn is_unquote(head: &InternedString) -> bool {
    *head == *UNQUOTE
        || *head == *UNQUOTE_COMMA
        || *head == *RAW_UNQUOTE
        || *head == *UNQUOTE_SPLICING
        || *head == *RAW_UNQUOTE_SPLICING
}

impl<'a> VisitorMutRef for RenameIdentifiersVisitor<'a> {
    type Output = ();

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        if let ExprKind::Atom(a) = &mut define.name {
            if let SyntaxObject {
                ty: TokenType::Identifier(ref s),
                ..
            } = a.syn
            {
                // If this is a special pattern variable, don't do any mangling of the variable
                if !self.pattern_variables.contains(s) {
                    self.add(*s);
                    // a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                }

                a.syn = renamed_binder(a.syn.mark, s);
                a.syn.introduced_via_macro = true;
            }
        }

        self.visit(&mut define.body)
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        let depth = self.scope.len();

        for arg in &mut lambda_function.args {
            if let ExprKind::Atom(a) = arg {
                if let SyntaxObject {
                    ty: TokenType::Identifier(ref s),
                    ..
                } = a.syn
                {
                    if !self.pattern_variables.contains(s) {
                        self.add(*s);
                    }

                    a.syn = renamed_binder(a.syn.mark, s);
                    a.syn.introduced_via_macro = true;
                }
            }
        }

        self.bind_defines(core::slice::from_ref(&lambda_function.body));
        self.visit(&mut lambda_function.body);
        self.scope.truncate(depth);
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        for expr in &mut begin.exprs {
            self.visit(expr)
        }
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) -> Self::Output {
        self.visit_data(&mut quote.expr, None);
    }

    fn visit_macro(&mut self, _m: &mut super::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &mut super::ast::Atom) -> Self::Output {
        let token = a.syn.ty.clone();
        if let TokenType::Identifier(s) = token {
            if self.syntax.contains(&s) || s == *DATUM_SYNTAX {
                return;
            }

            if self.is_gensym(&s) {
                a.syn.ty = TokenType::Identifier(("##".to_string() + s.resolve()).into());
            } else {
                a.syn.unresolved = true;
            }
        }
    }

    fn visit_list(&mut self, l: &mut super::ast::List) -> Self::Output {
        // A macro the template defines renames its own template when it's defined. Here only
        // the names this template has are renamed in it, once.
        if l.is_syntax_rules() {
            let in_nested = core::mem::replace(&mut self.in_nested, true);

            for expr in &mut l.args[1..] {
                self.visit_nested_template(expr);
            }

            self.in_nested = in_nested;
            return;
        }

        // TODO: So here, since this body isn't lowered all the way, we're not
        // actually _doing_ anything!
        match l.first() {
            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Quote || *ty == TokenType::Identifier(*RAW_QUOTE) => {
                for expr in &mut l.args[1..] {
                    self.visit_data(expr, None);
                }
            }

            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Identifier(*QUASIQUOTE) => {
                for expr in &mut l.args[1..] {
                    self.visit_data(expr, Some(1));
                }
            }

            // Some(ExprKind::Atom(Atom {
            //     syn: SyntaxObject { ty, .. },
            // })) if *ty == TokenType::Identifier(*DATUM_SYNTAX) => {
            //     for expr in &mut l.args {
            //         self.visit(expr);
            //     }
            // }
            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Define => {
                // A curried define's name is a function's, as the expander has it
                crate::parser::expand_visitor::uncurry_define(l);

                // Where the parameters of a function define start
                let mut body_scope = None;

                // TODO: Top level defines should be allowed to escape here
                match l.args.get_mut(1) {
                    // (define name expr)
                    Some(ExprKind::Atom(a)) => {
                        if let SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        } = a.syn
                        {
                            if !self.pattern_variables.contains(&s) {
                                self.add(s);
                            }

                            a.syn = renamed_binder(a.syn.mark, s);
                            a.syn.introduced_via_macro = true;

                            // TODO: Move the logic about resolving
                            // the name actually back to the expand visitor.
                            // This doesn't _quite_ make sense - this is the template
                            // value that is introduced. For example, in the example:
                            //
                            // (define-syntax letrec*-helper
                            //   (syntax-rules ()
                            //     [(letrec*-helper () body ...)
                            //      (begin
                            //        body ...)]
                            //     [(letrec*-helper ((var val) rest ...) body ...)
                            //      (begin
                            //        (define var val)
                            //        (letrec*-helper (rest ...) body ...))]))
                            // a.syn.previous_name = Some(s);
                        }
                    }

                    // (define (name args ...) exprs ...)
                    Some(ExprKind::List(shorthand_define_function)) => {
                        for (index, arg) in shorthand_define_function.args.iter_mut().enumerate() {
                            // The parameters are only the body's
                            if index == 1 {
                                body_scope = Some(self.scope.len());
                            }

                            if let ExprKind::List(parameter) = arg {
                                if index > 0 {
                                    self.bind_default_parameter(parameter);
                                }

                                continue;
                            }

                            if let ExprKind::Atom(a) = arg {
                                if let SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                } = a.syn
                                {
                                    if s != *DATUM_SYNTAX {
                                        if !self.pattern_variables.contains(&s) {
                                            self.add(s);
                                        }

                                        a.syn = renamed_binder(a.syn.mark, s);
                                        a.syn.introduced_via_macro = true;
                                        // a.syn.previous_name = Some(s);
                                    }
                                }
                            }
                        }

                        body_scope.get_or_insert(self.scope.len());
                    }

                    _ => {}
                }

                if let Some(rest) = l.args.get_mut(2..) {
                    if body_scope.is_some() {
                        self.bind_defines(rest);
                    }

                    for expr in rest.iter_mut() {
                        self.visit(expr)
                    }
                }

                if let Some(depth) = body_scope {
                    self.scope.truncate(depth);
                }
            }

            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Lambda
                || *ty == TokenType::Identifier(*LAMBDA_SYMBOL)
                || *ty == TokenType::Identifier(*LAMBDA)
                || *ty == TokenType::Identifier(*LAMBDA_FN) =>
            {
                let depth = self.scope.len();
                let arguments = l.args.get_mut(1);

                if let Some(ExprKind::List(arguments)) = arguments {
                    for arg in arguments.args.iter_mut() {
                        if let ExprKind::List(parameter) = arg {
                            self.bind_default_parameter(parameter);
                            continue;
                        }

                        if let ExprKind::Atom(a) = arg {
                            if let SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            } = a.syn
                            {
                                if !self.pattern_variables.contains(&s) {
                                    self.add(s);
                                }

                                a.syn = renamed_binder(a.syn.mark, s);
                                a.syn.introduced_via_macro = true;
                                // a.syn.previous_name = Some(s);
                            }
                        }
                    }
                }

                if let Some(ExprKind::Atom(a)) = arguments {
                    if let SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    } = a.syn
                    {
                        if !self.pattern_variables.contains(&s) {
                            self.add(s);
                        }

                        a.syn = renamed_binder(a.syn.mark, s);
                        a.syn.introduced_via_macro = true;
                        // a.syn.previous_name = Some(s);
                    }
                }

                if let Some(rest) = l.args.get_mut(2..) {
                    self.bind_defines(rest);

                    for expr in rest.iter_mut() {
                        self.visit(expr)
                    }
                }

                self.scope.truncate(depth);
            }

            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Let || *ty == TokenType::Identifier(*PLAIN_LET) => {
                let depth = self.scope.len();

                // The values are outside the bindings, and a named let's name is only its body's
                let (name, bindings, body) = match l.args.get(1) {
                    Some(ExprKind::Atom(_)) => (Some(1), 2, 3),
                    _ => (None, 1, 2),
                };

                if let Some(ExprKind::List(pairs)) = l.args.get_mut(bindings) {
                    for pair in &mut pairs.args {
                        if let ExprKind::List(p) = pair {
                            if let Some(expr) = p.args.get_mut(1) {
                                self.visit(expr)
                            }
                        }
                    }
                }

                let mut binders: Vec<&mut ExprKind> = Vec::new();
                let split = bindings.min(l.args.len());
                let (head, rest) = l.args.split_at_mut(split);

                if let Some(name) = name.and_then(|index| head.get_mut(index)) {
                    binders.push(name);
                }

                if let Some(ExprKind::List(pairs)) = rest.first_mut() {
                    for pair in &mut pairs.args {
                        if let ExprKind::List(p) = pair {
                            if let Some(binder) = p.args.get_mut(0) {
                                binders.push(binder);
                            }
                        }
                    }
                }

                for binder in binders {
                    if let ExprKind::Atom(a) = binder {
                        if let SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        } = a.syn
                        {
                            if !self.pattern_variables.contains(&s) {
                                self.add(s);
                            }

                            a.syn = renamed_binder(a.syn.mark, s);
                            a.syn.introduced_via_macro = true;
                        }
                    }
                }

                if let Some(remaining) = l.args.get_mut(body..) {
                    self.bind_defines(remaining);

                    for expr in remaining {
                        self.visit(expr);
                    }
                }

                self.scope.truncate(depth);
            }

            _ => {
                for expr in &mut l.args {
                    self.visit(expr);
                }
            }
        }

        // for expr in &mut l.args {
        //     self.visit(expr);
        // }
    }

    fn visit_syntax_rules(&mut self, _l: &mut super::ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable);
        self.visit(&mut s.expr);
    }

    fn visit_require(&mut self, s: &mut super::ast::Require) -> Self::Output {
        for module in &mut s.modules {
            self.visit(module);
        }
    }

    // TODO: This needs to be fixed!
    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        for (_, expr) in &mut l.bindings {
            self.visit(expr);
        }

        let depth = self.scope.len();

        for (arg, _) in &mut l.bindings {
            if let ExprKind::Atom(a) = arg {
                if let SyntaxObject {
                    ty: TokenType::Identifier(ref s),
                    ..
                } = a.syn
                {
                    if !self.pattern_variables.contains(s) {
                        self.add(*s);
                    }

                    a.syn = renamed_binder(a.syn.mark, s);
                    a.syn.introduced_via_macro = true;
                }
            }
        }

        self.bind_defines(core::slice::from_ref(&l.body_expr));
        self.visit(&mut l.body_expr);
        self.scope.truncate(depth);
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Self::Output {
        if v.bytes {
            return;
        }

        for arg in &mut v.args {
            self.visit(arg);
        }
    }
}

#[cfg(test)]
mod rename_visitor_tests {

    use steel_parser::tokens::IntLiteral;

    use super::TokenType::*;
    use super::*;
    use crate::parser::ast::{Atom, Define, If, LambdaFunction, List};

    use thin_vec::thin_vec;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(n).into(),
        )))
    }

    #[test]
    fn test_rename_identifiers() {
        let mut pre_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            thin_vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("x".into()),
            )))],
            ExprKind::List(List::new(thin_vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("+".into())))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("x".into())))),
                atom_int(10),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let post_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            thin_vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("##x".into()),
            )))],
            ExprKind::List(List::new(thin_vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("+".into())))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("##x".into())))),
                atom_int(10),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);

        visitor.visit(&mut pre_condition);

        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_should_do_nothing() {
        let mut pre_condition = ExprKind::If(Box::new(If::new(
            ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
            ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(false)))),
                atom_int(10),
                atom_int(20),
                SyntaxObject::default(If),
            ))),
            SyntaxObject::default(If),
        )));

        let post_condition = pre_condition.clone();

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_rename_define() {
        let mut pre_condition: ExprKind = Define::new(
            atom_identifier("applesauce"),
            atom_int(10),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let post_condition: ExprKind = Define::new(
            atom_identifier("##applesauce"),
            atom_int(10),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_rename_define_datum_syntax() {
        let mut pre_condition: ExprKind = Define::new(
            List::new(thin_vec![
                atom_identifier("datum->syntax"),
                atom_identifier("escaping-definition"),
            ])
            .into(),
            atom_int(1),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let post_condition: ExprKind = Define::new(
            List::new(thin_vec![
                atom_identifier("datum->syntax"),
                atom_identifier("escaping-definition"),
            ])
            .into(),
            atom_int(1),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }
}
