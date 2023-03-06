use super::parser::SyntaxObject;
use super::tokens::TokenType;
use super::{ast::*, interner::InternedString};
use crate::parser::parser::Parser;
use crate::parser::span::Span;
// use super::
use proptest::prelude::*;
use std::convert::TryFrom;

use crate::parser::ast::{Atom, Begin, Define, If, List, Quote};

// #[derive(Clone, Debug, PartialEq)]
// pub enum ExprKind {
//     Atom(Atom),
//     If(Box<If>),
//     Define(Box<Define>),
//     LambdaFunction(Box<LambdaFunction>),
//     Begin(Begin),
//     Return(Box<Return>),
//     Apply(Box<Apply>),
//     Panic(Box<Panic>),
//     Transduce(Box<Transduce>),
//     Read(Box<Read>),
//     Execute(Box<Execute>),
//     Quote(Box<Quote>),
//     Struct(Box<Struct>),
//     Macro(Macro),
//     SyntaxRules(SyntaxRules),
//     Eval(Box<Eval>),
//     List(List),
//     Set(Box<Set>),
// }

proptest! {
    // The next line modifies the number of tests.
    #![proptest_config(ProptestConfig::with_cases(100))]
    #[ignore]
    #[test]
    fn test_basic(expr in exprkind_strategy()) {

        let expr_string = expr.to_string();

        println!("{expr_string}");

        let resulting_expr = Parser::parse(&expr_string).unwrap();

        assert_eq!(expr, resulting_expr[0]);
    }
}

fn if_vec_strategy(
    inner: impl Strategy<Value = ExprKind> + Clone,
) -> impl Strategy<Value = ExprKind> {
    prop::collection::vec(inner, 3).prop_map(|x| {
        ExprKind::If(Box::new(If::new(
            x[0].clone(),
            x[1].clone(),
            x[2].clone(),
            SyntaxObject::default(TokenType::If),
        )))
    })
}

fn define_vec_strategy(
    inner: impl Strategy<Value = ExprKind> + Clone,
) -> impl Strategy<Value = ExprKind> {
    (
        prop::collection::vec(inner, 0..10),
        atom_identifier_strategy().prop_map(ExprKind::Atom),
    )
        .prop_map(|(vector, identifier)| {
            ExprKind::Define(Box::new(Define::new(
                identifier,
                ExprKind::try_from(vector).unwrap(),
                SyntaxObject::default(TokenType::Define),
            )))
        })
}

fn begin_vec_strategy(
    inner: impl Strategy<Value = ExprKind> + Clone,
) -> impl Strategy<Value = ExprKind> {
    prop::collection::vec(inner, 0..10).prop_map(|x| {
        ExprKind::Begin(Begin::new(
            x,
            SyntaxObject::new(TokenType::Begin, Span::new(0, 0, None)),
        ))
    })
}

fn naive_list_vec_strategy(
    inner: impl Strategy<Value = ExprKind> + Clone,
) -> impl Strategy<Value = ExprKind> {
    prop::collection::vec(inner, 0..10).prop_map(|x| {
        if x.is_empty() {
            ExprKind::Quote(Box::new(Quote::new(
                ExprKind::List(List::new(x)),
                SyntaxObject::default(TokenType::Quote),
            )))
        } else {
            ExprKind::List(List::new(x))
        }
    })
}

fn exprkind_strategy() -> BoxedStrategy<ExprKind> {
    let leaf = atom_strategy().prop_map(ExprKind::Atom);

    leaf.prop_recursive(4, 64, 10, |inner| {
        prop_oneof![
            define_vec_strategy(inner.clone()),
            if_vec_strategy(inner.clone()),
            begin_vec_strategy(inner.clone()),
            naive_list_vec_strategy(inner)
        ]
    })
    .boxed()
}

prop_compose! {
    fn atom_strategy()(
        syntax_object in syntax_object_strategy()
    ) -> Atom {
        Atom::new(syntax_object)
    }
}

prop_compose! {
    fn if_strategy()(
        test_expr in exprkind_strategy(),
        then_expr in exprkind_strategy(),
        else_expr in exprkind_strategy()
    ) -> If {
        If::new(test_expr, then_expr, else_expr, SyntaxObject::default(TokenType::If))
    }
}

prop_compose! {
    fn define_strategy()(
        name in atom_identifier_strategy().prop_map(ExprKind::Atom),
        body in exprkind_strategy()
    ) -> Define {
        Define::new(name, body, SyntaxObject::default(TokenType::Define))
    }
}

fn tokentype_strategy() -> impl Strategy<Value = TokenType<InternedString>> {
    use TokenType::*;
    prop_oneof![
        any::<char>().prop_map(CharacterLiteral),
        string_strategy().prop_map(StringLiteral),
        ident_strategy().prop_map(Identifier),
        any::<isize>().prop_map(IntegerLiteral),
        any::<bool>().prop_map(BooleanLiteral),
        any::<f64>().prop_map(NumberLiteral)
    ]
}

prop_compose! {
    fn string_strategy()(
        str_lit in r#"([^"\\]|\\t|\\u|\\n|\\")*"#
    ) -> String {
        str_lit
    }
}

prop_compose! {
    fn ident_strategy()(
        ident in r#"[_\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Start}\p{Emoji_Presentation}]['_\+\-\*\x2F%\&\|!?\~<>=@\.\p{XID_Continue}\p{Emoji_Presentation}]*"#
    ) -> InternedString {
        ident.into()
    }
}

prop_compose! {
    fn atom_identifier_strategy()(
        identifier in ident_strategy().prop_map(TokenType::Identifier)
    ) -> Atom {
        Atom::new(SyntaxObject::default(identifier))
    }
}

prop_compose! {
    fn syntax_object_strategy()(
        token_type in tokentype_strategy()
    ) -> SyntaxObject {
        SyntaxObject::new(token_type, Span::new(0, 0, None))
    }
}
