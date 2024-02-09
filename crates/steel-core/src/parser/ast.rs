use crate::parser::{
    parser::SyntaxObject, tokens::TokenType::*, tryfrom_visitor::TryFromExprKindForSteelVal,
};

use steel_parser::tokens::MaybeBigInt;

use crate::{
    rerrs::SteelErr,
    rvals::SteelVal::{self, *},
};

pub use steel_parser::ast::{
    AstTools, Atom, Begin, Define, ExprKind, If, IteratorExtensions, LambdaFunction, Let, List,
    Macro, PatternPair, Quote, Require, Return, Set, SyntaxRules, STANDARD_MODULE_GET,
    UNREADABLE_MODULE_GET,
};

impl TryFrom<ExprKind> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: ExprKind) -> std::result::Result<Self, Self::Error> {
        TryFromExprKindForSteelVal::try_from_expr_kind(e)
    }
}

/// Convert this ExprKind into a typed version of the AST
/// TODO: Matt -> actually do a full visitor on the AST
// pub(crate) fn from_list_repr_to_ast(expr: ExprKind) -> Result<ExprKind, ParseError> {
//     if let ExprKind::List(l) = expr {
//         ExprKind::try_from(
//             l.args
//                 .into_iter()
//                 .map(from_list_repr_to_ast)
//                 .collect::<Result<Vec<_>, ParseError>>()?,
//         )
//     } else {
//         Ok(expr)
//     }
// }

#[derive(Debug)]
pub(crate) struct TryFromSteelValVisitorForExprKind {
    pub(crate) qq_depth: usize,
    pub(crate) quoted: bool,
}

impl TryFromSteelValVisitorForExprKind {
    pub fn root(value: &SteelVal) -> std::result::Result<ExprKind, SteelErr> {
        // let now = std::time::Instant::now();

        Self {
            qq_depth: 0,
            quoted: false,
        }
        .visit(value)

        // log::debug!(target: "pipeline_time", "SteelVal->ExprKind time: {:?}", now.elapsed());

        // res
    }

    // type Error = &'static str;
    pub fn visit(&mut self, value: &SteelVal) -> std::result::Result<ExprKind, SteelErr> {
        match value {
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                BooleanLiteral(*x),
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                NumberLiteral(*x),
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                IntegerLiteral(MaybeBigInt::Small(*x)),
            )))),

            BigNum(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                IntegerLiteral(MaybeBigInt::Big(x.unwrap())),
            )))),

            VectorV(lst) => {
                let items: std::result::Result<Vec<ExprKind>, _> =
                    lst.iter().map(|x| self.visit(x)).collect();
                Ok(ExprKind::List(List::new(items?)))
            }
            Void => stop!(Generic => "Can't convert from Void to expression!"),
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                StringLiteral(x.to_string()),
            )))),
            FuncV(_) => stop!(Generic => "Can't convert from Function to expression!"),
            // LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            // MacroV(_) => Err("Can't convert from Macro to expression!"),
            SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier(x.as_str().into()),
            )))),

            // TODO: Change the implementation here to respect quotes correctly
            SyntaxObject(s) => self.visit_syntax_object(s),
            Custom(_) => {
                // TODO: if the returned object is a custom type, check
                // to see if its a Syntax struct to replace the span with
                stop!(Generic => "Can't convert from Custom Type to expression!")
            }
            ListV(l) => {
                // Rooted - things operate as normal
                if self.qq_depth == 0 {
                    let maybe_special_form = l.first().and_then(|x| {
                        x.as_symbol()
                            .or_else(|| x.as_syntax_object().and_then(|x| x.syntax.as_symbol()))
                    });

                    // dbg!(&maybe_special_form);

                    match maybe_special_form {
                        Some(x) if x.as_str() == "quote" => {
                            if self.quoted {
                                let items: std::result::Result<Vec<ExprKind>, _> =
                                    l.iter().map(|x| self.visit(x)).collect();

                                return Ok(ExprKind::List(List::new(items?)));
                            }

                            self.quoted = true;

                            let return_value = Ok(l
                                .into_iter()
                                .map(|x| self.visit(x))
                                .collect::<std::result::Result<Vec<_>, _>>()?
                                .try_into()?);

                            // dbg!(&return_value);

                            self.quoted = false;

                            return return_value;
                        } // "quasiquote" => {
                        //     self.qq_depth += 1;
                        // }

                        // Empty list, just return as a quoted list
                        // None => {
                        // return Ok(ExprKind::empty());
                        // }
                        _ => {}
                    }
                }

                Ok(l.into_iter()
                    .map(|x| self.visit(x))
                    .collect::<std::result::Result<Vec<_>, _>>()?
                    .try_into()?)

                // If we're not quoted, we need to just return this pushed down to an ast
                // let items: std::result::Result<Vec<ExprKind>, &'static str> =
                // l.iter().map(|x| self.visit(x)).collect();

                // Ok(ExprKind::List(List::new(items?)))
            }
            CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                CharacterLiteral(*x),
            )))),
            unknown => {
                stop!(Generic => "Unable to convert from value to expression: {:?}", unknown)
            }
        }
    }
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<&SteelVal> for ExprKind {
    type Error = &'static str;
    fn try_from(r: &SteelVal) -> std::result::Result<Self, Self::Error> {
        fn inner_try_from(
            r: &SteelVal,
            depth: usize,
        ) -> std::result::Result<ExprKind, &'static str> {
            if depth > 64 {
                return Err("Unable to convert steel val to exprkind - depth was too large!");
            }

            match r {
                BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    BooleanLiteral(*x),
                )))),
                NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    NumberLiteral(*x),
                )))),
                IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    IntegerLiteral(MaybeBigInt::Small(*x)),
                )))),
                Rational(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    FractionLiteral(
                        MaybeBigInt::Small(*x.numer() as isize),
                        MaybeBigInt::Small(*x.denom() as isize),
                    ),
                )))),
                BigRational(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    FractionLiteral(
                        MaybeBigInt::Big(x.numer().clone()),
                        MaybeBigInt::Big(x.denom().clone()),
                    ),
                )))),
                BigNum(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    IntegerLiteral(MaybeBigInt::Big(x.unwrap())),
                )))),
                VectorV(lst) => {
                    let items: std::result::Result<Vec<ExprKind>, &'static str> =
                        lst.iter().map(|x| inner_try_from(x, depth + 1)).collect();
                    Ok(ExprKind::List(List::new(items?)))
                }
                Void => Err("Can't convert from Void to expression!"),
                StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    StringLiteral(x.to_string()),
                )))),
                FuncV(_) => Err("Can't convert from Function to expression!"),
                // LambdaV(_) => Err("Can't convert from Lambda to expression!"),
                // MacroV(_) => Err("Can't convert from Macro to expression!"),
                SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    Identifier(x.as_str().into()),
                )))),
                SyntaxObject(s) => s
                    .to_exprkind()
                    .map_err(|_| "Unable to convert syntax object back to exprkind"),
                Custom(_) => {
                    // TODO: if the returned object is a custom type, check
                    // to see if its a Syntax struct to replace the span with
                    Err("Can't convert from Custom Type to expression!")
                }
                ListV(l) => {
                    let items: std::result::Result<Vec<ExprKind>, &'static str> =
                        l.iter().map(|x| inner_try_from(x, depth + 1)).collect();

                    Ok(ExprKind::List(List::new(items?)))
                }
                CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    CharacterLiteral(*x),
                )))),

                Pair(_) => Err("Can't convert from pair to expression!"),

                // StructClosureV(_) => Err("Can't convert from struct-function to expression!"),
                PortV(_) => Err("Can't convert from port to expression!"),
                Closure(_) => Err("Can't convert from bytecode closure to expression"),
                HashMapV(_) => Err("Can't convert from hashmap to expression!"),
                HashSetV(_) => Err("Can't convert from hashset to expression!"),
                IterV(_) => Err("Can't convert from iterator to expression!"),
                FutureFunc(_) => Err("Can't convert from future function to expression!"),
                FutureV(_) => Err("Can't convert future to expression!"),
                // Promise(_) => Err("Can't convert from promise to expression!"),
                StreamV(_) => Err("Can't convert from stream to expression!"),
                // Contract(_) => Err("Can't convert from contract to expression!"),
                // ContractedFunction(_) => Err("Can't convert from contracted function to expression!"),
                BoxedFunction(_) => Err("Can't convert from boxed function to expression!"),
                ContinuationFunction(_) => Err("Can't convert from continuation to expression!"),
                // #[cfg(feature = "jit")]
                // CompiledFunction(_) => Err("Can't convert from function to expression!"),
                MutFunc(_) => Err("Can't convert from function to expression!"),
                BuiltIn(_) => Err("Can't convert from function to expression!"),
                ReducerV(_) => Err("Can't convert from reducer to expression!"),
                MutableVector(_) => Err("Can't convert from vector to expression!"),
                CustomStruct(_) => Err("Can't convert from struct to expression!"),
                BoxedIterator(_) => Err("Can't convert from boxed iterator to expression!"),
                Boxed(_) => Err("Can't convert from boxed steel val to expression!"),
                Reference(_) => Err("Can't convert from opaque reference type to expression!"),
                HeapAllocated(_) => Err("Can't convert from heap allocated value to expression!"),
            }
        }

        inner_try_from(r, 0)
    }
}
