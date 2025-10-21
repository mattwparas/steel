use crate::{
    parser::{
        parser::SyntaxObject, tokens::TokenType::*, tryfrom_visitor::TryFromExprKindForSteelVal,
    },
    rvals::SteelComplex,
};
use alloc::format;
use alloc::vec::Vec;

use steel_parser::tokens::{IntLiteral, NumberLiteral, RealLiteral};

use crate::{
    rerrs::SteelErr,
    rvals::SteelVal::{self, *},
};

pub use steel_parser::ast::{
    AstTools, Atom, Begin, Define, ExprKind, If, IteratorExtensions, LambdaFunction, Let, List,
    Macro, PatternPair, Quote, Require, Return, Set, SyntaxRules, Vector, STANDARD_MODULE_GET,
    UNREADABLE_MODULE_GET,
};

impl TryFrom<ExprKind> for SteelVal {
    type Error = SteelErr;

    fn try_from(e: ExprKind) -> core::result::Result<Self, Self::Error> {
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
    pub fn root(value: &SteelVal) -> core::result::Result<ExprKind, SteelErr> {
        Self {
            qq_depth: 0,
            quoted: false,
        }
        .visit(value)
    }

    // type Error = &'static str;
    pub fn visit(&mut self, value: &SteelVal) -> core::result::Result<ExprKind, SteelErr> {
        match value {
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                BooleanLiteral(*x),
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                RealLiteral::Float(*x).into(),
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                RealLiteral::Int(IntLiteral::Small(*x)).into(),
            )))),

            BigNum(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                RealLiteral::Int(IntLiteral::Big(Box::new(x.unwrap()))).into(),
            )))),
            VectorV(lst) => {
                let items: core::result::Result<Vec<ExprKind>, _> =
                    lst.iter().map(|x| self.visit(x)).collect();
                Ok(ExprKind::List(List::new(items?)))
            }
            Void => stop!(Generic => "Can't convert from Void to expression!"),
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                StringLiteral(x.to_arc_string()),
            )))),
            FuncV(_) => stop!(Generic => "Can't convert from Function to expression!"),

            SymbolV(x) if x.starts_with("#:") => Ok(ExprKind::Atom(Atom::new(
                SyntaxObject::default(Keyword(x.as_str().into())),
            ))),

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
                                let items: core::result::Result<Vec<ExprKind>, _> =
                                    l.iter().map(|x| self.visit(x)).collect();

                                return Ok(ExprKind::List(List::new(items?)));
                            }

                            self.quoted = true;

                            let return_value = Ok(l
                                .into_iter()
                                .map(|x| self.visit(x))
                                .collect::<core::result::Result<Vec<_>, _>>()?
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
                    .collect::<core::result::Result<Vec<_>, _>>()?
                    .try_into()?)

                // If we're not quoted, we need to just return this pushed down to an ast
                // let items: core::result::Result<Vec<ExprKind>, &'static str> =
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

#[cold]
fn complex_to_literal(v: &SteelComplex) -> Result<NumberLiteral, &'static str> {
    let val_to_real = |real_val: &SteelVal| -> Result<RealLiteral, &'static str> {
        let l = match real_val {
            NumV(n) => (*n).into(),
            IntV(n) => (*n).into(),
            Rational(n) => (*n).into(),
            BigNum(n) => IntLiteral::Big(Box::new(n.as_ref().clone())).into(),
            BigRational(n) => RealLiteral::Rational(
                IntLiteral::Big(Box::new(n.numer().clone())),
                IntLiteral::Big(Box::new(n.denom().clone())),
            ),
            _ => return Err("SteelVal not a valid real number"),
        };
        Ok(l)
    };
    let re = val_to_real(&v.re)?;
    let im = val_to_real(&v.im)?;
    Ok(NumberLiteral::Complex(re, im))
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<&SteelVal> for ExprKind {
    type Error = &'static str;
    fn try_from(r: &SteelVal) -> core::result::Result<Self, Self::Error> {
        fn inner_try_from(
            r: &SteelVal,
            depth: usize,
        ) -> core::result::Result<ExprKind, &'static str> {
            if depth > 64 {
                return Err("Unable to convert steel val to exprkind - depth was too large!");
            }

            match r {
                BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    BooleanLiteral(*x),
                )))),
                NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    RealLiteral::Float(*x).into(),
                )))),
                IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    RealLiteral::Int(IntLiteral::Small(*x)).into(),
                )))),
                Rational(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    RealLiteral::Rational(
                        IntLiteral::Small(*x.numer() as isize),
                        IntLiteral::Small(*x.denom() as isize),
                    )
                    .into(),
                )))),
                BigRational(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    RealLiteral::Rational(
                        IntLiteral::Big(Box::new(x.numer().clone())),
                        IntLiteral::Big(Box::new(x.denom().clone())),
                    )
                    .into(),
                )))),
                BigNum(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    RealLiteral::Int(IntLiteral::Big(Box::new(x.unwrap()))).into(),
                )))),
                Complex(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    complex_to_literal(x)?.into(),
                )))),
                VectorV(lst) => {
                    let items: core::result::Result<Vec<ExprKind>, &'static str> =
                        lst.iter().map(|x| inner_try_from(x, depth + 1)).collect();
                    Ok(ExprKind::List(List::new(items?)))
                }
                Void => Err("Can't convert from Void to expression!"),
                StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    StringLiteral(x.to_arc_string()),
                )))),
                FuncV(_) => Err("Can't convert from Function to expression!"),
                SymbolV(x) if x.starts_with("#:") => Ok(ExprKind::Atom(Atom::new(
                    SyntaxObject::default(Keyword(x.as_str().into())),
                ))),
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
                    let items: core::result::Result<Vec<ExprKind>, &'static str> =
                        l.iter().map(|x| inner_try_from(x, depth + 1)).collect();

                    Ok(ExprKind::List(List::new(items?)))
                }
                CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                    CharacterLiteral(*x),
                )))),

                Pair(_) => Err("Can't convert from pair to expression!"),

                PortV(_) => Err("Can't convert from port to expression!"),
                Closure(_) => Err("Can't convert from bytecode closure to expression"),
                HashMapV(_) => Err("Can't convert from hashmap to expression!"),
                HashSetV(_) => Err("Can't convert from hashset to expression!"),
                IterV(_) => Err("Can't convert from iterator to expression!"),
                FutureFunc(_) => Err("Can't convert from future function to expression!"),
                FutureV(_) => Err("Can't convert future to expression!"),
                StreamV(_) => Err("Can't convert from stream to expression!"),
                BoxedFunction(_) => Err("Can't convert from boxed function to expression!"),
                ContinuationFunction(_) => Err("Can't convert from continuation to expression!"),
                MutFunc(_) => Err("Can't convert from function to expression!"),
                BuiltIn(_) => Err("Can't convert from function to expression!"),
                ReducerV(_) => Err("Can't convert from reducer to expression!"),
                MutableVector(_) => Err("Can't convert from vector to expression!"),
                CustomStruct(_) => Err("Can't convert from struct to expression!"),
                BoxedIterator(_) => Err("Can't convert from boxed iterator to expression!"),
                Boxed(_) => Err("Can't convert from boxed steel val to expression!"),
                Reference(_) => Err("Can't convert from opaque reference type to expression!"),
                HeapAllocated(_) => Err("Can't convert from heap allocated value to expression!"),
                ByteVector(_) => Err("Can't convert from bytevector to expression!"),
            }
        }

        inner_try_from(r, 0)
    }
}
