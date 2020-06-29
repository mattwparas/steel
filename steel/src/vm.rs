// pub enum ByteCode {}
// use std::cell::RefCell;
// use std::convert::TryFrom;
use std::iter::Iterator;
// use std::rc::Rc;
use std::result;

// use crate::env::{Env, FALSE, TRUE, VOID};
// use crate::parser::lexer::Tokenizer;
use crate::parser::lexer::TokenStream;
use crate::parser::tokens::Token;
use crate::parser::tokens::TokenError;
use crate::parser::{Expr, ParseError, Parser};
// use crate::primitives::ListOperations;
// use crate::rerrs::SteelErr;
// use crate::rvals::{FunctionSignature, Result, SteelLambda, SteelVal, StructClosureSignature};
// use crate::stop;
// use crate::structs::SteelStruct;
// use crate::throw;
use std::collections::HashMap;
// use std::ops::Deref;

// use crate::compiler::AST;
// use crate::rvals::MacroPattern;
// use crate::expander::SteelMacro;

/*
any time we have an expression returned for a lambda, I should just create a new stack frame
in the case of tail recursion, I can try to reuse the stack frame when possible based on the args
*/

// pub fn flatten_expression_tree(expr_str: &str) -> result::Result<Vec<Token>, TokenError> {
//     // match expr.as_ref() {}
//     // unimplemented!()]

//     let mut intern = HashMap::new();

//     let parsed: result::Result<Vec<Expr>, ParseError> =
//         Parser::new(expr_str, &mut intern).collect();
//     let parsed = parsed.unwrap();

//     let mut result = Vec::new();

//     for expr in parsed {
//         let input = expr.to_string();
//         let tok = Tokenizer::new(&input).collect::<result::Result<Vec<Token>, TokenError>>()?;
//         result.push(tok);
//     }

//     Ok(result.into_iter().flatten().collect())

//     // let result: result::Result<Vec<Token>, TokenError> = parsed
//     //     .into_iter()
//     //     .map(|x: Expr| {
//     //         let input = x.to_string();
//     //         Tokenizer::new(&input).collect::<result::Result<Vec<Token>, TokenError>>();
//     //     })
//     //     .flatten()
//     //     .collect();

//     // result

//     // let expr = Expr::VectorVal()

//     // let input = expr.to_string();
//     // Tokenizer::new(&input).collect()
// }
