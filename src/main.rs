// use std::vec::Vec;
// use this::lexer;

// use lang::Tokenizer;

// mod lexer;
mod evaluator;
mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");

    // let a = lexer::Tokenizer::new("(+ (* 1 2 3) 2 3)");
    let a = parser::Parser::new("(define (test a b) (+ a b)) application (((");

    for i in a {
        println!("{:?}", i);
    }
}

// use std::iter;
// use std::str;

// #[derive(Debug)]
// pub enum Token {
// Unknown(char),
// OpenParen,
// CloseParen,
// Operator(char),
// Number(String),
// }

// Ok(
//     ListVal(
//         [Atom(Identifier("define")),
//             ListVal(
//                 [
//                     Atom(Identifier("test")),
//                     Atom(Identifier("a")),
//                     Atom(Identifier("b"))
//                 ]),
//             ListVal(
//                 [
//                     Atom(Plus),
//                     Atom(Identifier("a")),
//                     Atom(Identifier("b"))
//                 ]
//             )
//         ]
//     )
// )
