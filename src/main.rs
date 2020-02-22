// use std::vec::Vec;
// use this::lexer;

// use lang::Tokenizer;

// mod lexer;
mod env;
mod evaluator;
mod lexer;
mod parser;
mod rerrs;
mod rvals;

fn main() {
    // println!("Hello, world!");

    // let a = lexer::Tokenizer::new("(+ 1 2)");

    let a = parser::Parser::new("(let ([a 10] [b 5]) (+ a b))");

    //let b = evaluator::evaluator(a.next().unwrap().unwrap());

    for i in a {
        // println!("{:?}", i);
        let e = evaluator::evaluator(i.unwrap());
        println!("{}", e.unwrap());
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
