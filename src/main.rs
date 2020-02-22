// use std::vec::Vec;
// use this::lexer;

// use std::io;
// use std::io::Write;

// use lang::Tokenizer;

// mod lexer;
mod env;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod rerrs;
mod rvals;

fn main() {
    // repl::repl();

    let a = parser::Parser::new("(let ([a 10] [b 5]) (+ a b))");

    for i in a {
        // println!("{:?}", i);
        let e = evaluator::evaluator(i.unwrap());
        println!("{}", e.unwrap());
    }
}
