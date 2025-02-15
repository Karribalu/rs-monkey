#![feature(let_chains)]
#![feature(unboxed_closures)]

use std::io;
mod ast;
mod constants;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    let input = io::stdin().lock();
    let output = io::stdout().lock();
    repl::start(input, output).expect("TODO: panic message");
}
