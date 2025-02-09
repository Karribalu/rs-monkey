#![feature(let_chains)]

use std::io;
mod lexer;
mod token;
mod repl;
mod ast;
mod parser;

fn main() {
    let input = io::stdin().lock();
    let output = io::stdout().lock();
    repl::start(input, output).expect("TODO: panic message");
}
