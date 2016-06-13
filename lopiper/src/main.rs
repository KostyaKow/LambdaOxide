//#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate oxicloak;

extern crate iron_llvm;
extern crate llvm_sys;

mod reader;
mod utils;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;
mod comp;

use reader::Driver;

fn main() {
   //let lexemes = lex("hi");
   let mut d = Driver::new();
   //d.interpreter();
   d.jitter_repl();

}

