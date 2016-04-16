#![feature(box_syntax, box_patterns)]
#![feature(as_unsafe_cell)]

mod utils;
mod types;
mod err;
mod lexer;
mod parser;
mod list;
mod unit_tests;
mod main;

pub fn interpreter() {
   main::interpreter();
}

