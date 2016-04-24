#![feature(box_syntax, box_patterns)]
#![feature(as_unsafe_cell)]

mod utils;
mod types;
mod err;
mod lexer;
mod parser;
mod list;
mod unit_tests;
pub mod main;
use std::cell::RefCell;

pub fn interpreter(e : Option<RefCell<main::Env>>) {
   main::interpreter(e);
}

