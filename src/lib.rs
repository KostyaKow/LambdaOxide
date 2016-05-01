#![feature(box_syntax, box_patterns)]
#![feature(as_unsafe_cell)]

mod utils;
pub mod types;
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

pub fn add_lisp_to_binary(code : &String, root : types::Root) -> types::Sexps {
   utils::load_internal(code, root)
}
