//TODO: check lexer errors (ranges)

use lexer::lex;

mod utils;
mod gentypes;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;

pub fn interpreter() {
   use std::io::{self, BufRead};
   let stdin = io::stdin();

   loop {
      print!("**> ");
      use std::io::{self, Write};
      io::stdout().flush().unwrap();
      let line = stdin.lock().lines()
                      .next().unwrap().unwrap();

      let mut acc = line;
      lex(&*acc);

      //display_run_result(&out);
      //display_sexps(&out);
      //root.borrow().print();
   }
}

fn main() {
   //let lexemes = lex("hi");
   interpreter();
}
