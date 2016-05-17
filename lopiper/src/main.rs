//TODO: check lexer errors (ranges)

use lexer::lex;
use parser::parse;
use utils::display_sexps;

mod utils;
mod genutils;
mod gentypes;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;

use gentypes::{SharedMut, to_shared_mut};
use errors::{StackInfo};

struct Runner {
   stack_info : SharedMut<StackInfo>,
}

impl Runner {
   fn new() -> Runner {
      Runner {
         stack_info : to_shared_mut(StackInfo::new()),
      }
   }
}

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
      let lexed_ret = lex(&*acc);
      //print_lexemes(lexed);
      if let Ok(lexed) = lexed_ret {
         let (parsed, success) = parse(&lexed);
         println!("success parse? : {}", success);
         display_sexps(&parsed);
      } else if let Err(e) = lexed_ret {
         println!("lexing error: {:?}",  e);
      }

      //display_run_result(&out);
      //display_sexps(&out);
      //root.borrow().print();
   }
}

fn main() {
   //let lexemes = lex("hi");
   interpreter();
}
