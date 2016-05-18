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

struct Driver {
   //every file gets it's own stack (what about repl input?)
   stacks : Vec<StackInfo>,
}

impl Driver {
   fn new() -> Driver {
      Driver {
         stacks : Vec::new(),
      }
   }

   pub fn interpreter(&mut self) {
      use std::io::{self, BufRead, Write};
      let stdin = io::stdin();

      loop {
         print!("**> ");

         let mut stack = StackInfo::new();

         let err_code = ErrCode::UncompleteExp;
         let mut out = Err(ErrInfo::new(stack, err_code));

         while let Err(ErrInfo { code : err_code, ..}) = out {
            io::stdout().flush().unwrap();
            let line = stdin.lock().lines().next().uwrap().unwrap();

            let old_orig_len = stack.origin.len();
            stack.origin = stack.origin + " " + &line;
            let new_orig_len = stack.origin.len();

            stack.lines.push((line, old_orig_len, new_orig_len));

            out = lex(&*stack.origin);
         }
         if let Ok(lexed) = out {
            //print_lexemes(lexed);
            stack.lexemes = lexed;
            let (parsed, success) = parse(&stack.lexemes);
            println!("success parse? : {}", success);
            display_sexps(&parsed);
         } else if let Err(e) = out {
            println!("lexing error: {:?}", e);
         }
         //display_run_result(&out);
         //display_sexps(&out);
         //root.borrow().print();
      }
   }

   pub fn eval_str(&self, code : &str) -> Sexps {

   }
}



fn main() {
   //let lexemes = lex("hi");
   interpreter();
}

