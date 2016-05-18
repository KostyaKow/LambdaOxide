extern crate oxicloak;
use oxicloak::*;

use lexer::lex;
use parser::parse;
use utils::display_sexps;
use exp::Sexps;
use errors::{ExecStage, ErrCode};

mod utils;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;

use errors::{StackInfo};

//TODO: check lexer errors (ranges)
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
   pub fn run(&mut self, code : &str) -> Sexps { Sexps::nil_new()
   }
   pub fn load_multiline(&mut self, code : &String) -> Sexps { Sexps::nil_new() }
   pub fn eval(&mut self, exp : &Sexps) -> Sexps { Sexps::nil_new() }

   pub fn interpreter(&mut self) {
      use std::io::{self, BufRead, Write};
      let stdin = io::stdin();

      loop {
         print!("**> ");

         let mut stack = StackInfo::new();
         stack.stage = ExecStage::Lex;

         let err_code = ErrCode::UncompleteExp;
         let mut out = Err((err_code, 0, 0));

         while let Err((err_code, _, _)) = out {
            io::stdout().flush().unwrap();
            let line = stdin.lock().lines().next().unwrap().unwrap();

            let old_orig_len = stack.origin.len();
            stack.origin = stack.origin + " " + &line;
            let new_orig_len = stack.origin.len();

            stack.lines.push((line.to_string(), old_orig_len, new_orig_len));

            out = lex(&*stack.origin);
         }
         if let Ok(lexed) = out {
            //print_lexemes(lexed);
            stack.lexemes = lexed;

            let mut new_l = Vec::new();
            for (l, start, end) in stack.lexemes {
               new_l.push(l);
            }
            let (parsed, success) = parse(&new_l);
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
      Sexps::nil_new()
   }
}



fn main() {
   //let lexemes = lex("hi");
   let mut d = Driver::new();
   d.interpreter();
}

