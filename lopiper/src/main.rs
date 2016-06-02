//#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate oxicloak;

extern crate iron_llvm;
extern crate llvm_sys;

use oxicloak::*;

use lexer::lex;
use parser::parse;
use utils::display_sexps;
use exp::Sexps;
use errors::{ExecStage, ErrCode, ErrInfo};

mod utils;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;
mod comp;

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
   pub fn run(&mut self, code : &str) -> Sexps { Sexps::nil_new() }
   //pub fn load_multiline(&mut self, code : &String) -> Sexps { Sexps::nil_new() }
   //pub fn eval(&mut self, exp : &Sexps) -> Sexps { Sexps::nil_new() }
   //pub fn eval_str(&self, code : &str) -> Sexps { Sexps::nil_new() }

   pub fn interpreter(&mut self) {
      use std::io::{self, BufRead, Write};
      let stdin = io::stdin();

      use comp::ExpCompInfo;
      let mut compiler = ExpCompInfo::new(None);
      use iron_llvm::core::value::Value;

      let mut context = comp::Context::new();
      let mut module_p = comp::SimpleModuleProvider::new("main_module");


      loop {
         let mut out = Sexps::nil_new();
         print!("**> ");

         let mut stack = StackInfo::new();
         stack.stage = ExecStage::Lex;

         //let mut parsed : Result<u8, (ErrCode, usize, usize)> = Err((ErrCode::UncompleteExp, 0, 0));
         use errors::parse_exp_err;
         let mut parsed = parse_exp_err(ErrCode::UncompleteExp, None);

         let mut line_n = 0;

         //while let Err((ErrCode::UncompleteExp, _, _)) = parsed {
         loop {
            if !utils::is_uncomplete_exp(parsed) { break; }

            io::stdout().flush().unwrap();
            let line = stdin.lock().lines().next().unwrap().unwrap();

            let old_orig_len = stack.origin.len();
            stack.origin = stack.origin + &line; // + " "; //TODO: with + " ", lexing is wrong
            let new_orig_len = stack.origin.len()-1;

            //println!("line ({}, {})", old_orig_len, new_orig_len);
            stack.lines.push((line.to_string(), old_orig_len, new_orig_len)); //TODO: check this

            match lex(&*stack.origin) {
               Ok(lexed) => {
                  //Use this to debug lexer:
                  //use utils::print_lexemes;
                  //print_lexemes(&lexed);
                  //break;
                  stack.lexemes = lexed;

                  let mut new_lexemes = Vec::new();
                  for (l, start, end) in stack.lexemes.clone() {
                     new_lexemes.push(l);
                  }
                  let (parsed, success) = parse(&new_lexemes);
                  println!("success parse? : {}", success);
                  //display_sexps(&parsed); //TODO: temporary

                  out = parsed; //TODO: temporary, only for compiler tests

                  if success { break; }
               },
               Err((code, start, end)) => {
                  //println!("lexing error: {:?}", e);
                  let mut ei = ErrInfo::new(code, Some(to_shared_mut(stack)));
                  ei.char_i = start;
                  ei.line_n = line_n;
                  ei.char_highlight_ranges.push((start, end));
                  out = Sexps::err_new(ei);
                  break;
               }
            } line_n += 1;
         }

         display_sexps(&out);
         //display_run_result(&out);
         //display_sexps(&out);
         //root.borrow().print();

         if let Some(len) = out.arr_len() {
            if len != 1 { println!("compiler works with single expression on each line"); }
            compiler.set_exp(out.arr_get_fast(0));

            let llvm_res = IRBuilder::codegen(&compiler, &mut context, &mut module_p);

            match llvm_res {
               Ok((val, _)) => {
                  println!("===compilation success: ===="); val.dump();
                  println!("====end compilaion===");
               },
               Err(s) => {
                  println!("bad compilation: {}", s);
               }
            }

            use comp::ModuleProvider;
            println!("====module ir: ====\n");
            ModuleProvider::dump(&module_p);
            println!("====end module ir ====\n");

         } else { println!("can't compile because not arr"); }

      }
   }

}



fn main() {
   //let lexemes = lex("hi");
   let mut d = Driver::new();
   d.interpreter();
}

