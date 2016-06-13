use oxicloak::*;

use lexer::lex;
use parser::parse;
use utils::display_sexps;
use exp::Sexps;
use errors::{ExecStage, ErrCode, ErrInfo};
use errors::{StackInfo};
use types::{Lexemes, ParseStrResult};

struct Driver {
   //every file gets it's own stack (what about repl input?)
   stacks : Vec<SharedMut<StackInfo>>,
   stack_num : usize
}

impl Driver {
   fn new() -> Driver {
      Driver {
         stacks : Vec::new(),
         stack_num : 0
      }
   }

   pub fn next_stack(&mut self) -> usize {
      let mut stacks = Vec::new();
      stacks.push(to_shared_mut(StackInfo::new()))
      self.stack_num += 1;
      self.stack_num - 1
   }

   pub fn get_stack(&mut self, n : usize) -> Option<SharedMut<StackInfo>> {
      self.stacks.get(n)
   }

   //returns (parsed_sexps, lexemes)
   //use this in repl to parse 1 line
   pub fn parse_str(&mut self, code : &str, stack_n : usize) -> (Sexps, Option<Lexemes>) {
      //let parsed = parse_wrapper(lexemes);
      //pub fn parse_wrapper(&mut self, lex_res : Result<Lexemes, LexErr>) {}
      let stack_opt = self.get_stack(stack_n);
      if let Err(stack) = stack_opt {
         let mut ei = ErrInfo::new(ErrCode::Unexpected, None);
         ei.msg = Some("couldn't get stack in parse_str".to_string());
         return (Sexps::err_new(ei), None);
      }
      let stack = stack_opt.unwrap();

      let lex_res = lex(code);
      if let Err((code, start, end)) = lex_res {
         //println!("lexing error: {:?}", e);
         let mut ei = ErrInfo::new(code, Some(self.stacks[stack_n]));
         ei.char_i = Some(start);
         ei.char_highlight_ranges.push((start, end));
         return (Sexps::err_new(ei), None);
      }

      let lexed = lex_res.unwrap();

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
      return (out, lexed);
      //if success { break; }

      //   Err((code, start, end)) => {
      //      break;
      //   }
      //}
   }

   //pub fn run(&mut self, code : &str) -> Sexps { Sexps::nil_new() }
   //pub fn eval(&mut self, exp : &Sexps) -> Sexps { Sexps::nil_new() }
   //pub fn eval_str(&self, code : &str) -> Sexps { Sexps::nil_new() }
   //pub fn load_multiline(&mut self, code : &String) -> Sexps { Sexps::nil_new() }
   //pub fn load_lisp_file(&self, path : String) -> Sexps {}

   /*test main can use this as repl_eval
      fn lex_printer(parsed_exp : Sexps, lex_res : Result<Lexemes, LexErr>) {
            //Use this to debug lexer:
            //use utils::print_lexemes;
            //print_lexemes(&lexed);
            //break;
      }
      pub fn scheme_interpreter() {} //pass this function to repl with repl_eval for normal normal lisp interpreter
      parse_printer
      jitter
      jit_printer
   */
   pub fn repl<F>(&mut self, repl_eval : F)
      where F : Fn(Sexps, Result<Lexemes, LexErr>) -> Sexps
   {
      use std::io::{self, BufRead, Write};
      let stdin = io::stdin();

      loop {
         let mut repl_eval_out : Sexps = Sexps::nil_new();
         print!("**> ");

         let mut stack = StackInfo::new();
         stack.stage = ExecStage::Lex;

         let mut parsed = errors::parse_exp_err(ErrCode::UncompleteExp, None);

         let mut line_n = 0;
         while utils::is_uncomplete_exp(&parsed) {
            io::stdout().flush().unwrap();
            let line = stdin.lock().lines().next().unwrap().unwrap();

            let old_origin_len = stack.origin.len();
            stack.origin = stack.origin + &line;  //TODO: if we do + " ", does it mess up lexing?
            let new_origin_len = stack.origin.len()-1;

             //println!("line ({}, {})", old_orig_len, new_orig_len);
            stack.lines.push((line.to_string(), old_orig_len, new_orig_len)); //TODO: check this

            /*let lex_res = lex(&*stack.origin);
            //let parsed = parse_wrapper(lexemes);
            pub fn parse_wrapper(&mut self, lex_res : Result<Lexemes, LexErr>) {}
            if let Ok(lexed) = lex_res {}*/
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
            }
            line_n += 1;


            //output from repl_eval() passed, we're gonna display it
            repl_eval_out = repl_eval(parsed, lexemes);
            line_n += 1;
         }
         display_sexp(&repl_eval_out);
      }
   }

   pub fn jitter_repl(&mut self) {
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
            if !utils::is_uncomplete_exp(&parsed) { break; }

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
            }
            line_n += 1;
         }

         display_sexps(&out);
         //display_run_result(&out);
         //display_sexps(&out);
         //root.borrow().print();

         if let Some(len) = out.arr_len() {
            if len != 1 { println!("compiler works with single expression on each line"); }
            compiler.set_exp(out.arr_get_fast(0));

            let llvm_res = compiler.codegen(&mut context, &mut module_p);

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



