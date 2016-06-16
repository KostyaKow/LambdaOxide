use errors::{ErrInfo, ErrCode, ExecStage, StackInfo};
use exp::Sexps;
use oxicloak::*;
use eval::{get_eval_f};
use sym_table::SymTableRoot;
use eval::ReplMode;
use lexer::lex;
use parser::parse;
use std::process::exit;
use utils::display_sexps;

use eval::EvalFunc;

pub struct Driver {
   //TODO: driver should contain symbol table too

   file_origin : Option<String>,

   //TODO: set this to none once finished with file
   file_lines : Option<Vec<String>>,
   file_line : usize,

   sym_table : Option<SymTableRoot>
}

impl Driver {
   //pub fn eval(&mut self, exp : &Sexps) -> Sexps { Sexps::new_nil() }
   //pub fn eval_str(&self, code : &str) -> Sexps { Sexps::new_nil() }
   //pub fn load_multiline(&mut self, code : &String) -> Sexps { Sexps::new_nil() }
   //pub fn load_lisp_file(&self, path : String) -> Sexps {}
   //pub fn load_file(&mut self, path : &str, file_eval : F) -> Sexps {}
   //pub fn load_multiline(&mut self, exp : &str, line_eval : F) -> Sexps {}
   /*pub fn next_stack(&mut self) -> usize {
      let mut stacks = Vec::new();
      stacks.push(to_shared_mut(StackInfo::new()));
      self.stack_num += 1;
      self.stack_num - 1
   }
   pub fn get_stack(&mut self, n : usize) -> Option<SharedMut<StackInfo>> {
      self.stacks.get(n)
   }*/

   pub fn new() -> Driver {
      Driver {
         file_origin : None,
         file_lines : None,
         file_line : 0,
         sym_table : None //TODO
      }
   }

   //TODO: calls eval with parsed Sexps
   //main calls this for command line argument --eval
   pub fn run(&mut self, code_raw : String,
              mode : ReplMode, from_main : bool)
   -> Sexps
   {
      let code = if from_main {
         //TODO: this breaks comments and strings with ";" in them
         let mut good = "".to_string();
         let splitted = code_raw.split(';');
         for expr_str in splitted  {
            good = good + "(" + expr_str + ")";
         }
         good.to_string()
      } else { code_raw };

      Sexps::new_nil()
   }

   //either repl driver, or gets line from path
   fn get_line(&mut self, path_opt : Option<String>) -> Option<String> {
      if let Some(path) = path_opt {
         //TODO: make sure not out of range, and check before unwrap
         let line = self.file_lines.clone().unwrap()[self.file_line].clone();
         self.file_line += 1;
         Some(line)
      } else {
         use std::io::{self, BufRead, Write};
         let stdin = io::stdin();
         print!("**> ");
         io::stdout().flush().unwrap();
         let line = stdin.lock().lines().next().unwrap().unwrap();
         Some(line)
      }
   }

   //TODO: implement comments correctly (;) (kinda done)
   //TODO: account for comments in error reporting
   fn load_file(&mut self, path : String) -> Sexps {
      use oxicloak::read_file;
      let file_data_opt = read_file(&*path);
      if let Err(e) = file_data_opt {
         let mut err = ErrInfo::new(ErrCode::FileFail, None);
         let msg = format!("could not read lisp file ({}): {}", path, e);
         err.msg = Some(msg);
         return Sexps::new_err(err);
      };
      let origin = file_data_opt.unwrap();
      self.file_origin = Some(origin.clone());
      self.file_line = 0;
      let mut lines_no_comment = Vec::new();
      let origin_lines = origin.split('\n');
      for line in origin_lines { //remove comments
         let mut line_data = "".to_string();
         //for char in line
         for (i, c) in line.chars().enumerate() {
            if c == ';' { break; }
            line_data.push(c);
         }
         lines_no_comment.push(line_data);
      }

      self.file_lines = Some(lines_no_comment);
      Sexps::new_nil()
   }

   //if path is None, we start repl, otherwise load file
   //if error, return it wrapped in Sexps
   //pub fn repl<EvalFunc>(&mut self, mode : ReplMode, path_opt : Option<String>)
   pub fn repl(&mut self, mode : ReplMode, path_opt : Option<String>)
   -> Sexps
      //where F : Fn(Sexps, Result<Lexemes, LexErr>) -> Sexps
   {
      self.file_origin = None;
      self.file_line = 0;

      //run repl in file mode
      if let Some(path) = path_opt {
         /*let status = load_file(path);
         if status.is_err() {
            return status;
         }*/
      }

      //kk move to repl_init
      use std::io::{self, BufRead, Write};
      let stdin = io::stdin();

      loop {
         let mut repl_eval_out : Sexps = Sexps::new_nil();
         print!("**> ");

         //TODO: move ExecStage and lines from StackInfo to driver.
         let mut stack = StackInfo::new();
         stack.stage = ExecStage::Lex;

         use errors::parse_exp_err;
         let mut parsed = parse_exp_err(ErrCode::UncompleteExp, None);

         let mut line_n = 0;
         use utils::is_uncomplete_exp;
         while is_uncomplete_exp(&parsed) {
            io::stdout().flush().unwrap();
            let line = stdin.lock().lines().next().unwrap().unwrap();

            let old_origin_len = stack.origin.len();
            stack.origin = stack.origin + &line;  //TODO: if we do + " ", does it mess up lexing?
            let new_origin_len = stack.origin.len()-1;

             //println!("line ({}, {})", old_orig_len, new_orig_len);
            stack.lines.push((line.to_string(), old_origin_len, new_origin_len)); //TODO: check this

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
                  /*let (parsed, success) = parse(&new_lexemes);
                  println!("success parse? : {}", success);*/
                  let parse_res = parse(&new_lexemes);
                  if let Err((code, start, end)) = parse_res { //TODO: check this
                     //TODO: create function from_range_err_to_sexps()
                     let mut ei = ErrInfo::new(code, None);
                     ei.char_i = Some(start);
                     ei.char_highlight_ranges.push((start, end));
                     parsed = Sexps::new_err(ei);
                     break;
                  }
                  //display_sexps(&parsed); //TODO: temporary

                  //out = parsed; //TODO: temporary, only for compiler tests
                  break;
                  //if success { break; }
               },
               Err((code, start, end)) => {
                  //println!("lexing error: {:?}", e);
                  //TODO: //Some(to_shared_mut(stack))
                  let mut ei = ErrInfo::new(code, None);
                  ei.char_i = Some(start);
                  ei.line_n = Some(line_n);
                  ei.char_highlight_ranges.push((start, end));
                  parsed = Sexps::new_err(ei);
                  break;
               }
            }
            line_n += 1;

            let repl_eval = get_eval_f(mode);

            let env = (Vec::new(), 0);
            //output from repl_eval() passed, we're gonna display it
            repl_eval_out = repl_eval(parsed.clone(), Ok(stack.lexemes), env);
            line_n += 1;
         }
         display_sexps(&repl_eval_out);
      }
   }

}


