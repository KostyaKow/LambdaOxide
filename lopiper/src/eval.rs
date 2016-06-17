//eval, apply, etc
use oxicloak::*;
use utils::print_lexemes;
use exp::Sexps;
use sym_table::Env;
use std::boxed::Box;
use types::{Lexemes, LexResult};

/*pub struct Evaluator {
}

#[derive(Clone)]
pub struct EvalInfo {
   //every file gets it's own stack (what about repl input?)
   stacks : Vec<SharedMut<StackInfo>>,
   stack_num : usize
}

pub impl EvalInfo {
   fn new() -> EvalInfo {
      EvalInfo {
         stacks : Vec::new(),
         stack_num : 0
      }
   }
}*/


/*pub struct InitComp {
   //get stuff from old/driver.old.rs
}

pub enum InitInfo {
   Comp(InitComp), Intr(SymTable)
}*/

#[derive(Debug)]
pub enum ReplMode { Lex, Parse, Asm, Jit, Eval }
pub type EvalFunc = Box<Fn(Sexps, LexResult, Env) -> Sexps>;

pub fn get_eval_f(mode : ReplMode) -> EvalFunc {
   match mode {
      ReplMode::Eval =>
         Box::new(move |a, b, c| scm_eval(a, b, c)),
      ReplMode::Lex =>
         Box::new(move |a, b, c| lex_printer(a, b, c)),
      ReplMode::Parse =>
         Box::new(move |a, b, c| parse_printer(a, b, c)),
      ReplMode::Asm =>
         Box::new(move |a, b, c| asm_printer(a, b, c)),
      ReplMode::Jit =>
         Box::new(move |a, b, c| jitter(a, b, c))
   }
}


pub fn get_init_f(mode : ReplMode) -> EvalFunc {
   match mode {
      ReplMode::Eval =>
         Box::new(move |a, b, c| scm_eval(a, b, c)),
      ReplMode::Lex =>
         Box::new(move |a, b, c| lex_printer(a, b, c)),
      ReplMode::Parse =>
         Box::new(move |a, b, c| parse_printer(a, b, c)),
      ReplMode::Asm =>
         Box::new(move |a, b, c| asm_printer(a, b, c)),
      ReplMode::Jit =>
         Box::new(move |a, b, c| jitter(a, b, c))
   }
}

//we use this functions to pass to driver repl, and they are used as repl_eval
#[allow(unused_variables)]
pub fn asm_printer(parsed_exp : Sexps, lex_res : LexResult, e : Env) -> Sexps {
   Sexps::new_nil()
}

#[allow(unused_variables)]
pub fn jitter(parsed_exp : Sexps, lex_res : LexResult, e : Env) -> Sexps {
   Sexps::new_nil()
}

//standard scheme interpreter eval for expression
#[allow(unused_variables)]
pub fn scm_eval(parsed_exp : Sexps, lex_res : LexResult, e : Env) -> Sexps {
   Sexps::new_nil()
}

#[allow(unused_variables)]
pub fn parse_printer(parsed_exp : Sexps, lex_res : LexResult, e : Env) -> Sexps {
   Sexps::new_nil()
}

#[allow(unused_variables)]
pub fn lex_printer(parsed_exp : Sexps, lex_res : LexResult, e : Env) -> Sexps {
   //Use this to debug lexer:
   //use utils::print_lexemes;
   //print_lexemes(&lexed);
   //break;
   if let Ok(lexed) = lex_res {
      print_lexemes(&lexed);
   } else {
      println!("lexing failed");
   }
   Sexps::new_nil()
}

