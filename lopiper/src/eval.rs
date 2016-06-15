//eval, apply, etc
use oxicloak::*;
use exp::Sexps;
use types::{Lexemes, LexResult};

pub struct Evaluator {

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
}

pub enum ReplMode { Lex, Parse, Asm, Jit, Eval, None }
pub type EvalFunc = Fn(Sexps, LexResult);

pub fn get_eval_f(mode : ReplMode) -> EvalFunc {
   match mode {
      ReplMode::None => panic!("TODO: ReplMode None"),
      ReplMode::Eval => scm_eval,
      ReplMode::Lex => lex_printer,
      ReplMode::Parse => parse_printer,
      ReplMode::Asm => asm_printer,
      ReplMode::Jit => jitter
   }
}

//we use this functions to pass to driver repl, and they are used as repl_eval

pub fn asm_printer(parsed_exp : Sexps, lex_res : LexResult) {}
pub fn jitter(parsed_exp : Sexps, lex_res : LexResult) {}

//standard scheme interpreter eval for expression
pub fn scm_eval(parsed_exp : Sexps, lex_res : LexResult) { }

pub fn parser_printer(parsed_exp : Sexps, lex_res : LexResult) {}
pub fn lex_printer(parsed_exp : Sexps, lex_res : LexResult) {
   //Use this to debug lexer:
   //use utils::print_lexemes;
   //print_lexemes(&lexed);
   //break;
}

