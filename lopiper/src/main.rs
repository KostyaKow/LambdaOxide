//#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate oxicloak;

extern crate iron_llvm;
extern crate llvm_sys;

mod reader;
mod utils;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;
mod comp;

use std::env;
use driver::Driver;

fn error_msg(msg : &str, terminate : bool) {
   if terminate { panic!("{}", msg); }
   else { println!("{}", msg); }
}

//bad_args determines if this message is being printed
//because of --help, or because bad arguments were passed
fn usage(bad_args : bool) {
   if bad_args {
      error_msg(format!("Bad arguments passed to {}" % env::args()[0]), false);
   }
   error_msg("usage: TODO", true);
}

enum RunMode { Eval, Lex, Parse, Asm, Jit, None }
enum ExtraArg { None, FileName(String), EvalCode(String) }

/*
./lo --help             = print this message
./lo                    = start interpreter repl
./lo -f <filename>      = evaluate content of filename and return
./lo --eval "<EXP>"     = evaluate given expression and print result to stdout
                        = don't use parenthesis in top level expressions
./lo --lex              = run repl lexer-only
./lo --parse            = run repl parser-only
./lo --asm              = run repl that outputs LLVM assembly for each expression
./lo --jit              = repl for LLVM JIT compiler

./lo -f <filename> [--lex|--parse|--asm|--jit] = do same as one of the options but use file instead of repl
*/
fn main() {
   let args = env::args();

   let driver = Driver::new();

   if args.len() == 1 {
      driver.repl(eval, None);
   }
   else if args.len() > 1 {
      if args.len() == 2 && args[1] == "--help" {
         usage(false); //return 0;
      }

      let mut mode = RunMode::None;
      let mut extra_arg  = ExtraArg::None; //file path or string to eval

      let mut next_fpath = false;
      let mut next_eval_str = false;

      let mut first = true;

      for arg in args {
         if first { first = false; continue; }
         match arg {
            "--lex" => mode = RunMode::Lex,
            "--parse" => mode = RunMode::Parse,
            "--asm" => mode = RunMode::Asm,
            "--jit" => mode = RunMode::Jit,
            _ => {
               if extra_arg != ExtraArg::None { usage(true); }
               if next_fpath {
                  next_fpath = false;
                  extra_arg = ExtraArg::FileName(arg);
               } else if next_eval_str {
                  next_eval_str = false;
                  extra_arg = ExtraArg::EvalCode(arg);
               } else { usage(true); }
            },
            "-f" => next_fpath = true,
            "--eval" => next_eval_str = true,
         }
      }

      let repl_eval = match mode {
         RunMode::None => {
            if let ExtraArg::FileName(path) = extra_arg {

            }

            if let ExtraArg::EvalCode(code) = extra_arg
         }
      }

      /*if mode != RunMode::None && args.len() == 2 {
         match mode {
            RunMode::Lex => driver.repl(lex_printer, None),
            RunMode::Parse => driver.repl(parser_printer, None)
         }
      }*/

      match mode

      if mode == RunMode::None {
         if args.len() ==
      }

      "--lex" => driver.repl(lex_printer),
      "--parse" => driver.repl(parser_printer),
      "--asm" => driver.repl(asm_printer),
      "--jit" => driver.repl(jitter),
      _ => usage(true)

else {
      let arg_num = 0;

      let mut stage = None;

      for arg in args {
         match arg {
            "--lex" => if
         }
         arg_num += 1;
      }
   }

}

//we use this functions to pass to driver repl, and they are used as repl_eval

pub fn asm_printer(parsed_exp : Sexps, lex_res : Result<Lexemes, LexErr>) {}
pub fn jitter(parsed_exp : Sexps, lex_res : Result<Lexemes, LexErr>) {}

//standard scheme interpreter eval for expression
pub fn scm_eval(parsed_exp : Sexps, lex_res : Result<Lexemes, LexErr>) { }

pub fn parser_printer(parsed_exp : Sepxs, lex_res : Result<Lexemes, LexErr) {}
pub fn lex_printer(parsed_exp : Sexps, lex_res : Result<Lexemes, LexErr>) {
      //Use this to debug lexer:
      //use utils::print_lexemes;
      //print_lexemes(&lexed);
      //break;
}

fn main_old() {
   //let lexemes = lex("hi");
   let mut d = Driver::new();
   //d.interpreter();
   d.jitter_repl();
}

