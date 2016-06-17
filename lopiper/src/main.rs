//#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate oxicloak;

//TODO: uncomment this for compiler
//extern crate iron_llvm;
//extern crate llvm_sys;

mod reader;
mod utils;
mod types;
mod exp;
mod lexer;
mod parser;
mod errors;
//mod comp; //TODO: uncomment this for compiler
mod driver;
mod eval;
mod sym_table;

use eval::ReplMode;
use std::env;
use driver::Driver;
use std::process::exit;

fn error_msg(msg : &str, terminate_status_opt : Option<i32>) {
   /*if terminate { panic!("{}", msg); }
   else { println!("{}", msg); }*/
   println!("{}", msg);
   if let Some(term_status) = terminate_status_opt {
      exit(term_status);
   }
}

//bad_args determines if this message is being printed
//because of --help, or because bad arguments were passed
fn usage(bad_args : bool) {
   if bad_args {
      let arg0 = env::args().collect::<Vec<String>>()[0].clone();
      error_msg(&*format!("Bad arguments passed to {}", arg0), None);
   }
   let s = "usage:\
   ./lo --help             = print this message \
   ./lo                    = start interpreter repl \
   ./lo -f <filename>      = evaluate content of filename and return \
   ./lo --eval \"<EXP>\"     = evaluate given expression and print result to stdout \
                           = don't use parenthesis in top level expressions \
   ./lo --lex              = run repl lexer-only \
   ./lo --parse            = run repl parser-only \
   ./lo --asm              = run repl that outputs LLVM assembly for each expression \
   ./lo --jit              = repl for LLVM JIT compiler \
\
   ./lo -f <filename> [--lex|--parse|--asm|--jit] = do same as one of the options but use file instead of repl \
   ";

   let status = if bad_args { 1 } else { 0 };
   error_msg(s, Some(status));
}

//use std::cmp::PartialEq;
#[derive(PartialEq, Clone)]
enum ExtraArg { None, FileName(String), EvalCode(String) }

//if eval_str_opt is None, then it's --eval flag so just call run
fn launch(mode : ReplMode,
          path_opt : Option<String>,
          eval_str_opt : Option<String>)
{
   let mut driver = Driver::new();

   if let Some(eval_str) = eval_str_opt {
      println!("running with eval flag");
      driver.run(eval_str, mode, true);
   } else {
      driver.repl(mode, path_opt);
   }
}

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
   let args = env::args().collect::<Vec<String>>();

   if args.len() == 1 {
      launch(ReplMode::Eval, None, None); exit(0);
   }
   else if args.len() > 1 {
      if args.len() == 2 && args[1] == "--help" {
         usage(false); //return 0;
      }

      let mut mode = ReplMode::None;
      let mut extra_arg  = ExtraArg::None; //file path or string to eval

      let mut next_fpath = false; //next argument is fpath
      let mut next_eval_str = false; //next arg is str to eval

      let mut first = true;

      for arg in args {
         if first { first = false; continue; }
         let mut good_unknown = false;

         match &*arg {
            "--lex" => mode = ReplMode::Lex,
            "--parse" => mode = ReplMode::Parse,
            "--asm" => mode = ReplMode::Asm,
            "--jit" => mode = ReplMode::Jit,
            "-f" => { next_fpath = true; continue; },
            "--eval" => { next_eval_str = true; continue; },
            _ => {
               if extra_arg != ExtraArg::None {
                  usage(true); //unknown arg & we already received file or evalstr
               }
               if next_fpath {
                  extra_arg = ExtraArg::FileName(arg);
                  next_fpath = false;
               } else if next_eval_str {
                  extra_arg = ExtraArg::EvalCode(arg);
                  next_eval_str = false;
               } else {
                  usage(true); //uknown arg & we don't have previous -f or --eval
               }
            },
         }
         if next_eval_str || next_fpath {
            //previus arg is -f or --eval, but this arg doesn't have their value
            usage(true);
         }
         next_fpath = false; next_eval_str = false; //reset file/eval str flag
      }

      //file path to pass to repl
      let repl_path = if let ExtraArg::FileName(path) = extra_arg.clone()
      { Some(path) } else { None };

      let eval_str = if let ExtraArg::EvalCode(code) = extra_arg {
         ////passed --eval without string
         //if mode != ReplMode::None { usage(true); }
         Some(code)
      } else { None };

      if let Some(code) = eval_str {
         launch(mode, None, Some(code)); exit(0); //--evall "blah"
      } else {
         //run repl, with or without file
         launch(mode, repl_path, None); exit(0);
      }
   }
}

fn main_old() {
   //let lexemes = lex("hi");
   let mut d = Driver::new();
   //d.interpreter();
   //d.jitter_repl();
}

