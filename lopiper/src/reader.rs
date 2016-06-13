use oxicloak::*;
use lexer::lex;
use parser::parse;
use utils::display_sexps;
use exp::Sexps;
use errors::{ExecStage, ErrCode, ErrInfo};
use errors::{StackInfo};
use types::{Lexemes, ParseStrResult};

#[derive(Clone)]
pub struct ReaderInfo {
   lines :
}
impl ReaderInfo {

}

pub struct Reader {
   origin : Option<String>, //original file text or repl input
   lexemes : Option<Lexemes>, //original lexemes
   lines : Vec<String>,

   parsed : Option<Sexps>
}

impl Reader {
   //pub fn finish() -> Reader {}

   fn new() -> Reader {
      Reader {
         origin : None, lexemes : None, parsed : None
      }
   }

   //pub fn get_lexemes()

   //just call parse_line repetitively
   //pub fn add_to_line(&mut self, new_code : &str);

   //returns (parsed_sexps, lexemes)
   //use this in repl to parse 1 line
   pub fn parse_line(&mut self, code : &str, stack_n : usize)
   -> (Sexps, Option<Lexemes>)
   {
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

}



