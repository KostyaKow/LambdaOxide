use oxicloak::*;
use lexer::lex;
use parser::parse;
use utils::display_sexps;
use exp::Sexps;
use errors::{ExecStage, ErrCode, ErrInfo};
use errors::{StackInfo};
use types::{Lexemes};

/*#[derive(Clone)]
pub struct ReaderInfo {
   lines : String
}
impl ReaderInfo {}*/

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
      /*let stack_opt = self.get_stack(stack_n);
      if let Err(stack) = stack_opt {
         let mut ei = ErrInfo::new(ErrCode::Unexpected, None);
         ei.msg = Some("couldn't get stack in parse_str".to_string());
         return (Sexps::new_err(ei), None);
      }
      let stack = stack_opt.unwrap();*/

      let lex_res = lex(code);
      if let Err((code, start, end)) = lex_res {
         //println!("lexing error: {:?}", e);
         //TODO: Some(self.stacks[stack_n]));
         let mut ei = ErrInfo::new(code, None);
         ei.char_i = Some(start);
         ei.char_highlight_ranges.push((start, end));
         return (Sexps::new_err(ei), None);
      }

      //stack.lexemes = lex_res.unwrap();
      let lexemes = lex_res.unwrap();

      let mut new_lexemes = Vec::new();
      for (l, start, end) in lexemes.clone() {
         new_lexemes.push(l);
      }

      /*let (parsed, success) = parse(&new_lexemes);
      println!("success parse? : {}", success);*/

      let parse_result = parse(&new_lexemes);

      //display_sexps(&parsed); //TODO: temporary

      //out = parsed; //TODO: temporary, only for compiler tests
      return (parsed, Some(lexemes));
   }

}



