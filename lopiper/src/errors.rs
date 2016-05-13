
//TODO: do something like rustc --explain E0123
//with my error codes
#[allow(dead_code)] //TODO
#[derive(Debug)]
pub enum ErrCode {
   //LEX:
   UnterminatedQuote,

   //PARSE: TODO (do we need all of this?), NoEndParen same as UncompleteExp
   //(UncompleteExp also if user presses enter without typing)
   /*NoStartParen,*/ NoEndParen, ExtraCloseParen, //parse_helper
   ChildParseFail, //TODO: tmp, look at parse_helper and fix this to return array of errors instead of just returning one ChildParseFail
   BadLexeme, //parse_lexeme needs Str, Sym, Int or Float
   Fail2Lexemes, //parse() got 2 lexemes, first needs to be quote, second parse_lexeme()
   BadRange, //TODO: this should be tmp for parse_helper, fix it once parser works right

   //EVAL/RUN fail
   Err(String),
   UncompleteExp, //TODO: is this parse error? we throw this when uncomplete exp

   //GENERIC:
   Unimplemented
}

#[derive(Debug)]
pub enum ErrStage { Lex, Parse, Eval }

#[derive(Debug)]
pub struct StackTrace {
   trace : Vec<String>
}

use gentypes::SizeRange;

//TODO: map lexemes to input
//TODO: line index, or expression index?
//in repl, char_index and line_index start from beginning of last command
//TODO: display range, but if it's too long, draw ...
#[derive(Debug)]
pub struct ErrInfo {
   pub code : Option<ErrCode>,
   pub stage : Option<ErrStage>,
   pub file_path : Option<String>, //path to file in which error occured
   pub origin : Option<String>, //original file text or repl input
   pub origin_lex : Option<Vec<Lexeme>>, //origin with lexemes TODO: is this needed?
   pub range_char : Option<SizeRange>, //character range relative to origin
   pub range_lex : Option<SizeRange>, //character range relative to origin
   pub char_i : Option<usize>, //char index from start of origin
   pub lex_i : Option<usize>, //lexeme index from start of origin
   pub line : Option<usize>, //line number
   pub line_char_i : Option<usize>, //character from beginning of line
   pub line_lex_i : Option<usize>, //lexeme index from line beginning
   pub trace : Option<StackTrace>, //stack trace
   pub msg : Option<String> //custom message
}

impl ErrInfo {
   pub fn new() -> ErrInfo {
      ErrInfo {
         code : None, stage : None, file_path : None,
         origin : None, origin_lex : None range_char : None,
         range_lex : None, char_i : None, lex_i : None, line : None,
         line_char_i : None, line_lex_i : None, trace : None, msg : None
      }
   }
   fn display(&self) {}
}

use std::boxed::Box;
pub fn lo_fail<T>(ei : ErrInfo) -> LoResult<T> { Err(Box::new(ei)) }

//TODO: make sure this doesn't take any space if not being used
//(is box a good idea?)
pub type LoResult<T> = Result<T, Box<ErrInfo>>;

//TODO
pub fn display_result<T>(res : &LoResult<T>) {
   /*match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }*/
}


pub fn parse_exp_err(code : ErrCode, origin_lex : &Vec<Lexeme>,
                     lex_i : usize, range_lex : Option<SizeRange>)
-> Sexps
{
   use exp::Sexps;
   Sexps::err_new(parse_err(code, origin_lex, lex_i, range_lex))
}

pub fn parse_err(code : ErrCode, origin_lex : &Vec<Lexeme>,
                 lex_i : usize, range_lex : Option<SizeRange>)
-> ErrInfo
{
   let mut ei = ErrInfo::new();
   ei.code = Some(code);
   ei.stage = Some(ErrStage::Parse);
   ei.origin_lex = Some(origin_lex.clone());
   ei.lex_i = Some(lex_i);
   ei.range_lex = range_lex;
   ei
}


