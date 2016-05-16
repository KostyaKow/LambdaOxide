use lexer::Lexeme;
use exp::Sexps;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ExecStage { Lex, Parse, Eval }

//TODO: do something like rustc --explain E0123
//with my error codes
#[allow(dead_code)] //TODO
#[derive(Debug, Clone)]
pub enum ErrCode {
   //LEX:
   UnterminatedQuote,
   MisformedInt, //bad format like 543a //TODO
   MisformedFloat, //bad format like 0.3sd //TODO
   BadChar, //TODO: add characters


   //TODO: probably remove UncompleteExp because it's same as NoEndParen
   //UncompleteExp also if user presses enter without finishing the expression

   NoStartParen, NoEndParen,
   ChildParseFail, //TODO: tmp, look at parse_helper and fix this to return array of errors instead of just returning one ChildParseFail
   BadLexeme, //parse_lexeme needs Str, Sym, Int or Float
   //BadRange might never get triggered
   BadRange, //TODO: this should be tmp for parse_helper, fix it once parser works right

   //EVAL/RUN fail
   //Err(String), //TODO: don't needs this because we have it in the ErrINfo
   //BadNumArgs(num_provided) BadArgTypes(given_types)
   BadNumArgs(u8), BadArgTypes(Vec<String>),
   //UncompleteExp, //TODO: is this parse error? we throw this when uncomplete exp

   //GENERIC:
   Unimplemented
}

fn get_err_desc(code : ErrCode, func_opt : Option<FuncInfo>) -> String {
   match code {
      UnterminatedQuote => "unterminated string",
      MisformedInt => "bad format integer such as 543a",
      MisformedFloat => "bad format float such as 0.3sd or --0.3 or 0.32.",
      BadChar => "badly formatted character",
      NoStartParen => "extra close parenthesis",
      NoEndParen => "not close parenthesis",
      ChildParseFail => "failed to parse subexpressions",
      BadLexeme => "expected lexeme of type String, Symbol, Integer or Float",
      BadRange => "parse_helper got is_atom, but start != end",
      Unimplemented => "this feature hasn't yet been implemented"
      BadNumArgs(num_args_provided) => {
         if let Some(func) = func_opt {
            format!("wrong number of arguments to {} (needed: {}, got: {})",
                     func.name, func.args.len(), num_args_provided)
         }
         else {
            format!("get_err_desc called with code=BadNumArgs but func_opt = None; provided ({})",
                    num_args_provided)
         }
      },
      BadArgTypes(given) => {
         if let Some(func) = func_opt {
            format!("wrong type of arguments to function {} (needed: {:?}, got: {:?})",
                     func.name, func.arg_types, given)
         }
         else {
            format!("get_err_desc called with code=BadArgTypes but func_opt = None; given args ({})",
                    given)
         }
      }
   }.to_string()
}

pub struct StackInfo {
   pub stage : Option<ExecStage>, //current execution stage
   pub file_path : Option<String>, //path to file in which error occured (None means repl)
   pub origin : Option<String>, //original file text or repl input
   pub lines : Option<Vec<String>>,
   pub lexemes : Option<Vec<Lexeme>>, //original lexemes
   //map of lexemes indices to character indices (start and end)
   pub char_to_lex_map : HashMap<usize, (usize, usize)>,
}
use std::fmt;
impl fmt::Debug for StackInfo {
   /*fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, " {:?}", self.stage);
   }*/
}

pub struct FuncInfo {
   name : String,
   args : Vec<String>,
   arg_types : Vec<String>,
   def_loc_char_range : SizeRange //location where function is defined
}
impl fmt::Debug for FuncInfo {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{:?}", name);
   }
}

//TODO: this gets constantly modified
#[derive(Debug, Clone)]
pub struct StackTrace {
   pub funcs : Option<Vec<FuncInfo>>,
   pub char_i : Option<usize>, //char index from start of origin
   pub lex_i : Option<usize>, //index into lexemes of StackInfo
}

use gentypes::SizeRange;

//TODO: map lexemes to input
//TODO: line index, or expression index?
//in repl, char_index and line_index start from beginning of last command
//TODO: display range, but if it's too long, draw ...
//#[derive(Debug, Clone)]
#[derive(Clone)]
pub struct ErrInfo {
   pub stack : Box<StackInfo>,
   pub trace : StackTrace,
   pub code : ErrCode,

   //char/lex range relative to origin
   pub range_lex_print : Option<SizeRange>, //range into lexemes to print
   pub range_char_print : Option<SizeRange>, //character range into origin to print

   pub highlight_ranges : Option<Vec<SizeRange>>, //ranges to underline

   /*
   //character/lex range relative to origin (gets converted to line_range_underlines[0])
   pub range_char : Option<SizeRange>,
   pub range_lex : Option<SizeRange>, //TODO: do conversions early, don't need this

   pub line_range : Option<SizeRange>, //range of lines to print
   pub line_range_underlines : Option<Vec<SizeRange>>, //range to underline

   pub line : Option<usize>, //TODO: need this? line number
   pub line_char_i : Option<usize>, //TODO: need this? character from beginning of line
   pub line_lex_i : Option<usize>, //TODO: need this? lexeme index from line beginning
   */

   pub msg : Option<String> //custom message
}

impl ErrInfo {
   pub fn new(stack : Box<StackInfo>, trace : StackTrace, err_code : ErrCode) -> ErrInfo {
      ErrInfo {
         stack : stack, trace : trace, code : err_code,
         range_char : None, range_lex : None,
         line : None, line_char_i : None, line_lex_i : None,
         msg : None
      }
   }
   fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
      use ExecStage::*;
      let stage_name = match self.stack.stage {
         Lex => "lexing", Parse => "parsing", Eval => "evaluating"
      }.to_string();

      let file = if let Some(path) = self.stack.file_path { path } else { "repl" };

      let func = if let Some(func_vec) = self.trace.funcs {
         let l = func_vec.len();
         if l > 0 { func_vec[l - 1] }
         else { None }
      } else { None };

      write!(f, "Encountered an error while {:?}", stage_name);
      write!(f, "\n{}:{}:{}: error code: {:?} error: ",
            file, self.line, self.line_char_i, self.code,
            get_err_desc(self.code, func));

      if let Some(msg) = self.msg {
         write!(f, "additional info: {}", msg);
      }

      write!(
      if let Some(ref r) = self.range_lex {
         write!(f, " lex range: {}-{};", r.0, r.1);
      }
      if let Some(ref r) = self.lex_i {
         write!(f, " lex error at: {}; ", r);
         if let Some(ref origin_lex) = self.origin_lex {
            write!(f, " bad lexeme: {:?};", origin_lex[*r]);
         }
      }

      if let Some(ref s) = self.stage {
         write!(f, " Execution stage: {:?};", s);
      }
      write!(f, "")

   }
}

use std::fmt;
impl fmt::Debug for ErrInfo {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      if let Some(ref err_code) = self.code {
         write!(f, "err: {:?};", err_code);
      }
      if let Some(ref r) = self.range_lex {
         write!(f, " lex range: {}-{};", r.0, r.1);
      }
      if let Some(ref r) = self.lex_i {
         write!(f, " lex error at: {}; ", r);
         if let Some(ref origin_lex) = self.origin_lex {
            write!(f, " bad lexeme: {:?};", origin_lex[*r]);
         }
      }

      if let Some(ref s) = self.stage {
         write!(f, " Execution stage: {:?};", s);
      }
      write!(f, "")
   }
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
   ei.origin_lex = Some((*origin_lex).clone());
   ei.lex_i = Some(lex_i);
   ei.range_lex = range_lex;
   ei
}


