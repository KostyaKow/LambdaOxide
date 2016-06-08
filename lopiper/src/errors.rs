use lexer::Lexeme;
use exp::Sexps;
use std::fmt;
use std::boxed::Box;
use oxicloak::{SizeRange, SizeRanges, SharedMut, to_shared_mut};
use types::{Lexemes, LexErr};

#[derive(Debug, Clone)]
pub enum ExecStage { Unknown, Lex, Parse, Eval }

#[derive(Debug, Clone)]
pub enum ErrCode {
   //LEX:
   UnterminatedQuote,
   MisformedNum, //bad format like 543a or 0.3sd
   BadChar, //TODO: doesn't exist yet

   //TODO: probably remove UncompleteExp because it's same as NoEndParen
   //but also same as UnterminatedQuote
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
   UncompleteExp, //TODO: is this parse error? we throw this when uncomplete exp

   //GENERIC:
   Unimplemented
}

fn get_err_desc(code : ErrCode, func_opt : Option<FuncInfo>) -> String {
   use self::ErrCode::*;
   match code {
      UnterminatedQuote => "unterminated string",
      MisformedNum => "bad format number such as 543a or 0.3sd or --0.3 or 0.32",
      BadChar => "badly formatted character",
      NoStartParen => "extra close parenthesis",
      NoEndParen => "not close parenthesis",
      ChildParseFail => "failed to parse subexpressions",
      BadLexeme => "expected lexeme of type String, Symbol, Integer or Float",
      BadRange => "parse_helper got is_atom, but start != end",
      Unimplemented => "this feature hasn't yet been implemented",
      /*BadNumArgs(num_args_provided) => {
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
      }*/
   _ => "unknown"
   }.to_string()
}

#[derive(Debug, Clone)]
pub struct StackInfo {
   pub stage : ExecStage, //current execution stage
   pub file_path : String, //path to file in which error occured (None means repl)
   pub origin : String, //original file text or repl input
   pub lines : Vec<(String, usize, usize)>, //has every line & and start and end of line in origin

   //original lexemes and their char indices (start and end)
   pub lexemes : Lexemes, //original lexemes

   //trace value get constantly modified (stack trace)
   pub funcs : Vec<FuncInfo>, //empty means haven't called anything yet
}
impl StackInfo {
   pub fn new() -> StackInfo {
      StackInfo {
         stage : ExecStage::Unknown,
         file_path : "<repl>".to_string(),
         origin : "".to_string(), lines : Vec::new(),
         lexemes : Vec::new(), funcs : Vec::new()
      }
   }
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
   name : String,
   args : Vec<String>,
   arg_types : Vec<String>,
   def_loc_char_range : SizeRange //location where function is defined
}

//TODO: map lexemes to input
//TODO: line index, or expression index?
//in repl, char_index and line_index start from beginning of last command
//TODO: display range, but if it's too long, draw ...
//#[derive(Debug, Clone)]
#[derive(Clone)]
pub struct ErrInfo {
   pub stack : Option<SharedMut<StackInfo>>,
   pub code : ErrCode,

   pub line_print_range : Option<SizeRange>, //line range to print from origin
   pub char_highlight_ranges : SizeRanges, //ranges to underline

   pub msg : Option<String>, //custom message

   //TODO: need char_i in err_info?
   //different from StackInfo char_i
   //(false!!!) char index from start of origin of error
   pub char_i : usize, //index from start of line
   pub line_n : usize,
}

impl ErrInfo {
   pub fn new(err_code : ErrCode, stack : Option<SharedMut<StackInfo>>) -> ErrInfo {
      ErrInfo {
         stack : stack, code : err_code, line_print_range : None,
         char_highlight_ranges : Vec::new(), msg : None,
         char_i : 0, line_n : 0
      }
   }

   fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
      use self::ExecStage::*;

      let s = self.stack.clone().unwrap();

      let stage_name = match s.borrow().stage {
         Lex => "lexing", Parse => "parsing",
         Eval => "evaluating", Unknown => "unknown"
      }.to_string();

      let f_len = s.borrow().funcs.len();
      let func = if f_len > 0 {
         Some(s.borrow().funcs[f_len - 1].clone())
      } else { None };

      write!(f, "Encountered an error while {:?}", stage_name);
      write!(f, "\n{}:{}:{}: error code: {:?} error: {}",
            s.borrow().file_path, self.line_n, self.char_i, self.code.clone(), //todo calculate line
            get_err_desc(self.code.clone(), func));

      if let Some(str) = self.msg.clone() {
         write!(f, "additional info: {}", str);
      }

      for r in self.char_highlight_ranges.clone() {
         write!(f, " range: {}-{};", r.0, r.1);
      }
      /*if let Some(ref r) = self.lex_i {
         write!(f, " lex error at: {}; ", r);
         if let Some(ref origin_lex) = self.origin_lex {
            write!(f, " bad lexeme: {:?};", origin_lex[*r]);
         }
      }*/
      write!(f, "\n...........")
   }
}

impl fmt::Debug for ErrInfo {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      /*if let Some(ref err_code) = self.code {
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
      write!(f, "")*/
      self.display(f)
   }
}

//TODO
pub fn display_result<T>(res : &Result<T, Box<ErrInfo>>) {
   /*match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }*/
}

/*TODO: remove this
pub fn lex_err(code : ErrCode, stack : SharedMut<StackInfo>, range : SizeRange)
-> Result<Lexemes, LexErr>
{
   //let mut ei = ErrInfo::new(stack, ErrCode::UnterminatedQuote);
   /TODO: removeme
   ei.origin = Some(code.to_string());
   ei.range_char = Some((c, len));
   ei.char_i = Some(len);/
   //ei.char_highlight_ranges.push(range);
   Err((code, range.0, range.1))
}*/

pub fn parse_exp_err(code : ErrCode, //stack : SharedMut<StackInfo>, //origin_lex : &Vec<Lexeme>, lex_i : usize,
                     range_lex : Option<SizeRange>)
-> Sexps
{
   Sexps::err_new(parse_err(code, range_lex))
}

pub fn parse_err(code : ErrCode, //stack : SharedMut<StackInfo>, //origin_lex : &Vec<Lexeme>, //lex_i : usize,
                 range_lex : Option<SizeRange>)
-> ErrInfo
{
   let mut stack = StackInfo::new();
   stack.stage = ExecStage::Parse;
   let mut ei = ErrInfo::new(code, Some(to_shared_mut(stack)));
//   ei.code = Some(code);
//   ei.stage = Some(ErrStage::Parse);
//   ei.origin_lex = Some((*origin_lex).clone());
//   ei.lex_i = Some(lex_i);
//   ei.range_lex = range_lex;
   ei
}


