use lexer::Lexeme;
use exp::Sexps;
use std::fmt;
use std::boxed::Box;
use gentypes::SizeRange;

#[derive(Debug, Clone)]
pub enum ExecStage { Unknown, Lex, Parse, Eval }

#[derive(Debug, Clone)]
pub enum ErrCode {
   //LEX:
   UnterminatedQuote,
   MisformedInt, //bad format like 543a
   MisformedFloat, //bad format like 0.3sd
   BadChar,

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
   use ErrCode::*;
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
      Unimplemented => "this feature hasn't yet been implemented",
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

#[derive(Debug, Clone)]
pub struct StackInfo {
   pub stage : ExecStage, //current execution stage
   pub file_path : String, //path to file in which error occured (None means repl)
   pub origin : String, //original file text or repl input
   pub lines : Vec<(String, usize, usize)>, //has every line & and start and end of line in origin
   pub lexemes : Vec<Lexeme>, //original lexemes
   //map of lexemes indices to character indices (start and end)
   pub lex_to_char : Vec<(usize, usize)>,

   //trace value get constantly modified (stack trace)
   pub funcs : Vec<FuncInfo>, //empty means haven't called anything yet
   pub char_i : usize, //char index from start of origin of current iteration
}
impl StackInfo {
   fn new(origin : &str) -> StackInfo {
      StackInfo {
         stage : ExecStage::Unknown,
         file_path : "<repl>".to_string(),
         origin : origin.to_string(), lines : Vec::new(),
         lexemes : Vec::new(), lex_to_char : Vec::new(),
         funcs : Vec::new(), char_i : 0
      }
   }
}

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
   pub stack : SharedMut<StackInfo>,
   pub code : ErrCode,

   pub line_print_range : Option<SizeRange>, //line range to print from origin
   pub char_highlight_ranges : SizeRanges, //ranges to underline

   pub msg : Option<String>, //custom message

   //TODO: need char_i in err_info?
   //different from StackInfo char_i
   //pub char_i : usize //char index from start of origin of error
}

impl ErrInfo {
   pub fn new(stack : SharedMut<StackInfo>, err_code : ErrCode) -> ErrInfo {
      ErrInfo {
         stack : stack, code : err_code, line_print_range : None,
         char_highlight_ranges : Vec::new(), msg : None
      }
   }

   fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
      use ExecStage::*;

      let s = self.stack.borrow();

      let stage_name = match s.stage {
         Lex => "lexing", Parse => "parsing",
         Eval => "evaluating", Unknown => "unknown"
      }.to_string();

      let f_vec_len = s.func_vec.len();
      let func = if if l > 0 { s.func_vec[f_vec_len - 1] } else { None };

      /*write!(f, "Encountered an error while {:?}", stage_name);
      write!(f, "\n{}:{}:{}: error code: {:?} error: ",
            s.file_path, self.line, self.line_char_i, self.code, //todo calculate line
            get_err_desc(self.code, func));

      if let Some(msg) = self.msg {
         write!(f, "additional info: {}", msg);
      }

      if let Some(ref r) = self.range_lex {
         write!(f, " lex range: {}-{};", r.0, r.1);
      }
      if let Some(ref r) = self.lex_i {
         write!(f, " lex error at: {}; ", r);
         if let Some(ref origin_lex) = self.origin_lex {
            write!(f, " bad lexeme: {:?};", origin_lex[*r]);
         }
      }*/
      write!(f, "")
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



//lambdaoxide result fail
pub fn lo_res_fail<T>(ei : ErrInfo) -> LoResult<T> { Err(Box::new(ei)) }

//TODO: is box a good idea?
pub type LoResult<T> = Result<T, Box<ErrInfo>>;

//TODO
pub fn display_result<T>(res : &LoResult<T>) {
   /*match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }*/
}

use types::LexResult;
pub fn lex_err(code : ErrCode, stack : SharedMut<StackInfo>, range : SizeRange) -> LexResult {
   let mut ei = ErrInfo::new(stack, ErrCode::UnterminatedQuote);
   /*TODO: removeme
   ei.origin = Some(code.to_string());
   ei.range_char = Some((c, len));
   ei.char_i = Some(len);*/
   ei.char_highlight_ranges.push(range);
   lo_res_fail(ei)
}

pub fn parse_exp_err(code : ErrCode, stack : SharedMut<StackInfo>, //origin_lex : &Vec<Lexeme>, lex_i : usize,
                     range_lex : Option<SizeRange>)
-> Sexps
{
   use exp::Sexps;
   Sexps::err_new(parse_err(code, origin_lex, lex_i, range_lex))
}

pub fn parse_err(code : ErrCode, stack : SharedMut<StackInfo>, //origin_lex : &Vec<Lexeme>, //lex_i : usize,
                 range_lex : Option<SizeRange>)
-> ErrInfo
{
   let mut ei = ErrInfo::new(stack, code);
//   ei.code = Some(code);
//   ei.stage = Some(ErrStage::Parse);
//   ei.origin_lex = Some((*origin_lex).clone());
//   ei.lex_i = Some(lex_i);
//   ei.range_lex = range_lex;
   ei
}


