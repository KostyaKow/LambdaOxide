use types::QuoteType;
use errors::{LoResult, ErrInfo, ErrCode, ErrStage, lo_fail};
use gentypes::{SizeRanges, SizeRange};

#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
   OpenParen, CloseParen, Sym(String), Quote(QuoteType),
   Int(i64), Float(f64), Str(String)
}

type CharRangeResult = LoResult<SizeRanges>;
fn get_char_ranges(code : &str, stack_info : SharedMut<StackInfo>) -> CharRangeResult {
   let mut ranges : Vec<(usize, usize)> = Vec::new();

   let mut start_quote : Option<usize> = None;
   let mut ignore_next_quote = false;

   let code_chars = code.chars();
   let mut len = 0;
   for (i, c) in code_chars.enumerate() {
      stack_info.borrow_mut().char_i = i;

      if c == '"' {
         match start_quote {
            //if we have start
            Some(start) if !ignore_next_quote => {
               ranges.push((start, i));
               start_quote = None;
            }
            None if !ignore_next_quote => {
               start_quote = Some(i);
            }
            _ => {}
         }
      }
      if c == '\\' { ignore_next_quote = true; }
      else { ignore_next_quote = false; }
      len = i;
   }

   if let Some(c) = start_quote {
      let mut ei = ErrInfo::new(stack_info, ErrCode::UnterminatedQuote);
      /*TODO: removeme
      ei.origin = Some(code.to_string());
      ei.range_char = Some((c, len));
      ei.char_i = Some(len);*/
      ei.char_highlight_ranges.push((c, len));
      lo_fail(ei)
   }
   else { Ok(ranges) }
}

fn collect_sym(col : &str) -> Lexeme {
   use genutils::{is_int, is_float, to_int, to_float};
   if is_int(&col) {
      Lexeme::Int(to_int(col))
   } else if is_float(col) {
      Lexeme::Float(to_float(col))
   } else { Lexeme::Sym(col.to_string()) }
}

pub type LexResult = LoResult<Vec<(Lexeme, SizeRange)>>;
pub fn lex(code : &str, stack_info : SharedMut<StackInfo>, debug : bool) -> LexResult {
   use genutils::{char_at, char_at_fast, contains, slice_str};

   stack_info.borrow_mut().stage = ExecStage::Lex;

   let mut lexemes : Vec<(Lexeme, SizeRange)> = Vec::new();

   let mut col = String::new(); //symbol collector

   //range of strings
   let range_opt = get_char_ranges(code);
   if let Err(ei) = range_opt { return lo_fail(*ei); }

   let ranges = range_opt.unwrap();
   let mut r_it = 0; //current string range
   let mut i = 0;

   while i < code.len() {
      //if haven't went through all strings, and i is beginning of string
      let start_of_str = r_it < ranges.len() && ranges[r_it].0 == i;
      let (start, end) = if start_of_str { ranges[r_it] } else { (0, 0) };

      //if current character c is string or special
      //character, then push previously collected
      let c = char_at_fast(code, i);
      //TODO: maybe manual comparison (c == ' ' || c == '(')
      let collect = start_of_str || contains(c, vec![' ', '(', ')', '\'', '`', ',']);

      if collect && !col.is_empty() { //push float, int and sym
         lexemes.push(collect_sym(&col));
         col = String::new();
      }
      if start_of_str { //push string if we have one
         let l = Lexeme::Str(slice_str(code, start+1, end-1));
         lexemes.push(l);
         i = end + 1;
         r_it += 1; //next string range
      }
      if let Some(c) = char_at(code, i) {
         match c {
            ',' | '`' | '\'' => lexemes.push(Lexeme::Quote(char_to_quote(c).unwrap())),
            '(' => lexemes.push(Lexeme::OpenParen),
            ')' => lexemes.push(Lexeme::CloseParen),
            '"' => i-=1, //"string""s2"
            ' ' => {}, //skip
            _   => col.push(c)

         }
      }
      i += 1;
   }

   if !col.is_empty() { lexemes.push(collect_sym(&col)); }

   if lexemes.len() == 0 {
      let mut ei = ErrInfo::new();
      ei.code = Some(ErrCode::UncompleteExp);
      ei.stage = Some(ErrStage::Lex);
      ei.origin = Some(code.to_string());
      ei.char_i = Some(0);
      lo_fail(ei)
   } else {
      Ok(lexemes)
   }
}

//TODO replace to_float, to_int with this
//pub fn get_str_type(s : &str) -> LexemeType { Lexeme::Int(0) }

fn char_to_quote(c : char) -> Option<QuoteType> {
   match c {
      '`'   => Some(QuoteType::BackQuote),
      '\''  => Some(QuoteType::Q),
      ','   => Some(QuoteType::Comma),
      _     => None
   }
}

