use types::{QuoteType, Lexemes, LexErr};
use errors::{ErrInfo, ErrCode, ExecStage, StackInfo, lex_err};
use oxicloak::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
   OpenParen, CloseParen, Sym(String), Quote(QuoteType),
   Int(i64), Float(f64), Str(String)
}

//Ok(range-of-chars), Err((type, start-highlight, end-highlight))
fn get_char_ranges(code : &str) -> Result<SizeRanges, LexErr> {
   let mut ranges : Vec<(usize, usize)> = Vec::new();

   let mut start_quote : Option<usize> = None;
   let mut ignore_next_quote = false;

   let code_chars = code.chars();
   let mut len = 0;
   for (i, c) in code_chars.enumerate() {
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
      Err((ErrCode::UnterminatedQuote, c, len))
   }
   else { Ok(ranges) }
}

pub fn lex(code : &str) -> Result<Lexemes, LexErr> {
   use oxicloak::{char_at, char_at_fast, contains, slice_str};

   let mut lexemes : Lexemes = Vec::new();
   let mut col = String::new(); //symbol collector

   //range of strings
   let range_opt = get_char_ranges(code);
   if let Err(lex_err) = range_opt { return Err(lex_err); }

   let ranges = range_opt.unwrap();
   let mut r_it = 0; //current string range
   let mut i = 0;
   let mut collect_start = 0; let mut collect_end = 0;

   while i < code.len() {
      //if haven't went through all strings, and i is beginning of string
      let start_of_str = r_it < ranges.len() && ranges[r_it].0 == i;
      let (start_str, end_str) = if start_of_str { ranges[r_it] } else { (0, 0) };

      //if current character c is string or special
      //character, then push previously collected
      let c = char_at_fast(code, i);
      //TODO: maybe manual comparison (c == ' ' || c == '(')
      let is_special = start_of_str || contains(c, vec![' ', '(', ')', '\'', '`', ',']);

      if is_special && !col.is_empty() { //push float, int and sym
         if let Some(lexeme) = collect_sym(&col) {
            lexemes.push((lexeme, collect_start, collect_end));
         } else { return Err((ErrCode::MisformedNum, collect_start, collect_end)); }
         col = String::new();
      }
      if start_of_str { //push string if we have one
         let l = Lexeme::Str(slice_str(code, start_str+1, end_str-1));
         lexemes.push((l, start_str, end_str)); //TODO: check all ranges
         i = end_str + 1; //TODO: blah? does this ever run
         r_it += 1; //next string range
      }
      if let Some(c) = char_at(code, i) {
         match c {
            ',' | '`' | '\'' => lexemes.push((Lexeme::Quote(char_to_quote(c).unwrap()), i, i)),
            '(' => lexemes.push((Lexeme::OpenParen, i, i)),
            ')' => lexemes.push((Lexeme::CloseParen, i, i)),
            '"' => { println!("error, should get here"); i-=1; }, //TODO: blah never runs? check this: "string""s2"
            ' ' => {}, //skip
            _   => {
               if col.is_empty() { collect_start = i; }
               else { collect_end = i; }
               col.push(c);
            }
         }
      }
      i += 1;
   }

   if !col.is_empty() {
      if let Some(lexeme) = collect_sym(&col) {
         lexemes.push((lexeme, collect_start, collect_end));
      } else { return Err((ErrCode::MisformedNum, collect_start, collect_end)); }
   }

   if lexemes.len() == 0 { Err((ErrCode::UncompleteExp, 0, 0)) }
   else { Ok(lexemes) }
}

//TODO replace to_float, to_int with this
//pub fn get_str_type(s : &str) -> LexemeType { Lexeme::Int(0) }
//TODO: have is_bad_num for 234asdf or 342.23432 which will cause lex error
fn collect_sym(col : &str) -> Option<Lexeme> {
   if is_int(&col) {
      Some(Lexeme::Int(from_str(col).unwrap()))
   } else if is_float(col) {
      Some(Lexeme::Float(from_str(col).unwrap()))
   } else {
      if is_number_like(col) { None }
      else { Some(Lexeme::Sym(col.to_string())) }
   }
}

fn char_to_quote(c : char) -> Option<QuoteType> {
   match c {
      '`'   => Some(QuoteType::BackQuote),
      '\''  => Some(QuoteType::Q),
      ','   => Some(QuoteType::Comma),
      _     => None
   }
}

