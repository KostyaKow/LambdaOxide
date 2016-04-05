use types::Lexeme;

use utils::{get_char_ranges, char_at, is_numeric, slice_str};
use types::{LexFail, LexResult}
//mod utils;
//mod types;

enum LexType {
   Num, Float, Sym
}

pub fn lex(code : &str) -> LexResult {
   let mut lexemes : Vec<Lexeme> = Vec::new();

   let mut col = String::new(); //symbol collector
   let ranges = get_char_ranges(code); //range of strings
   let mut r_it = 0; //current string range
   let mut i = 0;

   while i < code.len() {
      //get location of strings
      let str_start = r_it < ranges.len() && ranges[r_it].0 == i;
      let (start, end) = if str_start { ranges[r_it] } else { (0, 0) };

      //if current character c is string or
      //special character push previously collected
      let c = char_at(code, i).unwrap();
      //should we collect symbols
      let collect = str_start || c == '(' || c == ')' || c == ' ';

      if collect {
         if col.is_empty() { return Err((LexFail::BadCollect, i)); }

         if is_int(&col)
         let lex_type = if is_numeric(&col) {
            if is_float(&col) { LexType::Float } else { LexType::Num }
         } else { LexType::Sym }

         let lexeme = match lex_type {
            LexType::Num   => Lexeme::Num(to_int(col)),
            LexType::Float => Lexeme::Float(to_float(col)),
            LexType::Sym   => Lexeme::Sym(col)
         }
         lexemes.push(
            if is_numeric(&col) { Lexeme::Num(col.parse::<i64>().unwrap()) }
            else { Lexeme::Sym(col) });

         col = String::new();
      }
      if str_start {
         let l = Lexeme::Str(slice_str(code, start+1, end-1));
         lexemes.push(l);
         i = end + 1;
         r_it += 1;
      }
      if let Some(c) = char_at(code, i) {
         match c {
            '(' => lexemes.push(Lexeme::OpenParen),
            ')' => lexemes.push(Lexeme::CloseParen),
            ' ' => {},
            '"' => i-=1, //"string""s2"
            _   => col.push(c)
         }
      }
      i += 1;
   }

   lexemes
}
