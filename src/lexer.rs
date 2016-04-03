//extern crate types;
use types::Lexeme;

//extern crate utils;
use utils::{get_char_ranges, char_at, is_numeric, slice_str};

//mod utils;
//mod types;

pub fn lex(code : &str) -> Vec<Lexeme> {
   let mut lexemes : Vec<Lexeme> = Vec::new();

   let mut col = String::new(); //symbol collector
   let ranges = get_char_ranges(code); //range of strings
   let mut r_it = 0; //current string range
   let mut i = 0;
   let code_len = code.len();

   while i < code_len {
      let str_start = r_it < ranges.len() && ranges[r_it].0 == i;
      let (start, end) = if str_start { ranges[r_it] } else { (0, 0) };

      //if current character c is string or
      //special character push previously collected
      if let Some(c) = char_at(code, i) {
         //should we collect symbols
         let collect = str_start || c == '(' || c == ')' || c == ' ';

         if collect && !col.is_empty() {
            lexemes.push(
               if is_numeric(&col) { Lexeme::Num(col.parse::<i64>().unwrap()) }
               else { Lexeme::Sym(col) });

            col = String::new();
         }
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
