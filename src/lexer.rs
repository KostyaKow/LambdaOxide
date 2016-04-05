use types::Lexeme;

use utils::{get_char_ranges, char_at, is_float, is_int, slice_str, to_float, to_int, contains};

fn collect_sym(col : &str) -> Lexeme {
   if is_int(&col) {
      Lexeme::Int(to_int(col))
   } else if is_float(col) {
      Lexeme::Float(to_float(col))
   } else { Lexeme::Sym(col.to_string()) }
}

pub fn lex(code : &str) -> Vec<Lexeme> {
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
      let collect = str_start || contains(c, vec!['(', ')', '\'', '`', ',', ' ']);

      if collect && !col.is_empty() {
         lexemes.push(collect_sym(&col));
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
            ',' | '`' | '\'' => lexemes.push(Lexeme::Quote(c)),
            '(' => lexemes.push(Lexeme::OpenParen),
            ')' => lexemes.push(Lexeme::CloseParen),
            '"' => i-=1, //"string""s2"
            ' ' => {},
            _   => col.push(c)

         }
      }
      i += 1;
   }

   if !col.is_empty() { lexemes.push(collect_sym(&col)); }
   lexemes
}

