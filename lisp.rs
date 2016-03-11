
//#![feature(slice_patterns)]
#![feature(str_char)]

fn syntax_err(s: &str, location: u32) {
   println!("error at charachter {}: {}", location, s);
}
fn internal_err(s: &str) {
   println!("internal error: {}", s);
}

//replace with build-in
//slice_str("hello", 1, 3) => "ell"
fn slice_str(s: &str, start: usize, end: usize) -> String {
   let mut sub: String = String::new();
   let mut started: bool = false;

   if start >= end { internal_err("slice_str: start>=end"); }
   if end >= s.len() {  internal_err("slice_str: end >= string end"); }

   for (i, c) in s.chars().enumerate() {
      if i >= end+1 { return sub; }
      if started { sub.push(c); continue; }
      if i >= start { started = true; sub.push(c); }
   }
   sub
}

enum Lexeme {
   OpenParen, CloseParen, Str(String), Code(String)
}

fn get_char_ranges(code : &str) -> Vec<(usize, usize)> {
   let mut ranges : Vec<(usize, usize)> = Vec::new();

   let mut start_quote : Option<usize> = None;
   let mut ignore_next_quote = false;

   for (i, c) in code.chars().enumerate() {

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
   }

   if let Some(x) = start_quote { syntax_err("unterminated quote", x as u32); }

   ranges
}


fn lex(code : &str) -> Vec<Lexeme> {
   let mut lexemes : Vec<Lexeme> = Vec::new();

   let mut code_collector = String::new();
   let mut collect : bool;

   let ranges = get_char_ranges(code);
   let mut range_it = 0;

   let mut i = 0;
   let code_len = code.len();

   while i < code_len {
      collect = false;
      let mut c = code.char_at(i);
      if range_it < ranges.len() && (ranges[range_it].0 == i) {
         collect = true;
      } else if c == '(' || c == ')' { collect = true; }
      if collect && !code_collector.is_empty() {
         lexemes.push(Lexeme::Code(code_collector));
         code_collector = String::new();
      }

      if range_it < ranges.len() {
         let (start, end) = ranges[range_it];
         if start == i {
            range_it += 1;
            i = end+1;
            let l = Lexeme::Str(slice_str(code, start, end));
            lexemes.push(l);
         }
      }
      c = code.char_at(i);
      match c {
         '(' => lexemes.push(Lexeme::OpenParen),
         ')' => lexemes.push(Lexeme::CloseParen),
         ' ' => {},
         _   => code_collector.push(c)
      }

      i += 1;
   }

   lexemes
}

fn main() {

   let code : &str = "(hello (\"world\") \"another \\\"string\")";
   let z = lex(code);
   for c in z {
      match c {
         /*_ => {} empty match */
         Lexeme::OpenParen => println!("open paren"),
         Lexeme::CloseParen => println!("close paren"),
         Lexeme::Str(s) => println!("string {}", s),
         Lexeme::Code(s) => println!("code {}", s),
      }
   }

   /*for arg in env::args() {
      println!("{}", arg);
   }*/
   //let zz = lex(code); for c in zz {println!("{}", c)}

   /*let a :[i32; 3] = [1, 2, 3];
   println!("{}", a[0]);*/
}

//TODO: unit test
//println!("{}", slice_str("hello", 1, 3));
//use std::env; use std::io; use std::io::prelude::*; use std::io::BufReader; use std::fs::File;
/*fn lex(code : &str) -> Vec<String> {
   let mut lexemes = Vec::new();

   let mut lex = String::new();

   for c in code.chars() {
      if c == ' ' {
         lexemes.push(lex);
         lex = String::new();
         continue;
      }
      lex.push(c);
   }
   if !lex.is_empty() { lexemes.push(lex); }

   lexemes
}*/
