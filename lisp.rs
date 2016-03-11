
fn syntax_err(s: &str, location: u32) {
   println!("error at charachter {}: {}", location, s);
}
fn internal_err(s: &str) {
   println!("internal error: {}", s);
}

//replace with build-in
//slice_str("hello", 1, 3) => "ell"
fn slice_str(s: &str, start: u32, end: u32) -> String {
   let mut sub: String = String::new();
   let mut started: bool = false;

   if start >=end { internal_err("slice_str: start>=end"); }

   for (fake_i, c) in s.chars().enumerate() {
      let i = fake_i as u32;
      if i > end { return sub; }
      if started { sub.push(c); continue; }
      if i >= start { started = true; sub.push(c); }
   }
   internal_err("slice_str: end > string end");
   sub
}


enum Lexeme {
   OpenParen, CloseParen, Str(String), Code(String), None
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
               ranges.push((start, i as u32));
               start_quote = None;
            }
            None if !ignore_next_quote => {
               start_quote = Some(i as u32);
            }
            _ => {}
         }
      }
      if c == '\\' { ignore_next_quote = true; }
      else { ignore_next_quote = false; }
   }

   if let Some(x) = start_quote { syntax_err("unterminated quote", x); }
}


fn separate_strings(code : &str) -> Vec<Lexeme> {
   let mut lexemes : Vec<Lexeme> = Vec::new();
   lexemes.push(Lexeme::OpenParen);
   lexemes.push(Lexeme::Str("tetst".to_string()));
   lexemes.push(Lexeme::CloseParen);
   lexemes.push(Lexeme::Code("hello".to_string()));
   lexemes.push(Lexeme::None);

   lexemes
}


fn main() {

   let code : &str = "(hello \"world\" \"another \\\"string\"";
   let z = separate_strings(code);
   for c in z {
      match c {
         _ => {}
         /*Lexeme::OpenParen => println!("open paren"),
         Lexeme::CloseParen => println!("close paren"),
         Lexeme::Str(s) => println!("string {}", s),
         Lexeme::Code(s) => println!("code {}", s),
         Lexeme::None => println!("None")*/
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
//   println!("{}", slice_str("hello", 1, 3));
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
