//#![feature(slice_patterns)]
//replaced with custom char_at #![feature(str_char)]

use std::boxed::Box;

fn syntax_err(s: &str, location: u32) {
   println!("error at charachter {}: {}", location, s);
}
fn internal_err(s: &str) {
   println!("internal error: {}", s);
}

fn char_at(code : &str, n : usize) -> Option<char> {
    for (i, c) in code.chars().enumerate() {
        if i == n { return Some(c) }
    }
    return None
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

   let mut sym_collector = String::new();
   let mut collect : bool; //collect symbols

   //range of strings
   let ranges = get_char_ranges(code);
   let mut str_start : bool;
   let mut r_it = 0; //current string range

   let mut i = 0;
   let code_len = code.len();

   while i < code_len {
      collect = has_range = false;
      str_start = r_it < ranges.len() && ranges[r_it].0 == i;

      //collect if string or special character
      if let Some(c) = char_at(code, i) {
         collect = str_start || c == '(' || c == ')' || c == ' ';

         if collect && !sym_collector.is_empty() {
            lexemes.push(Lexeme::Sym(sym_collector));
            sym_collector = String::new();
         }
      }
      if r_it < ranges.len() {
         let (start, end) = ranges[r_it];
         if start == i {
            r_it += 1;
            i = end+1;
            let l = Lexeme::Str(slice_str(code, start, end));
            lexemes.push(l);
         }
      }
      if let Some(c) = char_at(code, i) {
         match c {
            '(' => lexemes.push(Lexeme::OpenParen),
            ')' => lexemes.push(Lexeme::CloseParen),
            ' ' => {},
            _   => sym_collector.push(c)
         }
      }
      i += 1;
   }

   lexemes
}

enum Lexeme {
   OpenParen, CloseParen, Str(String), Sym(String)
}

enum Sexps<'a> {
   Str(&'a String),
   Num(f64),
   Literal(String),
   SubSexps(Vec<Box<Sexps<'a>>>)
}

fn parse(lexemes : &Vec<Lexeme>) -> Sexps {

   /*let sexps = Sexps::SubSexps(
   for l in lexemes {

   }*/
   Sexps::Num(34.2)
}


fn main() {
   lex_test();
}

fn parse_test() {

   let code : &str = "(hello (+ world) \"string\")";
   let z = lex(code);
   let x = parse(&z);
}

fn lex_test() {
   let code : &str = "(hello (\"world\" + world) \"another \\\"string\")";
   let z = lex(code);
   for c in z {
      match c {
         /*_ => {} empty match */
         Lexeme::OpenParen => println!("open paren"),
         Lexeme::CloseParen => println!("close paren"),
         Lexeme::Str(s) => println!("string {}", s),
         Lexeme::Sym(s) => println!("code {}", s),
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
