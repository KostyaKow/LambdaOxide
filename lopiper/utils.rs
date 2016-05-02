#![allow(dead_code)]

use types::{Root, Sexps, err, RunFail};
use main::run;
pub fn load_internal(code : &String, root : Root) -> Sexps {
   let lines = code.split("\n").collect::<Vec<&str>>();

   let mut x : Sexps = err("empty file");

   let mut line_acc = String::new();
   let mut i = 0;

   for line in lines.iter() {
      i+=1;
      if line.len() < 1 { continue; }
      if char_at(line, 0).unwrap() == ';' { continue; }
      line_acc = line_acc + line;
      let out = run(&root, &line_acc);
      if let Result::Err((RunFail::UncompleteExp, _)) = out {
         continue;
      }
      else if let Result::Err((x, err_n)) = out {
         let ret = format!("cannot load code line {} ({}): {:?}",
                           i, err_n, x);
         return err(&*ret);
      }
      else {
         line_acc = String::new();
         x = out.unwrap();
      }
   }
   x
}

pub fn syntax_err(s : &str, n : u32) {
   println!("error at {}: {}", n, s);
}

pub fn contains<T : PartialEq>(item : T, vec : Vec<T>) -> bool {
   for x in vec {
      if x == item { return true; }
   }
   false
}
pub fn vec_eq<T: PartialEq>(v1 : &Vec<T>, v2 : &Vec<T>) -> bool {
   if v1.len() != v2.len() { return false; }
   for (x, y) in v1.iter().zip(v2.iter()) {
      if *x != *y { return false; }
   }
   true
}

pub fn is_int(s : &str) -> bool {
   let mut i = 0;
   while i < s.len() {
      let c = char_at(s, i).unwrap();
      if !c.is_digit(10) && !(i == 0 && c == '-' && s.len() > 1) { return false; }
      i += 1;
   }
   true
}
pub fn is_float(s : &str) -> bool {
   let mut i = 0;
   let mut have_dot = false;
   while i < s.len() {
      let c = char_at(s, i).unwrap();
      let first_neg = i == 0 && c == '-';

      if !c.is_digit(10) && !(first_neg && s.len() > 1) {
         if c == '.' && have_dot == false { have_dot = true; }
         else { return false; }
      }
      i += 1;
   }
   //if s == '.' then it's not number
   if have_dot == true && s.len() == 1 { false }
   else { true }
}
pub fn to_float(s : &str) -> f64 {
   s.parse::<f64>().unwrap()
}
pub fn to_int(s : &str) -> i64 {
   s.parse::<i64>().unwrap()
}

pub fn get_char_ranges(code : &str) -> Vec<(usize, usize)> {
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
//replace with build-in
//slice_str("hello", 1, 3) => "ell"
pub fn slice_str(s: &str, start: usize, end: usize) -> String {
   /*let mut sub: String = String::new();
   let mut started: bool = false;

   if start >= end { internal_err("slice_str: start>=end"); }
   if end >= s.len() {  internal_err("slice_str: end >= string end"); }

   for (i, c) in s.chars().enumerate() {
      if i >= end+1 { return &sub; }
      if started { sub.push(c); continue; }
      if i >= start { started = true; sub.push(c); }
   }
   &sub*/
   (&s[start..end+1]).to_string()
}

pub fn print_space(n: u8) {
   let mut i = 0;
   while i < n { print!(" "); i += 1; }
}
pub fn print_nest(s: &str, n: u8, extra: Option<&str>) {
   print_space(n);
   if let Some(ex) = extra { println!("{} {}", ex, s) }
   else { println!("{}", s) }
}
pub fn char_at(code : &str, n : usize) -> Option<char> {
    for (i, c) in code.chars().enumerate() {
        if i == n { return Some(c) }
    }
    return None
}
