/*#![allow(dead_code)]

use exp::QuoteType;
*/
pub fn contains<T : PartialEq>(item : T, vec : Vec<T>) -> bool {
   for x in vec {
      if x == item { return true; }
   }
   false
}
/*pub fn vec_eq<T: PartialEq>(v1 : &Vec<T>, v2 : &Vec<T>) -> bool {
   if v1.len() != v2.len() { return false; }
   for (x, y) in v1.iter().zip(v2.iter()) {
      if *x != *y { return false; }
   }
   true
}*/


//TODO: check the is_int, is_float, to_float, to_int
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

//slice_str("hello", 1, 3) => "ell"
pub fn slice_str(s: &str, start: usize, end: usize) -> String {
   (&s[start..end+1]).to_string()
}
/*
pub fn print_space(n: u8) {
   let mut i = 0;
   while i < n { print!(" "); i += 1; }
}
pub fn print_nest(s: &str, n: u8, extra: Option<&str>) {
   print_space(n);
   if let Some(ex) = extra { println!("{} {}", ex, s) }
   else { println!("{}", s) }
}*/
pub fn char_at(str : &str, n : usize) -> Option<char> {
   for (i, c) in str.chars().enumerate() {
      if i == n { return Some(c) }
   }
   return None
}
pub fn char_at_fast(str : &str, n : usize) -> char {
   for (i, c) in str.chars().enumerate() {
      if i == n { return c; }
   }
   '\0'
}

pub fn is_none<T>(x : Option<T>) -> bool {
   if let Some(_) = x { false } else { true }
}


//TODO: haskell-ish range with step and negatives and stuff

