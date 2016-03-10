/*use std::env;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;*/

fn lex(code : &str) -> Vec<String> {
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
}

fn main() {
   /*for arg in env::args() {
      println!("{}", arg);
   }*/

   let y : &str = "(hello world)";

   let z = lex(y);

   for c in z {
      println!("{}", c);
   }
   /*let a :[i32; 3] = [1, 2, 3];
   println!("{}", a[0]);*/

}
