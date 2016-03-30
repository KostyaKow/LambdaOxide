use std::boxed::Box;

extern crate err;
use err::DEBUG;

extern crate list;
use list::{Cons};


#[derive(Debug)]
pub enum Lexeme { OpenParen, CloseParen, Str(String), Sym(String), Num(i64) }

#[derive(Clone, Debug)]
pub enum Sexps {
   Str(String), Num(i64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), //Sub(Box<Vec<Sexps>>)
}

impl Drop for Sexps {
   fn drop(&mut self) {
      match *self {
         Sexps::Err(ref s) if DEBUG >= 3 => println!("err dropping: {}", s),
         _ if DEBUG >= 7 => println!("sexps going out of scope: {:?}", self),
         _ => {}
      }
   }
}

//or String::from(s)
pub fn err(s : &str) -> Sexps { Sexps::Err(s.to_string()) }
