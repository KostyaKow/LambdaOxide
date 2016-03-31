#![feature(box_syntax, box_patterns)]

use std::boxed::Box;

extern crate err; use err::DEBUG;
extern crate list; use list::{Cons, cons_map};
extern crate utils; use utils::{print_space, print_nest};


#[derive(Debug)]
pub enum Lexeme { OpenParen, CloseParen, Str(String), Sym(String), Num(i64) }

pub type EnvId = usize;

#[derive(Clone, Debug)] //Try to implement copy
pub enum Sexps {
   Str(String), Num(i64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), Lambda(EnvId, String) //Quote(Box<Cons<Sexps>>) //Sub(Box<Vec<Sexps>>)
}

impl Drop for Sexps {
   fn drop(&mut self) {
      match *self {
         Sexps::Err(ref s) if DEBUG >= 5 => println!("err dropping: {}", s),
         _ if DEBUG >= 7 => println!("sexps going out of scope: {:?}", self),
         _ => {}
      }
   }
}

//or String::from(s)
pub fn err(s : &str) -> Sexps { Sexps::Err(s.to_string()) }

pub fn display_sexps(exp: &Sexps) {
   match *exp {
      Sexps::Str(ref s) => println!("{}", s),
      Sexps::Num(ref n) => println!("{}", n),
      Sexps::Var(ref s) => println!("{}", s),
      Sexps::Err(ref s) => println!("{}", s),
      Sexps::Lambda(..) => println!("<lambda>"),
      Sexps::Sub(..)    => print_compact_tree(exp),
      /*_                 => println!("bad sexps, cant print")*/
   }
}
fn print_compact_tree_helper(t: &Sexps) {
   match *t {
      Sexps::Sub(box ref sub) => { //box ref sexps
         print!("(");
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_compact_tree_helper(x));
         print!(")");
      },
      _ => { print!("{:?} ", t) }
   }
}
pub fn print_compact_tree(t: &Sexps) {
   print_compact_tree_helper(t);
   println!("");
}
pub fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sexps::Sub(box ref sub) => { //box ref sexps
         print_nest("(", deepness, None);
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_tree(x, deepness+4));
         print_nest(")", deepness, None);
      },
      _ => { print_space(deepness); println!("{:?}", t) }
   }
}
pub fn cons_to_sexps(c : Cons<Sexps>) -> Sexps {
   Sexps::Sub(Box::new(c))
}

