#![feature(box_patterns)]

/*pub enum Sexps {
   Str(String), Int(i64),
   Cons(Box<(Sexps, Sexps)>)
}

fn mod_pair(exp : &mut Sexps, a_ : Sexps, b_ : Sexps) {
   let Sexps::Cons(box (ref mut a, ref mut b)) = exp;

   *a = a_;
   *b = b_;
}

fn main() {
   let n1 = Sexps::Str("test".to_string());
   let n2 = Sexps::Int(5);

   let mut s = Sexps::Cons(Box::new((n1, n2)));
}*/

/*
//use std::cell::RefCell;
use std::cell::Cell;

#[derive(Copy)]
pub enum Sexps {
   Str(String), Int(i64),
   Cons(Box<Cell<(Sexps, Sexps)>>)
}

fn main() {
   let n1 = Sexps::Str("test".to_string());
   let n2 = Sexps::Int(5);

   let mut s = Sexps::Cons(Box::new(Cell::new((n1, n2))));
}*/

pub enum Sexps {
   Str(String), Int(i64),
   Cons(Vec<Sexps>)
}

fn modify(exp: &mut Sexps, a : Sexps) {
   if let Sexps::Cons(ref mut v) = *exp {
      v[0] = a;
   }
}

fn main() {
   let n1 = Sexps::Str("test".to_string());
   let n2 = Sexps::Int(5);

   let mut c = Vec::new();
   c.push(n1);
   c.push(n2);

   let mut s = Sexps::Cons(c);
   modify(&mut s, Sexps::Int(10));
}
