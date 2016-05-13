//#![feature(box_patterns)]

use std::cell::RefCell;
use gentypes::{SharedMut, to_shared_mut};
use types::QuoteType;
use errors::ErrInfo;

#[derive(Clone)]
pub enum Sexps {
   Str(String), Int(i64), Float(f64), Bool(bool),
   Sym(String), Lambda(String),
   Cons(SharedMut<(Sexps, Sexps)>), Array(RefCell<Vec<Sexps>>),
   Quote(QuoteType, Sexps), Nil, Err(Box<ErrInfo>) //TODO: possibly later on
}

//TODO: deadcode
#[allow(dead_code)]
impl Sexps {
   pub fn quote_new(qtype : QuoteType, exp : Sexps) -> Sexps { Sexps::Quote(qtype, exp) }
   pub fn nil_new() -> Sexps { Sexps::Nil }
   pub fn err_new(ei : ErrInfo) -> Sexps { Sexps::Err(Box::new(ei)) }
   pub fn str_new(s : &str) -> Sexps { Sexps::Str(s.to_string()) }
   pub fn int_new(n : i64) -> Sexps { Sexps::Int(n)  }
   pub fn float_new(n : f64) -> Sexps { Sexps::Float(n) }
   pub fn bool_new(b : bool) -> Sexps { Sexps::Bool(b) }

   pub fn arr_new() -> Sexps {
      Sexps::Array(RefCell::new(Vec::new()))
   }
   pub fn arr_new_singleton(exp : Sexps) -> Sexps {
      Sexps::Array(RefCell::new(vec![exp]))
   }
   pub fn arr_new_from_vec(arr : Vec<Sexps>) -> Sexps {
      Sexps::Array(RefCell::new(arr))
   }
   pub fn arr_push(&self, exp : Sexps) -> bool {
      if let Sexps::Array(ref v) = *self {
         v.borrow_mut().push(exp);
         true
      } else { false }
   }
   pub fn arr_push_fast(&self, exp : Sexps) {
      if let Sexps::Array(ref v) = *self {
         v.borrow_mut().push(exp);
      }
   }
   pub fn arr_set(&self, loc : usize, new : Sexps) -> bool {
      if let Sexps::Array(ref vr) = *self {
         let mut v = vr.borrow_mut();

         if v.len() <= loc { false }
         else {
            v[loc] = new;
            true
         }
      } else { false }
   }
   pub fn arr_set_fast(&self, loc : usize, new : Sexps) {
      if let Sexps::Array(ref vr) = *self {
         let v = vr.borrow_mut();
         vr.borrow_mut()[loc] = new;
      }
   }
   pub fn cons_new(a : Sexps, b : Sexps) -> Sexps {
      Sexps::Cons(to_shared_mut((a, b)))
   }
   pub fn cons_set_1(&self, new : Sexps) -> bool {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().0 = new;
         true
      } else { false }
   }
   pub fn cons_set_2(&self, new : Sexps) -> bool {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().1 = new;
         true
      } else { false }
   }
   pub fn cons_set_1_fast(&self, new : Sexps) {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().0 = new;
      }
   }
   pub fn cons_set_2_fast(&self, new : Sexps) {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().1 = new;
      }
   }

   pub fn is_quote(&self) {}
   pub fn is_nil(&self) {}
   pub fn is_err(&self) -> bool { if let Sexps::Err(_) = self { true } else { false } }
   pub fn is_str(&self) {}
   pub fn is_int(&self) {}
   pub fn is_float(&self) {}
   pub fn is_bool(&self) {}
   pub fn is_sym(&self) {}
   pub fn is_lambda(&self) {}
   pub fn is_cons(&self) {}
   pub fn is_arr(&self) {}
}

fn main() {
   let n1 = Sexps::Str("test".to_string());
   let n2 = Sexps::Int(5);

   let mut v = Vec::new();
   v.push(n1.clone());
   v.push(n2);
   let c = Sexps::Array(RefCell::new(v));

   //modify(&mut c, Sexps::Int(10));

   //let s = Sexps::Cons((to_shared_mut(n1), to_shared_mut(c));
   //modify(&mut s, Sexps::Int(10));

}


