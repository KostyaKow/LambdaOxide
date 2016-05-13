//#![feature(box_patterns)]

use std::cell::RefCell;
use gentypes::{SharedMut, to_shared_mut};
use types::QuoteType;

#[derive(Clone)]
pub enum Sexps {
   Str(String), Int(i64), Float(f64), Bool(bool),
   Sym(String), Lambda(String),
   Cons(SharedMut<(Sexps, Sexps)>), Array(RefCell<Vec<Sexps>>),
   Quote(QuoteType, Sexps), Nil, Err //TODO: possibly later on
}

//TODO: deadcode
#[allow(dead_code)]
impl Sexps {
   fn quote_new(exp : Sexps) -> Sexps { Sexps::Quote(exp) }
   fn str_new(s : &str) -> Sexps { Sexps::Str(s.to_string()) }
   fn int_new(n : i64) -> Sexps { Sexps::Int(n)  }
   fn float_new(n : f64) -> Sexps { Sexps::Float(n) }
   fn bool_new(b : bool) -> Sexps { Sexps::Bool(b) }
   fn arr_new() -> Sexps {
      Sexps::Array(RefCell::new(Vec::new()))
   }
   fn arr_from_vec(arr : Vec<Sexps>) -> Sexps {
      Sexps::Array(RefCell::new(arr))
   }
   fn arr_push(&self, exp : Sexps) -> bool {
      if let Sexps::Array(ref v) = *self {
         v.borrow_mut().push(exp);
         true
      } else { false }
   }
   fn arr_push_fast(&self, exp : Sexps) {
      if let Sexps::Array(ref v) = *self {
         v.borrow_mut().push(exp);
      }
   }
   fn arr_set(&self, loc : usize, new : Sexps) -> bool {
      if let Sexps::Array(ref vr) = *self {
         let mut v = vr.borrow_mut();

         if v.len() <= loc { false }
         else {
            v[loc] = new;
            true
         }
      } else { false }
   }
   fn arr_set_fast(&self, loc : usize, new : Sexps) {
      if let Sexps::Array(ref vr) = *self {
         let v = vr.borrow_mut();
         vr.borrow_mut()[loc] = new;
      }
   }
   fn cons_new(a : Sexps, b : Sexps) -> Sexps {
      Sexps::Cons(to_shared_mut((a, b)))
   }
   fn cons_set_1(&self, new : Sexps) -> bool {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().0 = new;
         true
      } else { false }
   }
   fn cons_set_2(&self, new : Sexps) -> bool {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().1 = new;
         true
      } else { false }
   }
   fn cons_set_1_fast(&self, new : Sexps) {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().0 = new;
      }
   }
   fn cons_set_2_fast(&self, new : Sexps) {
      if let Sexps::Cons(ref sm) = *self {
         sm.borrow_mut().1 = new;
      }
   }

   fn is_str(&self) {}
   fn is_int(&self) {}
   fn is_float(&self) {}
   fn is_bool(&self) {}
   fn is_sym(&self) {}
   fn is_lambda(&self) {}
   fn is_cons(&self) {}
   fn is_arr(&self) {}
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


