//#![feature(box_patterns)]

use std::cell::RefCell;
use gentypes::{SharedMut, to_shared_mut};
use types::QuoteType;
use errors::ErrInfo;

#[derive(Clone, Debug)]
pub enum Sexps {
   Str(String), Int(i64), Float(f64), Bool(bool),
   Sym(String), Lambda(String), //TODO: lambda
   Cons(SharedMut<(Sexps, Sexps)>), Array(RefCell<Vec<Sexps>>),
   Quote(QuoteType, Box<Sexps>), Nil, Err(Box<ErrInfo>) //TODO: possibly later on
}

//TODO: deadcode
#[allow(dead_code)]
impl Sexps {
   //TODO: quote_new_box? like err_new?
   pub fn quote_new(qtype : QuoteType, exp : Sexps) -> Sexps { Sexps::Quote(qtype, Box::new(exp)) }
   pub fn nil_new() -> Sexps { Sexps::Nil }
   pub fn err_new(ei : ErrInfo) -> Sexps { Sexps::Err(Box::new(ei)) }
   pub fn err_new_box(ei : Box<ErrInfo>) -> Sexps { Sexps::Err(ei) } //TODO: is one of this a bad practice?
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
         vr.borrow_mut()[loc] = new;
      }
   }
   pub fn arr_get(&self, loc : usize) -> Option<Sexps> {
      if let Sexps::Array(ref vr) = *self {
         Some(vr.borrow_mut()[loc].clone())
      } else { None }
   }
   pub fn arr_get_fast(&self, loc : usize) -> Sexps {
      if let Sexps::Array(ref vr) = *self {
         vr.borrow_mut()[loc].clone()
      } else { Sexps::Nil }
   }
   pub fn arr_len(&self) -> Option<usize> {
      if let Sexps::Array(ref a) = *self {
         Some(a.borrow().len())
      } else { None }
   }
   pub fn arr_len_fast(&self) -> usize {
      if let Sexps::Array(ref a) = *self {
         a.borrow_mut().len()
      } else { 0 }
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
   pub fn cons_get_1(&self) -> Option<Sexps> {
      if let Sexps::Cons(ref x) = *self {
         Some(x.borrow_mut().0.clone())
      } else { None }
   }
   pub fn cons_get_2(&self) -> Option<Sexps> {
      if let Sexps::Cons(ref x) = *self {
         Some(x.borrow_mut().1.clone())
      } else { None }
   }
   pub fn cons_get_1_fast(&self) -> Sexps {
      if let Sexps::Cons(ref x) = *self {
         x.borrow_mut().0.clone()
      } else { Sexps::Nil }
   }
   pub fn cons_get_2_fast(&self) -> Sexps {
      if let Sexps::Cons(ref x) = *self {
         x.borrow_mut().1.clone()
      } else { Sexps::Nil }
   }
   /* TODO:
      pub fn map() => should map stuff on every element or only for cons, etc?
   pub fn cons_map<O, F>(&self, f : F) -> Option<Sexps>
      where F : Fn(&Sexps) -> O
   {
      if let Sexps(Cons(ref x)) = *self {
         x.borrow_mut()
      }
   }*/

   pub fn is_quote(&self) {}
   pub fn is_nil(&self) {}
   pub fn is_err(&self) -> bool { if let Sexps::Err(_) = *self { true } else { false } }
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


