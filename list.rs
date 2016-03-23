#![feature(box_syntax, box_patterns)]


pub enum Cons<T> {
   Cons(T, Box<Cons<T>>),
   Single(T),
   Nil
}
pub fn cons_nil<T>() -> Cons<T> { Cons::Nil }
pub fn cons_single<T>(x : T) -> Cons<T> { Cons::Single(x) }
pub fn cons<T>(x : T, xs : Cons<T>) -> Cons<T> {
   Cons::Cons(x, bb::<Cons<T>>(xs))
}
pub fn car<T>(cons : &Cons<T>) -> Option<&T> {
   match *cons {
      Cons::Cons(ref x, _) => Some(x),
      Cons::Nil | Cons::Single(_) => None
   }
}
pub fn cdr<T>(cons : &Cons<T>) -> Option<&Cons<T>> {
    match *cons {
      Cons::Cons(_, ref xs)         => Some(xs),
      Cons::Nil | Cons::Single(..)  => None
   }
}


/*
compared to other one, this is more cumbersome but
can have Cons::Pair(x, y) without Second element having to be list
pub enum Cons<T> {
   Pair(T, T),
   Cons(T, Box<Cons<T>>),
   Nil
}
pub fn cons_new<T>() -> Cons<T> { Cons::Nil }
pub fn cons_pair<T>(item1 : T, item2 : T) -> Cons<T> {
   Cons::Pair(item1, item2)
}
pub fn cons_lst<T>(item : T, cons : Cons<T>) -> Cons<T> {
   Cons::Cons(item, bb::<Cons<T>>(cons))
}
pub fn car<T>(cons : &Cons<T>) -> Option<&T> {
   match *cons {
      Cons::Pair(ref x, _) | Cons::Cons(ref x, _) => Some(x),
      Cons::Nil => None
   }
}
pub fn cdr_pair<T>(cons : &Cons<T>) -> Option<&T> {
    match *cons {
      Cons::Pair(_, ref xs)       => Some(xs),
      Cons::Nil | Cons::Cons(..)  => None
   }
}
pub fn cdr_lst<T>(cons : &Cons<T>) -> Option<&Cons<T>>
{
   match *cons {
      Cons::Cons(_, ref y) => Some(y),
      Cons::Pair(..) | Cons::Nil => None
   }

}
*/

//kkkkk


#[derive(Debug)]
pub enum List<T> {
   Cons(T, Box<List<T>>),
   Nil,
}

pub fn lst_new<T>() -> List<T> { List::Nil }
pub fn lst_new_0<T>() -> List<T> { List::Nil }
pub fn lst_new_1<T>(item : T) -> List<T> { lst_cons::<T>(item, List::Nil) }
pub fn bb<T>(x : T) -> Box<T> { Box::new(x) }

pub fn lst_cons<T>(item : T, lst : List<T>) -> List<T> {
   List::Cons(item, bb::<List<T>>(lst))
}
pub fn lst_car<T>(lst : &List<T>) -> Option<&T> {
   match *lst {
      List::Cons(ref x, _) => Some(x),
      List::Nil            => None
   }
}
pub fn lst_cdr<T>(lst : &List<T>) -> Option<&List<T>> {
    match *lst {
      List::Cons(_, ref xs) => Some(xs),
      List::Nil            => None
   }
}

//T : Copy + Clone
pub fn vec_to_lst<T : Clone + Copy>(vec : &Vec<T>) -> List<T> {
    let mut lst = List::Nil;
    let mut i = 0;
    while i < vec.len() {
        lst = List::Cons(vec[i], bb::<List<T>>(lst));
        i += 1;
    }
    lst
}

pub fn lst_len<T>(lst : &List<T>) -> u32 {
    match lst {
        &List::Nil => 0,
        &List::Cons(_, box ref xs) => (1 + lst_len::<T>(xs))
    }
}

/*example usage:
fn main() {
   let x : List<u32> = List::Cons(5, bb(List::Cons(3, bb(List::Nil))));
   println!("{}", list_len::<u32>(&x));

   let vec : Vec<i32> = vec![1, 2, 3];
   let lst = vec_to_lst::<i32>(&vec);
}
fn list_len<T>(lst: List<T>) -> i32 {
   match lst {
      List::Nil => 0,
      List::Cons(_, box xs) => (1 + list_len::<T>(xs))
   }
}
println!("{}", list_len::<u32>(x));*/


