
extern crate utils;


//list
#[derive(Debug)]
enum List<T> {
   Cons(T, Box<List<T>>),
   Nil,
}
fn lst_cons<T>(item : T, lst : List<T>) -> List<T> { List::Cons(item, bb::<List<T>>(lst)) }
fn lst_new<T>() -> List<T> { List::Nil }
fn lst_new_0<T>() -> List<T> { List::Nil }
fn lst_new_1<T>(item : T) -> List<T> { lst_cons::<T>(item, List::Nil) }

//T : Copy + Clone
fn vec_to_lst<T : Clone + Copy>(vec : &Vec<T>) -> List<T> {
    let mut lst = List::Nil;
    let mut i = 0;
    while i < vec.len() {
        lst = List::Cons(vec[i], bb::<List<T>>(lst));
        i += 1;
    }
    lst
}

fn lst_len<T>(lst : &List<T>) -> u32 {
    match lst {
        &List::Nil => 0,
        &List::Cons(_, box ref xs) => (1 + lst_len::<T>(xs))
    }
}

fn bb<T>(x : T) -> Box<T> { Box::new(x) }

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


