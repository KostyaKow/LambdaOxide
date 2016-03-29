#![feature(box_syntax, box_patterns)]
#![allow(unused_variables, dead_code, unused_imports)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::boxed::Box;

extern crate list;
use list::{Cons, cons, cons_map, cons_reverse, car, cdr};
//use list::{cons_len, List};

extern crate utils;
use utils::{get_char_ranges, slice_str, print_space, print_nest, char_at, is_numeric};


fn err(s : &str) -> Sexps { Sexps::Err(s.to_string()) }
fn debug_p(lvl : u32, s: &str) { println!("{}", s); }
//end header

#[derive(Clone, Debug)]
enum Sexps {
   Str(String), Num(i64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), //Sub(Box<Vec<Sexps>>)
}

enum FunType {
   BuiltIn(Box<Fn(Box<Cons<Sexps>>) -> Sexps>),
   Lambda(Sexps)
}
struct Callable { env : SymTable, f : FunType, arg_names : Cons<String> }
impl Callable {
   fn new(arg_names : Cons<String>, f : FunType, parent_env : Box<& SymTable>) -> Callable {
      Callable { env: SymTable::new(Some(parent_env)), f: f, arg_names: arg_names }
   }
   fn exec(&self, args : Box<Cons<Sexps>>) -> Sexps {
      err("calling .exec of callable");
      match self.f {
         FunType::BuiltIn(ref f) => {
            f(args)
         },
         FunType::Lambda(ref s) => { err("user defined lamda") }
      }
   }
}

//symtable
struct SymTable {
   bindings : HashMap<String, Callable>,
   parent : Option<RefCell<SymTable>>
}

impl SymTable {
   fn new(parent : Option<RefCell<SymTable>>) -> SymTable {
      SymTable {
         bindings : HashMap::new(),
         parent   : parent,
      }
   }
   fn add_defaults(&mut self) {
      let sum_ = |args_ : Box<Cons<Sexps>> | -> Sexps  {
         let mut args = args_;
         let mut sum = 0;
         loop {
            match *args {
               Cons::Cons(Sexps::Num(n), y) => { sum += n; args = y; },
               Cons::Cons(_, _) => { err("bad arguments"); break },
               Cons::Nil   => break,
               _ => return err("bad arguments to sum")
            };
         }
         Sexps::Num(sum)
      };
      let difference_ = |args_ : Box<Cons<Sexps>> | -> Sexps {
         let mut args = args_;
         let mut diff = 0;
         let mut first = true;
         loop {
            match *args {
               Cons::Cons(Sexps::Num(n), y) => {
                  diff = if first { first = false; n } else { diff-n };
                  args = y;
               },
               Cons::Cons(_, _) => { err("bad argument"); break },
               Cons::Nil   => break,
               _ => return err("bad arguments to sum")
            };
         }
         Sexps::Num(diff)
      };

      let sum = Callable::new(Cons::Single("*".to_string()), //* = any arg
                              FunType::BuiltIn(Box::new(sum_)),
                              Box::new(self));

      let difference = Callable::new(Cons::Single("*".to_string()),
                                     FunType::BuiltIn(Box::new(difference_)),
                                     Box::new(self));
      self.add("+".to_string(), sum);
      self.add("-".to_string(), difference);
      //self.add("-".to_string(), difference)
   }
   fn add(&mut self, key : String, f : Callable) { self.bindings.insert(key, f); }
   fn lookup(&self, s : &String) -> Option<&Callable> {
      //if !self.bindings.contains_key(s)
      let entry_opt = self.bindings.get(s);
      if let Some(ref entry) = entry_opt { Some(entry.clone()) }
      else {
         if let Some(ref parent) = self.parent { parent.lookup(s) }
         else {
            err("Cannot find symbol in symbol table");
            None //err("None")
         }
      }
   }
}
//end symtable

fn apply_macro(name : &str, args : &Cons<Sexps>, env : &/*mut*/ SymTable) -> Sexps {
   match &name[..] {
      "define" => {
         //match args { Cons::Cons(name, Cons::Cons::(binding, Nil)) }
         //env.add(name.to_string(), );
         err("new define")
      }
      "lambda" => {

         //if let Cons::Cons(x, xs) = *args {}
         //Callable::new(args, env)
         err("new lambda")
      }
      _ => { err("Cannot find symbol in envrionment") }
   }
}

fn eval(exp : &Sexps, env : &mut SymTable) -> Sexps {
   match *exp {
      Sexps::Str(_) | Sexps::Num(_) => exp.clone(), //self evaluation
      Sexps::Sub(_)                 => apply(exp, env),
      Sexps::Err(ref s)             => Sexps::Err(s.clone()),
      Sexps::Var(ref s)             => {
         let lookup_res = env.lookup(&s.clone());
         match lookup_res {
            Some(v)     => v.exec(Box::new(Cons::Nil)),
            None        => err("Undefined variable lookup")
         }
      }
   }
}

fn apply(exp : &Sexps, env : &mut SymTable) -> Sexps {
   match *exp {
      Sexps::Sub(ref e @ box Cons::Cons(_, _)) => {
         err("Calling apply for function");
         let maybe_f = car(e); //get function name
         let maybe_args = cdr(e); //arguments
         if let Some(f) = maybe_f {
            if let Sexps::Var(ref s) = *f { //kk left here
               if let Some(f) = env.lookup(s) {
                  debug_p(2, "Not macro!");
                  if let Some(args) = maybe_args {
                     //kk left here
                     //f.exec(helper(args))
                     //(cons_map(args, |arg| eval(arg, env)))
                     //f.exec(Box::new(cons_map(&args.clone(), |arg| eval(arg, env))))
                     f.exec(Box::new(cons_map(&args.clone(), |arg| {
                        eval(arg, &mut SymTable::new(Some(Box::new(env))))
                     })))
                  }
                  else { f.exec(Box::new(Cons::Nil)) }
               }
               else { //if can't find symbol assume it's macro
                  debug_p(2, "Macro!");
                  if let Some(args) = maybe_args {
                     apply_macro(s, args, env)
                  } else { err("bad args") }
               }
            }
            else { err("function not var") }
         }
         else { err("bad args") }
      }
      Sexps::Sub(box Cons::Nil) => err("Empty subexpression"),
      _ => err("Bad apply call")
   }
}

fn main() {}
