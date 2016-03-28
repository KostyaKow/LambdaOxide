#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
use std::collections::HashMap;
use std::boxed::Box;

extern crate list;
use list::{Cons, cons, cons_reverse, car, cdr};

extern crate utils;
use utils::{print_space, print_nest, char_at, is_numeric};
fn err(s : &str) -> Sexps { Sexps::Err(s.to_string()) }

type FunArgNames = Option<Vec<String>>;
//type FunArgs = Box<Cons<Sexps>>;  //none = nil
type FunArgs = Sexps;
//type EnvId = Option<u32>;
type EnvId = u32;

#[derive(Clone, Debug)]
enum Sexps {
   Str(String), Num(i64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), Func(Callable) //Sub(Box<Vec<Sexps>>)
}

enum Callable {
   BuiltIn(Box<Fn(Sexps) -> Sexps>),
   Lambda(FunArgNames, Sexps, EnvId)
}
impl Callable {
   fn exec(&self, args : Sexps) -> Sexps {
      err("calling .exec of callable");

      match self {
         FunType::BuiltIn(ref f) => { f(args) },
         FunType::Lambda(ref arg_names_opt, ref exp, ref env_parent) => {
            err("user defined lamda");
            let mut env = SymTable::new(env_parent);
            if let Some(arg_names) = arg_names_opt {
               let mut i = 0;
               for arg in args { env.add(arg_names[i], arg); i+=1; }
            }
            else { env.add("*", args); }
            eval(exp, env)
         }
      }
   }
}

//RootEnv
struct RootEnv {
   bindings : HashMap<EnvId, SymTable>,
   env_ctr  : EnvId //counts number of environments
}
impl RootEnv {
   fn new() -> RootEnv {
      RootEnv { bindings : HashMap::new(), env_ctr : 0 }
   }
   fn add(&mut self, env : SymTable) {
      self.bindings.insert(self.env_ctr, env);
      self.env_ctr += 1;
   }
   fn get(&self, id : EnvId) -> Option<&SymTable> {
      self.bindings.get(id)
   }
   fn get_next_id(&self) { self.env_ctr }
}
//end RootEnv

//type TableEntry = Sexps;
//enum TableEntry { Func(Callable), Expr(Sexps), Env(Box<SymTable>) }
//enum TableEntry { Expr(Box<Sexps>), Env(EnvId) }

struct SymTable {
   bindings : HashMap<String, Sexps>,
   //envs     : Vec<EnvId>, //for children
   id       : EnvId,
   parent   : Option<EnvId>,
   root     : Box<RootEnv>
}

impl SymTable {
   fn new(root : &mut RootEnv, parent : Option<EnvId>) -> SymTable {

      let ret = SymTable {
         bindings : HashMap::new(),
         id : root.get_next_id()
         parent : parent,
         root : root
      };
      root.add(ret);
      //add to parent our id
      ret
   }

   fn add_table(&mut self, key : String, val : EnvId) {
      self.add(key, TableEntry::Env(val));
   }
   fn add_sexps(&mut self, key : String, val : Box<Sexps>) {
      self.add(key, TableEntry::Expr(val));
   }
   fn add(&mut self, key : String, val : TableEntry) {
      self.bindings.insert(key, val);
   }
   fn lookup(&self, s : &String) -> Option<TableEntry> {
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
                              FunType::BuiltIn(Box::new(sum_))/*,
                              Box::new(self)*/);

      let difference = Callable::new(Cons::Single("*".to_string()),
                                     FunType::BuiltIn(Box::new(difference_))/*,
                                     Box::new(self)*/);
      self.add("+".to_string(), sum);
      self.add("-".to_string(), difference);
      //self.add("-".to_string(), difference)
   }
}

fn main() {
   let x : RootEnv = RootEnv::new();
}
