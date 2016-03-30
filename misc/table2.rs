#![allow(dead_code)]
//#![allow(unused_variables)]
//#![allow(unused_imports)]
use std::collections::HashMap;
use std::boxed::Box;

//extern crate list; use list::{Cons, cons, cons_reverse, car, cdr};
//extern crate utils; use utils::{print_space, print_nest, char_at, is_numeric};

extern crate types; use types::{Sexps, err};
//extern crate e

type FunArgNames = Option<Vec<String>>;
//type FunArgs = Box<Cons<Sexps>>;  //none = nil
type FunArgs = Sexps;
//type EnvId = Option<u32>; type EnvId = u32;
type EnvId = usize;

enum Callable {
   BuiltIn(EnvId, Box<Fn(Sexps, &mut Env, EnvId) -> Sexps>),
   Lambda(EnvId, FunArgNames, Sexps)
}
impl Callable {
   fn exec(&self, args : Sexps, root : &mut Env) -> Sexps {
      err("calling .exec of callable");

      match *self {
         Callable::BuiltIn(parent, ref f) => { f(args, root, parent) },
         Callable::Lambda(ref parent, ref arg_names_opt, ref exp) => {
            let new_table = root.table_new(parent.clone());

            root.table_add(new_table, "*", sym_table_val(args));
            //let mut env = SymTable::new(root, env_parent.clone());
            /*if let Some(ref arg_names) = *arg_names_opt {
               //let mut i = 0;
               //for arg in args { env.add(arg_names[i], arg); i+=1; }
            } else { kkleft: env.add("*", args); } */
            eval(exp, root, parent.clone())
         }
      }
   }
}

fn sym_table_val(exp : Sexps) -> Callable {
   let root = Env { tables: Vec::new() };
   let ret : Box<Fn(Sexps, &mut Env, EnvId) -> Sexps> = Box::new(move |exp, root, env| -> Sexps { exp });
   Callable::BuiltIn(0, ret)
}

fn eval(s : &Sexps, root : &mut Env, id : EnvId) -> Sexps {
   err("Sup")
}

struct Table { bindings: HashMap<String, Callable>, parent: EnvId }
struct Env { tables : Vec<Table>, }

impl Env {
   fn new() -> Env {
      let mut env = Env { tables : Vec::new() };
      env.table_new(0);
      env
   }
   fn table_new(&mut self, parent : EnvId) -> EnvId {
      self.tables.push(Table { bindings : HashMap::new(), parent: parent });
      self.tables.len()-1
   }
   fn table_add(&mut self, table_id : EnvId, key : &str, entry : Callable) -> bool {
      let mut i = 0;
      for table in self.tables.iter_mut() {
         if i == table_id {
            table.bindings.insert(key.to_string(), entry);
            return true;
         }
         i += 1;
      }
      false
   }
   fn lookup(&self, table_id : EnvId, key : &str) -> Option<&Callable> {
      match self.tables.get(table_id) {
         Some(table) => {
            let entry_opt = table.bindings.get(key);
            match entry_opt {
               Some(val) => return Some(val),
               None if table_id == 0 => return None,
               None => self.lookup(table.parent, key)
            }
         }
         None => return None
      }
   }

   fn add_defaults(&mut self) { }
}

fn main() {
   let x : Env = Env::new();
}
