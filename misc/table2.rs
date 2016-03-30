#![allow(dead_code, unused_variables)]

//#![allow(unused_variables)]
//#![allow(unused_imports)]
use std::collections::HashMap;
use std::boxed::Box;

//extern crate list; use list::{Cons, cons, cons_reverse, car, cdr};
//extern crate utils; use utils::{print_space, print_nest, char_at, is_numeric};

extern crate types; use types::{Sexps, err, display_sexps};

//tmp TODO kkleft: move main interpreter to separate file
//and only use this one for tables
extern crate lexer; use lexer::lex;
extern crate parser; use parser::parse;
extern crate list; use list::{Cons};
//end tmp

type FunArgNames = Option<Vec<String>>;
//type FunArgs = Box<Cons<Sexps>>;  //none = nil
type FunArgs = Sexps;
//type EnvId = Option<u32>; type EnvId = u32;
type EnvId = usize;


//callable
enum Callable {
   BuiltIn(EnvId, Box<Fn(Sexps, &mut Env, EnvId) -> Sexps>), //args, root, our env
   Lambda(EnvId, FunArgNames, Sexps)
}
impl Callable {
   fn exec(&self, args : Sexps, root : &mut Env) -> Sexps {
      err("calling .exec of callable");

      match *self {
         Callable::BuiltIn(parent, ref f) => { f(args, root, parent) },
         Callable::Lambda(ref parent, ref arg_names_opt, ref exp) => {
            let new_table = root.table_new(parent.clone());

            root.table_add(new_table, "~", make_sym_table_val(args));
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

fn make_sym_table_val(exp : Sexps) -> Callable {
   //let root = Env::new();
   let ret : Box<Fn(Sexps, &mut Env, EnvId) -> Sexps> = Box::new(move |args, root, env| -> Sexps {
      exp.clone()
   });
   Callable::BuiltIn(0, ret)
}
fn get_sym_table_val(v : Option<&Callable>) -> Sexps {
   match v {
      None => err("Not found"),
      Some(f) => f.exec(err(""), &mut Env::new())
   }
}
//callable

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
   fn add_defaults(&mut self) {
      //let
   }
}

fn eval(exp : &Sexps, root : &mut Env, table : EnvId) -> Sexps {
   match *exp {
      Sexps::Str(_) | Sexps::Num(_) => exp.clone(), //self evaluation
      Sexps::Sub(_)                 => apply(exp, root, table),
      Sexps::Err(ref s)             => Sexps::Err(s.clone()),
      Sexps::Var(ref s)             => {
         let lookup_opt = root.lookup(table, &s.clone());
         get_sym_table_val(lookup_opt)
      }
   }
}

fn apply_macro(name : &str, args : &Cons<Sexps>, env : &mut Env) -> Sexps {
   err("hi")
   /*match &name[..] {
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
   }*/
}

fn apply(exp : &Sexps, env : &mut Env, table : EnvId) -> Sexps {
   err("yo")
   /*match *exp {
      Sexps::Sub(ref e @ box Cons::Cons(_, _)) => {
         err("Calling apply for function");
         let maybe_f = car(e); //get function name
         let maybe_args = cdr(e); //arguments
         if let Some(f) = maybe_f {
            if let Sexps::Var(ref s) = *f { //kk left here
               if let Some(f) = env.lookup(s) {
                  debug_p(2, "Not macro!");
                  if let Some(args) = maybe_args {
                     //kk left here kkleft
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
   }*/
}

fn run(code : &str) -> Sexps {
   let lexemes = lex(code);
   let exp_opt = parse(&lexemes);

   let mut env = Env::new();
   env.add_defaults();

   if let Some(exp) = exp_opt {
      eval(&exp, &mut env, 0)
   }
   else { err("Couldn't parse code") }
}

fn interpreter() {
   use std::io::{self, BufRead};
   let stdin = io::stdin();
   loop {
      let line = stdin.lock().lines().next().unwrap().unwrap();
      let out = run(&line);
      display_sexps(&out)
   }
}


fn main() {}

fn table_test() {
   let mut x : Env = Env::new();
   let child = x.table_new(0);
   let child2 = x.table_new(child);

   x.table_add(child, "hello", make_sym_table_val(err("test")));
   x.table_add(child2, "test", make_sym_table_val(err("yo")));
   x.table_add(child, "hello3", make_sym_table_val(err("yo2")));

   display_sexps(&get_sym_table_val(x.lookup(child2, "hello")));

}


