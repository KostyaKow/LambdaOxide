#![allow(dead_code, unused_variables)]
#![feature(box_syntax, box_patterns)]
#![feature(as_unsafe_cell)]

//#![allow(unused_variables)]
//#![allow(unused_imports)]
use std::collections::HashMap;
use std::boxed::Box;

use std::rc::Rc;
use std::cell::RefCell;

//extern crate list; use list::{Cons, cons, cons_reverse, car, cdr};
//extern crate utils; use utils::{print_space, print_nest, char_at, is_numeric};

extern crate types; use types::{Sexps, err, display_sexps, print_tree};

//tmp TODO kkleft: move main interpreter to separate file
//and only use this one for tables
extern crate err; use err::debug_p;
extern crate lexer; use lexer::lex;
extern crate parser; use parser::parse;
extern crate list; use list::{Cons, car, cdr, cons_map};
//end tmp

type FunArgNames = Option<Vec<String>>;
//type FunArgs = Box<Cons<Sexps>>;  //none = nil
type FunArgs = Sexps;
//type EnvId = Option<u32>; type EnvId = u32;
type EnvId = usize;

type Root<'a> = &'a RefCell<Env>;

//callable
enum Callable {
   BuiltIn(EnvId, Box<Fn(Sexps, Root, EnvId) -> Sexps>), //args, root, our env
   Lambda(EnvId, FunArgNames, Sexps)
}
impl Callable {
   fn exec(&self, args : Sexps, root : Root) -> Sexps {
      err("calling .exec of callable");

      match *self {
         Callable::BuiltIn(parent, ref f) => { f(args, root, parent) },
         Callable::Lambda(ref parent, ref arg_names_opt, ref exp) => {
            let new_table = root.borrow_mut().table_new(parent.clone());

            root.borrow_mut().table_add(new_table, "~", make_sym_table_val(args));
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
   let ret : Box<Fn(Sexps, Root, EnvId) -> Sexps> = Box::new(move |args, root, env| -> Sexps {
      exp.clone()
   });
   Callable::BuiltIn(0, ret)
}
fn get_sym_table_val(v : Option<&Callable>) -> Sexps {
   match v {
      None => err("Not found"),
      Some(f) => f.exec(err(""), &(RefCell::new(Env::new())))
   }
}
//end callable

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
      let sum_ = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let mut sum = 0;
         if let Sexps::Sub(box ref args_) = args_sexps {
            let mut args : &Cons<Sexps> = args_; //Box::new(args_);

            loop {
               match *args {
                  Cons::Cons(Sexps::Num(n), ref ns) => { sum += n; args = ns; },
                  Cons::Cons(_, _) => { err("bad arguments"); break },
                  Cons::Nil   => break,
                  _ => return err("bad arguments to sum")
               };
            }
            Sexps::Num(sum)
         }
         else { err("bad arguments") }
      };

      let  sum = Callable::BuiltIn(0, Box::new(sum_));
      self.table_add(0, "+", sum);

      let diff_ = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let mut diff = 0;
         let mut first = true;

         if let Sexps::Sub(box ref args_) = args_sexps {
            let mut args : &Cons<Sexps> = args_; //Box::new(args_);

            loop {
               match *args {
                  Cons::Cons(Sexps::Num(n), ref ns) => {
                     if first { diff = n; first = false; } else { diff -= n; }
                     args = ns;
                  },
                  Cons::Cons(_, _) => { err("bad arguments"); break },
                  Cons::Nil   => break,
                  _ => return err("bad arguments to sum")
               };
            }
            Sexps::Num(diff)
         }
         else { err("bad arguments") }
      };

      let diff = Callable::BuiltIn(0, Box::new(diff_));
      self.table_add(0, "-", diff);

   }
}

fn eval(exp : &Sexps, root : Root, table : EnvId) -> Sexps {
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(err("test")));
   //print_tree(&get_sym_table_val(root.borrow_mut().lookup(0, "hello")), 0);
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(Sexps::Num(5)));

   match *exp {
      Sexps::Str(_) | Sexps::Num(_) => exp.clone(), //self evaluation
      Sexps::Sub(_)                 => apply(exp, root, table),
      Sexps::Err(ref s)             => Sexps::Err(s.clone()),
      Sexps::Var(ref s)             => {
         let borrowed = root.borrow();
         let lookup_opt = borrowed.lookup(table, &s.clone());
         //comment start
         let x = get_sym_table_val(lookup_opt);
         println!("eval variable {}", s);
         print_tree(&x, 0);
         x
         //comment end
         //get_sym_table_val(lookup_opt)
      }
   }
}

fn apply_macro(name : &str, args : &Cons<Sexps>, root : Root, t : EnvId) -> Sexps {
   match &name[..] {
      "define" => {
         //match args { Cons::Cons(name, Cons::Cons::(binding, Nil)) }
         //env.add(name.to_string(), );
         err("new define");
         if let Cons::Cons(Sexps::Var(ref name), ref binding) = *args {
            //let eval_result = eval(&Sexps::Sub(binding.clone()), root, t);
            //root.borrow_mut().table_add(t, &*name, make_sym_table_val(eval_result));

            //if let Some(x) = cdr(binding) { if let Some(y) = car(x) { print_tree(y, 0); } }
            //print_tree(&Sexps::Sub(binding.clone()), 0);
            let eval_result = if let Some(x) = car(binding) {
               println!("defining: {}", &*name);
               print_tree(x, 2);
               make_sym_table_val(eval(x, /*&root.clone()*/root, t))
            }
            else { make_sym_table_val(err("bad define")) };

            //print_tree(&get_sym_table_val(Some(&eval_result)), 0);
            //let eval_result = make_sym_table_val(eval(&Sexps::Sub(binding.clone()), root.clone(), t));

            //let borrowed = unsafe { root.as_unsafe_cell().get() };
            //unsafe { (*borrowed).table_add(0, &*name, eval_result); } //table_add(t, ...)
            root.borrow_mut().table_add(t, name, eval_result);

            Sexps::Var("success".to_string())
         }
         else { err("bad define syntax") }
      }
      "lambda" => {
         //var params = kkzz
         //if let Cons::Cons(x, xs) = *args {}
         //Callable::new(args, env)
         err("new lambda")
      }
      _ => { err("Cannot find symbol in envrionment") }
   }
}

fn apply(exp : &Sexps, root : Root, table : EnvId) -> Sexps {
   match *exp {
      Sexps::Sub(/*ref e @*/ box Cons::Cons(ref f, ref args)) => {
         err("Calling apply for function");

         if let Sexps::Var(ref s) = *f { //if first element is variable look it up
            //let func_lookup = root.borrow().lookup(table, s);
            let borrowed = unsafe { root.as_unsafe_cell().get() };
            let func_lookup = unsafe { (*borrowed).lookup(table, s) };

            if let Some(f) = func_lookup { //if function look up successful
               debug_p(2, "Found symbol, executing function");
               let evaled_args = cons_map(&args.clone(), |arg| {
                  //let new_env = borrowed.table_new(table);
                  let new_table = unsafe { (*borrowed).table_new(table) };
                  eval(arg, root, new_table)
                  //eval(arg, root, root.borrow().table_new(table))
               });
               f.exec(Sexps::Sub(Box::new(evaled_args)), root)
               //err("yo")
            }
            else { //if can't find symbol assume it's macro
               debug_p(2, "Macro!");
               apply_macro(s, &args, root, table)
            }

         }
         else { err("(x y z) <- x has to be macro or function") }
         /*let maybe_f = car(e); //get function name
         let maybe_args = cdr(e); //arguments

         if let Some(f) = maybe_f { //first element of sexps
            if let Sexps::Var(ref s) = *f { //if first element is variable look it up
               if let Some(f) = root.lookup(table, s) { //if look up successful
                  debug_p(2, "Not macro!");
                  if let Some(args) = maybe_args { //if we have rest of arguments
                     //f.exec(helper(args))
                     //(cons_map(args, |arg| eval(arg, env)))
                     //f.exec(Box::new(cons_map(&args.clone(), |arg| eval(arg, env))))
                     f.exec(Box::new(cons_map(&args.clone(), |arg| {
                        //eval(arg, &mut SymTable::new(Some(Box::new(env))))
                        eval(arg, root, root.table_new(table))
                     })))
                  }
                  else { f.exec(Box::new(Cons::Nil)) }
               }
               else { //if can't find symbol assume it's macro
                  debug_p(2, "Macro!");
                  if let Some(args) = maybe_args {
                     apply_macro(s, args, root, table)
                  } else { err("bad args") }
               }
            }
            else { err("function not var") }
         }
         else { err("bad args") } */
    }
      Sexps::Sub(box Cons::Nil) => err("Empty subexpression"),
      _ => err("Bad apply call")
   }
}

fn run(root : Root, code : &str) -> Sexps {
   let lexemes = lex(code);
   let exp_opt = parse(&lexemes);

   if let Some(exp) = exp_opt {
      eval(&exp, root, 0) //was &mut root
   }
   else { err("Couldn't parse code") }
}

fn interpreter() {
   use std::io::{self, BufRead};
   let stdin = io::stdin();

   let mut root = RefCell::new(Env::new());
   root.borrow_mut().add_defaults();

   loop {
      let line = stdin.lock().lines().next().unwrap().unwrap();
      let out = run(&root, &line);
      display_sexps(&out)
   }
}


fn main() {
   interpreter();
}

fn table_test() {
   let mut x : Env = Env::new();
   let child = x.table_new(0);
   let child2 = x.table_new(child);

   x.table_add(child, "hello", make_sym_table_val(err("test")));
   x.table_add(child2, "test", make_sym_table_val(err("yo")));
   x.table_add(child, "hello3", make_sym_table_val(err("yo2")));

   display_sexps(&get_sym_table_val(x.lookup(child2, "hello")));

}


