#![allow(dead_code, unused_variables)]
#![feature(box_syntax, box_patterns)]
#![feature(as_unsafe_cell)]

//#![allow(unused_variables)]
//#![allow(unused_imports)]
use std::collections::HashMap;
use std::boxed::Box;

use std::rc::Rc;
use std::cell::RefCell;

use utils::*;//{print_space, print_nest, char_at};
use types::*;
use types::Sexps::*;
use err::{debug_p, DEBUG};
use lexer::lex;
use parser::parse;
use list::{Cons, car, cdr, cons_map, cons, cons_get}; //cons_reverse
//use list::Cons::*;

mod utils;
mod types;
mod err;
mod lexer;
mod parser;
mod list;

type FunArgNames = Sexps;
type FunArgs = Sexps;

type Root<'a> = &'a RefCell<Env>;
type BuiltInFunc = Fn(Sexps, Root, EnvId) -> Sexps;

//callable
enum Callable {
   BuiltIn(EnvId, Box<BuiltInFunc>), //args, root, our env
   Lambda(EnvId, FunArgNames, Sexps)
}
impl Callable {
   fn exec(&self, args_exp : Sexps, root : Root) -> Sexps {
      debug_p(2, "calling .exec of callable");

      match *self {
         Callable::BuiltIn(parent, ref f) => { f(args_exp, root, parent) },
         Callable::Lambda(ref t, ref arg_names_exp, ref exp) => {
            let exec_table = root.borrow_mut().table_new(t.clone());

            debug_p(2, "Calling .exec() of lambda");
            if let Sexps::Sub(box ref args_@Cons::Cons(_, _)) = args_exp {
               if let Sexps::Sub(box ref arg_names_@Cons::Cons(Sexps::Var(_), _)) = *arg_names_exp {
                  //if let Sexps::Sub(box ) arg_names
                  let mut arg_names : &Cons<Sexps> = arg_names_;
                  let mut i = 0;
                  while let Cons::Cons(Sexps::Var(ref arg_name), box ref rest) = *arg_names {
                     if let Some(arg) = cons_get(&args_, i) {
                        let mut borrowed = root.borrow_mut();
                        let val = make_sym_table_val(arg.clone());
                        borrowed.table_add(exec_table.clone(), &*arg_name, val);
                        arg_names = &rest;
                        i += 1;
                     } else { return err("argument number doesn't match") }
                  }
               } else { return err("Bad argument names in lambda") }
            }
            else { err("bad args to lambda"); }
            /*if let Sexps::Sub(box args_@Cons::Cons(Sexps::Var(_), _) = args_exp {
               //if let Sexps::Sub(box = ) arg_names
               let mut args : Cons<Sexps> = args_;
               while let Cons::Cons(Sexps::Var(arg), box rest) {
                  args = rest;
               }
            }*/

            //let new_table = root.borrow_mut().table_new(parent.clone());
            //if let Sexps::Sub(box args_@Cons::Cons(Sexps::Var(arg), box rest) = args_ {
            //root.borrow_mut().table_add(new_table, "~", make_sym_table_val(args));
            //let mut env = SymTable::new(root, env_parent.clone());
            /*if let Some(ref arg_names) = *arg_names_opt {
               //let mut i = 0;
               //for arg in args { env.add(arg_names[i], arg); i+=1; }
            } else { kkleft: env.add("*", args); } */
            eval(exp, root, exec_table.clone()) //kkleft: eval or apply
         }
      }
   }
}

fn make_sym_table_val(exp : Sexps) -> Callable {
   //let root = Env::new();
   let ret : Box<Fn(Sexps, Root, EnvId) -> Sexps> = Box::new(move |args, root, env| -> Sexps {
      cons_to_sexps(cons(err("__var"), cons(exp.clone(), Cons::Nil)))
   });
   Callable::BuiltIn(0, ret)
}
fn sym_table_is_var(v : Option<&Callable>) -> bool {
   if let Some(f) = v {
      match f.exec(err("__sym"), &(RefCell::new(Env::new()))) {
         Sub(box Cons::Cons(Err(ref s), _)) if s == "__var" => true,
         _ => false
      }
   } else { false }
}
fn get_sym_table_val(v : Option<&Callable>) -> Sexps {
   if let Some(f) = v {
      match f.exec(err("__sym"), &(RefCell::new(Env::new()))) {
         Sexps::Sub(box Cons::Cons(Sexps::Err(ref s), box Cons::Cons(ref exp, _))) if s == "__var"
            => exp.clone(),
         _ => err("Bad value")
      }
   } else { err("Not found") }
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
   fn print(&self) {
      /*println!("printing table");*/
      let mut i = 0;
      for table in &self.tables {
         println!("table {}...parent: {}", i, table.parent);
         i+=1;
         for (ref key, ref binding) in &table.bindings {
            //println!("key: {}, binding: {}", key, binding);
            print_space(3); println!("key: {}", key);
            let b = Some(*binding);
            let is_var = sym_table_is_var(b);
            print_space(6); println!("is_var: {}", is_var);
            if is_var {
               print_space(6); print!("val: ");
               display_sexps(&get_sym_table_val(b));
            }
         };
      }
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
      let sum = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sexps::Err(ref s) = args_sexps {
            debug_p(5, "variable lookup calling exec of diff"); return err(s);
         }
         let mut sum = 0;
         if let Sub(box ref args_) = args_sexps {
            let mut args : &Cons<Sexps> = args_; //Box::new(args_);

            loop {
               match *args {
                  Cons::Cons(Num(n), ref ns) => { sum += n; args = ns; },
                  Cons::Cons(_, _) => { err("bad arguments"); break },
                  Cons::Nil   => break,
                  _ => return err("bad arguments to sum")
               };
            }
            Sexps::Num(sum)
         }
         else { err("bad arguments") }
      };
      self.table_add(0, "+", Callable::BuiltIn(0, Box::new(sum)));

      let diff = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sexps::Err(ref s) = args_sexps {
            debug_p(5, "variable lookup calling exec of diff"); return err(s);
         }
         let mut diff = 0;
         let mut first = true;

         if let Sub(box ref args_) = args_sexps {
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
      self.table_add(0, "-", Callable::BuiltIn(0, Box::new(diff)));

      let eq = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sexps::Err(ref s) = args_sexps {
            debug_p(5, "variable lookup calling exec of diff"); return err(s);
         }
         let args = arg_extractor(&args_sexps).unwrap(); //fixme, check this
         if args.len() < 2 { return err("equality test needs at least 2 args") }
         let first = args[0].clone();
         for arg in args {
            if !same_type(&first, &arg) { return Sexps::Bool(false) }
            if arg != first { return Sexps::Bool(false) }
         }
         Sexps::Bool(true)
      };
      self.table_add(0, "=", Callable::BuiltIn(0, Box::new(eq)));

      let load = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Err(ref s) = args_sexps {
            debug_p(5, "variable lookup calling exec of load"); return err(s);
         }

         //TODO: kkleft assert rest = Null
         //use std::io::prelude::*;
         use std::fs::File;
         use std::path::Path;
         use std::error::Error;
         use std::io::Read;

         if let Sub(box Cons::Cons(Str(ref path_str), ref rest)) = args_sexps {
            println!("loading file {}", path_str); //err("file")
            /*let mut f = try!(File::open(s));
            try!(f.read_to_string(&mut file_content));*/
            let path = Path::new(path_str); //path_str);
            match File::open(&path) {
               Ok(mut file) => {
                  let mut file_content = String::new();
                  match file.read_to_string(&mut file_content) {
                     Ok(_) => {
                        let lines = file_content.split("\n").collect::<Vec<&str>>();

                        let mut x : Sexps = err("empty file");
                        for line in lines.iter() {
                           if let Some(first) = char_at(line, 0) {
                              if first != ';' {
                                 x = run(root, line)
                              }
                           }
                        }
                        x
                     }
                     Result::Err(why) => { panic!("{}", Error::description(&why)); err("failed to read file") }
                  }
               }
               Result::Err(why) => { panic!("{}", Error::description(&why)); err("failed to open file") }
            }
         }
         else { err("cannot load file: bad name") }
      };
      self.table_add(0, "load", Callable::BuiltIn(0, Box::new(load)));

      let print_root = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps
      {
         if let Err(ref s) = args_sexps {
            debug_p(5, "variable lookup calling exec of load"); return err(s);
         }

         root.borrow().print();
         err("succ")
      };
      self.table_add(0, "print_env", Callable::BuiltIn(0, Box::new(print_root)));

      let is_nil = |args : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sub(box Cons::Cons(Err(_), _)) = args {
            Bool(true)
         }
         else { Bool(false) }
      };
      self.table_add(0, "null?", Callable::BuiltIn(0, Box::new(is_nil)));

      self.table_add(0, "nil", make_sym_table_val(err("")));
   }
}

fn eval(exp : &Sexps, root : Root, table : EnvId) -> Sexps {
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(err("test")));
   //print_tree(&get_sym_table_val(root.borrow_mut().lookup(0, "hello")), 0);
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(Sexps::Num(5)));
   if DEBUG >= 2 {
      print!("==evaluating: ");
      print_compact_tree(exp);
   }

   match *exp {
      Str(_) | Num(_)
             | Bool(_)       => exp.clone(), //self evaluation
      Sub(_)                 => apply(exp, root, table),
      //Sexps::Lambda(..)           => apply(exp, root, table),
      ref l@Lambda(..)       => l.clone(),
      Err(ref s)             => Sexps::Err(s.clone()),
      Var(ref s)             => {
         debug_p(2, "Looking up variable");
         let borrowed = root.borrow();
         let lookup_opt = borrowed.lookup(table, &s.clone());
         //if let Some(&Callable::BuiltIn(..)) = lookup_opt { Sexps::Var(s.clone()) }
         //else { get_sym_table_val(lookup_opt) }
         if sym_table_is_var(lookup_opt) {
            let x = get_sym_table_val(lookup_opt);
            if DEBUG >=3 { print!("eval variable {}: ", s); print_tree(&x, 0); }
            x
         }
         else { Sexps::Var(s.clone()) }
         //get_sym_table_val(lookup_opt)
      }
   }
}

fn apply_macro(name : &str, args : &Cons<Sexps>, root : Root, t : EnvId) -> Sexps
{
   debug_p(2, "==applying macro!");
   match &name[..] {
      "define" => {
         debug_p(3, "macro define");
         if let Cons::Cons(Var(ref name), ref binding) = *args {
            let eval_result = if let Some(x) = car(binding) {
               if DEBUG >= 3 { print_space(3); print!("defining {}: ", &*name); print_compact_tree(x); }
               make_sym_table_val(eval(x, root, t))
            }
            else { make_sym_table_val(err("bad define")) };
            root.borrow_mut().table_add(t, name, eval_result);
            Var("success".to_string())
         }
         else { err("bad define syntax") }
      }
      "lambda" => {
         debug_p(3, "macro lambda");
         if let Cons::Cons(ref args@Sub(_), box Cons::Cons(ref exp, _)) = *args {
            if DEBUG >= 3 {
               print_space(3); print!("args: "); print_compact_tree(args);
               print_space(5); print!("exp: "); print_compact_tree(exp);
            }
            let borrowed = unsafe { root.as_unsafe_cell().get() };
            let lambda_table = unsafe { (*borrowed).table_new(t) };

            debug_p(2, &format!("new lambda table number {}", lambda_table));
            let lambda = Callable::Lambda(lambda_table, args.clone(), exp.clone());
            root.borrow_mut().table_add(lambda_table, "self", lambda);
            Lambda(lambda_table, "self".to_string())
            //var params = kkzz
            //if let Cons::Cons(x, xs) = *args {}
            //Callable::new(args, env)
         } else { err("bad arguments to lambda") }
      },
      "if" => {
         debug_p(3, "if statement");
         if let Cons::Cons(ref cond, box Cons::Cons(ref if_t, box Cons::Cons(ref if_f, _))) = *args
         {
            match eval(cond, root, t) {
               Bool(true)  => eval(if_t, root, t),
               Bool(false) => eval(if_f, root, t),
               _           => err("if x y z <-- x needs to return boolean")
            }
         }
         else { err("bad argument format to if") }
      }
      "defmacro" => {
         err("not yet supported")
      }
      _ => { err(&*format!("Cannot find symbol in envrionment and not macro {}", name.to_string())) }
   }
}

fn is_macro(exp : &Sexps) -> bool {
   if let Sub(box Cons::Cons(Var(ref s), ref args)) = *exp {
      if s == "if" || s == "lambda" || s == "define" || s == "defmacro" { return true }
   }
   false
}

fn apply(exp : &Sexps, root : Root, table : EnvId) -> Sexps {
   debug_p(2, "==apply");

   match *exp {
      Sexps::Sub(box Cons::Cons(ref f, box ref args)) => {
         if is_macro(exp) {
            if let Sexps::Var(ref s) = *f { apply_macro(s, &args, root, table) }
            else { err("bad macro (unreachable)") }
         }
         else {
            debug_p(2, "Calling apply for function");
            let e = Cons::Cons(f.clone(), Box::new(args.clone()));
            let evaled = cons_map(&e, |exp| { eval(exp, root, table) });

            if let Cons::Cons(ref evaled_f, ref evaled_args) = evaled {
               if let Var(ref s) = *evaled_f {
                  debug_p(2, &format!("applying: {}", s));
                  let borrowed = unsafe { root.as_unsafe_cell().get() };
                  let func_lookup = unsafe { (*borrowed).lookup(table, s) }; //if first element function, look it up
                  if let Some(f) = func_lookup { //if function look up successful
                     debug_p(2, "Found symbol, executing function");
                     f.exec(Sexps::Sub(evaled_args.clone()), root)
                  }
                  else { err(&format!("symbol not found: {}", s)) }
               }
               else if let Sexps::Lambda(ref table, ref name) = *evaled_f {
                  debug_p(2, "applying lambda");
                  let borrowed = unsafe { root.as_unsafe_cell().get() };
                  let func_lookup = unsafe { (*borrowed).lookup(*table, &*name) };
                  if let Some(lambda) = func_lookup {
                     lambda.exec(Sexps::Sub(evaled_args.clone()), root)
                  } else { err("lambda not found") }
               }
               else {
                  display_sexps(evaled_f);
                  err("(x y z) <- x has to be macro or function")
               }
            }
            else { err("bad arguments to apply") }
         }
      },
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

   let mut cmd;
   cmd = "(load \"core.lam\")";
   println!("**> {}", cmd);
   display_sexps(&run(&root, cmd));
   /*let mut cmd;
   cmd = "(define f (lambda (x) (+ x x)))";
   println!("**> {}", cmd);
   display_sexps(&run(&root, cmd));
   cmd = "(f 5)";
   println!("**> {}", cmd);
   display_sexps(&run(&root, "(f 5)"));*/

   loop {
      print!("**>");
      use std::io::{self, Write};
      io::stdout().flush().unwrap();
      let line = stdin.lock().lines().next().unwrap().unwrap();
      let out = run(&root, &line);
      display_sexps(&out);
      /*root.borrow().print();*/
   }
}

fn main() {
   use std::thread;
   let child = thread::Builder::new().stack_size(8*32*1024*1024).spawn(move || {
      interpreter();
   }).unwrap();
   let test = child.join().unwrap();
   //table_test();
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


