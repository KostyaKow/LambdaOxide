#![allow(unused_variables)]

use std::collections::HashMap;
use std::boxed::Box;

use std::cell::RefCell;

use err::{debug_p, DEBUG};
use utils::*;
use types::*;
use types::Sexps::*;
use lexer::lex;
use parser::parse;
use list::*;

//callable
pub enum Callable {
   BuiltIn(EnvId, Box<BuiltInFunc>), //args, root, our env
   Lambda(EnvId, FunArgNames, Sexps)
}
impl Callable {
   pub fn exec(&self, args_exp : Sexps, root : Root) -> Sexps {
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
            eval(exp, root, exec_table.clone()) //kkleft: eval or apply
         }
      }
   }
}
//end callable

struct Table { bindings: HashMap<String, Callable>, parent: EnvId }
pub struct Env { tables : Vec<Table>, }

fn builtin_sum(args_sexps : Sexps, root : Root, table : EnvId) -> Sexps {
   let args = arg_extractor(&args_sexps).unwrap();

   let mut has_string = false;
   for i in 0..args.len() {
      if let Some(_) = arg_extract_str(&args, i) {
         has_string = true;
         break;
      }
   }
   if has_string {
      let mut ret = String::new();
      for i in 0..args.len() {
         let s_val = if let Some(x) = arg_extract_int(&args, i) {
            x.to_string()
         } else if let Some(x) = arg_extract_float(&args, i) {
            x.to_string()
         } else if let Some(x) = arg_extract_str(&args, i) {
            x
         } else {
            return err("bad argument to sum"); "error"
         };
         ret = ret + &*s_val;
      }
      return Sexps::Str(ret);
   }

   let mut sum = 0.0;
   let mut is_int = true;

   if let Sub(box ref args_) = args_sexps {
      let mut args : &Cons<Sexps> = args_; //Box::new(args_);

      loop {
         match *args {
            Cons::Cons(Int(n), ref ns) => {
               sum += n as f64; args = ns;
            },
            Cons::Cons(Float(n), ref ns) => {
               is_int = false; sum += n; args = ns;
            }
            Cons::Nil => break,
            _ => return err("bad arguments to sum")
         };
      }
      if is_int { Int(sum as i64) }
      else { Float(sum) }
   }
   else { err("bad arguments") }
}

fn builtin_mul(args_ : Sexps, root : Root, table : EnvId) -> Sexps {
   let args = arg_extractor(&args_).unwrap();
   let mut ret = 1.0;
   let mut has_float = false;

   for i in 0..args.len() {
      if let Some(x) = arg_extract_float(&args, i) { has_float = true; }
      if let Some(x) = arg_extract_num(&args, i) { ret *= x; }
      else { return err("bad argument to *"); }
   }
   if has_float { Float(ret) }
   else { Int(ret as i64) }
}

impl Env {
   pub fn new() -> Env {
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
   pub fn table_add(&mut self, table_id : EnvId, key : &str, entry : Callable) -> bool {
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
   pub fn table_add_f<F>(&mut self, key : &str, entry : F)
      -> bool
      where F : Fn(Sexps, Root, EnvId) -> Sexps + 'static
   {
      let checker = move |args : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sexps::Err(ref s) = args {
            debug_p(5, "variable lookup calling exec"); return err(s);
         }
         else { entry(args, root, table) }
      };
      self.table_add(0, key, Callable::BuiltIn(0, Box::new(checker)))
   }
   pub fn add_defaults(&mut self) {
      self.table_add_f("+", builtin_sum);
      self.table_add_f("*", builtin_mul);

      let diff = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let mut diff = 0;
         let mut first = true;

         if let Sub(box ref args_) = args_sexps {
            let mut args : &Cons<Sexps> = args_; //Box::new(args_);

            loop {
               match *args {
                  Cons::Cons(Int(n), ref ns) => {
                     if first { diff = n; first = false; } else { diff -= n; }
                     args = ns;
                  },
                  Cons::Cons(_, _) => { err("bad arguments"); break },
                  Cons::Nil   => break,
                  _ => return err("bad arguments to sum")
               };
            }
            Int(diff)
         }
         else { err("bad arguments") }
      };
      self.table_add_f("-", diff);

      let lt = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_sexps).unwrap(); //fixme, check this
         if args.len() < 2 { return err("< needs at least 2 args") }
         Sexps::Bool(args[0] < args[1])
      };
      self.table_add_f("<", lt);

      let gt = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_sexps).unwrap(); //fixme, check this
         if args.len() < 2 { return err("> needs at least 2 args") }
         Sexps::Bool(args[0] > args[1])
      };
      self.table_add_f(">", gt);

      let eq = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_sexps).unwrap(); //fixme, check this
         if args.len() < 2 { return err("equality test needs at least 2 args") }
         let first = args[0].clone();
         for arg in args {
            //if !same_type(&first, &arg) { return Sexps::Bool(false) }
            if arg != first { return Sexps::Bool(false) }
         }
         Sexps::Bool(true)
      };
      self.table_add_f("=", eq);

      let not_ = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();
         if args.len() != 1 { err("not only takes 1 argument"); }
         if let Some(b) = arg_extract_bool(&args, 0) { Bool(!b) }
         else { err("bad argument type passed to not") }
      };
      self.table_add_f("not", not_);

      let and_ = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();
         if args.len() != 2 { err("and only takes 2 argument"); }
         if let Some(a) = arg_extract_bool(&args, 0) {
             if let Some(b) = arg_extract_bool(&args, 1) {
               return Bool(a && b);
            }
         }
         err("bad argument types passed to and")
      };
      self.table_add_f("and", and_);

      let or_ = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();
         if args.len() != 2 { err("or only takes 2 argument"); }
         if let Some(a) = arg_extract_bool(&args, 0) {
             if let Some(b) = arg_extract_bool(&args, 1) {
               return Bool(a || b);
            }
         }
         err("bad argument types passed to or")
      };
      self.table_add_f("or", or_);

      let load_file = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         use utils::load_internal;
         //TODO: kkleft assert rest = Null
         use std::fs::File;
         use std::path::Path;
         use std::error::Error;
         use std::io::Read;

         if let Sub(box Cons::Cons(Str(ref path_str), ref rest)) = args_sexps {
            println!("loading file {}", path_str);
            /*let mut f = try!(File::open(s));
            try!(f.read_to_string(&mut file_content));*/
            let path = Path::new(path_str); //path_str);

            let f_res = File::open(&path);
            if let Result::Err(why) = f_res {
               return err(&*format!("failed to open file: {}", Error::description(&why)));
            }
            let mut file = f_res.unwrap();

            let mut content = String::new();
            let read_res = file.read_to_string(&mut content);
            if let Result::Err(why) = read_res {
               return err(&*format!("failed to read file: {}", Error::description(&why)));
            }
            load_internal(&content, root)
         }
         else { err("cannot load file: bad argument") }
      };
      self.table_add_f("load_file", load_file);

      let load = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sub(box Cons::Cons(Str(ref code), ref rest)) = args_sexps {
            load_internal(code, root)
         }
         else { err("cannot load code: bad argument") }
      };
      self.table_add_f("load", load);

      let print_root = |args_sexps : Sexps, root : Root, table : EnvId| -> Sexps {
         root.borrow().print();
         err("succ")
      };
      self.table_add_f("print_env", print_root);

      let print = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();
         /*if let Sexps::Str(ref s) = args[0] { println!("{}", s); }
         else { println!("unsupported print"); }*/
         display_sexps(&args[0]);
         err("success")
      };
      self.table_add_f("print", print);

      let sleep = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();
         if args.len() != 1 { return err("sleep needs 1 argument"); }
         let time = arg_extract_num(&args, 0).unwrap()*1000.0;

         use std::thread::sleep_ms;
         sleep_ms(time as u32);

         Sexps::Str("success".to_string())
      };
      self.table_add_f("sleep", sleep);

      let do_ = |args_ : Sexps, root : Root, table : EnvId| -> Sexps {
         let args = arg_extractor(&args_).unwrap();

         let mut result = err("empty do");
         for arg in args {
            result = eval(&arg, root, table);
         }
         result
      };
      self.table_add_f("do", do_);

      let is_nil = |args : Sexps, root : Root, table : EnvId| -> Sexps {
         if let Sub(box Cons::Cons(Err(_), _)) = args {
            Bool(true)
         }
         else { Bool(false) }
      };
      self.table_add_f("null?", is_nil);

      self.table_add(0, "nil", make_sym_table_val(err("")));
      self.table_add(0, "true", make_sym_table_val(Bool(true)));
      self.table_add(0, "false", make_sym_table_val(Bool(false)));

   }
}

pub fn eval(exp : &Sexps, root : Root, table : EnvId) -> Sexps {
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(err("test")));
   //print_tree(&get_sym_table_val(root.borrow_mut().lookup(0, "hello")), 0);
   //root.borrow_mut().table_add(0, "hello", make_sym_table_val(Sexps::Num(5)));
   if DEBUG >= 2 {
      print!("==evaluating: ");
      print_compact_tree(exp);
   }

   match *exp {
      Str(_) | Float(_) | Quote(_) |
      Int(_) | Bool(_)       => exp.clone(), //self evaluation
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
         /*else if let Cons::Cons(Sub(box Cons::Cons(ref name, ref args)), ref binding) = *args {
            let l = apply_macro("lambda", Cons::Cons(args, binding), root, t);
            apply_macro("define", Cons::Cons(name, l), root, t)
         }*/
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
   if let Sub(box Cons::Cons(Var(ref s), _)) = *exp {
      /*let v1 = vec!["if", "lambda", "define", "defmacro"];
      let v = v1.iter().map(|&x| x.to_string()).collect::<Vec<_>>();
      return contains(s.clone(), v);*/
      (s == "if" || s == "lambda" || s == "define" || s == "defmacro")
   }
   else { false }
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

pub fn run(root : Root, code : &str) -> RunResult {
   let lexemes = lex(code);
   let exp_res = parse(&lexemes);

   match exp_res {
      Ok(exp) => Ok(eval(&exp, root, 0)),
      Result::Err((ParseFail::NoEndParen, n)) =>
         Result::Err((RunFail::UncompleteExp, n)),
      Result::Err((err_type, n)) =>
         Result::Err((RunFail::FailParse(err_type), n))
   }
}

pub fn setup_env() -> RefCell<Env> {
   let root = RefCell::new(Env::new());
   root.borrow_mut().add_defaults();
   root
}

pub fn interpreter(env : Option<RefCell<Env>>) {
   use std::io::{self, BufRead};
   let stdin = io::stdin();

   let root = if let Some(root_) = env { root_ }
   else { setup_env() };

   use std::str::from_utf8;
   let core_file = from_utf8(&include_bytes!("../core.lo")[..]).unwrap().to_string();
   load_internal(&core_file, &root);

  /* let cmd = "(load_file \"core.lo\")";
   println!("**> {}", cmd);
   display_run_result(&run(&root, cmd));
   let mut cmd;
   cmd = "(define f (lambda (x) (+ x x)))";
   println!("**> {}", cmd);
   display_sexps(&run(&root, cmd));
   cmd = "(f 5)";
   println!("**> {}", cmd);
   display_sexps(&run(&root, "(f 5)"));*/

   loop {
      print!("**> ");
      use std::io::{self, Write};
      io::stdout().flush().unwrap();
      let line = stdin.lock().lines().next().unwrap().unwrap();

      let mut acc = line;
      let mut out = run(&root, &acc);
      while let Result::Err((RunFail::UncompleteExp, _)) = out {
         io::stdout().flush().unwrap();
         let line2 = stdin.lock().lines().next().unwrap().unwrap();
         acc = acc + &line2;
         out = run(&root, &acc);
      }

      display_run_result(&out);
      //display_sexps(&out);
      //root.borrow().print();
   }
}

#[allow(dead_code)]
fn thread_intepreter() {
   use std::thread;
   let stack_size = 8*32*1024*1024;

   let child = thread::Builder::new().stack_size(stack_size).spawn(move || {
      interpreter(None);
   }).unwrap();
   //let ret = child.join().unwrap();
   child.join().unwrap();

   interpreter(None);
}




