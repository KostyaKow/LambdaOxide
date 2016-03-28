//import/use header
#![feature(box_syntax, box_patterns, slice_patterns)]

#![allow(dead_code)] //kk removme
#![allow(unused_variables)] //kk removeme

//#![feature(slice_patterns)]
//replaced with custom char_at #![feature(str_char)]

use std::collections::HashMap;
use std::boxed::Box;

extern crate list;
use list::{Cons, cons, cons_map, cons_reverse, car, cdr};
//use list::{cons_len, List};

extern crate utils;
use utils::{get_char_ranges, slice_str, syntax_err, syntax_err_lex, print_space, print_nest, char_at, is_numeric};
//use utils::internal_err;

//lex and parse
fn lex(code : &str) -> Vec<Lexeme> {
   let mut lexemes : Vec<Lexeme> = Vec::new();

   let mut col = String::new(); //symbol collector
   let ranges = get_char_ranges(code); //range of strings
   let mut r_it = 0; //current string range
   let mut i = 0;
   let code_len = code.len();

   while i < code_len {
      let str_start = r_it < ranges.len() && ranges[r_it].0 == i;
      let (start, end) = if str_start { ranges[r_it] } else { (0, 0) };

      //if current character c is string or
      //special character push previously collected
      if let Some(c) = char_at(code, i) {
         //should we collect symbols
         let collect = str_start || c == '(' || c == ')' || c == ' ';

         if collect && !col.is_empty() {
            lexemes.push(
               if is_numeric(&col) { Lexeme::Num(col.parse::<i64>().unwrap()) }
               else { Lexeme::Sym(col) });

            col = String::new();
         }
      }
      if str_start {
         let l = Lexeme::Str(slice_str(code, start, end));
         lexemes.push(l);
         i = end + 1;
         r_it += 1;
      }
      if let Some(c) = char_at(code, i) {
         match c {
            '(' => lexemes.push(Lexeme::OpenParen),
            ')' => lexemes.push(Lexeme::CloseParen),
            ' ' => {},
            '"' => i-=1, //"string""s2"
            _   => col.push(c)
         }
      }
      i += 1;
   }

   lexemes
}

//parsing
//inclusive let i = start; while (i <= end)
fn get_child_sexps(lexemes : &Vec<Lexeme>, start : usize, end : usize) -> Vec<(usize, usize)>
{
   let mut nestedness = 0;
   let mut children : Vec<(usize, usize)> = Vec::new();
   let mut child_start : Option<usize> = None;

   let mut i = start;
   while i <= end {
      match &lexemes[i] {
         &Lexeme::OpenParen => {
            nestedness += 1;
            if nestedness == 1 { child_start = Some(i); }
         },
         &Lexeme::CloseParen => {
            nestedness -= 1;
            if nestedness == 0 {
               if let Some(start) = child_start {
                  children.push((start, i)); child_start = None;
               }
            }
         },
         _ => {}
      }
      i += 1;
   }
   children
}

//range without include parenthesis
fn parse_range(lexemes : &Vec<Lexeme>, start : usize, end : usize) -> Option<Sexps> {
   let mut sub : Cons<Sexps> = Cons::Nil; //: Vec<Sexps> = Vec::new()

   let children = get_child_sexps(lexemes, start, end);
   let mut c_it = 0; //current child
   let mut i = start;
   while i <= end {
      let child_start = c_it < children.len() && children[c_it].0 == i;
      let (c_start, c_end) = if child_start { children[c_it] } else { (0, 0) };

      if child_start {
         let child = parse_range(lexemes, c_start+1, c_end-1);
         if let Some(c) = child { sub = cons(c, sub); }
         else { println!("Couldn't parse child"); return None; }
         c_it += 1;
         i = c_end + 1;
         continue;
      }

      //Sexps::Str(String::from("Test"));
      let ref l = lexemes[i];
      match l {
         &Lexeme::Str(ref s) => { sub = cons(Sexps::Str(s.to_string()), sub) },
         &Lexeme::Sym(ref s) => { sub = cons(Sexps::Var(s.to_string()), sub) },
         &Lexeme::Num(ref n) => { sub = cons(Sexps::Num(*n), sub) },
         _ => { sub = cons(err("Parsing failed: bad lexeme"), sub) }
      }
      i += 1;
   }

   Some(Sexps::Sub(Box::new(cons_reverse(sub))))
}

fn parse(lexemes : &Vec<Lexeme>) -> Option<Sexps> {
   let mut start_paren : Option<usize> = None;
   let mut end_paren : Option<usize> = None;
   let mut nestedness : i32 = 0;

   for (i, l) in lexemes.iter().enumerate() {
      match l {
         &Lexeme::OpenParen => {
            nestedness += 1;
            if nestedness == 1 { start_paren = Some(i); }
         },
         &Lexeme::CloseParen => {
            nestedness -= 1;
            if nestedness == 0 { end_paren = Some(i); }
         },
         _ => {}
      }
      if nestedness < 0 { syntax_err_lex("Extra close parenthesis", 0) }
   }

   let mut good_range = true;
   let mut start = 0;
   let mut end = 0;

   if let Some(x) = start_paren { start = x; println!("start paren: {}", x) }
   else { good_range = false; syntax_err_lex("No start paren", 0) }
   if let Some(x) = end_paren { end = x; println!("got end paren: {}", x) }
   else { good_range = false; syntax_err_lex("No end paren", 0) }

   if good_range { return parse_range(lexemes, start+1, end-1) }
   return None
}
//end parsing
//end lex and parse

enum Lexeme { OpenParen, CloseParen, Str(String), Sym(String), Num(i64) }

#[derive(Clone)]
enum Sexps {
   Str(String), Num(i64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), //Sub(Box<Vec<Sexps>>)
}
impl Drop for Sexps {
   fn drop(&mut self) {
      match *self {
         Sexps::Err(ref s) => println!("err dropping: {}", s),
         _ => {} //println!("Sexps going out of scope")
      }
   }
}

enum FunType {
   BuiltIn(Box<Fn(Box<Cons<Sexps>>) -> Sexps>),
   Lambda(Sexps)
}
struct Callable<'a> { env : SymTable<'a>, f : FunType, arg_names : Cons<String> }
impl<'a> Callable<'a> {
   fn new(arg_names : Cons<String>, f : FunType, /*parent_env : Box<&'a SymTable<'a>>*/) -> Callable<'a> {
      Callable { env: SymTable::new(/*Some(parent_env)*/None), f: f, arg_names: arg_names }
   }
   fn exec(&self, args : Box<Cons<Sexps>>) -> Sexps {
      Sexps::Err("calling .exec of callable".to_string());
      match self.f {
         FunType::BuiltIn(ref f) => {
            f(args)
         },
         FunType::Lambda(ref s) => { Sexps::Err("user defined lamda".to_string()) }
      }
   }
}

//symtable
struct SymTable<'a> {
   bindings : HashMap<String, Callable<'a>>,
   parent : Option<Box<&'a SymTable<'a>>>
}

impl<'a> SymTable<'a> {
   fn new(parent : Option<Box<&'a SymTable<'a>>>) -> SymTable<'a> {
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
                              FunType::BuiltIn(Box::new(sum_))/*,
                              Box::new(self)*/);

      let difference = Callable::new(Cons::Single("*".to_string()),
                                     FunType::BuiltIn(Box::new(difference_))/*,
                                     Box::new(self)*/);
      self.add("+".to_string(), sum);
      self.add("-".to_string(), difference);
      //self.add("-".to_string(), difference)
   }
   fn add(&mut self, key : String, f : Callable<'a>) { self.bindings.insert(key, f); }
   fn lookup(&self, s : &String) -> Option<&Callable> {
      //if !self.bindings.contains_key(s)
      let entry_opt = self.bindings.get(s);
      if let Some(ref entry) = entry_opt { Some(entry.clone()) }
      else {
         if let Some(ref parent) = self.parent { parent.lookup(s) }
         else {
            syntax_err("Cannot find symbol in symbol table", 0);
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
      _ => { Sexps::Err("Cannot execute symbolic expression which isn't func or macro".to_string()) }
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
            None        => Sexps::Err("Undefined variable lookup".to_string())
         }
      }
   }
}


fn err(s : &str) -> Sexps { Sexps::Err(s.to_string()) } //or String::from(s)

fn apply(exp : &Sexps, env : &mut SymTable) -> Sexps {
   match *exp {
      Sexps::Sub(ref e @ box Cons::Cons(_, _)) => {
         err("Calling apply for function");
         let maybe_f = car(e); //get function name
         let maybe_args = cdr(e); //arguments
         if let Some(f) = maybe_f {
            if let Sexps::Var(ref s) = *f { //kk left here
               if let Some(f) = env.lookup(s) {
                  println!("Not macro!");
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
                  println!("Macro!");
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

fn run(code : &str) -> Sexps {
   let lexemes = lex(code);
   let exp_opt = parse(&lexemes);

   let mut env = SymTable::new(None);
   env.add_defaults();

   if let Some(exp) = exp_opt {
      eval(&exp, &mut env)
   }
   else { err("Couldn't parse code") }
}

fn main() {
   //let code : &str = "(define (6 +) () (+ (test) 5))";
   //let code : &str = "(+ (- 6 4) (+ 3 5))";
   let code : &str = "(+  (+ 1 2) 2 (+ 6 2))"; //-1
   //let code : &str = "(hello (+ world) \"string\")";
   //let code : &str = "(hello (world) (yo test) 5)";
   //let code : &str = "(hello (\"world\"\"test1\" + test) \"another \\\"string\")";

   //lex_test();
   //parse_test();
   display_sexps(&run(code));
}

fn helper(a : &list::Cons<Sexps>) -> Box<list::Cons<Sexps>> {
   Box::new(a.clone())
}

#[allow(dead_code)]
fn parse_test() {
   let code : &str = "'((6 +) () (+ (test) 5))";
   let lexemes = lex(code);
   let tree_maybe = parse(&lexemes);
   if let Some(tree) = tree_maybe {
      print_tree(&tree, 0);
   }
   else { syntax_err_lex("Parsing failed", 0); }
}

#[allow(dead_code)]
fn lex_test() {
   let code : &str = "'(hello (\"world\"\"test1\" + 69 test 42) 47 \"another \\\"string\")";
   let lexemes = lex(code);
   print_lexemes(&lexemes);
}

fn display_sexps(exp: &Sexps) {
   match *exp {
      Sexps::Str(ref s) => println!("{}", s),
      Sexps::Num(ref n) => println!("{}", n),
      Sexps::Var(ref s) => println!("{}", s),
      Sexps::Err(ref s) => println!("{}", s),
      _                 => println!("bad sexps, cant print")
   }
}
fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sexps::Str(ref s) => { print_nest(&s, deepness, Some("str")) },
      Sexps::Var(ref s) => { print_space(deepness); println!("var: {}", s) },
      Sexps::Num(ref n) => { print_space(deepness); println!("num: {}", n) },
      Sexps::Sub(box ref sub) => { //box ref sexps
         print_nest("(", deepness, None);
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_tree(x, deepness+4));
         print_nest(")", deepness, None);
      },
      Sexps::Err(ref s) => { println!("error: {}", s) }
   }
}
fn print_lexemes(lexemes: &Vec<Lexeme>) {
   for l in lexemes.iter() {
      match *l {
         /*_ => {} empty match */
         Lexeme::OpenParen => println!("open paren"),
         Lexeme::CloseParen => println!("close paren"),
         Lexeme::Str(ref s) => println!("string {}", s),
         Lexeme::Sym(ref s) => println!("sym {}", s),
         Lexeme::Num(ref n) => println!("number {}", n),
      }
   }
}


