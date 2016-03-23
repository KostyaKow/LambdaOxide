#![feature(box_syntax, box_patterns, slice_patterns)]

#![allow(dead_code)] //kk removme
#![allow(unused_variables)] //kk removeme

//#![feature(slice_patterns)]
//replaced with custom char_at #![feature(str_char)]

use std::collections::HashMap;
use std::boxed::Box;

extern crate list;
use list::List;
use list::lst_cons;
use list::lst_new;
use list::bb;

extern crate utils;
use utils::get_char_ranges;
use utils::slice_str;
use utils::syntax_err;
use utils::syntax_err_lex;
use utils::internal_err;
use utils::print_space;
use utils::print_nest;
use utils::char_at;
use utils::is_numeric;

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
   let mut sexps : Vec<Sexps> = Vec::new();

   let children = get_child_sexps(lexemes, start, end);
   let mut c_it = 0; //current child
   let mut i = start;
   while i <= end {
      let child_start = c_it < children.len() && children[c_it].0 == i;
      let (c_start, c_end) = if child_start { children[c_it] } else { (0, 0) };

      if child_start {
         let child = parse_range(lexemes, c_start+1, c_end-1);
         if let Some(c) = child { sexps.push(c); }
         else { println!("Couldn't parse child"); return None; }
         c_it += 1;
         i = c_end + 1;
         continue;
      }

      //Sexps::Str(String::from("Test"));
      let ref l = lexemes[i];
      match l {
         &Lexeme::Str(ref s) => { sexps.push(Sexps::Str(s.to_string())) },
         &Lexeme::Sym(ref s) => { sexps.push(Sexps::Var(s.to_string())) },
         &Lexeme::Num(ref n) => { sexps.push(Sexps::Num(*n)) },
         _ => { syntax_err_lex("Parsing failed: bad lexeme", i as u32) }
      }
      i += 1;
   }

   Some(Sexps::Sub(Box::new(sexps)))
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

enum Lexeme {
   OpenParen, CloseParen, Str(String), Sym(String), Num(i64)
}

/*
#[allow(dead_code)]
//#[derive(Copy, Clone)]
enum Sexps {
   Str(String), Num(f64), Sym(String), Err(String),
   Sub(Box<Vec<Sexps>>),
}


enum Binding { Normal(Sexps), Special(Sexps) }
struct SymTable {
   bindings : HashMap<String, Binding>,
   //unused children : Box<List<SymTable>>,
   //maybe needed children: Box<Vec<SymTable>>,
   parent : Option<Box<SymTable>>
}

impl SymTable {
   fn new(parent : Option<Box<SymTable>>) -> SymTable {
      SymTable {
         bindings : HashMap::new(),
         parent   : parent,
         //maybe needed later children : Box::new(Vec::new()), //(List::Nil),
         //sexps    : Sexp::Err("".to_string())
      }
   }

   fn lookup(&self, s : &String) -> Box<Binding> {
      //Sexps::Err("none".to_string())
      //if !self.bindings.contains_key(s)
      let b = self.bindings.get(s);
      match b {
         Some(&x) => box x,
         None => {
            match self.parent {
               None => {
                  syntax_err("Cannot find symbol", 0);
                  box Binding::Normal(Sexps::Err("None".to_string()))
               },
               Some(ref parent) => parent.lookup(s)
            }
         }
      }
   }

   fn eval(&mut self, sexps : &Sexps) -> Sexps {
      match sexps {
         e @ &Sexps::Str(_) => { *e },
         e @ &Sexps::Num(_) => { *e },
         e @ &Sexps::Var(_) => { *e },
         e @ &Sexps::Err(_) => { *e },
         e @ &Sexps::Sub(_) => {
            self.apply(&e)
          kk*/ /*
            //let mut children = Vec::new();
            //let mut first_child : Option<SymTable> = None;

            for subsexp in (*subsexps).iter() {
               let t = SymTable::new(Some(box *self));
               t.eval(subsexp);
               if let None = first_child { first_child = Some(t); }
               else { children.push(t); }
            }
            self.children = Box::new(children);

            if let Some(first) = first_child {
               self.apply(first, self.children)
            } //kk left here
            else {
               Sexps::Err("Cannot eval empty".to_string())
            }*/ /*kk
         },
      }
      //if let Sexps::Sub(box v) = *sexps
   }
   fn apply(&mut self, args: &Sexps) -> Sexps {
      if let Sexps::Sub(args) = *args {
         let first : Sexps = args[0];
         if let Sexps::Var(op) = first {
            if op == "+" { println!("detected +") }
            Sexps::Num(1.3)
         }
         else { syntax_err("first element needs to be symbol", 0); Sexps::Err("non".to_string()) }
      }
      else { syntax_err("apply needs Sub", 0); Sexps::Err("non".to_string()) }
   }
   //fn apply(&mut self, func : &Sexps, args: Option<&Sexps>) -> Sexps {Sexps::Num(5.4)}

   fn run(&mut self, code : &str) -> Sexps {
      let lexemes = lex(code);
      let sexps_opt = parse(&lexemes);

      if let Some(sexps) = sexps_opt {
         self.eval(&sexps)
      }
      else { Sexps::Err(String::from("Couldn't parse code")) }
   }
}
//Sexps::Num(0.3)
*/

fn display_sexps(exp: &Sexps) {
   match *exp {
      Sexps::Str(ref s) => println!("{}", s),
      Sexps::Num(ref n) => println!("{}", n),
      Sexps::Var(ref s) => println!("{}", s),
      Sexps::Err(ref s) => println!("{}", s),
      _                 => println!("bad sexps, cant print")
   }
}

#[derive(Clone)]
enum Sexps {
   Str(String), Num(i64), Var(String), Err(String),
   Sub(Box<List<Sexps>>),
}

struct SymTable {
   bindings : HashMap<String, Sexps>,
   parent : Option<Box<SymTable>>
}

impl SymTable {
   fn new(parent : Option<Box<SymTable>>) -> SymTable {
      SymTable {
         bindings : HashMap::new(),
         parent   : parent,
      }
   }

   fn lookup(&self, s : &String) -> Sexps {
      //Sexps::Err("none".to_string())
      //if !self.bindings.contains_key(s)
      let b = self.bindings.get(s);
      match b {
         Some(x) => x.clone(),
         None => {
            match self.parent {
               None => {
                  syntax_err("Cannot find symbol", 0);
                  Sexps::Err("None".to_string())
               },
               Some(ref parent) => parent.lookup(s)
            }
         }
      }
   }
}

fn eval_sub(exp : &Sexps, table : &mut SymTable) -> Sexps {
   //let Sexps::Sub(x)
   Sexps::Err("error".to_string())
}

fn eval(exp : &Sexps, table : &mut SymTable) -> Sexps {
   match *exp {
      Sexps::Str(_) | Sexps::Num(_) => exp.clone(), //self evaluation
      Sexps::Var(ref s)             => table.lookup(&s.clone()),
      Sexps::Sub(_)                 => eval_sub(exp, table),
      Sexps::Err(ref s)             => Sexps::Err(s.clone()),
   }
}

fn apply() {}

fn run(code : &str) -> Sexps {
   let lexemes = lex(code);
   let exp_opt = parse(&lexemes);

   let mut env = SymTable::new(None);

   if let Some(exp) = exp_opt {
      eval(&exp, &mut env)
   }
   else { Sexps::Err(String::from("Couldn't parse code")) }
}

fn main() {
   let code : &str = "((6 +) (+ (test) 5))";
   //let code : &str = "(hello (+ world) \"string\")";
   //let code : &str = "(hello (world) (yo test) 5)";
   //let code : &str = "(hello (\"world\"\"test1\" + test) \"another \\\"string\")";

   //lex_test();
   //parse_test();
   display_sexps(&run(code));
}

#[allow(dead_code)]
fn parse_test() {
   let code : &str = "'((6 +) (+ (test) 5))";
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

fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sexps::Str(ref s) => { print_nest(&s, deepness, Some("str")) },
      Sexps::Var(ref s) => { print_space(deepness); println!("var: {}", s) },
      Sexps::Num(ref n) => { print_space(deepness); println!("num: {}", n) },
      Sexps::Sub(box ref sexps) => { //box ref sexps
         print_nest("(", deepness, None);
         for x in sexps { print_tree(&x, deepness+4); }
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


