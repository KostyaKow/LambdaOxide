#![feature(box_syntax, box_patterns, slice_patterns)]

#![allow(dead_code)] //kk removme
#![allow(unused_variables)] //kk removeme

//#![feature(slice_patterns)]
//replaced with custom char_at #![feature(str_char)]

use std::collections::HashMap;
use std::boxed::Box;

extern crate lexer;
use lexer::lex;

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
         &Lexeme::Sym(ref s) => { sexps.push(Sexps::Sym(s.to_string())) },
         _ => { syntax_err_lex("Parsing failed: bad lexeme", i as u32) }
      }
      i += 1;
   }

   Some(Sexps::SubSexps(Box::new(sexps)))
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
   OpenParen, CloseParen, Str(String), Sym(String)
}

#[allow(dead_code)]
//#[derive(Copy, Clone)]
enum Sexps {
   Str(String), Num(f64), Sym(String),
   SubSexps(Box<Vec<Sexps>>),
   Err(String)
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

   fn lookup(&self, s : String) -> Sexps { Sexps::Err("none".to_string()) }

   fn eval(&mut self, sexps : &Sexps) -> Sexps {
      match *sexps {
         e @ Sexps::Str(_) => { e },
         e @ Sexps::Num(ref n) => { e },
         e @ Sexps::Sym(ref s) => { e },
         e @ Sexps::Err(ref s) => { e },
         e @ Sexps::SubSexps(ref subsexps) => {
            let mut children = Vec::new();
            let mut first_child : Option<SymTable> = None;

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
            }
         },
      }
      //if let Sexps::SubSexps(box v) = *sexps
   }

   fn apply(&mut self, func : &Sexps, args: List<Sexps>) -> Sexps {
      Sexps::Num(5.4)
   }

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

fn main() {
   let code : &str = "((6 +) (+ (test) 5))";
   //let code : &str = "(hello (+ world) \"string\")";
   //let code : &str = "(hello (world) (yo test) 5)";
   //let code : &str = "(hello (\"world\"\"test1\" + test) \"another \\\"string\")";

   //lex_test();
   //parse_test();
   eval_test(code);
}

#[allow(dead_code)]
fn eval_test(code : &str) {
   let mut sym_table = SymTable::new(None);
   sym_table.run(code);
}

#[allow(dead_code)]
fn parse_test() {
   let code : &str = "((6 +) (+ (test) 5))";
   let lexemes = lex(code);
   let tree_maybe = parse(&lexemes);
   if let Some(tree) = tree_maybe {
      print_tree(&tree, 0);
   }
   else { syntax_err_lex("Parsing failed", 0); }
}

#[allow(dead_code)]
fn lex_test() {
   let code : &str = "(hello (\"world\"\"test1\" + test) \"another \\\"string\")";
   let lexemes = lex(code);
   print_lexemes(&lexemes);
}

fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sexps::Str(ref s) => { print_nest(&s, deepness) },
      Sexps::Sym(ref s) => { print_space(deepness); println!("{}", s) },
      Sexps::Num(ref n) => { print_space(deepness); println!("{}", n) },
      Sexps::SubSexps(box ref sexps) => { //box ref sexps
         print_nest("(", deepness);
         for x in sexps { print_tree(&x, deepness+4); }
         print_nest(")", deepness);
      },
      Sexps::Err(ref s) => { println!("{}", s) }
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
      }
   }
}


