#![allow(dead_code)]
//extern crate err;use err::DEBUG;
//extern crate list; use list::{Cons, cons_map};
//extern crate utils; use utils::{print_space, print_nest};

use err::DEBUG;
use std::boxed::Box;
use list::{Cons, cons_map};
use utils::{print_space, print_nest};
use self::Sexps::*;

#[derive(Debug, PartialEq)]
pub enum Lexeme {
   OpenParen, CloseParen, Str(String), Sym(String), Int(i64), Float(f64), Quote(char)
}

pub type EnvId = usize;

//start game kkgame
use std::sync::mpsc::Sender;
pub type GameObjId = u32;
pub type GameCoord = f32;
pub type GamePoint = [GameCoord; 2];

//Obj(<triangle|square|circle>, <color|pic_path>)
#[derive(Debug, Clone)]
pub enum GameCommand {
   Obj(String, String), Move(GameObjId, GamePoint),
   Rotate(GameObjId, GameCoord),
   Scale(GameObjId, GamePoint), Exit
}
pub type GameCmdSender = Sender<GameCommand>;
//end game

#[derive(Clone, Debug)] //Try to implement copy
pub enum Sexps {
   Str(String), Int(i64), Float(f64), Var(String), Err(String), //Literal(String),
   Sub(Box<Cons<Sexps>>), Lambda(EnvId, String),
   Bool(bool) //Quote(Box<Cons<Sexps>>) //Sub(Box<Vec<Sexps>>)
   //, GameCmd(GameCommand, GameCmdSender) //kkgame addition
}

impl PartialEq for Sexps {
   fn eq(&self, other: &Sexps) -> bool {
      match (self, other) {
         (&Str(ref s1), &Str(ref s2))     => s1 == s2,
         (&Int(ref n1), &Int(ref n2))     => n1 == n2,
         (&Float(ref n1), &Float(ref n2)) => n1 == n2,
         (&Bool(ref b1), &Bool(ref b2))   => b1 == b2,
         _                                => false
      }
   }
}

impl Drop for Sexps {
   fn drop(&mut self) {
      match *self {
         Sexps::Err(ref s) if DEBUG >= 5 => println!("err dropping: {}", s),
         _ if DEBUG >= 7 => println!("sexps going out of scope: {:?}", self),
         _ => {}
      }
   }
}

#[derive(Debug)]
pub enum LexFail {}
#[derive(Debug)]
pub enum EvalFail {}
#[derive(Debug)]
pub enum ParseFail {
   NoStartParen, NoEndParen, ExtraCloseParen, ChildParseFail, BadLexeme
}
pub type ParseResult = Result<Sexps, (ParseFail, usize)>;

#[derive(Debug)]
pub enum RunFail {
   FailParse(ParseFail), FailLex(LexFail), FailEval(EvalFail), UncompleteExp,
}
pub type RunResult = Result<Sexps, (RunFail, usize)>;

pub fn display_run_result(res : &RunResult) {
   match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }
}
pub fn same_type(exp1 : &Sexps, exp2 : &Sexps) -> bool {
   let mut same = false;
   match *exp1 {
      Sexps::Str(..)     => if let Sexps::Str(..) = *exp2 { same = true; },
      Sexps::Int(..)     => if let Sexps::Int(..) = *exp2 { same = true; },
      Sexps::Float(..)   => if let Sexps::Float(..) = *exp2 { same = true; },
      Sexps::Var(..)     => if let Sexps::Var(..) = *exp2 { same = true; },
      Sexps::Err(..)     => if let Sexps::Err(..) = *exp2 { same = true; },
      Sexps::Sub(..)     => if let Sexps::Sub(..) = *exp2 { same = true; },
      Sexps::Lambda(..)  => if let Sexps::Lambda(..) = *exp2 { same = true; },
      Sexps::Bool(..)    => if let Sexps::Bool(..) = *exp2 { same = true; }
      //,Sexps::GameCmd(..) => if let Sexps::GameCmd(..) = *exp2 { same = true; } //kkgame
   }
   same
}
pub fn arg_extractor(exp : &Sexps) -> Option<Vec<Sexps>> {
   let mut ret = Vec::new();

   if let Sexps::Sub(box ref args_) = *exp {
      let mut args =  args_;
      loop {
         if let Cons::Cons(ref arg, ref rest) = *args {
            if let Sexps::Sub(_) = *arg { return None; }
            else {
               ret.push(arg.clone());
               args = rest;
            }
         } else { break; }

      }
      Some(ret)
   }
   else { None }
}

//works well, but we have derive(Debug) on lexemes so we can just debug print them
pub fn print_lexemes(lexemes: &Vec<Lexeme>) {
   for l in lexemes.iter() {
      match *l {
         /*_ => {} empty match */
         Lexeme::OpenParen    => println!("open paren"),
         Lexeme::CloseParen   => println!("close paren"),
         Lexeme::Str(ref s)   => println!("string {}", s),
         Lexeme::Sym(ref s)   => println!("sym {}", s),
         Lexeme::Int(ref n)   => println!("integer {}", n),
         Lexeme::Float(ref n)  => println!("float {}", n),
         Lexeme::Quote(ref c) => println!("quote {}", c)
      }
   }
}
pub fn display_sexps(exp: &Sexps) {
   match *exp {
      Str(ref s)  => println!("{}", s),
      Int(ref n)  => println!("{}", n),
      Float(ref n)=> println!("{}", n),
      Var(ref s)  => println!("{}", s),
      Err(ref s)  => println!("{}", s),
      Lambda(..)  => println!("<lambda>"),
      Bool(x)     => println!("{}", x),
      Sub(..)     => print_compact_tree(exp)
      //,GameCmd(..) => println!("game_cmd") //kkgame
      /*_                 => println!("bad sexps, cant print")*/
   }
}
fn print_compact_tree_helper(t: &Sexps) {
   match *t {
      Sub(box ref sub) => { //box ref sexps
         print!("(");
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_compact_tree_helper(x));
         print!(")");
      },
      _ => { print!("{:?} ", t) }
   }
}
pub fn print_compact_tree(t: &Sexps) {
   print_compact_tree_helper(t);
   println!("");
}
pub fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sub(box ref sub) => { //box ref sexps
         print_nest("(", deepness, None);
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_tree(x, deepness+4));
         print_nest(")", deepness, None);
      },
      _ => { print_space(deepness); println!("{:?}", t) }
   }
}
pub fn cons_to_sexps(c : Cons<Sexps>) -> Sexps { Sub(Box::new(c)) }
pub fn err(s : &str) -> Sexps { Err(s.to_string()) } //or String::from(s)
