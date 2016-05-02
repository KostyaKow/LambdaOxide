use list::{Cons, cons, cons_reverse};
use types::*;

//inclusive let i = start; while (i <= end)
fn get_child_sexps(lexemes : &Vec<Lexeme>, start : usize, end : usize)
-> Vec<(usize, usize)>
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

fn parse_lexeme(l : &Lexeme) -> Option<Sexps> {
   use types::Sexps::*;

   let exp = match l {
         &Lexeme::Str(ref s)  => { Str(s.to_string()) },
         &Lexeme::Sym(ref s)  => { Var(s.to_string()) },
         &Lexeme::Int(ref n)  => { Int(*n) },
         &Lexeme::Float(ref n)=> { Float(*n) },
         &Lexeme::Quote(ref q)=> { Quote(q.clone()) }
         _ => { return None; }
   };
   Some(exp)
}

//range without include parenthesis
fn parse_range(lexemes : &Vec<Lexeme>, start : usize, end : usize) -> ParseResult
{
   let mut sub : Cons<Sexps> = Cons::Nil; //: Vec<Sexps> = Vec::new()

   let children = get_child_sexps(lexemes, start, end);
   let mut c_it = 0; //current child
   let mut i = start;
   while i <= end {
      let child_start = c_it < children.len() && children[c_it].0 == i;
      let (c_start, c_end) = if child_start { children[c_it] } else { (0, 0) };

      if child_start {
         let child = parse_range(lexemes, c_start+1, c_end-1);
         if let Ok(c) = child { sub = cons(c, sub); }
         else { return Result::Err((ParseFail::ChildParseFail, c_start)); }
         c_it += 1;
         i = c_end + 1;
         continue;
      }
      //Sexps::Str(String::from("Test"));
      let ref l = lexemes[i];
      let exp_opt = parse_lexeme(l);
      if let Some(exp) = exp_opt { sub = cons(exp, sub); }
      else { return Result::Err((ParseFail::BadLexeme, i)); }

      i += 1;
   }
   Ok(cons_to_sexps(cons_reverse(sub)))
}

fn parse_helper(lexemes : &Vec<Lexeme>) -> ParseResult {
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
      if nestedness < 0 {
         return Result::Err((ParseFail::ExtraCloseParen, i));
      }
   }

   let start; let end;
   if let Some(x) = start_paren { start = x; }
   else { return Result::Err((ParseFail::NoStartParen, 0)) }
   if let Some(x) = end_paren { end = x; }
   else { return Result::Err((ParseFail::NoEndParen, 0)) }

   parse_range(lexemes, start+1, end-1)
}

pub fn parse(lexemes : &Vec<Lexeme>) -> ParseResult {
   if lexemes.len() == 1 {
      if let Some(exp) = parse_lexeme(&lexemes[0]) { Ok(exp) }
      else { Result::Err((ParseFail::BadLexeme, 0)) }
   }
   /*else if lexemes.len() == 2 {
      if let Lexeme::Quote(_) = lexemes[0] {

      }
   }*/
   else { parse_helper(lexemes) }
}
