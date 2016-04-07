use list::{Cons, cons, cons_reverse};
use types::*; //{Sexps, ParseFail, ParseResult, Lexeme, err, cons_to_sexps}; //*;
use err::{internal_err, debug_p};

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
         else { return Result::Err((ParseFail::CHILD_PARSE_FAIL, c_start)); }
         c_it += 1;
         i = c_end + 1;
         continue;
      }
      //Sexps::Str(String::from("Test"));
      let ref l = lexemes[i];
      use types::Sexps::*;
      match l {
         &Lexeme::Str(ref s)  => { sub = cons(Str(s.to_string()), sub) },
         &Lexeme::Sym(ref s)  => { sub = cons(Var(s.to_string()), sub) },
         &Lexeme::Int(ref n)  => { sub = cons(Int(*n), sub) },
         &Lexeme::Float(ref n)=> { sub = cons(Float(*n), sub) },
         _ => {
            return Result::Err((ParseFail::BAD_LEXEME, i));
            sub = cons(err("fail"), sub)
         }
      }
      i += 1;
   }
   Ok(cons_to_sexps(cons_reverse(sub)))
}

pub fn parse(lexemes : &Vec<Lexeme>) -> ParseResult {
   let mut start_paren : Option<usize> = None;
   let mut end_paren : Option<usize> = None;
   let mut nestedness : i32 = 0;

   let place_extra_paren = -1;

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
      if nestedness < 0 { return Result::Err((ParseFail::EXTRA_CLOSE_PAREN, i)); }
   }

   let mut start = 0;
   let mut end = 0;

   if let Some(x) = start_paren { start = x; }
   else { return Result::Err((ParseFail::NO_START_PAREN, 0)) }
   if let Some(x) = end_paren { end = x; }
   else { return Result::Err((ParseFail::NO_END_PAREN, 0)) }

   parse_range(lexemes, start+1, end-1)
}


fn parse2(lexemes : &Vec<Lexeme>) -> ParseResult {
   Result::Err((ParseFail::BAD_LEXEME, 0))
}
