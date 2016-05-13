use gentypes::{SizeRange, SizeRanges};
use errors::{LoResult, ErrInfo, ErrCode, ErrStage, lo_fail};
use exp::Sexps;

pub type ParseResult = LoResult<Sexps>;

//range_start, range_end, quotes, atom
type Child = (usize, usize, Vec<QuoteType>, bool);

type ChildExpResult = LoResult<Vec<Child>>;
//TODO: what if unmatched close paren, and different nestedness begin and start. Should it be error?
//inclusive let i = start; while (i <= end)
fn get_child_exps(lexemes : &Vec<Lexeme>, range : SizeRange) -> ChildExpResult {
   let mut (start, end) = range;
   let mut nestedness = 0;
   let mut children : Vec<Child> = Vec::new();
   let mut child_start : Option<usize> = None;
   let mut quotes = Vec::new();

   while start <= end {
      match &lexemes[start] { //TODO: do we need the address?
         &Lexeme::OpenParen => {
            nestedness += 1;
            if nestedness == 1 { child_start = Some(start); }
         },
         &Lexeme::CloseParen => {
            nestedness -= 1;
            if nestedness == 0 {
               children.push((child_start.unwrap(), start, quotes, false));
               quotes = Vec::new();
               child_start = None;
            } else if nestedness < 0 {
               return lo_fail(parse_err(ErrCode::ExtraCloseParen, lexemes, start, None));
            }
         },
         &Lexeme::Quote(q) {
            if nestedness == 0 { quotes.push(q); }
         }
         _ => {
            if nestedness == 0 {
               children.push((start, start, quotes, true));
               quotes = Vec::new();
            }
         }
      }
      start += 1;
   }
   if nestedness > 0 {
      return lo_fail(parse_err(ErrCode::NoEndParen, lexemes, end, None));
   }
   Ok(children)
}

//range without include parenthesis
fn parse_range(lexemes : &Vec<Lexeme>, start : usize, end : usize) -> ParseResult
{
   if start == end {
      let l = parse_lexeme(lexemes[start]);
      //TODO: **> 'fsdf 3
      if let Some(exp) = parse_lexeme(&lexemes[start]) {
         return Ok(exp);
      } else {
         return parse_err(ErrCode::BadLexeme, lexemes, start, None);
      }
   }

   let mut sub : Cons<Sexps> = Cons::Nil; //: Vec<Sexps> = Vec::new()

   let children = child_exp(lexemes, (start, end));
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

/*fn parse_helper(lexemes : &Vec<Lexeme>) -> ParseResult {
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
         return lo_fail(parse_err(ErrCode::ExtraCloseParen, 0)
         return Result::Err((ParseFail::ExtraCloseParen, i));
      }
   }

   let start; let end;
   if let Some(x) = start_paren { start = x; }
   else { return Result::Err((ParseFail::NoStartParen, 0)) }
   if let Some(x) = end_paren { end = x; }
   else { return Result::Err((ParseFail::NoEndParen, 0)) }

   parse_range(lexemes, start+1, end-1)
}*/

fn parse_helper(lexemes : &Vec<Lexeme>, child : Child) -> Sexps {
   let (start, end, )
}

//either returns (Sexps::Array of parsed exp's, true)
//or (Sexps::Array of Sexps::Err's, false)
pub fn parse(lexemes : &Vec<Lexeme>) -> (Sexps, bool) {
   let childs_ret = get_child_exps(lexemes, 0, lexemes.len());
   if let Ok(childs) = childs_ret {
      let ret = Sexps::arr_new();
      let errs = Sexps::arr_new();

      for child in childs {
         let parsed_child = parse_helper(lexemes, child);
         ret.push(parsed
         if let Sexps::Err(e) = parsed_child {
         }
      }
      ret
   } else if let Err(e) = childs_ret {
      Sexps::arr_new_singleton(Sexps::err_new(e))
   }
   /*if lexemes.len() == 1 {
      if let Some(exp) = parse_lexeme(&lexemes[0]) {
         Ok(exp)
      } else {
         //Do we need lexemes range and char_i?
         lo_fail(parse_err(ErrCode::BadLexeme, lexemes, 0, None))
      }
   }
   else if lexemes.len() == 2 { //TODO: actually do quote stuff
      if let Lexeme::Quote(q) = lexemes[0] &&
         let Some(e) = parse_lexeme(&lexemes[1])
      {
         Ok(Sexps::Quote(q, e))
      }
      else {
         //TODO: do we need: ei.char_i = Some(0)
         lo_fail(parse_err(ErrCode::Fail2Lexemes, lexemes, 0, Some((0, 1))))
      }
   }
   else {
      let childs = child_exp(lexemes, (0, lexemes.len()));
      if childs.len() == 0 {
         parse_err(ErrCode::UnCompleteExp, lexemes, 0, Some((0, lexemes.len())));
      }
      for (start, end) in childs {
         parse_range(lexemes, start, end)
      }
      //parse_helper(lexemes)
   }
*/
}


fn parse_lexeme(l : &Lexeme) -> Option<Sexps> {
   use exp::Sexps::*;

   let exp = match l {
         &Lexeme::Str(ref s)  => { Str(s.to_string()) },
         &Lexeme::Sym(ref s)  => { Sum(s.to_string()) },
         &Lexeme::Int(ref n)  => { Int(*n) },
         &Lexeme::Float(ref n)=> { Float(*n) },
         //&Lexeme::Quote(ref q)=> { Quote(q.clone()) } //TODO: ??
         _ => { return None; }
   };
   Some(exp)
}


