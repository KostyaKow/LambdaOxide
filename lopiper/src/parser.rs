use gentypes::{SizeRange, SizeRanges};
use errors::{LoResult, ErrInfo, ErrCode, ErrStage, lo_fail};
use exp::Sexps;

pub type ParseResult = LoResult<Sexps>;

//range_start, range_end, quotes, atom
type Child = (usize, usize, Vec<QuoteType>, bool);

type ChildExpResult = LoResult<Vec<Child>>;

//TODO: what if we have unmatched close paren, and different nestedness begin and start. Should it be error?
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


//child/range doesn't include parenthesis (TODO: is this comment still relavant?)
//either Sexps::Err() or parsed body
fn parse_helper(lexemes : &Vec<Lexeme>, child : Child) -> Sexps {
   use genutils::is_none;
   let (start, end, quotes_vec, is_atom) = child;
   if is_atom {
      if start != end {
         return parse_exp_err(ErrCode::BadRange, lexemes, start, (start, end));
      }

      let exp_opt = parse_lexeme(&lexemes[start]);
      if is_none(exp_opt) {
         return parse_exp_err(ErrCode::BadLexeme, lexemes, start, None);
      }
      let mut ret_exp = exp_opt.unwrap();
      for i in 0..quotes_vec.len() {
         ret_exp = Sexps::quote_new(quotes_vec[i], ret_exp);
      }
      return ret_exp;
   }

   let mut sub : Vec<Sexps> = Vec::new();
   let children = get_child_exps(lexemes, start, end);
   let mut c_it = 0; //current child
   let mut i = start;
   while i <= end {
      let child_start = c_it < children.len() && children[c_it].0 == i;
      let (c_start, c_end) = if child_start { children[c_it] } else { (0, 0) };

      if child_start {
         let child = parse_helper(lexemes, c_start+1, c_end-1);
         if child.is_err() {
            return parse_exp_err(ErrCode::ChildParseFail, lexemes, c_start, (c_start, c_end));
         }
         children.push(child);
         c_it += 1;
         i = c_end + 1;
         continue;
      }
      //if atom
      let ref l = lexemes[i];
      let exp_opt = parse_lexeme(l);
      if let Some(exp) = exp_opt { children.push(exp); }
      else { return parse_exp_err(ErrCode::BadLexeme, lexemes, i, None); }
      i += 1;
   }
}

//either returns (Sexps::Array of parsed exp's, true)
//or (Sexps::Array of Sexps::Err's, false)
pub fn parse(lexemes : &Vec<Lexeme>) -> (Sexps, bool) {
   let childs_ret = get_child_exps(lexemes, 0, lexemes.len());
   if let Ok(childs) = childs_ret {
      let ret = Vec::new();
      let errs = Vec::new();

      for child in childs {
         let parsed_child = parse_helper(lexemes, child);
         if let Sexps::Err(e) = parsed_child { errs.push(parsed_child); }
         else { ret.push(parsed_child); }
      }

      let good = errs.len() == 0;
      (Sexps::arr_new_from_vec(if good { ret } else { errs }), good)
   } else if let Err(e) = childs_ret {
      (Sexps::arr_new_singleton(Sexps::err_new(e)), false)
   }
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


