use gentypes::{SizeRange, SizeRanges};
use errors::{LoResult, ErrInfo, ErrCode, ErrStage, lo_fail, parse_exp_err, parse_err};
use types::QuoteType;
use lexer::Lexeme;
use exp::Sexps;

pub type ParseResult = LoResult<Sexps>;

//range_start, range_end, quotes, atom
type ChildRange = (usize, usize, Vec<QuoteType>, bool);
type ChildRangesResult = LoResult<Vec<ChildRange>>;

/*get_child_ranges
   returns starting and ending location of parenthesis
      start and end are inclusive: let i = start; while (i <= end) ..
   collects top-level quotes
   collects top-level atoms
   if nestedness < 0, throws ExtraCloseParen error
   if at the end of loop, nestedness > 0, throws NoEndParen
   TODO: (don't understand what this means) what if we have unmatched close paren, and different nestedness begin and start. Should it be error?*/
fn get_child_ranges(lexemes : &Vec<Lexeme>, range : SizeRange) -> ChildRangesResult {
   let (mut start, mut end) = range;
   let mut nestedness = 0;
   let mut children : Vec<ChildRange> = Vec::new();
   let mut child_start : Option<usize> = None;
   let mut quotes = Vec::new();

   while start <= end {
      match &lexemes[start] { //TODO: do we need the address & thingy?
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
         &Lexeme::Quote(ref q) => {
            if nestedness == 0 { quotes.push(q.clone()); }
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


//when passing child/range, don't include parenthesis
//TODO:::::::::::::::::::::::fix parse to not include parenthesis
//returns either Sexps::Err() or parsed body
fn parse_helper(lexemes : &Vec<Lexeme>, child : ChildRange) -> Sexps {
   use genutils::is_none;
   let (start, end, quotes_vec, is_atom) = child;
   if is_atom {
      if start != end { //TODO: I don't think we need this
         return parse_exp_err(ErrCode::BadRange, lexemes, start, Some((start, end)));
      }
      let exp_opt = parse_lexeme(&lexemes[start]);
      if is_none(exp_opt.clone()) {
         return parse_exp_err(ErrCode::BadLexeme, lexemes, start, None);
      }
      let mut ret_exp = exp_opt.unwrap();
      for i in (0..quotes_vec.len()).rev() {
         ret_exp = Sexps::quote_new(quotes_vec[i].clone(), ret_exp);
      }
      return ret_exp;
   }

   let mut sub : Vec<Sexps> = Vec::new();
   let children_ranges_res = get_child_ranges(lexemes, (start, end));
   if let Err(e) = children_ranges_res {
      return Sexps::err_new_box(e.clone());
   }
   let children_ranges = children_ranges_res.unwrap();

   //TODO: rename children_ranges into ranges
   let mut c_it = 0; //current child
   let mut i = start;
   while i <= end {
      let child_start = c_it < children_ranges.len() && children_ranges[c_it].0 == i;
      //println!("child start? {}", child_start);
      if child_start {
         let (mut c_start, mut c_end, quotes, is_atom) = children_ranges[c_it].clone();
         if !is_atom {
            c_start += 1; c_end -= 1;
         }
         let parsed_child = parse_helper(lexemes, (c_start, c_end, quotes, is_atom));
         if parsed_child.is_err() {
            println!("failed child parse: {:?}", parsed_child);
            return parse_exp_err(ErrCode::ChildParseFail, lexemes, c_start, Some((c_start, c_end)));
         }
         sub.push(parsed_child);
         c_it += 1;
         i = c_end + 1;
         continue;
      }
      i += 1; //kk inserted
      continue; //kk inserted
      //kk skilling stuff below

      //TODO: this seems redundant
      //if atom
      let ref l = lexemes[i];
      let exp_opt = parse_lexeme(&l);
      if let Some(exp) = exp_opt { sub.push(exp); }
      else { return parse_exp_err(ErrCode::BadLexeme, lexemes, i, None); }
      i += 1;
   }
   //quotes for complex expressions
   let mut ret = Sexps::arr_new_from_vec(sub);


   for i in (0..quotes_vec.len()).rev() {
      ret = Sexps::quote_new(quotes_vec[i].clone(), ret);
   }
   ret
   //Sexps::arr_new_from_vec(sub)
}

//either returns (Sexps::Array of parsed exp's, true)
//or             (Sexps::Array of Sexps::Err's, false)
//TODO: if we have recursive error arrays, we would have to flatten it out before returning from parse
pub fn parse(lexemes : &Vec<Lexeme>) -> (Sexps, bool) {
   if lexemes.len() == 0 {
      return (Sexps::arr_new_singleton(parse_exp_err(ErrCode::UncompleteExp, lexemes, 0, None)), false);
   }

   let childs_ret = get_child_ranges(lexemes, (0, lexemes.len()-1));
   if let Ok(childs) = childs_ret {
      let mut ret = Vec::new();
      let mut errs = Vec::new();

      //for child in childs
      for (mut start, mut end, quotes, is_atom) in childs {
         if !is_atom { start += 1; end -= 1; }
         let parsed_child = parse_helper(lexemes, (start, end, quotes, is_atom)); //child);
         if let Sexps::Err(ref e) = parsed_child { errs.push(parsed_child.clone()); }
         else { ret.push(parsed_child); }
      }

      let good = errs.len() == 0;
      (Sexps::arr_new_from_vec(if good { ret } else { errs }), good)
   } else if let Err(e) = childs_ret {
      (Sexps::arr_new_singleton(Sexps::err_new_box(e)), false)
   } else { (Sexps::Nil, false) } //TODO: fixme
}

fn parse_lexeme(l : &Lexeme) -> Option<Sexps> {
   use exp::Sexps::*;

   let exp = match l {
         &Lexeme::Str(ref s)  => { Str(s.to_string()) },
         &Lexeme::Sym(ref s)  => { Sym(s.to_string()) },
         &Lexeme::Int(ref n)  => { Int(*n) },
         &Lexeme::Float(ref n)=> { Float(*n) },
         //&Lexeme::Quote(ref q)=> { Quote(q.clone()) } //TODO: ??
         _ => { return None; }
   };
   Some(exp)
}


