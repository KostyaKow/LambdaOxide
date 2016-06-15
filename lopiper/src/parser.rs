use oxicloak::{SizeRange, SizeRanges, SharedMut, is_none};
use errors::{ErrInfo, ErrCode, ExecStage, StackInfo, parse_exp_err, parse_err};
use types::{QuoteType, RangeErr, ParseResult};
use lexer::Lexeme;
use exp::Sexps;

//TODO: track ranges

//range_start, range_end, quotes, is_atom
type ChildRange = (usize, usize, Vec<QuoteType>, bool);
type ChildRangesResult = Result<Vec<ChildRange>, RangeErr>;

/*get_child_ranges
   returns starting and ending location of parenthesis
      start and end are inclusive: let i = start; while (i <= end) ..
   collects top-level quotes
   collects top-level atoms
   if nestedness < 0, throws NoStartParen error
   if at the end of loop, nestedness > 0, throws NoEndParen
   TODO: (don't understand what this means) what if we have unmatched close paren, and different nestedness begin and start. Should it be error?*/
fn get_child_ranges(lexemes : &Vec<Lexeme>, range : SizeRange) -> ChildRangesResult {
   let (mut i, mut end) = range;
   let mut nestedness = 0;
   let mut children : Vec<ChildRange> = Vec::new();
   let mut child_start : Option<usize> = None;
   let mut quotes = Vec::new();

   while i <= end {
      match &lexemes[i] { //TODO: do we need the address & thingy?
         &Lexeme::OpenParen => {
            nestedness += 1;
            if nestedness == 1 { child_start = Some(i); }
         },
         &Lexeme::CloseParen => {
            nestedness -= 1;
            if nestedness == 0 {
               children.push((child_start.unwrap(), i, quotes, false));
               quotes = Vec::new();
               child_start = None;
            } else if nestedness < 0 {
               return Err((ErrCode::NoStartParen, end, end));
            }
         },
         &Lexeme::Quote(ref q) => {
            if nestedness == 0 { quotes.push(q.clone()); }
         }
         _ => {
            if nestedness == 0 {
               children.push((i, i, quotes, true));
               quotes = Vec::new();
            }
         }
      }
      i += 1;
   }
   if nestedness > 0 {
      return Err((ErrCode::NoEndParen, child_start.unwrap(), end));
   }
   Ok(children)
}


//TODO: combine parser into 1 function and remove helper
//when passing child/range, don't include parenthesis
//TODO:::::fix parse to not include parenthesis
//returns either Sexps::Err() or parsed body
fn parse_helper(lexemes : &Vec<Lexeme>, child : ChildRange) -> ParseResult {
   let (start, end, quotes_vec, is_atom) = child;
   if is_atom {
      if start != end { //TODO: I don't think we need this
         //let range = Some((start, end)); //TODO: remove this 2 lines
         //return Sexps::err_new(parse_err(ErrCode::BadRange, range));
         return Err((ErrCode::BadRange, start, end));
      }
      let exp_opt = parse_lexeme(&lexemes[start]);
      if is_none(exp_opt.clone()) {
         //TODO: remove //return Sexps::err_new(parse_err(ErrCode::BadLexeme, None));
         return Err((ErrCode::BadLexeme, start, end));
      }
      let mut ret_exp = exp_opt.unwrap();
      for i in (0..quotes_vec.len()).rev() {
         ret_exp = Sexps::new_quote(quotes_vec[i].clone(), ret_exp);
      }
      return Ok(ret_exp);
   }

   let mut sub : Vec<Sexps> = Vec::new();
   let children_ranges_res = get_child_ranges(lexemes, (start, end));
   if let Err(e) = children_ranges_res {
      //TODO: removeme //return Sexps::err_new(e.clone());
      return Err(e);
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
         let child_data = (c_start, c_end, quotes, is_atom);
         let parsed_child_opt = parse_helper(lexemes, child_data);
         if let Ok(parsed_child) = parsed_child_opt {
            sub.push(parsed_child);
         } else {
            //println!("failed child parse: {:?}", parsed_child);
            //TODO: removeme
            //return parse_exp_err(ErrCode::ChildParseFail, Some((c_start, c_end)));
            //return Err(ErrCode::ChildParseFail, c_start, c_end);
            return parsed_child_opt;
         }
         c_it += 1;
         i = c_end + 1;
         continue;
      }
      i += 1; //kk inserted
      continue; //kk inserted
      /*TODO: removeme
      //kk skilling stuff below //you mean skipping dummy?
      //TODO: this seems redundant
      //if atom
      let ref l = lexemes[i];
      let exp_opt = parse_lexeme(&l);
      if let Some(exp) = exp_opt { sub.push(exp); }
      else {
         //TODO: removeme //return parse_exp_err(ErrCode::BadLexeme, None);
         let (start, end, _, _) = children_ranges; //TODO: is this actual range of err?
         return Err((ErrCode::BadLexeme, start, end));
      }
      i += 1;*/
   }
   //quotes for complex expressions
   let mut ret = Sexps::arr_new_from_vec(sub);

   for i in (0..quotes_vec.len()).rev() {
      ret = Sexps::new_quote(quotes_vec[i].clone(), ret);
   }
   Ok(ret)
   //Sexps::arr_new_from_vec(sub)
}

//either returns (Sexps::Array of parsed exp's, true)
//or             (Sexps::Array of Sexps::Err's, false)
//TODO: if we have recursive error arrays, we would have to flatten it out before returning from parse
//pub fn parse(lexemes : &Vec<Lexeme>) -> (Sexps, bool)
pub fn parse(lexemes : &Vec<Lexeme>) -> ParseResult {
   if lexemes.len() == 0 {
      //return (Sexps::arr_new_singleton(parse_exp_err(ErrCode::UncompleteExp, None)), false);
      //return (parse_exp_err(ErrCode::UncompleteExp, None), false);
      return Err((ErrCode::UncompleteExp, 0, 0));
   }

   let childs_ret = get_child_ranges(lexemes, (0, lexemes.len()-1));
   match childs_ret {
      Ok(childs) => {
         let mut ret = Vec::new();
         //TODO: removeme //let mut errs = Vec::new();

         //for child in childs
         for (mut start, mut end, quotes, is_atom) in childs {
            if !is_atom { start += 1; end -= 1; }
            let parsed_child = parse_helper(lexemes, (start, end, quotes, is_atom)); //child);
            /*if let Sexps::Err(ref e) = parsed_child {
               return Err(e.clone());
            }*/
            if let Err(ref e) = parsed_child {
               return Err(e.clone());
            }
            else { ret.push(parsed_child.unwrap()); }
         }

         Ok(Sexps::arr_new_from_vec(ret))
      },
      Err(e) => {
         //(Sexps::arr_new_singleton(Sexps::err_new(e)), false)
         Err(e)
         //(Sexps::err_new(e), false)
      }
   }
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


