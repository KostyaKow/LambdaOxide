//range without include parenthesis
fn parse_range_old(lexemes : &Vec<Lexeme>, start : usize, end : usize) -> ParseResult
{
   if start == end {
      let l = parse_lexeme(lexemes[start]);
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
fn parse_helper_old(lexemes : &Vec<Lexeme>) -> ParseResult {
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
}

pub fn parse(lexemes : &Vec<Lexeme>) -> ... {
   if lexemes.len() == 1 {
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
}


