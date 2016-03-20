
fn lex(code : &str) -> Vec<Lexeme> {
   let mut lexemes : Vec<Lexeme> = Vec::new();

   let mut sym_collector = String::new();
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

         if collect && !sym_collector.is_empty() {
            lexemes.push(Lexeme::Sym(sym_collector));
            sym_collector = String::new();
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
            _   => sym_collector.push(c)
         }
      }
      i += 1;
   }

   lexemes
}

fn get_char_ranges(code : &str) -> Vec<(usize, usize)> {
   let mut ranges : Vec<(usize, usize)> = Vec::new();

   let mut start_quote : Option<usize> = None;
   let mut ignore_next_quote = false;

   for (i, c) in code.chars().enumerate() {
      if c == '"' {
         match start_quote {
            //if we have start
            Some(start) if !ignore_next_quote => {
               ranges.push((start, i));
               start_quote = None;
            }
            None if !ignore_next_quote => {
               start_quote = Some(i);
            }
            _ => {}
         }
      }
      if c == '\\' { ignore_next_quote = true; }
      else { ignore_next_quote = false; }
   }

   if let Some(x) = start_quote { syntax_err("unterminated quote", x as u32); }

   ranges
}

//replace with build-in
//slice_str("hello", 1, 3) => "ell"
fn slice_str(s: &str, start: usize, end: usize) -> String {
   let mut sub: String = String::new();
   let mut started: bool = false;

   if start >= end { internal_err("slice_str: start>=end"); }
   if end >= s.len() {  internal_err("slice_str: end >= string end"); }

   for (i, c) in s.chars().enumerate() {
      if i >= end+1 { return sub; }
      if started { sub.push(c); continue; }
      if i >= start { started = true; sub.push(c); }
   }
   sub
}


