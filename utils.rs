
pub fn get_char_ranges(code : &str) -> Vec<(usize, usize)> {
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
pub fn slice_str(s: &str, start: usize, end: usize) -> String {
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

pub fn syntax_err(s: &str, char_loc: u32) {
   println!("error at charachter {}: {}", char_loc, s);
}
pub fn syntax_err_lex(s: &str, lex_num: u32) {
   println!("error at lexeme {}: {}", lex_num, s);
}
pub fn internal_err(s: &str) {
   println!("internal error: {}", s);
}
pub fn print_space(n: u8) {
   let mut i = 0;
   while i < n { print!(" "); i += 1; }
}
pub fn print_nest(s: &str, n: u8) {
   print_space(n); println!("{}", s);
}
pub fn char_at(code : &str, n : usize) -> Option<char> {
    for (i, c) in code.chars().enumerate() {
        if i == n { return Some(c) }
    }
    return None
}
