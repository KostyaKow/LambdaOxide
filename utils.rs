
mod utils {
   fn syntax_err(s: &str, char_loc: u32) {
      println!("error at charachter {}: {}", char_loc, s);
   }
   fn syntax_err_lex(s: &str, lex_num: u32) {
      println!("error at lexeme {}: {}", lex_num, s);
   }
   fn internal_err(s: &str) {
      println!("internal error: {}", s);
   }
   fn print_space(n: u8) {
      let mut i = 0;
      while i < n { print!(" "); i += 1; }
   }
   fn print_nest(s: &str, n: u8) {
      print_space(n); println!("{}", s);
   }
   fn char_at(code : &str, n : usize) -> Option<char> {
       for (i, c) in code.chars().enumerate() {
           if i == n { return Some(c) }
       }
       return None
   }
}

