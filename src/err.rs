//default development debug = 5, shipping = 0 or 1
pub static DEBUG: u8 = 0; //0 = None, 1 = minimum, 10 = max
//syntax_err, syntax_err_lex, internal_err >=1
//macro or not macro >=2
//beginning/end paren >=6
//dropping Sexps err debug only for >=3
//dropping Sexps all debug >=7

pub fn debug_p(min_lvl: u8, s : &str) {
   if DEBUG >= min_lvl { println!("{}", s) }
}

pub fn internal_err(s: &str) {
   if DEBUG > 0 { println!("internal error: {}", s); }
}
