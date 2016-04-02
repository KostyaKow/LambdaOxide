Requires 1.9 nightlies for char_at().

old eval:
      /*if let Sexps::SubSexps(box v) = *sexps {
         match &v[..] {
            [] => Sexps::Err(String::from("Trying to evaluate empty list")),
            [ref x] => self.apply(self.eval(&x), lst_new::<Sexps>()),
            [ref x, ref xs..] => self.apply(self.eval(&x), vec_to_lst::<Sexps>(&hack(&xs))),
         }
      }
      else { *sexps } //we're an atom*/

other old stuff:
   //TODO: unit test
   //println!("{}", slice_str("hello", 1, 3));

   use std::env; use std::io; use std::io::prelude::*;
   use std::io::BufReader; use std::fs::File;

      for arg in env::args() {
         println!("{}", arg);
      }
      let zz = lex(code); for c in zz {println!("{}", c)}

      let a :[i32; 3] = [1, 2, 3];
      println!("{}", a[0]);

      fn lex(code : &str) -> Vec<String> {
         for c in code.chars() {
               lex = String::new();
            lex.push(c);
         if !lex.is_empty() { lexemes.push(lex); }
      }
   */

   //cool lists
   //https://gist.github.com/lovasoa/5260e87e994009ca658a
   //http://rustbyexample.com/custom_types/enum/testcase_linked_list.html


add env to print_sexps

#http://rustbyexample.com/crates/lib.html
#rustc --crate-name list --crate-type lib list.rs
#rustc --crate-name utils --crate-type lib utils.rs
#rustc -L . --crate-name err --crate-type lib err.rs
#rustc -L . --crate-name types --crate-type lib types.rs
#rustc -L . --crate-name lexer --crate-type lib lexer.rs
#rustc -L . --crate-name parser --crate-type lib parser.rs
#rustc -L . lisp.rs


