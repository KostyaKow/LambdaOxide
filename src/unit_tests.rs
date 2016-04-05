
use utils::vec_eq;
use types::{Lexeme, print_lexemes};
use lexer::lex;
use err::PRINT_TESTS;

#[test]
fn test_lex() {
   let mut s = "(+ 3 5 0.1 -5 -5.0";
   //let mut s = "-5 -5.0";

   let lexed = lex(s);

   let mut good = Vec::new();
   good.push(Lexeme::Sym("+".to_string()));
   good.push(Lexeme::Int(3));
   good.push(Lexeme::Float(0.1));
   good.push(Lexeme::Int(-5));
   good.push(Lexeme::Float(-5.0));

   if !vec_eq(&good, &lexed) {
      print_lexemes(&lexed);
      panic!("bad lexemes test");
   }
   //assert!(vec_eq(good, lexed));
   //assert_eq!(iter.next(),
   //panic!("fail");
}
