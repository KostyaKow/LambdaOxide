
use utils::vec_eq;
use types::{Lexeme, print_lexemes};
use lexer::lex;
use err::PRINT_TESTS;

type LexTestResult = (String, Vec<Lexeme>);

#[test]
fn test_lex() {
   check_lex_test(test_lex_1());
}

fn check_lex_test(r : LexTestResult) {
   let (input, expected) = r;

   let lexed = lex(&input);

   if !vec_eq(&expected, &lexed) {
      print_lexemes(&lexed);
      panic!("failed lexeme test 1");
   }
}

fn test_lex_1() -> LexTestResult {
   let input = "(+ 3 5 0.1 -5 -5.0";
   //let mut s = "-5.0";

   let mut expected = Vec::new();
   expected.push(Lexeme::Sym("+".to_string()));
   expected.push(Lexeme::Int(3));
   expected.push(Lexeme::Float(0.1));
   expected.push(Lexeme::Int(-5));
   expected.push(Lexeme::Float(-5.0));

   return (input.to_string(), expected);
   //assert!(vec_eq(expected, lexed));
   //assert_eq!(iter.next(),
   //panic!("fail");
}
