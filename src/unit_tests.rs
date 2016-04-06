#![allow(dead_code)]

use utils::vec_eq;
use types::{Lexeme, print_lexemes};
use lexer::lex;
use err::PRINT_TESTS;

type LexTestResult = (String, Vec<Lexeme>);

#[test]
fn test_lex() {
   check_lex_test(test_lex_1());
   check_lex_test(test_lex_2());
   check_lex_test(test_lex_3());
   check_lex_test(test_lex_4());

   //assert!(vec_eq(expected, lexed));
   //assert_eq!(iter.next(),
   //panic!("fail");

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
   let input = "3.5".to_string();
   let expected = vec![Lexeme::Float(3.5)];
   return (input, expected);
}
fn test_lex_2() -> LexTestResult {
   let input = "(() ())".to_string();
   let expected = vec![Lexeme::OpenParen, Lexeme::OpenParen, Lexeme::CloseParen, Lexeme::OpenParen, Lexeme::CloseParen, Lexeme::CloseParen];
   return (input, expected);
}
fn test_lex_3() -> LexTestResult {
   //let input = "`(test ,blah '3.5".to_string();
   let input = "`(test ,blah '3.5".to_string();

   let expected = vec![Lexeme::Quote('`'), Lexeme::OpenParen, Lexeme::Sym("test".to_string()), Lexeme::Quote(','), Lexeme::Sym("blah".to_string()), Lexeme::Quote('\''), Lexeme::Float(3.5)];
   return (input, expected);
}
fn test_lex_4() -> LexTestResult {
   let input = "(+ 3 5 0.1 -5 -5.0";
   //let mut s = "-5.0";

   let mut expected = Vec::new();
   expected.push(Lexeme::OpenParen);
   expected.push(Lexeme::Sym("+".to_string()));
   expected.push(Lexeme::Int(3));
   expected.push(Lexeme::Int(5));
   expected.push(Lexeme::Float(0.1));
   expected.push(Lexeme::Int(-5));
   expected.push(Lexeme::Float(-5.0));

   return (input.to_string(), expected);
}

