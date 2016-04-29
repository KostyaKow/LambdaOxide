#![allow(dead_code)]

use utils::vec_eq;
use types::{Lexeme, QuoteType, print_lexemes, Sexps};
use lexer::lex;
use parser::parse;
//use err::PRINT_TESTS;
use main::*;
//use list::Cons;

type LexTestResult = (String, Vec<Lexeme>);

#[test]
fn test_lex() {
   check_lex_test(test_lex_1());
   check_lex_test(test_lex_2());
   check_lex_test(test_lex_3());
   check_lex_test(test_lex_4());
   check_lex_test(test_lex_5());
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
   use types::Lexeme::*;
   let expected = vec![OpenParen, OpenParen, CloseParen, OpenParen, CloseParen, CloseParen];
   return (input, expected);
}
fn test_lex_3() -> LexTestResult {
   //let input = "`(test ,blah '3.5".to_string();
   let input = "`(test ,blah '3.5".to_string();
   use types::Lexeme::*;
   use types::QuoteType::*;
   let expected = vec![Quote(BackQuote), OpenParen, Sym("test".to_string()), Quote(Comma),
                       Sym("blah".to_string()), Quote(Q), Float(3.5)];
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
fn test_lex_5() -> LexTestResult {
   let input = "(print \"hello\")";
   let mut expected = Vec::new();
   expected.push(Lexeme::OpenParen);
   expected.push(Lexeme::Sym("print".to_string()));
   expected.push(Lexeme::Str("hello".to_string()));
   expected.push(Lexeme::CloseParen);
   return (input.to_string(), expected);
}

#[test]
fn test_parse() {
   test_parse_1();
   test_parse_2();
}

fn test_parse_1() {
   let code = "3";
   let lexed = lex(code);

   let parsed = parse(&lexed).unwrap();
   assert_eq!(parsed, Sexps::Int(3));
}

//:Sub(box Cons(:Var(+), box Cons(Sub(Cons(*, Cons(10, Cons(5.1, Nil)))))
fn test_parse_2() {
   /*let code = "(+ 1 (* 10 5.1))";
   let lexed = lex(code);

   let parsed = parse(&lexed).unwrap();

   use types::Sexps::*;
   let plus = Var("+".to_string());
   let n1 = Int(10);
   let n2 = Float(5.1);

   assert_eq!(parsed, Sub(Box::new(Cons(plus, Box::new(Sub(Box::new(Cons(n1, Box::new(Cons(n2, Cons::Nil))))))))));*/
}



#[test]
fn test_run() {
   test_run_fib();
   test_run_church();
   test_floats_ints();
}

fn test_floats_ints() {
   let root = setup_env();
   let mut cmd = "(+ 3.1 1.1)";
   let mut result = run(&root, &cmd).unwrap();
   assert_eq!(result, Sexps::Float(4.2));

   cmd = "(+ 3 0.0)";
   result = run(&root, &cmd).unwrap();
   assert_eq!(result, Sexps::Float(3.0));

   cmd = "(+ 40 2)";
   result = run(&root, &cmd).unwrap();
   assert_eq!(result, Sexps::Int(42));
}

fn test_run_fib() {
   let root = setup_env();

   let mut cmd = "(load \"examples/fib.lo\")";
   let mut result = run(&root, &cmd).unwrap();

   cmd = "(fib 10)";
   result = run(&root, &cmd).unwrap();
   assert_eq!(result, Sexps::Int(55));
}

fn test_run_church() {
   let root = setup_env();
   let mut cmd = "(load \"examples/church.lo\")";
   let mut result = run(&root, &cmd).unwrap();

   cmd = "(fromc (mul (add two one) (mul two two)))";
   result = run(&root, &cmd).unwrap();
   assert_eq!(result, Sexps::Int(12));
}

//TODO: finish me
/*#[test]
fn test_table() {
   let mut x : Env = Env::new();
   let child = x.table_new(0);
   let child2 = x.table_new(child);

   x.table_add(child, "hello", make_sym_table_val(err("test")));
   x.table_add(child2, "test", make_sym_table_val(err("yo")));
   x.table_add(child, "hello3", make_sym_table_val(err("yo2")));

   display_sexps(&get_sym_table_val(x.lookup(child2, "hello")));
}*/

