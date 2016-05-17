
/*
TODO: copy guile with $3 = last eval result
comma = unquote
//BackQuote = quasiquote
//Q = quote
//(+ 3 5) => 8
//'(+ 3 5) => (+ 3 5)
//(car '(f)) => f
//`((+ 1 2) '(+ 1 2) ,(+ 1 2)) => ((+ 1 2) (quote (+ 1 2)) 3)
//(eval '(+ 3 5)) => 8
//(eval (+ 3 5)) => 8
//(eval ''(+ 3 5)) => '(+ 3 5)
//(eval (eval ''(+ 3 5))) => 8
//'''3 => '''3 or (quote (uqote (quote 3)))
//'fsdf 3 => fsdf 3
*/

#[derive(Debug, PartialEq, Clone)]
pub enum QuoteType { BackQuote, Comma, Q }


use lexer::Lexeme;
use gentypes::SizeRange;
use errors::LoResult;

pub type LexResult = LoResult<Vec<(Lexeme, SizeRange)>>;

