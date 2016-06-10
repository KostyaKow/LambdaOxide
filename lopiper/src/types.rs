
#[derive(Debug, PartialEq, Clone)]
pub enum QuoteType { BackQuote, Comma, Q }

use lexer::Lexeme;
use errors::ErrCode;
use exp::Sexps;

pub type Lexemes = Vec<(Lexeme, usize, usize)>;
pub type RangeErr = (ErrCode, usize, usize); //lexeme indices for parsing error, and char indices for lexing error
//pub type LexErr = ParseErr; //char indices
//pub type ParseErr = (ErrCode, usize, usize); //lexeme indices

pub type LexResult = Result<Lexemes, RangeErr>;
pub type ParseResult = Result<Sexps, RangeErr>;
//pub type ParseStrResult = Result<(Sexps, Lexemes), RangeErr>;

