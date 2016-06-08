
#[derive(Debug, PartialEq, Clone)]
pub enum QuoteType { BackQuote, Comma, Q }

use lexer::Lexeme;
use errors::ErrCode;
pub type Lexemes = Vec<(Lexeme, usize, usize)>;
pub type LexErr = (ErrCode, usize, usize); //char indices
pub type ParseErr = (ErrCode, usize, usize); //lexeme indices

