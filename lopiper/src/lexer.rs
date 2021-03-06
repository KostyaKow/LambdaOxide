use types::{QuoteType, Lexemes, RangeErr, LexResult};
use errors::{ErrInfo, ErrCode, ExecStage, StackInfo};
use oxicloak::*;

#[derive(Debug, PartialEq, Clone)]
pub enum CommentType { Simple, Extended }

#[derive(Debug, PartialEq, Clone)]
pub enum Lexeme {
   OpenParen, CloseParen, Sym(String), Quote(QuoteType),
   Int(i64), Float(f64), Str(String), Comment(String, CommentType)
}

//Special is either Comment or String
#[derive(Clone)]
enum BlockType { SimpleComment, ExtendedComment, Str }
type BlockRange = Vec<(usize, usize, BlockType)>;

//Ok(range-of-blocks), Err((type, start-highlight, end-highlight))
//TODO: priority of strings vs comments, comment syntax in string, string in comments
fn get_block_blk_ranges(code : &str) -> Result<BlockRange, RangeErr> {
   let mut blk_ranges : BlockRange = Vec::new();

   let mut cmnt_start = None;
   let mut str_start = None;
   let mut multi_first_char = false;
   let mut escape = false;

   //index in code of current line start, (length of previous lines)
   let mut line_start = 0;

   let lines = code.split('\n');

   for line in lines {
      let line_len = line.len();
      let line_end = line_start + line_len;

      for (i, c) in line.chars().enumerate() {
         let real_i = line_start + i; //index into code of current char

         //TODO: only for strings???
         if escape { escape = false; continue; }

         match c {
            '\\' if !is_none(str_start) => {
               escape = true;
            },
            '"' if is_none(cmnt_start) => {
               if is_none(str_start) {
                  str_start = Some(real_i);
               } else {
                  blk_ranges.push((str_start.unwrap(), real_i, BlockType::Str));
                  str_start = None;
               }
            },
            ';' if is_none(cmnt_start) && is_none(str_start) => {
               blk_ranges.push((real_i, line_end, BlockType::SimpleComment));
               break;
            },
            '|' if is_none(str_start) => {
               if is_none(cmnt_start) && multi_first_char {
                  cmnt_start = Some(real_i-1);
               } else {
                  multi_first_char = true;
                  continue;
               }
            },
            '#' if is_none(str_start) => {
               if is_none(cmnt_start) { multi_first_char = true; continue; }
               else if multi_first_char {
                  let t = BlockType::ExtendedComment;
                  blk_ranges.push((cmnt_start.unwrap(), real_i, t));
                  cmnt_start = None;
               }
            },
            _ => {}
         }

         multi_first_char = false;
      }
      line_start = line_end;
   }

   if let Some(start) = cmnt_start {
      Err((ErrCode::UnterminatedComment, start, code.len()-1))
   } else if let Some(start) = str_start {
      Err((ErrCode::UnterminatedQuote, start, code.len()-1))
   } else {
      Ok(blk_ranges)
   }
}

pub fn lex(code : &str) -> LexResult {
   /*TODO: removeme let x = get_special_blk_ranges(code);
   println!("{:?}", x);*/

   use oxicloak::{char_at, char_at_fast, contains, slice_str};

   let mut lexemes : Lexemes = Vec::new();
   let mut col = String::new(); //symbol collector

   //range of comment/string blocks
   let blk_range_opt = get_block_blk_ranges(code);
   if let Err(lex_err) = blk_range_opt { return Err(lex_err); }

   let blk_ranges = blk_range_opt.unwrap();
   let mut r_it = 0; //current string/comment range
   let mut i = 0;
   let mut collect_start = 0; let mut collect_end = 0;

   while i < code.len() {
      //if haven't went through all blocks, and i is beginning of block
      let is_blk_start = r_it < blk_ranges.len() && blk_ranges[r_it].0 == i;
      let (blk_start, blk_end, blk_type) = if is_blk_start {
         blk_ranges[r_it].clone()
      } else { (0, 0, BlockType::Str) };

      //if current character c is string or special
      //character, then push previously collected
      let c = char_at_fast(code, i);
      //TODO: maybe manual comparison (c == ' ' || c == '(')
      let special_chars = vec![' ', '(', ')', '\'', '`', ',', '[', ']'];
      let is_special = is_blk_start || contains(c, special_chars);

      if is_special && !col.is_empty() { //push float, int and sym
         if let Some(lexeme) = collect_sym(&col) {
            lexemes.push((lexeme, collect_start, collect_end));
         } else { return Err((ErrCode::MisformedNum, collect_start, collect_end)); }
         col = String::new();
      }
      if is_blk_start { //push string if we have one
         let l = match blk_type {
            BlockType::SimpleComment => {
               let slice = slice_str(code, blk_start+1, blk_end-1);
               Lexeme::Comment(slice, CommentType::Simple)
            },
            BlockType::ExtendedComment => {
               let slice = slice_str(code, blk_start+2, blk_end-2);
               Lexeme::Comment(slice, CommentType::Extended)
            },
            BlockType::Str => {
               Lexeme::Str(slice_str(code, blk_start+1, blk_end-1))
            }
         };
         lexemes.push((l, blk_start, blk_end));
         i = blk_end + 1; //TODO: maybe continue
         r_it += 1; //next string range
      }
      if let Some(c) = char_at(code, i) {
         match c {
            ',' | '`' | '\'' => {
               let q = Lexeme::Quote(char_to_quote(c).unwrap());
               lexemes.push((q, i, i));
            },
            '(' | '[' => lexemes.push((Lexeme::OpenParen, i, i)),
            ')' | ']' => lexemes.push((Lexeme::CloseParen, i, i)),
            //TODO: why do we have to i-=1
            '"' | '#' | ';' => i-=1, //gets triggered if you type in """" in repl
            ' ' => {}, //skip
            _   => {
               if col.is_empty() { collect_start = i; collect_end = i; }
               else { collect_end = i; }
               col.push(c);
            }
         }
      }
      i += 1;
   }

   if !col.is_empty() {
      if let Some(lexeme) = collect_sym(&col) {
         lexemes.push((lexeme, collect_start, collect_end));
      } else {
         return Err((ErrCode::MisformedNum, collect_start, collect_end));
      }
   }

   Ok(lexemes)
   //if lexemes.len() == 0 { Err((ErrCode::UncompleteExp, 0, 0)) }
   //else { Ok(lexemes) }
}

//TODO replace to_float, to_int with this
//pub fn get_str_type(s : &str) -> LexemeType { }
fn collect_sym(col : &str) -> Option<Lexeme> {
   if is_int(&col) {
      Some(Lexeme::Int(from_str(col).unwrap()))
   } else if is_float(col) {
      Some(Lexeme::Float(from_str(col).unwrap()))
   } else {
      if is_number_like(col) { None }
      else { Some(Lexeme::Sym(col.to_string())) }
   }
}

fn char_to_quote(c : char) -> Option<QuoteType> {
   match c {
      '`'   => Some(QuoteType::BackQuote),
      '\''  => Some(QuoteType::Q),
      ','   => Some(QuoteType::Comma),
      _     => None
   }
}

