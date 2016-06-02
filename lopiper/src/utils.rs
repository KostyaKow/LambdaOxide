//non-generic utilities (loscript specific stuff)

use types::{QuoteType, Lexemes};
use lexer::Lexeme;
use exp::Sexps;
use errors::{ErrInfo, ErrCode};

pub fn quote_to_str(q : QuoteType) -> String {
   match q {
      QuoteType::BackQuote   => "`",
      QuoteType::Q           => "'",
      QuoteType::Comma       => ","
   }.to_string()
}

//works
pub fn print_lexemes(lexemes: &Lexemes) {
   for l in lexemes.iter() {
      let (ref lexeme, ref start, ref end) = *l;
      print!("({}, {})  \t", start, end);

      match *lexeme {
         /*_ => {} empty match */
         Lexeme::OpenParen    => println!("open ("),
         Lexeme::CloseParen   => println!("close )"),
         Lexeme::Str(ref s)   => println!("str {}", s),
         Lexeme::Sym(ref s)   => println!("sym {}", s),
         Lexeme::Int(ref n)   => println!("int {}", n),
         Lexeme::Float(ref n) => println!("float {}", n),
         Lexeme::Quote(ref q) => println!("quote {}", quote_to_str(q.clone()))
      }
   }
}


//TODO: print sexps, print lexed, print parsed tree
//TODO: re-write display_sexps, print_compact_tree, etc.

/*pub fn display_run_result(res : &RunResult) {
   match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }
}*/
//TODO: figure out when I need to print debug, and when not
//display sexps should print real sexps,
//print_tree should debug print with types of Sexps {:?}

pub fn display_sexps(exp: &Sexps) {
   use exp::Sexps::*;
   match *exp {
      Str(ref s)  => println!("{}", s),
      Int(ref n)  => println!("{}", n),
      Float(ref n)=> println!("{}", n),
      Bool(ref b) => println!("{}", b),
      Sym(ref s)  => println!("{}", s),
      Lambda(..)  => println!("<lambda>"),
      Nil         => println!("Nil"),
      Err(..)     => print_compact_tree(exp),

      Cons(..)    => {
         /*print!("Cons: ");
         print_compact_tree(&exp.cons_get_1_fast());
         print_compact_tree(&exp.cons_get_2_fast());*/
         print_compact_tree(exp);
      },
      Array(..)   => { //TODO: fixme
         //print!("Arr: ");
         /*for i in 0..exp.arr_len_fast() {
            print!("   "); print_compact_tree(&exp.arr_get_fast(i));
         }*/
         print_compact_tree(exp);
      },
      Quote(ref q, ref exp) => println!("{}", quote_to_str(q.clone())),
      //_                 => println!("bad sexps, cant print")
   }
}

fn print_compact_tree_helper(t: &Sexps, is_square : bool) {
   use exp::Sexps::*;
   let (o_paren, c_paren) = if is_square { ("[", "]") } else { ("(", ")") };
   let is_square = !is_square;

   match *t {
      Cons(..) => { //box ref sexps
         print!("{}cons ", o_paren);
         //kk for x in sub { print_tree(&x, deepness+4); }
         //cons_map(sub, |x| print_compact_tree_helper(x));
         print_compact_tree_helper(&t.cons_get_1_fast(), is_square);
         print_compact_tree_helper(&t.cons_get_2_fast(), is_square);
         print!("{}", c_paren);
      },
      Array(..) => { //TODO: fixme
         print!("{}arr ", o_paren);
         for i in 0..t.arr_len_fast() {
            if i != 0 { print!(" "); }
            print_compact_tree_helper(&t.arr_get_fast(i), is_square);
         }
         print!("{}", c_paren);
      },
      Err(..) => { print!("{:?} ", t) },
      _ => { print!("{:?}", t) },
      //_ => print_sexps
   }
}

pub fn print_compact_tree(t: &Sexps) {
   print_compact_tree_helper(t, false);
   println!("");
}

/*pub fn print_tree(t: &Sexps, deepness: u8) {
   match *t {
      Sub(box ref sub) => { //box ref sexps
         print_nest("(", deepness, None);
         //kk for x in sub { print_tree(&x, deepness+4); }
         cons_map(sub, |x| print_tree(x, deepness+4));
         print_nest(")", deepness, None);
      },
      _ => { print_space(deepness); println!("{:?}", t) }
   }
}
pub fn cons_to_sexps(c : Cons<Sexps>) -> Sexps { Sub(Box::new(c)) }
pub fn err(s : &str) -> Sexps { Err(s.to_string()) } //or String::from(s)*/


fn is_uncomplete_exp(exp : Sexps) -> bool {
   if let Sexps::Err(box ErrInfo { code : ErrCode::UncompleteExp, ..}) = exp
   { true } else { false }
}




