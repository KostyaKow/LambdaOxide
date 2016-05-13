//non-generic utilities (langauge specific stuff)

//TODO: print sexps, print lexed, print parsed tree

/*pub fn display_run_result(res : &RunResult) {
   match *res {
      Ok(ref exp) => display_sexps(exp),
      _           => println!("error: {:?}", res)
   }
}*/
use types::QuoteType;
use lexer::Lexeme;
use exp::Sexps;

pub fn quote_to_str(q : QuoteType) -> String {
   match q {
      QuoteType::BackQuote   => "`",
      QuoteType::Q           => "'",
      QuoteType::Comma       => ","
   }.to_string()
}

//works well, but we have derive(Debug) on lexemes so we can just debug print them
pub fn print_lexemes(lexemes: &Vec<Lexeme>) {
   for l in lexemes.iter() {
      match *l {
         /*_ => {} empty match */
         Lexeme::OpenParen    => println!("open paren"),
         Lexeme::CloseParen   => println!("close paren"),
         Lexeme::Str(ref s)   => println!("string {}", s),
         Lexeme::Sym(ref s)   => println!("sym {}", s),
         Lexeme::Int(ref n)   => println!("integer {}", n),
         Lexeme::Float(ref n) => println!("float {}", n),
         Lexeme::Quote(ref q) => println!("quote: {}", quote_to_str(q.clone()))
      }
   }
}
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
fn print_compact_tree_helper(t: &Sexps) {
   use exp::Sexps::*;
   match *t {
      Cons(..) => { //box ref sexps
         print!("(cons ");
         //kk for x in sub { print_tree(&x, deepness+4); }
         //cons_map(sub, |x| print_compact_tree_helper(x));
         print_compact_tree_helper(&t.cons_get_1_fast());
         print_compact_tree_helper(&t.cons_get_2_fast());
         print!(")");
      },
      Array(..)   => { //TODO: fixme
         print!("(list ");
         for i in 0..t.arr_len_fast() {
            print!(" "); print_compact_tree_helper(&t.arr_get_fast(i));
         }
         print!(")");
      }
      _ => { print!("{:?} ", t) }
   }
}
pub fn print_compact_tree(t: &Sexps) {
   print_compact_tree_helper(t);
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

