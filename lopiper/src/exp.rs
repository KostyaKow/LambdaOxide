#![feature(box_patterns)]


#[derive(Debug, PartialEq, Clone)]
pub enum QuoteType {
   BackQuote, Comma, Q
}

#[derive(Debug, PartialEq)]
pub enum Lexeme {
   OpenParen, CloseParen, Str(String), Sym(String), Int(i64), Float(f64), Quote(QuoteType)
}

pub enum Sexps {
   Str(String), Int(i64), Float(f64), Bool(bool),
   Var(String), Lambda(String),
   Cons(Box<Sexps>, Box<Sexps>), List(Vec<Sexps>)
}

fn modify(exp: &mut Sexps, a : Sexps) {
   if let Sexps::List(ref mut v) = *exp {
      v[0] = a;
   }
}

fn main() {
   let n1 = Sexps::Str("test".to_string());
   let n2 = Sexps::Int(5);

   let mut c = Vec::new();
   c.push(n1);
   c.push(n2);

   let mut s = Sexps::Cons(c);
   modify(&mut s, Sexps::Int(10));
}


