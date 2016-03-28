#[allow(dead_code)]
//#[derive(Copy, Clone)]
enum Sexps {
   Str(String), Num(f64), Sym(String), Err(String),
   Sub(Box<Vec<Sexps>>),
}

enum Binding { Normal(Sexps), Special(Sexps) }
struct SymTable {
   bindings : HashMap<String, Binding>,
   //unused children : Box<List<SymTable>>,
   //maybe needed children: Box<Vec<SymTable>>,
   parent : Option<Box<SymTable>>
}

impl SymTable {
   fn new(parent : Option<Box<SymTable>>) -> SymTable {
      SymTable {
         bindings : HashMap::new(),
         parent   : parent,
         //maybe needed later children : Box::new(Vec::new()), //(List::Nil),
         //sexps    : Sexp::Err("".to_string())
      }
   }

   fn lookup(&self, s : &String) -> Box<Binding> {
      //Sexps::Err("none".to_string())
      //if !self.bindings.contains_key(s)
      let b = self.bindings.get(s);
      match b {
         Some(&x) => box x,
         None => {
            match self.parent {
               None => {
                  syntax_err("Cannot find symbol", 0);
                  box Binding::Normal(Sexps::Err("None".to_string()))
               },
               Some(ref parent) => parent.lookup(s)
            }
         }
      }
   }

   fn eval(&mut self, sexps : &Sexps) -> Sexps {
      match sexps {
         e @ &Sexps::Str(_) => { *e },
         e @ &Sexps::Num(_) => { *e },
         e @ &Sexps::Var(_) => { *e },
         e @ &Sexps::Err(_) => { *e },
         e @ &Sexps::Sub(_) => {
            self.apply(&e)
          /*//let mut children = Vec::new();
            //let mut first_child : Option<SymTable> = None;

            for subsexp in (*subsexps).iter() {
               let t = SymTable::new(Some(box *self));
               t.eval(subsexp);
               if let None = first_child { first_child = Some(t); }
               else { children.push(t); }
            }
            self.children = Box::new(children);

            if let Some(first) = first_child {
               self.apply(first, self.children)
            } //kk left here
            else {
               Sexps::Err("Cannot eval empty".to_string())
            }*/
         },
      }
      //if let Sexps::Sub(box v) = *sexps
   }
   fn apply(&mut self, args: &Sexps) -> Sexps {
      if let Sexps::Sub(args) = *args {
         let first : Sexps = args[0];
         if let Sexps::Var(op) = first {
            if op == "+" { println!("detected +") }
            Sexps::Num(1.3)
         }
         else { syntax_err("first element needs to be symbol", 0); Sexps::Err("non".to_string()) }
      }
      else { syntax_err("apply needs Sub", 0); Sexps::Err("non".to_string()) }
   }
   //fn apply(&mut self, func : &Sexps, args: Option<&Sexps>) -> Sexps {Sexps::Num(5.4)}

   fn run(&mut self, code : &str) -> Sexps {
      let lexemes = lex(code);
      let sexps_opt = parse(&lexemes);

      if let Some(sexps) = sexps_opt {
         self.eval(&sexps)
      }
      else { Sexps::Err(String::from("Couldn't parse code")) }
   }
}
//Sexps::Num(0.3)

