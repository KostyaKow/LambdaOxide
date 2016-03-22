use std::collections::HashMap;

struct Table {
   x : Vec<&'static str>,
   parent : Option<Box<Table>>
}

struct Table2 {
   x : HashMap<&'static str, >
}

fn main() {
   let x = Table { x: vec!["hello", "world"], parent: None };
   let y = Table { x: vec!["hello", "world"], parent: Some(Box::new(x)) };

}
