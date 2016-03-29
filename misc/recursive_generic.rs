use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

enum Callable {
   Env(Rc<RefCell<RecTable<Callable>>>)
}

struct RecTable<T> {
   bindings : HashMap<String, T>,
   parent : Option<Rc<RefCell<RecTable<T>>>>
}
impl<T> RecTable<T> {
   fn new(parent : Option<Rc<RefCell< RecTable<T> >>>) -> RecTable<T> {
      RecTable {
         bindings : HashMap::new(),
         parent   : parent
      }
   }
   fn lookup(&self, s : &String) -> Option<&T> {
      let item_opt = self.bindings.get(s);
      if let Some(ref item) = item_opt { Some(item.clone()) }
      else {
         if let Some(parent) = self.parent { parent.borrow_mut().lookup(s) }
         else { None }
      }
   }
}

fn main() {
   //let x : SymTable<Callable> = SymTable { bindings : HashMap::new(), parent : None };

}
