use std::cell::RefCell;
use std::rc::Rc;

pub type SharedMut<T> = Rc<RefCell<T>>;

pub fn to_shared_mut<T>(x : T) -> SharedMut<T> {
   Rc::new(RefCell::new(x))
}

pub type SizeRange = (usize, usize);
pub type SizeRanges = Vec<SizeRange>;

