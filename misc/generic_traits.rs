//use std::ops::{Add, Sub, Mul};

trait Shape<T>
   where T: Mul<Output=T> + Copy
{
   fn get_height(&self) -> T;
   fn get_width(&self) -> T;
   fn get_area(&self) -> T;
   fn get_perimiter(&self) -> T;
}

struct Square<T> {
   height: T, width: T
}
struct Circle<T> {
   radius: T
}

impl<T> Shape<T> for Square<T> {
   fn get_height(&self) -> T { self.height }
   fn get_width(&self) -> T { self.width }
   fn get_area(&self) -> T { self.height * self.width }
   fn get_perimiter(&self) -> T { self.height+self.height + self.width+self.width }
}

fn main() {
   //let x = Square{ height: 10, width: 10 };
   println!("{}", 3);//x.get_height());
}
