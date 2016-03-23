//use std::ops::{Add, Sub, Mul};

trait Shape {
   fn get_height(&self) -> i32;
   fn get_width(&self) -> i32;
   fn get_area(&self) -> i32;
   fn get_perimiter(&self) -> i32;
}

#[derive(Copy, Clone)]
struct Rectangle {
   height: i32, width: i32
}
//struct Circle { radius: i32 }

impl Shape for Rectangle {
   fn get_height(&self) -> i32 { self.height }
   fn get_width(&self) -> i32 { self.width }
   fn get_area(&self) -> i32 { self.height * self.width }
   fn get_perimiter(&self) -> i32 { self.height+self.height + self.width+self.width }
}

fn test<T: Shape>(x: T) -> i32 { x.get_height() }

fn test2(x: &Shape) -> i32 { x.get_height() } //x.get_area() }

fn main() {
   let x = Rectangle { height: 10, width: 10 };
   //println!("{}", x.get_height());
   println!("{}", test(x));
   println!("{}", test2(&x));

}
