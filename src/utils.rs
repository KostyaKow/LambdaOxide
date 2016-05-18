#![allow(dead_code)]

use oxicloak::*;

use types::{Root, Sexps, err, RunFail};
use main::run;
pub fn load_internal(code : &String, root : Root) -> Sexps {
   let lines = code.split("\n").collect::<Vec<&str>>();

   let mut x : Sexps = err("empty file");

   let mut line_acc = String::new();
   let mut i = 0;

   for line in lines.iter() {
      i+=1;
      if line.len() < 1 { continue; }
      if char_at(line, 0).unwrap() == ';' { continue; }
      line_acc = line_acc + line;
      let out = run(&root, &line_acc);
      if let Result::Err((RunFail::UncompleteExp, _)) = out {
         continue;
      }
      else if let Result::Err((x, err_n)) = out {
         let ret = format!("cannot load code line {} ({}): {:?}",
                           i, err_n, x);
         return err(&*ret);
      }
      else {
         line_acc = String::new();
         x = out.unwrap();
      }
   }
   x
}

pub fn get_char_ranges(code : &str) -> Vec<(usize, usize)> {
   let mut ranges : Vec<(usize, usize)> = Vec::new();

   let mut start_quote : Option<usize> = None;
   let mut ignore_next_quote = false;

   for (i, c) in code.chars().enumerate() {
      if c == '"' {
         match start_quote {
            //if we have start
            Some(start) if !ignore_next_quote => {
               ranges.push((start, i));
               start_quote = None;
            }
            None if !ignore_next_quote => {
               start_quote = Some(i);
            }
            _ => {}
         }
      }
      if c == '\\' { ignore_next_quote = true; }
      else { ignore_next_quote = false; }
   }

   if let Some(x) = start_quote { syntax_err("unterminated quote", x as u32); }

   ranges
}

pub fn print_nest(s: &str, n: u8, extra: Option<&str>) {
   print_spaces(n);
   if let Some(ex) = extra { println!("{} {}", ex, s) }
   else { println!("{}", s) }
}

pub fn syntax_err(s : &str, n : u32) { println!("error at {}: {}", n, s); }

