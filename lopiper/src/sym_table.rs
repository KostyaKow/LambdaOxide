use std::collections::HashMap;
use exp::Sexps;


pub struct SymTable {
   bindings : HashMap<String, Sexps>,
   parent : SymTableId,
}

pub type SymTableRoot = Vec<SymTable>;
pub type SymTableId = usize;
pub type Env = (SymTableRoot, SymTableId);

