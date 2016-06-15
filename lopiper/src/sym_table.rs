use std::collections::HashMap;
use exp::Sexps;

pub struct SymTable {
   bindings : HashMap<String, Sexps>,
   parent : SymTableId,
}

type SymTableRoot = Vec<SymTable>;

