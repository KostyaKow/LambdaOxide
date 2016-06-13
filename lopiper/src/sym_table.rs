
use std::collections::HashMap;

pub struct SymTable {
   bindings : HashMap<String, Sexps>,
   parent : SymTableId,
}

type SymTableRoot = Vec<SymTable>;


