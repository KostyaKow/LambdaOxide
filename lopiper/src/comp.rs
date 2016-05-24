use std::collections::HashMap;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::{Function, FunctionRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

//context
pub struct Context {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: RealTypeRef
}

impl Context {
    pub fn new() -> Context {
        let context = core::Context::get_global();
        let builder = core::Builder::new();
        let named_values = HashMap::new();
        let ty = RealTypeRef::get_double();

        Context { context: context,
                  builder: builder,
                  named_values: named_values,
                  ty: ty
        }
    }
}
//end context

//module
pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
}

pub struct SimpleModuleProvider {
    module: core::Module
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        let module = core::Module::new(name);

        SimpleModuleProvider {
            module: module
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }
}
//module

//code generation traits
pub type IRBuildingResult = Result<(LLVMValueRef, bool), String>;

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult;
}

//

use exp::Sexps;
pub struct ExpCompInfo {
   exp : Sexps,
   lambda_i : usize, //lambdaIndex //TODO: unneeded until I remove names from lambda
   args : Vec<String>,
}

impl ExpCompInfo {
   fn new(exp_opt : Option<Sexps>) -> ExpCompInfo {
      let exp = if let Some(exp_) = exp_opt { exp_ }
      else { Sexps::nil_new() };

      ExpCompInfo {
         exp : exp, lambda_i : 0, args : Vec::new()
      }
   }
   fn set_exp(&mut self, exp : Sexps) { self.exp = exp; }
   fn inc_i(&mut self) { self.lambda_i += 1; }
   fn get_i(&self) -> usize { self.lambda_i }

   fn codegen_lambda(&self, name : String,
                     context : &mut Context,
                     module_p : &mut ModuleProvider)
   -> IrBuildingsResult
   {

   }
}

impl IRBuilder for ExpCompInfo {
   fn codegen(&self, context: &mut Context, module_p : &mut ModuleProvider) -> IRBuildingResult {
      match self.exp {
         Sexps::Nil => error("nil passed to ExpCompInfo.codegen()"),
         Sexps::Array(arr_mref) => {
            let arr_borr = arr_mref.borrow();
            let arr_len = arr_borr.len();
            if arr_len < 1 { return error("codegen exp needs array with at least 1 item"); }
            if arr_bor[0].is_sym() {
               let func_name = arr_borr[0].get_sym_fast();

               if func_name == "lambda" {
                  if arr_len != 4 { return error("lambda needs 3 arguments: (lambda name (args) (exp))"); }
                  if !arr_borr[1].is_sym() {
                     return error("lambda's 1st argument needs to be function name: (lambda name (args) (exp))");
                  }
                  let new_func_name = arr_borr[1].get_sym_fast();

                  let args_len_opt = arr_borr[2].arr_len();
                  if let Some(args_len) = args_len_opt {


                  } else {
               }
            }
         },
         _ => error("ExpCompInfo.exp has to be an expression of type Array() (returned from parser)")
      }

      /*let mut result = error("empty AST");
      for node in self.iter() {
         result = Ok(try!(node.codegen(context, module_p)));
      }
      result*/
   }
}



