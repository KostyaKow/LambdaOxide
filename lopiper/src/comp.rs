use std::collections::HashMap;
use llvm_sys::prelude::LLVMValueRef;
use std::iter;

use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef, FunctionTypeRef, FunctionTypeCtor};
use iron_llvm::core::value::{Function, FunctionRef, FunctionCtor, Value};
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
}

impl ExpCompInfo {
   pub fn new(exp_opt : Option<Sexps>) -> ExpCompInfo {
      let exp = if let Some(exp_) = exp_opt { exp_ }
      else { Sexps::nil_new() };

      ExpCompInfo { exp : exp, lambda_i : 0 }
   }
   pub fn set_exp(&mut self, exp : Sexps) { self.exp = exp; }
   pub fn inc_i(&mut self) { self.lambda_i += 1; }
   pub fn get_i(&self) -> usize { self.lambda_i }

   fn codegen_lambda(&self, name : String, args : Vec<String>, exp : Sexps,
                     context : &mut Context, module_p : &mut ModuleProvider)
   -> IRBuildingResult
   {
      if let Some((prev_def, redef)) = module_p.get_function(&*name) {
         return error("re-declaring functions isn't allowed");
      }

      let mut param_types = iter::repeat(context.ty.to_ref())
                              .take(args.len())
                              .collect::<Vec<_>>();
      let fty = FunctionTypeRef::get(&context.ty, param_types.as_mut_slice(), false);

      let function = FunctionRef::new(&mut module_p.get_module(), &name, &fty);

      for (param, arg) in function.params_iter().zip(args) {
         param.set_name(&*arg);
      }
      Ok((function.to_ref(), false))
   }

}

impl IRBuilder for ExpCompInfo {
   fn codegen(&self, context: &mut Context, module_p : &mut ModuleProvider) -> IRBuildingResult {
      match self.exp {
         Sexps::Nil => error("nil passed to ExpCompInfo.codegen()"),
         Sexps::Array(ref arr_mref) => {
            let arr_borr = arr_mref.borrow();
            let arr_len = arr_borr.len();
            if arr_len < 1 { return error("codegen exp needs array with at least 1 item"); }
            if arr_borr[0].is_sym() {
               let func_name = arr_borr[0].get_sym_fast();

               if func_name == "lambda" {
                  if arr_len != 4 { return error("lambda needs 3 arguments: (lambda name (args) (exp))"); }
                  if !arr_borr[1].is_sym() {
                     return error("lambda's 1st argument needs to be function name: (lambda name (args) (exp))");
                  }
                  let new_func_name = arr_borr[1].get_sym_fast();

                  let args_len_opt = arr_borr[2].arr_len();
                  if let Some(args_len) = args_len_opt {
                     let mut args = Vec::new();
                     for i in 0..args_len {
                        let curr_arg = arr_borr[2].arr_get_fast(i);
                        if !curr_arg.is_sym() { return error("every argument name to lambda needs to be a sym"); }
                        args.push(curr_arg.get_sym_fast());
                     }
                     self.codegen_lambda(new_func_name, args, arr_borr[3].clone(), context, module_p)
                  } else {
                     return error("lambda's 2nd argument needs to be list of args: (lambda name (args) (exp))");
                  }
               }
               else {
                  error("unsupported. right now you can only define lambda's in this language")
               }
            }
            else {
               error("(x ...) x needs to be symbol")
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



