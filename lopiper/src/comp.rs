use std::collections::HashMap;
use llvm_sys::prelude::LLVMValueRef;
use std::iter;

use iron_llvm::core;
use llvm_sys::LLVMRealPredicate::LLVMRealOLT;
use llvm_sys::core::LLVMDeleteFunction;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef, FunctionTypeRef, FunctionTypeCtor};
use iron_llvm::core::value::{Function, FunctionRef, FunctionCtor, Value, RealConstRef, RealConstCtor};
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

/*pub trait JITter : ModuleProvider {
    // TODO: fix https://github.com/rust-lang/rust/issues/5665
    fn get_module_provider(&mut self) -> &mut builder::ModuleProvider;

    fn run_function(&mut self, f: LLVMValueRef) -> f64;
}*/

pub struct SimpleModuleProvider {
    module: core::Module
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        SimpleModuleProvider { module: core::Module::new(name) }
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

//TODO: finish function prototype and calling, recursive +/-/*/<

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
      //no global variables, so we can clear all the previously
      //defined named values as they come from other functions
      context.named_values.clear();

      let (f, _) = try!(self.codegen_lambda_proto(name.clone(), args.clone(), context, module_p));
      let mut function = unsafe {FunctionRef::from_ref(f)};

      //basic block will contain generated instrs
      let mut bb = function.append_basic_block_in_context(&mut context.context, "entry");
      context.builder.position_at_end(&mut bb);

      //set function params
      for (param, arg) in function.params_iter().zip(&args) {
         context.named_values.insert(arg.clone(), param.to_ref());
      }

      //emit function body. if error, remove function so user can redefine
      let rec_comp = ExpCompInfo::new(Some(exp));
      let body = match IRBuilder::codegen(&rec_comp, context, module_p) {
         Ok((val, _)) => val,
         Err(msg) => { unsafe { LLVMDeleteFunction(function.to_ref()) }; return Err(msg); }
      };

      //the last instruction should be return
      context.builder.build_ret(&body);
      function.verify(LLVMAbortProcessAction);

      //clear local variables
      context.named_values.clear();
      Ok((function.to_ref(), name == ""))
   }

   fn codegen_lambda_proto(&self, name : String, args : Vec<String>,
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
                  if arr_len != 4 && arr_len != 3 { return error("lambda needs 3 or 4 arguments: (lambda name (args) (exp))"); }
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
                     if arr_len == 3 {
                        self.codegen_lambda_proto(new_func_name, args, context, module_p)
                     } else {
                       self.codegen_lambda(new_func_name, args, arr_borr[3].clone(), context, module_p)
                     }
                  } else {
                     return error("lambda's 2nd argument needs to be list of args: (lambda name (args) (exp))");
                  }
               }
               else {
                  let built_in = func_name == "+" || func_name == "-" || func_name == "*" || func_name == "<";
                  if built_in {
                     if arr_len != 3 { return error("build-in operators take 2 argument"); }

                     let mut rec_comp = ExpCompInfo::new(Some(arr_borr[1].clone()));
                     let (left_val, _) = try!(IRBuilder::codegen(&rec_comp, context, module_p));
                     rec_comp.exp = arr_borr[2].clone();
                     let (right_val, _) = try!(IRBuilder::codegen(&rec_comp, context, module_p));

                     match &*func_name {
                        "+" => Ok((context.builder.build_fadd(left_val, right_val, "addtmp"), false)),
                        "-" => Ok((context.builder.build_fsub(left_val, right_val, "addtmp"), false)),
                        "*" => Ok((context.builder.build_fmul(left_val, right_val, "addtmp"), false)),
                        "<" => {
                           let cmp = context.builder.build_fcmp(LLVMRealOLT, left_val, right_val, "cmptmp");
                           Ok((context.builder.build_ui_to_fp(cmp, context.ty.to_ref(), "booltmp"), false))
                        },
                        _ => error("if/else for testing primitive operators doesn't match builtin assignment")
                     }

                  }
                  else { //this is function call
                     if let Some((function, _)) = module_p.get_function(&*func_name) {
                        if function.count_params() as usize != arr_len-1 {
                           return error("incorrect number of arguments");
                        }
                        let mut args_value = Vec::new();
                        for i in 1..arr_len {
                           let mut rec_comp = ExpCompInfo::new(Some(arr_borr[i].clone()));
                           let (arg_val, _) = try!(IRBuilder::codegen(&rec_comp, context, module_p));
                           args_value.push(arg_val);
                        }
                        Ok((context.builder.build_call(function.to_ref(), args_value.as_mut_slice(), "calltmp"), false))
                     }
                     else { error(&*format!("unknown function name: {}", func_name)) }
                  }
               }
            }
            else {
               error("(x ...) x needs to be symbol")
            }
         },
         Sexps::Sym(ref s) => {
            if let Some(value) = context.named_values.get(s) {
               Ok((*value, false))
            } else { error("unknown variable name") }
         },
         Sexps::Int(ref n) => {
            Ok((RealConstRef::get(&context.ty, *n as f64).to_ref(), false))
         },
         Sexps::Float(ref n) => {
            Ok((RealConstRef::get(&context.ty, *n).to_ref(), false))
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


