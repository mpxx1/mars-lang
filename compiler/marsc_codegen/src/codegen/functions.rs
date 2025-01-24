use crate::codegen::codegen::Codegen;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use lir::{LIRFunc, LIRFuncCall};
use std::fmt::Debug;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_function_definition(
        &mut self,
        func_decl: &'ctx LIRFunc,
    ) -> Option<FunctionValue<'ctx>> {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|(_, ty)| self.codegen_type(ty.clone()).into())
            .collect();

        let return_type = self.codegen_type(func_decl.return_type.clone());
        let fn_type = return_type.fn_type(&arg_types, false);

        let value = self.module.add_function(&func_decl.name, fn_type, None);

        if self.lir.sys_funs.contains(&func_decl.name) {
            None
        } else {
            Some(value)
        }
    }

    pub(crate) fn codegen_void_function_definition(
        &mut self,
        func_decl: &'ctx LIRFunc,
    ) -> Option<FunctionValue<'ctx>> {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|(_, ty)| self.codegen_type(ty.clone()).into())
            .collect();

        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&arg_types, false);

        let value = self.module.add_function(&func_decl.name, fn_type, None);

        if self.lir.sys_funs.contains(&func_decl.name) {
            None
        } else {
            Some(value)
        }
    }

    pub(crate) fn codegen_function_args(
        &mut self,
        func_decl: &'ctx LIRFunc,
        function_value: FunctionValue<'ctx>,
    ) {
        for ((name, lir_type), value) in func_decl.args
            .iter()
            .zip(function_value.get_param_iter()) {
            let arg_as_variable = self.builder.build_alloca(value.get_type(), name.as_str()).unwrap();
            self.builder.build_store(arg_as_variable, value).unwrap();

            self.store_variable(name.as_str(), arg_as_variable, lir_type.clone(), value.get_type());
        }
    }

    pub(crate) fn codegen_function_entry(
        &mut self,
        func_decl: &'ctx LIRFunc,
        function_value: FunctionValue<'ctx>
    ) {
        let entry = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry);

        self.codegen_function_args(func_decl, function_value);

        self.codegen_block_by_id(&func_decl.block_id, function_value);
    }

    pub(crate) fn codegen_func_call(
        &self,
        func_call: &'ctx LIRFuncCall,
    ) -> Option<BasicValueEnum<'ctx>> {
        let function = self.module
            .get_function(func_call.function.as_str())
            .unwrap();

        let args: Vec<BasicMetadataValueEnum> = func_call
            .args
            .iter()
            .map(|arg| self.codegen_expr(arg).into())
            .collect();

        let call_site = self.builder.build_call(function, &args, "call");

        if let Some(_) = function.get_type().get_return_type() {
            let value = call_site
                .unwrap().try_as_basic_value()
                .left()
                .unwrap();

            Some(value)
        } else {
            call_site.unwrap();
            None
        }
    }
}