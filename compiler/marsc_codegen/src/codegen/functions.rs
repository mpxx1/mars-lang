use crate::codegen::codegen::Codegen;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use mir::stages::s2::{MIRFunc, MIRFuncCall, MIRScope};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_function_definition(
        &mut self,
        func_decl: &'ctx MIRFunc<'src>
    ) -> Option<FunctionValue<'ctx>> {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|arg| self.codegen_type(&arg.ty).into())
            .collect();

        let return_type = self.codegen_type(&func_decl.return_type);
        let fn_type = return_type.fn_type(&arg_types, false);

        let value = self.module.add_function(&func_decl.ident, fn_type, None);

        if self.mir.sys_funs.contains(&func_decl.ident) {
            None
        } else {
            Some(value)
        }
    }

    pub(crate) fn codegen_void_function_definition(
        &mut self,
        func_decl: &'ctx MIRFunc<'src>
    ) -> Option<FunctionValue<'ctx>> {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_decl
            .args
            .iter()
            .map(|arg| self.codegen_type(&arg.ty).into())
            .collect();

        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&arg_types, false);

        let value = self.module.add_function(&func_decl.ident, fn_type, None);

        if self.mir.sys_funs.contains(&func_decl.ident) {
            None
        } else {
            Some(value)
        }
    }

    pub(crate) fn codegen_function_args(
        &mut self,
        func_decl: &'ctx MIRFunc<'src>,
        function_value: FunctionValue<'ctx>,
    ) {
        for (arg, value) in func_decl.args.iter().zip(function_value.get_param_iter()) {
            let arg_as_variable = self.builder.build_alloca(value.get_type(), arg.ident.as_str()).unwrap();
            self.builder.build_store(arg_as_variable, value).unwrap();

            self.store_variable(arg.ident.as_str(), arg_as_variable, value.get_type());
        }
    }

    pub(crate) fn codegen_function_entry(
        &mut self,
        func_decl: &'ctx MIRFunc<'src>,
        function_value: FunctionValue<'ctx>
    ) {
        let entry = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry);

        self.codegen_function_args(func_decl, function_value);

        self.codegen_scope_by_id(&func_decl.node_id, Some(function_value));
    }

    pub(crate) fn codegen_func_call(
        &self,
        func_call: &'ctx MIRFuncCall<'src>,
        scope: &'ctx MIRScope<'ctx>
    ) -> Option<BasicValueEnum<'ctx>> {
        let error_msg = format!("Cannot find function '{}'", func_call.ident.as_str());

        let function = self.module
            .get_function(func_call.ident.as_str())
            .expect(error_msg.as_str());

        let args: Vec<BasicMetadataValueEnum> = func_call
            .args
            .iter()
            .map(|arg| self.codegen_expr(arg, scope).into())
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