use crate::codegen::Codegen;
use ast::Type;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use mir::{FuncProto};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx {
    fn generate_void_external_function(&self, func_proto: &'src FuncProto<'src>) {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_proto
            .args
            .iter()
            .map(|arg_decl| self.codegen_type(&arg_decl.ty).into())
            .collect();

        let fn_type = self
            .context
            .void_type()
            .fn_type(&arg_types, false);
        self.module.add_function(func_proto.ident, fn_type, None);
    }

    fn generate_typed_external_function(&self, func_proto: &'src FuncProto<'src>) {
        let arg_types: Vec<BasicMetadataTypeEnum> = func_proto
            .args
            .iter()
            .map(|arg_decl| self.codegen_type(&arg_decl.ty).into())
            .collect();
        
        let return_type = self.codegen_type(&func_proto.return_type);
        let fn_type = return_type
            .fn_type(&arg_types, false);
        self.module.add_function(func_proto.ident, fn_type, None);
    }
    
    pub(crate) fn generate_external_function(&self, func_proto: &'src FuncProto<'src>) {
        if func_proto.return_type == Type::Void {
            self.generate_void_external_function(func_proto);
        } else {
            self.generate_typed_external_function(func_proto);
        }
    }
}