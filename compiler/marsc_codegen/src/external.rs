use inkwell::module::Linkage;
use crate::codegen::Codegen;
use ast::Type;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use marsc_mir::context::ExternalFunctions;
use marsc_mir::ExternalFunction;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx {
    fn generate_void_external_function(&self, external_function: &'ctx ExternalFunction<'ctx>) {
        let arg_types: Vec<BasicMetadataTypeEnum> = external_function
            .args
            .iter()
            .map(|ty| self.codegen_type(ty).into())
            .collect();

        let fn_type = self
            .context
            .void_type()
            .fn_type(&arg_types, false);
        self.module.add_function(external_function.ident, fn_type, None);
    }

    fn generate_typed_external_function(&self, external_function: &'ctx ExternalFunction<'ctx>) {
        let arg_types: Vec<BasicMetadataTypeEnum> = external_function
            .args
            .iter()
            .map(|ty| self.codegen_type(ty).into())
            .collect();
        
        let return_type = self.codegen_type(&external_function.return_type);
        let fn_type = return_type
            .fn_type(&arg_types, false);
        self.module.add_function(external_function.ident, fn_type, None);
    }
    
    fn generate_external_function(&self, external_function: &'ctx ExternalFunction<'ctx>) {
        if external_function.return_type == Type::Void {
            self.generate_void_external_function(external_function);
        } else {
            self.generate_typed_external_function(external_function);
        }
    }
    
    pub(crate) fn generate_external_functions(&self, external_functions: &'ctx ExternalFunctions<'ctx>) {
        for external_function in external_functions.get_all() {
            self.generate_external_function(&external_function);
        }
    }
}