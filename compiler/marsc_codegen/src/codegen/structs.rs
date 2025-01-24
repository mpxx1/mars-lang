use crate::codegen::codegen::Codegen;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use pest::Span;
use err::CompileError;
use mir::stages::s2::{MIRExpr, MIRScope, MIRStruct};

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_struct(&mut self, struct_decl: &'ctx MIRStruct<'src>) {
        let field_types: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|field| self.codegen_type(&field.ty))
            .collect();

        let struct_type = self.context.struct_type(&field_types, false);
        
        self.store_struct_type(struct_decl.ident.as_str(), struct_type);

        struct_type.set_body(&field_types, false);
    }
    
    pub(crate) fn codegen_struct_init(
        &self,
        ident: &'ctx str,
        fields: &'ctx Vec<(String, MIRExpr)>,
        scope: &'ctx MIRScope<'src>,
    ) -> BasicValueEnum<'ctx> {
        let struct_type = self.get_struct_type(ident);
        let pointer = self.builder.build_alloca(struct_type, ident).unwrap();
        
        println!("{:#?}", pointer);
        
        for (i, (field_name, expr)) in fields.iter().enumerate()
        {
            let field_value = self.codegen_expr(expr, scope);
            
            let field_name = format!("{ident}_{field_name}");
            let field_ptr = self.builder.build_struct_gep(
                struct_type,
                pointer,
                i as u32,
                field_name.as_str())
                .unwrap();

            println!("{:#?}", field_ptr);
            
            self.builder.build_store(field_ptr, field_value).unwrap();
        }
        
        pointer.as_basic_value_enum()
    }

    pub(super) fn codegen_struct_assignment(
        &mut self,
        ident: &'ctx str,
        variable_type: BasicTypeEnum<'ctx>,
        expr: &'ctx MIRExpr<'src>,
        span: Span<'src>,
        scope: &'ctx MIRScope<'ctx>
    ) -> Result<(), CompileError>
    {
        let value = self.codegen_expr(expr, scope);
        if let BasicValueEnum::PointerValue(value) = value {
            self.store_variable(ident, value, variable_type);
            Ok(())
        } else {
            Err(CompileError::new(span, "Invalid right value type".to_owned()))
        }
    }
}