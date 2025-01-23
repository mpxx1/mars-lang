use inkwell::types::BasicTypeEnum;
use mir::StructProto;
use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_struct(&self, struct_decl: &'ctx StructProto<'src>) {
        let struct_type = self.context.opaque_struct_type(struct_decl.ident);

        let field_types: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|field| self.codegen_type(&field.ty))
            .collect();

        struct_type.set_body(&field_types, false);
    }
}