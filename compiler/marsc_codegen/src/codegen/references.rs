use inkwell::values::{BasicValue, BasicValueEnum};
use mir::stages::s2::{MIRExpr, MIRScope};
use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(in crate::codegen) fn codegen_get_referenced(
        &self,
        inner: &'ctx MIRExpr<'ctx>,
        scope: &'ctx MIRScope<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let pointer = match inner {
            MIRExpr::Identifier { ident, .. } => {
                self.codegen_identifier_pointer(ident, scope)
            },
            _ => unreachable!(),
        };

        let variable = self.builder.build_alloca(pointer.get_type(), "referenced").unwrap();

        self.builder.build_store(variable, pointer).unwrap();

        variable.as_basic_value_enum()
    }

    pub(in crate::codegen) fn codegen_get_dereferenced(
        &self,
        inner: &'ctx MIRExpr<'ctx>,
        scope: &'ctx MIRScope<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.codegen_expr(inner, scope);
        let pointer = match value {
            BasicValueEnum::PointerValue(pointer) => {
                pointer
            },
            _ => unreachable!(),
        };

        let dereferenced_value = self.builder.build_load(
            self.context.i64_type(),
            pointer,
            "dereferenced_value").unwrap();

        dereferenced_value
    }
}