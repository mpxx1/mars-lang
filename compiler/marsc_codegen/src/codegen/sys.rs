use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    /*pub(in crate::codegen) fn codegen_array_len_function(&self) {
        let arg_types = vec![
            self.context.ptr_type(AddressSpace::default()).into(),
        ];

        let return_type = self.context.i64_type();
        let fn_type = return_type.fn_type(&arg_types, false);

        let function_value = self.module.add_function("array_len", fn_type, None);

        let entry = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry);
        
        let value = self.builder.build_load()

        self.builder.build_return(Some());
    }*/
}