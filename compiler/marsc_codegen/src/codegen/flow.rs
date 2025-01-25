use inkwell::values::FunctionValue;
use lir::LIRExpr;
use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(in crate::codegen) fn codegen_while(
        &mut self,
        condition: &'ctx LIRExpr,
        body_block: usize,
        function_value: FunctionValue<'ctx>,
    ) {
        let condition_bb = self.context.append_basic_block(function_value, "condition");
        let body_bb = self.context.append_basic_block(function_value, "body");
        let exit_bb = self.context.append_basic_block(function_value, "exit");

        self.builder.build_unconditional_branch(condition_bb).unwrap();

        self.builder.position_at_end(condition_bb);

        let cond_value = self.codegen_expr(condition).into_int_value();

        self.builder.build_conditional_branch(cond_value, body_bb, exit_bb).unwrap();

        self.builder.position_at_end(body_bb);
        self.codegen_antioptimize_mark();
        self.codegen_block_by_id(&body_block, function_value);

        self.builder.build_unconditional_branch(condition_bb).unwrap();

        self.builder.position_at_end(exit_bb);
        self.codegen_antioptimize_mark();
    }
    
    pub(in crate::codegen) fn codegen_conditional(
        &mut self,
        condition: &'ctx LIRExpr,
        then_block: usize,
        else_block: Option<usize>,
        function_value: FunctionValue<'ctx>,
    ) {
        let cond_value = self.codegen_expr(condition).into_int_value();

        let then_bb = self.context.append_basic_block(function_value, "then");
        let else_bb = if else_block.is_some() {
            Some(self.context.append_basic_block(function_value, "else"))
        } else {
            None
        };
        let merge_bb = self.context.append_basic_block(function_value, "merge");

        if let Some(else_bb) = else_bb {
            self.builder.build_conditional_branch(cond_value, then_bb, else_bb).unwrap();
        } else {
            self.builder.build_conditional_branch(cond_value, then_bb, merge_bb).unwrap();
        }

        self.builder.position_at_end(then_bb);
        self.codegen_antioptimize_mark();
        self.codegen_block_by_id(&then_block, function_value);
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        if let Some(else_bb) = else_bb {
            self.builder.position_at_end(else_bb);
            self.codegen_antioptimize_mark();
            self.codegen_block_by_id(&else_block.unwrap(), function_value);
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(merge_bb);
        self.codegen_antioptimize_mark();
    }

    fn codegen_antioptimize_mark(&self) {
        let alloca = self.builder.build_alloca(self.context.i64_type(), "block_mark").unwrap();
        let const_value = self.context.i64_type().const_int(0, true);
        self.builder.build_store(alloca, const_value).unwrap().set_volatile(true).unwrap()
    }
}