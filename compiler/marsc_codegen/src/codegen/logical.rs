use inkwell::IntPredicate;
use inkwell::values::{BasicValue, BasicValueEnum};
use ast::{CmpOp, LogicalExpr};
use mir::Scope;
use crate::codegen::codegen::Codegen;

impl<'ctx, 'src> Codegen<'ctx, 'src>
where
    'src: 'ctx
{
    pub(crate) fn codegen_logical_expr(&self, logical_expr: &'ctx LogicalExpr<'src>, scope: &'ctx Scope<'ctx>) -> BasicValueEnum<'ctx> {
        match logical_expr {
            LogicalExpr::Not { inner, .. } => {
                let inner_value = self
                    .codegen_logical_expr(inner, scope)
                    .into_int_value();

                self.builder
                    .build_not(inner_value, "not")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LogicalExpr::Or { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right, scope)
                    .into_int_value();

                self.builder
                    .build_or(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LogicalExpr::And { left, right, .. } => {
                let left_value = self
                    .codegen_logical_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_logical_expr(right, scope)
                    .into_int_value();

                self.builder
                    .build_and(left_value, right_value, "or")
                    .unwrap()
                    .as_basic_value_enum()
            },
            LogicalExpr::Comparison { left, right, op, .. } => {
                let left_value = self
                    .codegen_math_expr(left, scope)
                    .into_int_value();
                let right_value = self
                    .codegen_math_expr(right, scope)
                    .into_int_value();

                match op {
                    CmpOp::Equal => {
                        self.builder.build_int_compare(IntPredicate::EQ, left_value, right_value, "eq").unwrap().into()
                    }
                    CmpOp::NotEqual => {
                        self.builder.build_int_compare(IntPredicate::NE, left_value, right_value, "noteq").unwrap().into()
                    }
                    CmpOp::More => {
                        self.builder.build_int_compare(IntPredicate::SGT, left_value, right_value, "more").unwrap().into()
                    }
                    CmpOp::MoreEqual => {
                        self.builder.build_int_compare(IntPredicate::SGE, left_value, right_value, "moreeq").unwrap().into()
                    }
                    CmpOp::Less => {
                        self.builder.build_int_compare(IntPredicate::SLT, left_value, right_value, "less").unwrap().into()
                    }
                    CmpOp::LessEqual => {
                        self.builder.build_int_compare(IntPredicate::SLE, left_value, right_value, "lesseq").unwrap().into()
                    }
                }
            },
            LogicalExpr::Primary(primary) => {
                self.codegen_expr(primary, scope)
            }
        }
    }
}