use err::CompileError;
use hir::Hir;
use crate::Mir;
use crate::provider::Providers;
use crate::stages::type_checker::TypeChecker;

pub mod type_checker;

pub trait TypeCheckerProvider<'ctx> {
    fn check_types<'src>(
        &self,
        hir: Hir<'src>
    ) -> Result<Mir<'src>, CompileError<'src>>;
}

impl<'ctx> TypeCheckerProvider<'ctx> for Providers<'ctx> {
    fn check_types<'src>(
        &self,
        hir: Hir<'src>
    ) -> Result<Mir<'src>, CompileError<'src>> {
        TypeChecker::check_types(self.type_context, hir)
    }
}