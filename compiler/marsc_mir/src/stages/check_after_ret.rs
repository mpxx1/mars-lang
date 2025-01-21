use crate::Mir;
use err::CompileError;

pub(crate) fn check_after_return(mir: Mir) -> Result<Mir, CompileError> {
    Ok(mir)
}
