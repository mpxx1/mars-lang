use crate::stages::s1::MirS1;
use err::CompileError;

type Mir<'src> = MirS1<'src>;

pub(crate) fn check_after_return(mut mir: Mir) -> Result<Mir, CompileError> {
    for (_, scope) in mir.scopes.iter_mut() {
        for instr in scope.instrs.iter_mut().rev().skip(1) {
            if let ast::Stmt::Return { span, .. } = instr {
                return Err(CompileError::new(
                    *span,
                    "Code after this return statement is unreachable".to_owned(),
                ));
            }
        }
    }

    Ok(mir)
}

#[test]
fn check_unreacheble<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> i64 {  
            if 10 > 30 {
                return 0;
            }
            
            var a = 10;
            return 10;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = crate::stages::s1::compile_mir_s1(hir)?;
    println!("{mir:#?}");

    Ok(())
}
