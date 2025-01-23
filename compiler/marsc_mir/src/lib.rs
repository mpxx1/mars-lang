pub mod stages;

use crate::stages::s1;

pub const GLOBAL_SCOPE_ID: usize = 0;

use err::CompileError;
use stages::s2::compile_mir_s2;

pub type Mir<'src> = crate::stages::s2::MirS2<'src>;

pub trait ToMir<'src> {
    fn compile_mir(self) -> Result<Mir<'src>, CompileError<'src>>;
}

impl<'src> ToMir<'src> for hir::Hir<'src> {
    fn compile_mir(self) -> Result<Mir<'src>, CompileError<'src>> {
        let m1 = s1::compile_mir_s1(self)?;
        let m2 = m1.into();
        let mir = compile_mir_s2(m2)?;
        
        Ok(mir)
    }
}

#[test]
fn mir_2<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn main() -> void { 
            var a = 10;
            {
                a += 20;
                println("{a}");
            }
            
            println("{a}");
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn resolving_parent_ids<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        struct A { a: i64 }
        
        fn main() -> void { 
            var fir = A { a: 10 };
            var sec = A { a: 11 };
            var thi = A { a: 12 };
            
            var a: Vec<A> = [fir, sec, thi];
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;

    println!("{mir:#?}");

    Ok(())
}
