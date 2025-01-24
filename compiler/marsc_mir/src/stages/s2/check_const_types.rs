use core::panic;

use err::CompileError;

use crate::stages::s2::*;

type Mir<'src> = MirS2<'src>;

pub(crate) fn check_const_types<'src>(mir: &Mir<'src>) -> Result<(), err::CompileError<'src>> {
    
    for (_, s) in mir.scopes.iter() {
        for instr in s.instrs.iter() {
            if let MIRInstruction::Assign { lhs, span, .. } = instr {
                check_inner(lhs, &s.vars, span, &false)?;
            }
        }
    }
    
    Ok(())
}

fn check_inner<'src>(expr: &MIRExpr<'src>, vars: &HashMap<String, MIRVariable>, span: &Span<'src>, unwp: &bool) -> Result<(), err::CompileError<'src>> {
    match expr {
        MIRExpr::Dereference { inner, .. } => {
            check_inner(inner.as_ref(), vars, span, &true)?;
        }
        
        MIRExpr::Identifier { ident, .. } => {
            let mut resolved_ty = vars.get(ident).unwrap().ty.clone();
            if *unwp {
                let MIRType::Ref(ty) = vars.get(ident).unwrap().ty.clone() else {
                    panic!("Something went wrong");
                };
                resolved_ty = *ty;
            }
            
            if resolved_ty == MIRType::Str {
                return Err(CompileError::new(*span, "Can not mutate strings".to_owned()));
            }
        }
        
        MIRExpr::MemLookup { ident, indices, .. } => {
            let steps_in = indices.len();
            let mut ty = vars.get(ident).unwrap().ty.clone();
            
            for _ in 0..steps_in {
                match ty {
                    MIRType::Vec(x) | MIRType::Array(x,_) => {
                        ty = *x;
                    },
                    _ => panic!("Unexpected type"),
                }
            }
            
            if ty == MIRType::Str {
                return Err(CompileError::new(*span, "Can not mutate strings".to_owned()));
            }
        }
        
        MIRExpr::StructFieldCall { ident: _, field: _, .. } => {
            // todo unreacheble
        }
        
        _ => ()
    }
    
    Ok(())
}

#[test]
fn change_arr_test<'src>() -> Result<(), err::CompileError<'src>> {
    use crate::*;
    
    let inp = r#"
        struct A { a: str, }
        fn main() -> i64 {
           var st = A { a: "hello", };
            
            return 0;
        }
    "#;
    
    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;

    println!("{:#?}", mir);
    
    Ok(())
}