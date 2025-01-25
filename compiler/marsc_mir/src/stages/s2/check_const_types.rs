use core::panic;
use std::thread::scope;

use err::CompileError;

use crate::stages::s2::*;

type Mir<'src> = MirS2<'src>;

pub(crate) fn check_const_types<'src>(mir: &Mir<'src>) -> Result<(), err::CompileError<'src>> {
    
    for (i, s) in mir.scopes.iter() {
        for instr in s.instrs.iter() {
            if let MIRInstruction::Assign { lhs, span, .. } = instr {
                check_inner(mir, i, lhs, &s.vars, span, &false)?;
            }
        }
    }
    
    Ok(())
}

fn check_inner<'src>(mir: &Mir, scope_id: &usize, expr: &MIRExpr<'src>, vars: &HashMap<String, MIRVariable>, span: &Span<'src>, unwp: &bool) -> Result<(), err::CompileError<'src>> {
    match expr {
        MIRExpr::Dereference { inner, .. } => {
            check_inner(mir, scope_id, inner.as_ref(), vars, span, &true)?;
        }
        
        MIRExpr::Identifier { ident, .. } => {
            let mut resolved_ty = get_ty_from_parent(mir, scope_id, ident);
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

fn get_ty_from_parent(mir: &Mir, scope_id: &usize, ident: &String,) -> MIRType {
    
    let mut ty = MIRType::Any;
    let mut scope = mir.scopes.get(scope_id).unwrap();
    
    loop {
        if let Some(x) = scope.vars.get(ident) {
            ty = x.ty.clone();
            break;
        }
        
        scope = mir.scopes.get(&scope.parent_id).unwrap();
    }
    
    ty
}

#[test]
fn change_arr_test<'src>() -> Result<(), err::CompileError<'src>> {
    use crate::*;
    
    let inp = r#"
        fn main() -> i64 {
            var a = 0;
            while a < 10 {
                a += 1;
            }
           
            return 0;
        }
    "#;
    
    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;

    println!("{:#?}", mir);
    
    Ok(())
}

#[test]
fn zero_len_arr<'src>() -> Result<(), err::CompileError<'src>> {
    use crate::*;
    
    let inp = r#"
        fn main() -> i64 {
           var st = [];
            
            return 0;
        }
    "#;
    
    let hir = hir::compile_hir(&inp)?;
    let mir = hir.compile_mir()?;

    println!("{:#?}", mir);
    
    Ok(())
}