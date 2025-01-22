use std::collections::HashMap;

use crate::{gen_id, Mir};
use ast::{Stmt, Type};
use err::CompileError;

use super::check_types::resolve_expr_type;

pub(crate) fn block_var_decl(mut mir: Mir) -> Result<Mir, CompileError> {
    
    // let mut map = HashMap::new();
    // for (i, scope) in mir.scopes.iter() {
        
    //     let mut variables = vec![];
    //     for instr in scope.instrs.iter() {
            
    //         for var in instr.get_variables() {
    //             variables.push(var);
    //         }
    //     }
        
    //     map.insert(i, variables);
    // }
    
    // todo for elem in map - resolve types for assignment instruction -> Stmt::Assignment { node_id: gen_id(), ident: var, ty: resolve_expr_type(
    //     //     scope_id,
    //     //     mir,
    //     //     expr,
    //     //     opt_type
    //     // ), expr: Expr::Identifier, span: Span::new(..).unwrap() }
    // todo push elements in the begining of instruction set
    
    // scope_instrs.reverse();
    // for _var in variables {
    //     // scope_instrs.push(Stmt::Assignment { node_id: gen_id(), ident: var, ty: resolve_expr_type(
    //     //     scope_id,
    //     //     mir,
    //     //     expr,
    //     //     opt_type
    //     // ), expr: (), span: () });
    // }
    // scope_instrs.reverse();
    
    Ok(mir)
}

trait GetVariables {
    fn get_variables(&self) -> Vec<&str>;
}

impl GetVariables for Stmt<'_> {
    fn get_variables(&self) -> Vec<&str> {
        unimplemented!()
    }
}

#[test]
fn block_var_decl_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> i64 {  
            
            var a = 10;
            {
                a += 20;
            }
            println("{a}");
        
            return 0;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = crate::compile_mir(hir)?;
    println!("{mir:#?}");

    Ok(())
}