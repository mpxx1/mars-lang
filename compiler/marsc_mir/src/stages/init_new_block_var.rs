use std::collections::{HashMap, HashSet};
use pest::Span;
use crate::{gen_id, Mir};
use ast::*;
use err::CompileError;

use super::check_types::resolve_expr_type;

pub(crate) fn block_var_decl<'src>(mir: Mir<'src>) -> Result<Mir<'src>, CompileError<'src>> {
    
    // let (mir, missing_vars) = get_missing_vars(mir);
    // let (mir, instrs) = create_decl_instructions(mir, missing_vars)?;
    // dbg!(&missing_vars);
    // mut_mir(&mut mir);
    
    // 2. Модифицируем MIR после сбора данных
    /**/

    Ok(mir)
}


fn modify_instrs<'src>(mir: &mut Mir<'src>, missing_vars: HashMap<usize, Vec<&'src str>>) -> Result<(), CompileError<'src>> {
    
    // for (scope_id, vars) in missing_vars {
    //     let scope = mir.scopes.get_mut(&scope_id).unwrap();

    //     let mut new_instrs = Vec::new();
        
    //     for var_name in vars {
    //         let fake_ident = Identifier {
    //             node_id: gen_id(),
    //             ident: var_name,
    //             span: Span::new("generated", 0, 9).unwrap(),
    //         };

    //         // Проверяем существование переменной через resolve_expr_type
    //         let ty = resolve_expr_type(
    //             scope.parent_id,
    //             mir,
    //             &mut Expr::Identifier(fake_ident),
    //             Type::Unresolved,
    //         )?;

    //         // Создаем объявление
    //         let assignment = Stmt::Assignment {
    //             node_id: gen_id(),
    //             ident: &var_name,
    //             ty: ty.clone(),
    //             expr: Expr::Identifier(Identifier {
    //                 node_id: gen_id(),
    //                 ident: &var_name,
    //                 span: Span::new("generated", 0, 9).unwrap(),
    //             }),
    //             span: Span::new("generated", 0, 9).unwrap(),
    //         };

    //         new_instrs.push(assignment);
            
    //         // Добавляем в переменные скоупа
    //         scope.vars.insert(&var_name, crate::Variable {
    //             parent_id: scope_id,
    //             node_id: gen_id(),
    //             ident: &var_name,
    //             ty,
    //             is_used: true,
    //             decl_span: Span::new("generated", 0, 9).unwrap(),
    //         });
    //     }
        
    //     // Вставляем новые инструкции в начало
    //     new_instrs.extend(scope.instrs.drain(..));
    //     scope.instrs = new_instrs;
    // }
    
    Ok(())
}

fn create_decl_instructions<'src>(mut mir: Mir<'src>, missing_vars: HashMap<usize, Vec<&'src str>>) -> Result<(Mir<'src>, HashMap<usize, Vec<Stmt<'src>>>), CompileError<'src>> {
    
    let mut map = HashMap::new();
    
    for (i, vars) in missing_vars {
        let mut v = vec![];
        
        for var in vars {
            v.push(create_decl_instr(i, &mut mir, var)?);
        }
        
        map.insert(i, v);
    }
    
    Ok((mir, map))
}

fn create_decl_instr<'src>(scope_id: usize, mir: &mut Mir<'src>, name: &'src str) -> Result<Stmt<'src>, CompileError<'src>> {
    
    let mut expr = Expr::Identifier(Identifier { 
        node_id: gen_id(),
        ident: name, 
        span: Span::new("compiler generated", 0, 17).unwrap()
    });
    
    Ok(Stmt::Assignment { 
        node_id: gen_id(),
        ident: name, 
        ty: resolve_expr_type(scope_id, mir, &mut expr, Type::Unresolved)?,
        expr,
        span: Span::new("compiler generated", 0, 17).unwrap() 
    })
}

fn get_missing_vars<'src>(mir: Mir<'src>) -> (Mir<'src>, HashMap<usize, Vec<&'src str>>) {
    let mut declarations = HashMap::new();
    let mir_out = Mir { code: mir.code.clone(), scopes: HashMap::new(), sys_funs: mir.sys_funs.clone() };
    
    for (scope_id, scope) in mir.scopes.iter() {
        
        let mut needed_vars = HashSet::new();
        
        for instr in scope.instrs.iter() {
            for var in instr.get_variables(&mir) {
                needed_vars.insert(var);
            }
        }
        
        let missing_vars: Vec<&str> = needed_vars
            .iter()
            .filter(|&&var| !scope.vars.contains_key(var))
            .map(|x| *x)
            .collect();
        
        if !missing_vars.is_empty() {
             declarations.insert(*scope_id, missing_vars);
        }
    }
    
    // mir
    //     .scopes
    //     .iter()
    //     .for_each(|(i, scope)| {
    //         let mut needed_vars = HashSet::new();
            
    //         scope
    //             .instrs
    //             .iter()
    //             .for_each(|instr| {
    //                 instr
    //                     .get_variables(&mir)
    //                     .iter()
    //                     .for_each(|&x| { needed_vars.insert(x); } );
    //             });
            
    //         let missing_vars = needed_vars
    //             .iter()
    //             .filter(|&&x| !scope.vars.contains_key(x))
    //             .map(|x| *x)
    //             .collect::<Vec<_>>();
            
    //         if !missing_vars.is_empty() {
    //             declarations.insert(*i, missing_vars);
    //         }
    //     });
        
    
    (mir_out, declarations)
}

trait GetVariables<'src> {
    fn get_variables(&'src self, mir: &'src Mir<'src>) -> Vec<&'src str>;
}

impl<'src> GetVariables<'src> for Stmt<'src> {
    fn get_variables(&'src self, mir: &'src Mir) -> Vec<&'src str> {
        match self {
            
            Stmt::Return { expr, .. } => expr.iter().flat_map(|e| e.get_variables(mir)).collect(),
            Stmt::Assignment { expr, .. } => expr.get_variables(mir),
            Stmt::Assign { lhs, rhs, .. } => {
                let mut vars = lhs.get_variables(mir);
                vars.extend(rhs.get_variables(mir));
                vars
            }
            Stmt::FuncCall(call) => call.args.iter().flat_map(|a| a.get_variables(mir)).collect(),
            Stmt::GoToBlock { node_id } => mir.scopes.get(node_id)
                .map(|scope| scope.instrs.iter()
                    .flat_map(|s| s.get_variables(mir))
                    .collect())
                .unwrap_or_default(),

            Stmt::GoToIfCond { cond, then_block_id, else_block_id, .. } => {
                let mut vars = cond.get_variables(mir);
                
                if let Some(scope) = mir.scopes.get(then_block_id) {
                    vars.extend(scope.instrs.iter()
                        .flat_map(|s| s.get_variables(mir)));
                }

                if let Some(else_id) = else_block_id {
                    if let Some(scope) = mir.scopes.get(else_id) {
                        vars.extend(scope.instrs.iter()
                            .flat_map(|s| s.get_variables(mir)));
                    }
                }
                
                vars
            }

            Stmt::GoToWhile { cond, loop_id, .. } => {
                let mut vars = cond.get_variables(mir);
                
                if let Some(scope) = mir.scopes.get(loop_id) {
                    vars.extend(scope.instrs.iter()
                        .flat_map(|s| s.get_variables(mir)));
                }
                
                vars
            }
            _ => vec![],
        }
    }
}

impl<'src> GetVariables<'src> for Expr<'src> {
    fn get_variables(&'src self, mir: &'src Mir) -> Vec<&'src str> {
        match self {
            Expr::Identifier(ident) => vec![ident.ident],
            Expr::FuncCall(call) => call.args.iter().flat_map(|a| a.get_variables(mir)).collect(),
            Expr::MemLookup { ident, indices, .. } => {
                let mut vars = vec![ident.ident];
                vars.extend(indices.iter().flat_map(|i| i.get_variables(mir)));
                vars
            }
            Expr::StructInit { fields, .. } => fields.iter().flat_map(|f| f.expr.get_variables(mir)).collect(),
            Expr::CastType { expr, .. } => expr.get_variables(mir),
            Expr::Dereference { inner, .. } => inner.get_variables(mir),
            Expr::Reference { inner, .. } => inner.get_variables(mir),
            Expr::LogicalExpr(lexpr) => lexpr.get_variables(mir),
            Expr::MathExpr(mexpr) => mexpr.get_variables(mir),
            _ => vec![],
        }
    }
}

impl<'src> GetVariables<'src> for LogicalExpr<'src> {
    fn get_variables(&'src self, mir: &'src Mir) -> Vec<&'src str> {
        match self {
            LogicalExpr::Not { inner, .. } => inner.get_variables(mir),
            LogicalExpr::Or { left, right, .. } => {
                let mut vars = left.get_variables(mir);
                vars.extend(right.get_variables(mir));
                vars
            }
            LogicalExpr::And { left, right, .. } => {
                let mut vars = left.get_variables(mir);
                vars.extend(right.get_variables(mir));
                vars
            }
            LogicalExpr::Comparison { left, right, .. } => {
                let mut vars = left.get_variables(mir);
                vars.extend(right.get_variables(mir));
                vars
            }
            LogicalExpr::Primary(expr) => expr.get_variables(mir),
        }
    }
}

impl<'src> GetVariables<'src> for MathExpr<'src> {
    fn get_variables(&'src self, mir: &'src Mir) -> Vec<&'src str> {
        match self {
            MathExpr::Additive { left, right, .. } => {
                let mut vars = left.get_variables(mir);
                vars.extend(right.get_variables(mir));
                vars
            }
            MathExpr::Multiplicative { left, right, .. } => {
                let mut vars = left.get_variables(mir);
                vars.extend(right.get_variables(mir));
                vars
            }
            MathExpr::Power { base, exp, .. } => {
                let mut vars = base.get_variables(mir);
                vars.extend(exp.get_variables(mir));
                vars
            }
            MathExpr::Primary(expr) => expr.get_variables(mir),
        }
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