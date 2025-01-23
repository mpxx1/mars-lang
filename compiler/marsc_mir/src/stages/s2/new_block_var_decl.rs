use err::CompileError;
use std::collections::HashSet;
use super::*; 

type Mir<'src> = MirS2<'src>;

pub(crate) fn block_var_decl(mut mir: Mir) -> Result<Mir, CompileError> {
    let scope_ids: Vec<usize> = mir.scopes.keys().cloned().collect();
    
    for scope_id in scope_ids {
        let (parent_id, vars_to_check) = {
            
            let scope = mir.scopes.get_mut(&scope_id).unwrap();

            if !matches!(scope.scope_type, MIRScopeType::Block) {
                continue;
            }

            let all_vars: HashSet<String> = scope.instrs.iter()
                .flat_map(|instr| collect_vars_in_instr(instr))
                .collect();

            let vars_to_check: Vec<String> = all_vars.into_iter()
                .filter(|var| !scope.vars.contains_key(var))
                .collect();

            (scope.parent_id, vars_to_check)
        };
        
        // dbg!(&parent_id, &vars_to_check);

        let mut new_instrs = Vec::new();

        for var in vars_to_check {
            if let Some(parent_var) = find_var_in_parent_scopes(&mir, parent_id, &var) {
                let span = Span::new("Compiler generated", 0, 18).unwrap();
                let expr_span = span.clone();

                let expr = MIRExpr::Identifier {
                    ident: var.clone(),
                    span: expr_span,
                };

                new_instrs.push(MIRInstruction::Assignment {
                    ident: var,
                    ty: parent_var.ty.clone(),
                    expr,
                    span,
                });
            }
        }

        let scope = mir.scopes.get_mut(&scope_id).unwrap();
        for instr in new_instrs {
            scope.instrs.insert(0, instr);
        }
        
    }

    Ok(mir)
}


fn collect_vars_in_instr(instr: &MIRInstruction) -> HashSet<String> {
    let mut vars = HashSet::new();
    match instr {
        MIRInstruction::Return { expr, .. } => {
            if let Some(e) = expr {
                collect_vars_in_expr(e, &mut vars);
            }
        },
        MIRInstruction::Assignment { expr, .. } => {
            collect_vars_in_expr(expr, &mut vars);
        },
        MIRInstruction::Assign { lhs, rhs, .. } => {
            collect_vars_in_expr(lhs, &mut vars);
            collect_vars_in_expr(rhs, &mut vars);
        },
        MIRInstruction::FuncCall(call) => {
            for arg in &call.args {
                collect_vars_in_expr(arg, &mut vars);
            }
        },
        MIRInstruction::GoToIfCond { cond, .. } => {
            collect_vars_in_expr(cond, &mut vars);
        },
        MIRInstruction::GoToWhile { cond, .. } => {
            collect_vars_in_expr(cond, &mut vars);
        },
        _ => {}
    }
    vars
}

fn collect_vars_in_expr(expr: &MIRExpr, vars: &mut HashSet<String>) {
    match expr {
        MIRExpr::Identifier { ident, .. } => {
            vars.insert(ident.clone());
        },
        MIRExpr::FuncCall(call) => {
            for arg in &call.args {
                collect_vars_in_expr(arg, vars);
            }
        },
        MIRExpr::ArrayDecl { list, .. } => {
            for e in list {
                collect_vars_in_expr(e, vars);
            }
        },
        MIRExpr::MemLookup { ident, indices, .. } => {
            vars.insert(ident.clone());
            for idx in indices {
                collect_vars_in_expr(idx, vars);
            }
        },
        MIRExpr::StructFieldCall { ident, .. } => {
            vars.insert(ident.clone());
        },
        MIRExpr::StructInit { fields, .. } => {
            for (_, expr) in fields {
                collect_vars_in_expr(expr, vars);
            }
        },
        MIRExpr::CastType { expr, .. } => {
            collect_vars_in_expr(expr, vars);
        },
        MIRExpr::Dereference { inner, .. } => {
            collect_vars_in_expr(inner, vars);
        },
        MIRExpr::Reference { inner, .. } => {
            collect_vars_in_expr(inner, vars);
        },
        MIRExpr::LogicalExpr(lexpr) => {
            collect_vars_in_logical_expr(lexpr, vars);
        },
        MIRExpr::MathExpr(mexpr) => {
            collect_vars_in_math_expr(mexpr, vars);
        },
        _ => {}
    }
}

fn collect_vars_in_logical_expr(expr: &MIRLogicalExpr, vars: &mut HashSet<String>) {
    match expr {
        MIRLogicalExpr::Not { inner, .. } => collect_vars_in_logical_expr(inner, vars),
        MIRLogicalExpr::Or { left, right, .. } => {
            collect_vars_in_logical_expr(left, vars);
            collect_vars_in_logical_expr(right, vars);
        },
        MIRLogicalExpr::And { left, right, .. } => {
            collect_vars_in_logical_expr(left, vars);
            collect_vars_in_logical_expr(right, vars);
        },
        MIRLogicalExpr::Comparison { left, right, .. } => {
            collect_vars_in_math_expr(left, vars);
            collect_vars_in_math_expr(right, vars);
        },
        MIRLogicalExpr::Primary(expr) => collect_vars_in_expr(expr, vars),
    }
}

fn collect_vars_in_math_expr(expr: &MIRMathExpr, vars: &mut HashSet<String>) {
    match expr {
        MIRMathExpr::Additive { left, right, .. } => {
            collect_vars_in_math_expr(left, vars);
            collect_vars_in_math_expr(right, vars);
        },
        MIRMathExpr::Multiplicative { left, right, .. } => {
            collect_vars_in_math_expr(left, vars);
            collect_vars_in_math_expr(right, vars);
        },
        MIRMathExpr::Power { base, exp, .. } => {
            collect_vars_in_math_expr(base, vars);
            collect_vars_in_math_expr(exp, vars);
        },
        MIRMathExpr::Primary(expr) => collect_vars_in_expr(expr, vars),
    }
}

fn find_var_in_parent_scopes<'a>(
    mir: &'a MirS2,
    mut current_scope_id: usize,
    var_name: &str,
) -> Option<&'a MIRVariable> {
    loop {
        let current_scope = mir.scopes.get(&current_scope_id)?;
        if let Some(var) = current_scope.vars.get(var_name) {
            return Some(var);
        }
        let new_parent = current_scope.parent_id;
        if new_parent == current_scope_id {
            return None;
        }
        current_scope_id = new_parent;
    }
}