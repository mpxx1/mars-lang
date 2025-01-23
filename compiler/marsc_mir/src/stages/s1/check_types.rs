use crate::stages::s1::sys_funs::*;
use crate::stages::s1::{FuncProto, MirS1, Scope, ScopeType, StructProto, Variable};
use ast::*;
use err::CompileError;
use pest::Span;
use std::collections::{HashMap, HashSet};

type Mir<'src> = MirS1<'src>;

use crate::GLOBAL_SCOPE_ID;

pub(crate) fn check_types(hir: hir::Hir) -> Result<Mir, CompileError> {
        
    let mut mir = Mir {
        code: hir.code,
        scopes: HashMap::new(),
        sys_funs: vec![],
    };
    mir.scopes.insert(
        GLOBAL_SCOPE_ID,
        Scope {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: GLOBAL_SCOPE_ID,
            structs: HashMap::new(),
            funs: HashMap::new(),
            vars: HashMap::new(),
            instrs: vec![],
            scope_type: ScopeType::Global,
        },
    );

    // sys funs init
    for f in sys_funs_init() {
        mir.sys_funs.push(f.ident);
        mir.scopes
            .get_mut(&GLOBAL_SCOPE_ID)
            .unwrap()
            .funs
            .insert(f.ident, f);
    }

    let mut structs = vec![];
    let mut funs = vec![];
    for stmt in hir.ast.program {
        match stmt {
            ast::ProgStmt::StructDecl(x) => {
                structs.push(x);
            }
            ast::ProgStmt::FuncDecl(x) => {
                funs.push(x);
            }
        }
    }

    for s in structs {
        scope_push_struct(GLOBAL_SCOPE_ID, &mut mir, s)?;
    }

    for f in funs {
        scope_push_func(GLOBAL_SCOPE_ID, &mut mir, f)?;
    }

    Ok(mir)
}

fn scope_push_struct<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    struct_obj: StructDecl<'src>,
) -> Result<(), CompileError<'src>> {
    // 1 check if same named structure already been declarated
    let scope_ref = mir.scopes.get(&scope_id).unwrap();
    if scope_ref.structs.contains_key(struct_obj.ident) {
        return Err(CompileError::new(
            struct_obj.span,
            format!(
                "Struct with name '{}' has already been declarated in this scope (line number {})",
                struct_obj.ident,
                get_span_line_index(scope_ref.structs.get(struct_obj.ident).unwrap().span)
            ),
        ));
    }

    // 2 check struct fields for uniq names
    let mut set = HashSet::new();
    if !struct_obj.fields.iter().all(|x| set.insert(x.ident)) {
        return Err(CompileError::new(
            struct_obj.span,
            format!("Struct '{}' fields has duplicated names", struct_obj.ident),
        ));
    }
     
    // проверка, что тип декларирован
    let mut missed_type = "".to_string();
    if struct_obj.fields.iter().any(|field| {
        if let Type::Custom(ident) = &field.ty {
            if check_struct_declared(scope_id, mir,  ident.ident).is_none() {
                missed_type = ident.ident.to_owned();
                return true;
            }
            return false;
        }
        false
    }) {
        return Err(CompileError::new(
            struct_obj.span,
            format!("Type {:?} is not declared in this scope", missed_type),
        ));
    }

    // todo recursive check by node_id
    // 3 check fields for no having inner type as itself (possible as ref only)
    for field in struct_obj.fields.iter() {
        if check_rec_ty(scope_id, mir, &field.ty, struct_obj.ident, field.span)? {
            return Err(CompileError::new(
                struct_obj.span,
                "Structs can't be recursive, try to use reference instead:\n\tstruct Example {\n\t\tfield: &Example\n\t}".to_owned()
            ));
        }
    }
    // if struct_obj.fields.iter().any(|field| {
    //     // let mut visited = HashSet::new();
    //     check_rec_ty(scope_id, mir, &field.ty, struct_obj.ident, field.span)?
    //     // contains_recursive_type(mir, scope_id, &field.ty, struct_obj.ident, &mut visited)
    // }) {
    //     return Err(CompileError::new(
    //         struct_obj.span,
    //         "Structs can't be recursive, try to use reference instead:\n\tstruct Example {\n\t\tfield: &Example\n\t}".to_owned()
    //     ));
    // }

    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    // 4 add struct to scope
    scope_ref
        .structs
        .insert(struct_obj.ident, StructProto::from(scope_id, struct_obj));

    Ok(())
}

fn check_struct_declared<'src>(
    mut scope_id: usize,
    mir: &MirS1<'src>,
    type_name: &str,
) -> Option<usize> {
    loop {
        
        let scope = mir.scopes.get(&scope_id).unwrap();

        if scope.structs.contains_key(type_name) {
            return Some(scope.node_id);
        }
        
        if scope_id == GLOBAL_SCOPE_ID {
            return None;
        }
        
        scope_id = scope.parent_id;
    }
}

fn check_rec_ty<'src>(
    mut scope_id: usize,
    mir: &MirS1<'src>,
    ty: &Type<'src>,
    target_ty: &str,
    span: Span<'src>,
) -> Result<bool, CompileError<'src>> { 
    match ty {
        Type::Custom(x) => {
            if x.ident == target_ty {
                return Ok(true);
            }
            
            let dst_scope = check_struct_declared(scope_id, mir, x.ident);
            if dst_scope.is_none() {
                return Err(CompileError::new(span, "Type not found".to_owned()));
            }
            scope_id = dst_scope.unwrap();
            
            let proto = mir.scopes.get(&scope_id).unwrap().structs.get(x.ident).unwrap();
            
            for field in proto.fields.iter() {
                if check_rec_ty(scope_id, mir, &field.ty, target_ty, field.span)? {
                    return Ok(true);
                }
            }
        
            Ok(false)
        }
        
        Type::Array(inner, _) | Type::Vec(inner) => {
            check_rec_ty(scope_id, mir, inner, target_ty, span)
        }
        Type::Ref(_) => Ok(false),
        _ => Ok(false),
    }
}

fn scope_push_func<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    func_obj: FuncDecl<'src>,
) -> Result<(), CompileError<'src>> {
    // 1 check if same named func already been declarated
    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    if scope_ref.funs.contains_key(func_obj.ident) {
        return Err(
            CompileError::new(
                func_obj.span,
                format!(
                    "Function with name '{}' has already been declarated in this scope (line number {})",
                    func_obj.ident,
                    get_span_line_index(scope_ref.funs.get(func_obj.ident).unwrap().span)
                ))
        );
    }

    // 2 check if same named func is sys func
    if mir.sys_funs.contains(&func_obj.ident) {
        return Err(
            CompileError::new(
                func_obj.span,
                format!(
                    "Can not define function with name '{}'\n\tIt's already defined in list of system functions: \n\t\t{:#?}",
                    func_obj.ident,
                    mir.sys_funs
                )
            ));
    }

    // 3 check func args
    let mut set = HashSet::new();
    if !func_obj.args.iter().all(|x| set.insert(x.ident)) {
        return Err(CompileError::new(
            func_obj.span,
            format!(
                "Function '{}' arguments has duplicated names",
                func_obj.ident
            ),
        ));
    }

    // 4 add func to scope
    let ret_ty = func_obj.return_type.clone();
    let (fn_proto, logic_block) = func_decl_split(scope_id, func_obj);
    let fn_span = fn_proto.span;
    let fn_id = fn_proto.node_id;
    let fn_args = fn_proto.args.clone();
    scope_ref.funs.insert(fn_proto.ident, fn_proto);

    // 5 procceed with function body
    // create new scope
    mir.scopes.insert(
        fn_id,
        Scope {
            parent_id: scope_id,
            node_id: fn_id,
            structs: HashMap::new(),
            funs: HashMap::new(),
            vars: HashMap::new(),
            instrs: vec![],
            scope_type: ScopeType::Function,
        },
    );

    // 6 push fn args to new scope
    for arg in fn_args {
        scope_push_var(
            fn_id,
            mir,
            Variable {
                parent_id: fn_id,
                node_id: arg.node_id,
                ident: arg.ident,
                ty: arg.ty,
                decl_span: arg.span,
            },
        )?;
    }

    // 7 push instrs to fn scope
    base_block_proceed(fn_id, mir, logic_block, ret_ty.clone(), false)?;

    // 8 check if there is no return in the end of fn
    let fn_scope = mir.scopes.get_mut(&fn_id).unwrap();
    let instrs = &mut fn_scope.instrs;
    let last_instr = instrs.pop();
    if last_instr.is_none() {
        return Err(CompileError::new(
            fn_span,
            "Remove empty function".to_owned(),
        ));
    }
    let last_instr = last_instr.unwrap();
    if !matches!(last_instr, Stmt::Return { .. }) {
        return Err(CompileError::new(
            fn_span,
            "No return in the end of function".to_owned(),
        ));
    }
    instrs.push(last_instr);

    Ok(())
}

fn scope_push_var<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    var_obj: Variable<'src>,
) -> Result<(), CompileError<'src>> {
    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    if scope_ref.vars.contains_key(var_obj.ident) {
        let idx = get_span_line_index(scope_ref.vars.get(var_obj.ident).unwrap().decl_span);
        return Err(
            CompileError::new(
                var_obj.decl_span,
                format!(
                    "Variable with name '{}' has already been declarated in this scope (line number {})",
                    var_obj.ident,
                    idx,
                )
            )
        );
    }

    scope_ref.vars.insert(var_obj.ident, var_obj);

    Ok(())
}

fn scope_push_instr<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
    ret_ty: Type<'src>,
    in_loop: bool,
) -> Result<(), CompileError<'src>> {
    match instr {
        x if matches!(x, ast::Stmt::Assignment { .. }) => scope_push_assignment(scope_id, mir, x)?,
        x if matches!(x, ast::Stmt::Assign { .. }) => scope_push_assign(scope_id, mir, x)?,
        ast::Stmt::FuncCall(fc) => scope_push_func_call(scope_id, mir, fc)?,
        ast::Stmt::FuncDecl(x) => scope_push_func(scope_id, mir, x)?,
        ast::Stmt::StructDecl(x) => scope_push_struct(scope_id, mir, x)?,
        ast::Stmt::Block(x) => scope_push_block(scope_id, mir, x, ret_ty, in_loop)?,
        x if matches!(x, ast::Stmt::IfElse { .. }) => {
            scope_push_if_else(scope_id, mir, x, ret_ty, in_loop)?
        }
        x if matches!(x, ast::Stmt::WhileLoop { .. }) => {
            scope_push_while_loop(scope_id, mir, x, ret_ty)?
        }
        x if matches!(x, ast::Stmt::Return { .. }) => scope_push_return(scope_id, mir, x, ret_ty)?,
        x if matches!(x, ast::Stmt::Break { .. }) => scope_push_break(scope_id, mir, x, in_loop)?,

        x => {
            panic!("Parsing stmt '{:?}' to mir - not implemented", x);
        }
    }

    Ok(())
}

fn scope_push_break<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
    in_loop: bool,
) -> Result<(), CompileError<'src>> {
    let Stmt::Break { node_id, span } = instr else {
        panic!("Something went wrong");
    };
    if !in_loop {
        return Err(CompileError::new(span, "Not in loop".to_owned()));
    }
    let scope = mir.scopes.get_mut(&scope_id).unwrap();
    scope.instrs.push(Stmt::Break { node_id, span });

    Ok(())
}

fn scope_push_return<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
    ret_ty: Type<'src>,
) -> Result<(), CompileError<'src>> {
    let Stmt::Return {
        node_id,
        mut expr,
        span,
    } = instr
    else {
        panic!("Something went wrong");
    };
    if ret_ty == Type::Void && expr.is_some() {
        return Err(CompileError::new(
            span,
            "Remove expression from return, function returns void".to_owned(),
        ));
    }

    if ret_ty != Type::Void && expr.is_none() {
        return Err(CompileError::new(
            span,
            "Missing expression in return body".to_owned(),
        ));
    };

    if let Some(ref mut expr) = expr {
        let expr_ty = resolve_expr_type(scope_id, mir, expr, ret_ty.clone())?;
        if expr_ty != ret_ty {
            return Err(CompileError::new(
                span,
                format!("Expected type: '{:?}, actual: '{:?}'", ret_ty, expr_ty),
            ));
        }
    }

    let scope = mir.scopes.get_mut(&scope_id).unwrap();
    scope.instrs.push(Stmt::Return {
        node_id,
        expr,
        span,
    });

    Ok(())
}

fn scope_push_while_loop<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
    ret_ty: Type<'src>,
) -> Result<(), CompileError<'src>> {
    let Stmt::WhileLoop {
        node_id,
        mut cond,
        body,
        span,
    } = instr
    else {
        panic!("Something went wrong");
    };
    let cond_ty = resolve_expr_type(scope_id, mir, &mut cond, Type::Unresolved)?;
    if cond_ty != Type::Bool {
        return Err(CompileError::new(
            span,
            format!(
                "Condition expression must have bool type only. Got type '{:?}'",
                cond_ty
            ),
        ));
    }
    let scope = mir.scopes.get_mut(&scope_id).unwrap();
    scope.instrs.push(Stmt::GoToWhile {
        cond,
        loop_id: node_id,
    });
    mir.scopes.insert(
        node_id,
        Scope {
            parent_id: scope_id,
            node_id,
            structs: HashMap::new(),
            funs: HashMap::new(),
            vars: HashMap::new(),
            instrs: vec![],
            scope_type: ScopeType::Block,
        },
    );
    base_block_proceed(node_id, mir, body, ret_ty, true)?;

    Ok(())
}

fn scope_push_if_else<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
    ret_ty: Type<'src>,
    in_loop: bool,
) -> Result<(), CompileError<'src>> {
    let Stmt::IfElse {
        node_id,
        else_id,
        mut cond,
        then_block,
        else_block,
        span,
    } = instr
    else {
        panic!("Something went wrong");
    };
    let cond_ty = resolve_expr_type(scope_id, mir, &mut cond, Type::Unresolved)?;
    if cond_ty != Type::Bool {
        return Err(CompileError::new(
            span,
            format!(
                "Condition expression must have bool type only. Got type '{:?}'",
                cond_ty
            ),
        ));
    }

    let scope = mir.scopes.get_mut(&scope_id).unwrap();
    scope.instrs.push(Stmt::GoToIfCond {
        cond,
        then_block_id: node_id,
        else_block_id: else_id,
    });
    mir.scopes.insert(
        node_id,
        Scope {
            parent_id: scope_id,
            node_id,
            structs: HashMap::new(),
            funs: HashMap::new(),
            vars: HashMap::new(),
            instrs: vec![],
            scope_type: ScopeType::Block,
        },
    );
    base_block_proceed(node_id, mir, then_block, ret_ty.clone(), in_loop)?;

    if let Some(bl) = else_block {
        mir.scopes.insert(
            else_id.unwrap(),
            Scope {
                parent_id: scope_id,
                node_id: else_id.unwrap(),
                structs: HashMap::new(),
                funs: HashMap::new(),
                vars: HashMap::new(),
                instrs: vec![],
                scope_type: ScopeType::Block,
            },
        );
        base_block_proceed(else_id.unwrap(), mir, bl, ret_ty, in_loop)?;
    }

    Ok(())
}

fn base_block_proceed<'src>(
    block_id: usize,
    mir: &mut Mir<'src>,
    instr: Block<'src>,
    ret_ty: Type<'src>,
    in_loop: bool,
) -> Result<(), CompileError<'src>> {
    let mut structs = vec![];
    let mut funs = vec![];
    let mut vars = vec![];
    let mut instrs = vec![];
    for stmt in instr.stmts {
        match stmt {
            ast::Stmt::StructDecl(x) => structs.push(x),
            ast::Stmt::FuncDecl(x) => funs.push(x),
            ast::Stmt::Assignment {
                node_id,
                ident,
                ty,
                expr,
                span,
            } => {
                vars.push(Variable {
                    parent_id: block_id,
                    node_id,
                    ident,
                    ty: ty.clone(),
                    decl_span: span,
                });
                instrs.push(ast::Stmt::Assignment {
                    node_id,
                    ident,
                    ty,
                    expr,
                    span,
                })
            }
            x => instrs.push(x),
        }
    }

    for struct_obj in structs {
        scope_push_struct(block_id, mir, struct_obj)?;
    }

    for fun in funs {
        scope_push_func(block_id, mir, fun)?;
    }

    for var in vars {
        scope_push_var(block_id, mir, var)?;
    }

    for instr in instrs {
        scope_push_instr(block_id, mir, instr, ret_ty.clone(), in_loop)?;
    }

    Ok(())
}

fn scope_push_block<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Block<'src>,
    ret_ty: Type<'src>,
    in_loop: bool,
) -> Result<(), CompileError<'src>> {
    let scope = mir.scopes.get_mut(&scope_id).unwrap();
    scope.instrs.push(Stmt::GoToBlock {
        node_id: instr.node_id,
    });
    mir.scopes.insert(
        instr.node_id,
        Scope {
            parent_id: scope_id,
            node_id: instr.node_id,
            structs: HashMap::new(),
            funs: HashMap::new(),
            vars: HashMap::new(),
            instrs: vec![],
            scope_type: ScopeType::Block,
        },
    );

    base_block_proceed(instr.node_id, mir, instr, ret_ty, in_loop)
}

fn scope_push_assign<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
) -> Result<(), CompileError<'src>> {
    let Stmt::Assign {
        node_id,
        mut lhs,
        mut rhs,
        span,
    } = instr
    else {
        panic!("Something went wrong")
    };

    let lty = resolve_expr_type(scope_id, mir, &mut lhs, Type::Unresolved)?;
    let rty = resolve_expr_type(scope_id, mir, &mut rhs, lty.clone())?;

    if lty != rty {
        return Err(CompileError::new(
            span,
            format!("Expected type: '{:?}', actual: '{:?}'", lty, rty),
        ));
    }

    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    scope_ref.instrs.push(Stmt::Assign {
        node_id,
        lhs,
        rhs,
        span,
    });

    Ok(())
}

fn scope_push_func_call<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    mut fc: FuncCall<'src>,
) -> Result<(), CompileError<'src>> {
    check_fn_call_args(scope_id, mir, &mut fc)?;
    mir.scopes
        .get_mut(&scope_id)
        .unwrap()
        .instrs
        .push(Stmt::FuncCall(fc));

    Ok(())
}

fn scope_push_assignment<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
) -> Result<(), CompileError<'src>> {
    let Stmt::Assignment {
        node_id,
        ident,
        ty,
        mut expr,
        span,
    } = instr
    else {
        panic!("Something went wrong");
    };

    let mut var = mir
        .scopes
        .get_mut(&scope_id)
        .unwrap()
        .vars
        .remove(ident)
        .unwrap();

    // check if this type exists (for struct)
    if var.ty != Type::Unresolved {
        // get inner type for var.ty
        fn unwrap_to_core_type(mut ty: Type) -> Type {
            use Type::*;
            while let Some(inner) = match ty.clone() {
                Vec(inner) | Array(inner, _) | Ref(inner) => Some(*inner),
                _ => None,
            } {
                ty = inner;
            }
            ty
        }

        if let Type::Custom(ref x) = unwrap_to_core_type(var.ty.clone()) {
            if !check_struct_exists(scope_id, mir, x.ident) {
                return Err(CompileError::new(
                    span,
                    format!("Struct '{}' not found", x.ident),
                ));
            }
        }
    }

    debug_assert_eq!(var.ty, ty);
    let expr_type = resolve_expr_type(scope_id, mir, &mut expr, ty)?;

    if var.ty == Type::Unresolved {
        var.ty = expr_type;
    } else if var.ty != expr_type {
        return Err(CompileError::new(
            span,
            format!(
                "Can not assign to variable '{}': it's type is '{:?}', type of expr: '{:?}'",
                var.ident, var.ty, expr_type,
            ),
        ));
    }

    let var_type = var.ty.clone();
    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    scope_ref.vars.insert(var.ident, var);
    scope_ref.instrs.push(ast::Stmt::Assignment {
        node_id,
        ident,
        ty: var_type,
        expr,
        span,
    });

    Ok(())
}

pub(crate) fn resolve_expr_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    expr: &mut Expr<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    // dbg!("resolve_expr_type", &opt_type);
    let out_type = match expr {
        Expr::Identifier(x) => {
            let opt_type = resolve_ident_type(scope_id, mir, x.ident);
            if opt_type.is_none() {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find identifier {:?}", x.ident),
                ));
            }
            opt_type.unwrap()
        }

        Expr::Literal(x) => resolve_lit_type(x, opt_type)?,

        Expr::FuncCall(x) => {
            // 1 check fn args
            check_fn_call_args(scope_id, mir, x)?;

            // 2 check fn return type
            resolve_fn_ret_type(mir, x)
        }

        Expr::ArrayDecl {
            node_id: _,
            list,
            span,
        } => resolve_arr_decl_type(scope_id, mir, list, span, opt_type)?,

        x if matches!(x, Expr::MemLookup { .. }) => resolve_memlookup_type(scope_id, mir, x)?,
        x if matches!(x, Expr::StructFieldCall { .. }) => {
            resolve_struct_field_type(scope_id, mir, x)?
        }
        x if matches!(x, Expr::StructInit { .. }) => resolve_struct_init_type(scope_id, mir, x)?,
        x if matches!(x, Expr::CastType { .. }) => resolve_cast_type(scope_id, mir, x)?,
        x if matches!(x, Expr::Reference { .. }) => resolve_ref_type(scope_id, mir, x)?,
        x if matches!(x, Expr::Dereference { .. }) => resolve_deref_type(scope_id, mir, x)?,

        Expr::MathExpr(x) => resolve_math_expr_type(scope_id, mir, x, opt_type)?,
        Expr::LogicalExpr(x) => resolve_logical_expr_type(scope_id, mir, x, opt_type)?,

        x => {
            panic!("Unimplemented expression: {x:?}");
        }
    };

    Ok(out_type)
}

fn resolve_logical_expr_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    logical_expr: &mut LogicalExpr<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    fn resolve_base<'src>(
        scope_id: usize,
        mir: &mut Mir<'src>,
        left: &mut LogicalExpr<'src>,
        right: &mut LogicalExpr<'src>,
        opt_type: Type<'src>,
        span: &mut Span<'src>,
    ) -> Result<Type<'src>, CompileError<'src>> {
        if opt_type != Type::Unresolved && opt_type != Type::Bool {
            return Err(CompileError::new(
                *span,
                "Logical expressions can be used with bool type only".to_owned(),
            ));
        }

        let left_ty = resolve_logical_expr_type(scope_id, mir, left, opt_type)?;
        if left_ty != Type::Bool {
            return Err(CompileError::new(
                *span,
                format!(
                    "Logical expressions can be used with bool type only, got type: '{:?}'",
                    left_ty
                ),
            ));
        }

        let right_ty = resolve_logical_expr_type(scope_id, mir, right, left_ty.clone())?;
        if right_ty != Type::Bool {
            return Err(CompileError::new(
                *span,
                format!(
                    "Logical expressions can be used with bool type only, got type: '{:?}'",
                    right_ty
                ),
            ));
        }

        Ok(left_ty)
    }

    match logical_expr {
        LogicalExpr::And {
            node_id: _,
            left,
            right,
            span,
        } => resolve_base(scope_id, mir, left, right, opt_type, span),

        LogicalExpr::Or {
            node_id: _,
            left,
            right,
            span,
        } => resolve_base(scope_id, mir, left, right, opt_type, span),

        LogicalExpr::Comparison {
            node_id: _,
            left,
            right,
            op: _,
            span,
        } => {
            if opt_type != Type::Unresolved && opt_type != Type::Bool {
                return Err(CompileError::new(
                    *span,
                    "Logical expressions can be used with bool type only".to_owned(),
                ));
            }

            let left_ty = resolve_math_expr_type(scope_id, mir, left, opt_type.clone())?;

            if left_ty != Type::I64 && left_ty != Type::F64 {
                return Err(CompileError::new(
                    *span,
                    format!("Can use comperison operations with i64, f64 types only, actual type: '{:?}'", left_ty),
                ));
            }

            let right_ty = resolve_math_expr_type(scope_id, mir, right, left_ty.clone())?;
            if right_ty != Type::I64 && right_ty != Type::F64 {
                return Err(CompileError::new(
                    *span,
                    format!("Can use comperison operations with i64, f64 types only, actual type: '{:?}'", right_ty),
                ));
            }

            if right_ty != left_ty {
                return Err(CompileError::new(
                    *span,
                    format!("Expected type: '{:?}', actual: '{:?}'", left_ty, right_ty),
                ));
            }

            Ok(Type::Bool)
        }

        LogicalExpr::Not {
            node_id: _,
            inner,
            span,
        } => {
            if opt_type != Type::Unresolved && opt_type != Type::Bool {
                return Err(CompileError::new(
                    *span,
                    "Not expression can be used with bool type only".to_owned(),
                ));
            }
            let act_ty = resolve_logical_expr_type(scope_id, mir, inner, opt_type)?;
            if act_ty != Type::Bool {
                return Err(CompileError::new(
                    *span,
                    format!("Expecting bool type, but actual was: '{:?}'", act_ty),
                ));
            }

            Ok(Type::Bool)
        }

        LogicalExpr::Primary(x) => resolve_expr_type(scope_id, mir, x, opt_type),
    }
}

fn resolve_math_expr_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    math_expr: &mut MathExpr<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    fn base_math<'src>(
        scope_id: usize,
        mir: &mut Mir<'src>,
        left: &mut MathExpr<'src>,
        right: &mut MathExpr<'src>,
        opt_type: Type<'src>,
        span: &mut Span<'src>,
    ) -> Result<Type<'src>, CompileError<'src>> {
        if opt_type != Type::Unresolved && opt_type != Type::I64 && opt_type != Type::F64 {
            return Err(CompileError::new(
                *span,
                format!(
                    "Can not use math expressions with type: '{:?}'. f64 and i64 only possible.",
                    opt_type
                ),
            ));
        }

        let left_ty = resolve_math_expr_type(scope_id, mir, left, opt_type.clone())?;
        if opt_type != Type::Unresolved && opt_type != left_ty {
            return Err(CompileError::new(
                *span,
                format!("Expected type: '{:?}', actual: '{:?}'", opt_type, left_ty),
            ));
        }

        if left_ty != Type::I64 && left_ty != Type::F64 {
            return Err(CompileError::new(
                *span,
                format!(
                    "Can use math operations with i64, f64 types only, actual type: '{:?}'",
                    left_ty
                ),
            ));
        }

        let right_ty = resolve_math_expr_type(scope_id, mir, right, left_ty.clone())?;
        if right_ty != Type::I64 && right_ty != Type::F64 {
            return Err(CompileError::new(
                *span,
                format!(
                    "Can use math operations with i64, f64 types only, actual type: '{:?}'",
                    right_ty
                ),
            ));
        }

        if right_ty != left_ty {
            return Err(CompileError::new(
                *span,
                format!("Expected type: '{:?}', actual: '{:?}'", left_ty, right_ty),
            ));
        }

        Ok(left_ty)
    }

    match math_expr {
        MathExpr::Additive {
            node_id: _,
            left,
            right,
            op: _,
            span,
        } => base_math(scope_id, mir, left, right, opt_type, span),

        MathExpr::Multiplicative {
            node_id: _,
            left,
            right,
            op: _,
            span,
        } => base_math(scope_id, mir, left, right, opt_type, span),

        MathExpr::Power {
            node_id: _,
            base,
            exp,
            span,
        } => base_math(scope_id, mir, base, exp, opt_type, span),

        MathExpr::Primary(x) => resolve_expr_type(scope_id, mir, x, opt_type.clone()),
    }
}

fn resolve_arr_decl_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    list: &mut Vec<Expr<'src>>,
    span: &mut Span<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let av_type = opt_type.clone();
    let opt_type = if let Type::Array(x, _) = opt_type {
        *x
    } else if let Type::Vec(x) = opt_type {
        *x
    } else {
        Type::Unresolved
    };
    let vec_ty = resolve_vec_type(scope_id, mir, list, span, opt_type)?;

    // check arr or vec
    if let Type::Array(_, 0) = av_type {
        return Err(CompileError::new(
            *span,
            "Array can not have lenght = 0".to_owned(),
        ));
    }

    if let Type::Array(ty, len) = av_type.clone() {
        // проверить эти поля на совпадение
        if len != list.len() {
            return Err(CompileError::new(
                *span,
                format!(
                    "Array lenght in declaration = {}, actual lenght = {}",
                    len,
                    list.len()
                ),
            ));
        }

        let Type::Vec(x) = vec_ty else {
            panic!("Something went wrong")
        };
        if *ty != *x {
            return Err(CompileError::new(
                *span,
                "Type of expressions in array declaration does not match type in declareatoin"
                    .to_owned(),
            ));
        }

        return Ok(Type::Array(ty, len));
    }

    if av_type == Type::Unresolved && !list.is_empty() {
        let Type::Vec(x) = vec_ty else {
            panic!("Something went wrong")
        };

        return Ok(Type::Array(Box::new(*x), list.len()));
    } else if av_type == Type::Unresolved && list.is_empty() {
        return Err(CompileError::new(
            *span,
            "Array can not have lenght = 0".to_owned(),
        ));
    }

    Ok(vec_ty)
}

fn resolve_deref_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    deref_obj: &mut Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let Expr::Dereference {
        node_id: _,
        inner,
        span,
    } = deref_obj
    else {
        panic!("Something went wrong");
    };
    let inner_type = resolve_expr_type(scope_id, mir, inner, Type::Unresolved)?;
    let Type::Ref(x) = inner_type else {
        return Err(CompileError::new(
            *span,
            format!(
                "Can call dereference operator (*) on referenced type objects only. Expression type: '{:?}'",
                inner_type
            )
        ));
    };

    Ok(*x.clone())
}

fn resolve_ref_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    reference: &mut Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let Expr::Reference {
        node_id: _,
        inner,
        span: _,
    } = reference
    else {
        panic!("Something went wrong");
    };
    let inner_ty = resolve_expr_type(scope_id, mir, inner, Type::Unresolved)?;

    Ok(Type::Ref(Box::new(inner_ty)))
}

fn resolve_cast_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    cast: &mut Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    // can cast only i64 to f64 and f64 to i64
    let Expr::CastType {
        node_id: _,
        cast_to,
        expr,
        span,
    } = cast
    else {
        panic!("Something went wrong");
    };
    let dst_ty = *(*cast_to).clone();
    if dst_ty != Type::I64 && dst_ty != Type::F64 {
        return Err(CompileError::new(
            *span,
            "Can cast only i64 to f64 and f64 to i64".to_owned(),
        ));
    }

    let src_ty = resolve_expr_type(scope_id, mir, expr, Type::Unresolved)?;
    if src_ty == dst_ty {
        return Err(CompileError::new(
            *span,
            format!(
                "Remove redundant type cast: expr type - '{:?}', distanation type - '{:?}'",
                src_ty, dst_ty
            ),
        ));
    }

    if src_ty != Type::I64 && src_ty != Type::F64 {
        return Err(CompileError::new(
            *span,
            "Can cast only i64 to f64 and f64 to i64".to_owned(),
        ));
    }

    Ok(dst_ty)
}

fn resolve_struct_init_type<'src>(
    mut scope_id: usize,
    mir: &mut Mir<'src>,
    struct_init: &mut Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let Expr::StructInit {
        ident,
        fields,
        span,
        ..
    } = struct_init
    else {
        panic!("Something went wrong");
    };

    while let Some(scope) = mir.scopes.get(&scope_id) {
        if let Some(struct_proto) = scope.structs.get(ident.ident) {
            // Check if the number of fields matches.
            if struct_proto.fields.len() != fields.len() {
                return Err(CompileError::new(
                    *span,
                    format!(
                        "Struct '{}' expects {} fields, but {} were provided.",
                        ident.ident,
                        struct_proto.fields.len(),
                        fields.len()
                    ),
                ));
            }

            break;
        }

        if scope_id == 0 {
            return Err(CompileError::new(
                *span,
                format!("Struct '{}' not found.", ident.ident),
            ));
        }

        scope_id = scope.parent_id;
    }

    // Check each field's type.
    let mut fields_proto = HashMap::new();
    let mut fields_init = HashMap::new();
    let struct_proto = mir
        .scopes
        .get(&scope_id)
        .unwrap()
        .structs
        .get(ident.ident)
        .unwrap();
    let tmp_args = struct_proto.fields.clone();

    for elem in tmp_args.iter() {
        fields_proto.insert(elem.ident, elem);
    }

    for elem in fields.iter_mut() {
        fields_init.insert(elem.ident.ident, elem);
    }

    for (name, fld) in fields_init.iter_mut() {
        let actual_type = fields_proto.get(name);
        if actual_type.is_none() {
            return Err(CompileError::new(
                *span,
                format!(
                    "Struct '{:?}' does not have field '{:?}'",
                    ident.ident, name
                ),
            ));
        }
        let actual_type = actual_type.unwrap().ty.clone();
        let fld_init_type = resolve_expr_type(scope_id, mir, &mut fld.expr, actual_type.clone())?;

        if actual_type != fld_init_type {
            return Err(CompileError::new(
                *span,
                format!(
                    "Field '{:?}' has type '{:?}', but expression provided has type '{:?}'.",
                    name, actual_type, fld_init_type
                ),
            ));
        }
    }

    Ok(Type::Custom(ident.clone()))
}

fn resolve_struct_field_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    struct_field: &mut Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let Expr::StructFieldCall {
        ident, field, span, ..
    } = struct_field
    else {
        panic!("Something went wrong");
    };

    let Some(custom_type) = resolve_ident_type(scope_id, mir, ident.ident) else {
        return Err(CompileError::new(
            *span,
            format!("Can not find variable with name {:?}", ident.ident),
        ));
    };
    let Type::Custom(Identifier { ident, span, .. }) = custom_type else {
        return Err(CompileError::new(
            *span,
            format!("Can not find struct prototype with name {:?}", ident.ident),
        ));
    };

    let mut cur_scope = scope_id;
    while let Some(scope) = mir.scopes.get(&cur_scope) {
        if let Some(struct_proto) = scope.structs.get(&ident) {
            if let Some(arg) = struct_proto
                .fields
                .iter()
                .find(|arg| arg.ident == field.ident)
            {
                return Ok(arg.ty.clone());
            } else {
                return Err(CompileError::new(
                    span,
                    format!("Field '{}' not found in struct '{}'", field.ident, ident),
                ));
            }
        }

        if cur_scope == 0 {
            return Err(CompileError::new(
                span,
                format!("Struct '{}' not found", ident),
            ));
        }

        cur_scope = scope.parent_id;
    }

    Err(CompileError::new(
        span,
        format!("Struct '{}' not found", ident),
    ))
}

fn check_struct_exists<'src>(mut scope_id: usize, mir: &mut Mir<'src>, ident: &'src str) -> bool {
    while let Some(scope) = mir.scopes.get(&scope_id) {
        if scope.structs.contains_key(&ident) {
            return true;
        }

        if scope_id == 0 {
            return false;
        }

        scope_id = scope.parent_id;
    }

    false
}

fn resolve_memlookup_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    mem_lookup: &Expr<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let Expr::MemLookup {
        ident,
        indices,
        span,
        ..
    } = mem_lookup
    else {
        panic!("Something went wrong");
    };
    let mut current_type = resolve_expr_type(
        scope_id,
        mir,
        &mut Expr::Identifier(ident.clone()),
        Type::Unresolved,
    )?;

    if current_type == Type::Str && indices.len() == 1 {
        return Ok(Type::Char);
    } else if current_type == Type::Str && indices.len() != 1 {
        return Err(CompileError::new(
            *span,
            "Can not use more than one index with string".to_owned(),
        ));
    }

    for (i, _index) in indices.iter().enumerate() {
        match current_type {
            Type::Array(inner, _) | Type::Vec(inner) => {
                current_type = *inner;
            }
            _ => {
                return Err(CompileError::new(
                    *span,
                    format!(
                        "Invalid type access: attempted to index non-array type at index {}",
                        i + 1
                    ),
                ));
            }
        }
    }

    Ok(current_type)
}

fn resolve_vec_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    list: &mut [Expr<'src>],
    span: &mut Span<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    // dbg!("resolve_vec_type", &opt_type);
    if list.is_empty() {
        return Ok(Type::Vec(Box::new(opt_type)));
    }
    let ty = resolve_expr_type(scope_id, mir, &mut list[0], opt_type.clone())?;

    for item in list.iter_mut().skip(1) {
        if resolve_expr_type(scope_id, mir, item, opt_type.clone())? != ty {
            return Err(CompileError::new(
                *span,
                "Expressions in array/vec must have single type".to_owned(),
            ));
        }
    }

    Ok(Type::Vec(Box::new(ty)))
}

fn check_fn_call_args<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    func: &mut FuncCall<'src>,
) -> Result<(), CompileError<'src>> {
    if func.decl_scope_id.is_none() {
        let mut current_scope_id = scope_id;

        while let Some(scope) = mir.scopes.get(&current_scope_id) {
            if scope.funs.contains_key(func.ident.ident) {
                func.decl_scope_id = Some(scope.node_id);
                break;
            }

            if current_scope_id == 0 {
                return Err(CompileError::new(
                    func.span,
                    format!(
                        "Can not find declaration of function {:?}",
                        func.ident.ident
                    ),
                ));
            }

            current_scope_id = scope.parent_id;
        }
    }

    // check argumetns
    let proto = mir
        .scopes
        .get_mut(&func.decl_scope_id.unwrap()) 
        .unwrap()
        .funs
        .get(func.ident.ident)
        .unwrap();
    if func.args.len() != proto.args.len() {
        return Err(CompileError::new(
            func.span,
            format!(
                "Calling function '{}' with wrong arguments",
                func.ident.ident
            ),
        ));
    }

    // check system funs
    if mir.sys_funs.contains(&func.ident.ident) {
        check_sys_fn_args_types(scope_id, mir, func.clone())?;
    }

    for (i, expr) in func.args.iter_mut().enumerate() {
        let actual_type = mir
            .scopes
            .get_mut(&func.decl_scope_id.unwrap())
            .unwrap()
            .funs
            .get(func.ident.ident)
            .unwrap()
            .args[i]
            .ty
            .clone();
        if actual_type != Type::Any
            && resolve_expr_type(scope_id, mir, expr, actual_type.clone())? != actual_type
        {
            return Err(CompileError::new(
                func.span,
                format!(
                    "Calling function '{}' with wrong arguments",
                    func.ident.ident
                ),
            ));
        }
    }

    Ok(())
}

fn resolve_fn_ret_type<'src>(mir: &mut Mir<'src>, func: &mut FuncCall<'src>) -> Type<'src> {
    mir.scopes
        .get(&func.decl_scope_id.unwrap())
        .unwrap()
        .funs
        .get(&func.ident.ident)
        .unwrap()
        .return_type
        .clone()
}

fn resolve_ident_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    ident: &'src str,
) -> Option<Type<'src>> {
    let mut current_scope_id = scope_id;
    loop {
        let scope = mir.scopes.get(&current_scope_id)?;

        if let Some(var) = scope.vars.get(ident) {
            return Some(var.ty.clone());
        }

        if scope.scope_type == ScopeType::Function {
            return None;
        }

        current_scope_id = scope.parent_id;
    }
}

fn resolve_lit_type<'src>(
    lit: &Literal<'src>,
    possible_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    match lit {
        Literal::Int { .. } => Ok(Type::I64),
        Literal::Float { .. } => Ok(Type::F64),
        Literal::Str { .. } => Ok(Type::Str),
        Literal::Bool { .. } => Ok(Type::Bool),
        Literal::Char { .. } => Ok(Type::Char),
        Literal::NullRef { node_id: _, span } => {
            if possible_type == Type::Unresolved {
                return Err(CompileError::new(
                    *span,
                    "Must specify type of reference\n\tExample: var a: &A = null;".to_owned(),
                ));
            }
            if let Type::Ref(x) = possible_type {
                return Ok(Type::Ref(x));
            }

            Err(CompileError::new(
                *span,
                "Null can be used as refrence only".to_owned(),
            ))
        }
    }
}

fn func_decl_split(scope_id: usize, func: FuncDecl) -> (FuncProto, Block) {
    (
        FuncProto {
            parent_id: scope_id,
            node_id: func.node_id,
            ident: func.ident,
            args: func.args,
            return_type: func.return_type,
            span: func.span,
        },
        func.body,
    )
}

fn get_span_line_index(span: pest::Span) -> usize {
    let input = span.get_input();
    let start = span.start();
    input[..start].chars().filter(|&c| c == '\n').count() + 1
}

#[test]
fn main_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
    struct A { a: i64 }
    
    fn main() -> i64 {
        var a_s = A { a: 10 };
        var a = &a_s.a;
        var b = *a;
        
        b += 20;
        *a = 45;
        
        var arr = ["0", "1", "30"];
        arr[0] = "1e3";
        
        return 0;
    }
    
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_if_else<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn main() -> void {
            var a = 10 < 20;
            if a {
                println("ya");
            }
            
            if a {
                println("hello");
            } else {
                println("hello");
            }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_fn<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        struct A { a: i64 }
        fn hello(a: i64, b: str) -> i64 {
            
            var s_a = A { a: 10 };
            
            return 10;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_struct_recursive<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn main() -> void {
            struct B { b: &B }
            struct A { a: B }
            struct C { c: A }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_inner_fn<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn hello(a: i64, b: str) -> i64 {
            
            struct A {}
            fn tt(a: i64) -> i64 { return 2; }
            
            {
                var c = a;
                tt(c);
            }
            
            print("hello");
            
            return 0;
        }
        
        fn main() -> void { hello(10, "hello"); return; }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_struct<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn hello() -> Vec<i64> { 
            var b = 10;
            var v: Vec<i64> = [0, b];
            return v;
        } 
        
        fn main() -> void {
            
            var a: [Vec<i64>; 2] = [[0, 10, 100], hello(),];
            
            var c = a[0][0];
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_math_expr<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
            var a = "hello";
            var b: str = "var_a = " + a;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    // let mir = check_types(hir)?;

    // println!("{mir:#?}");
    println!("{:#?}", hir.ast.program);

    Ok(())
}

#[test]
fn print_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
            
            var d = 10.44345234525;
            var c = (i64) d;
            var b = &c;
            var a = 10 * *b;
            
            var wow = 10 * 59 ** 3435 ** 343 - 3434 + 343 - 3423 * *b / ((i64) d) % a * 8 ;
            print("{a}, {wow}");
            
            var o = (f64) wow;
            println("hello w{o}rld");
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn math_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
            
            var d = 10.44345234525;
            var c = ((i64) d) + 10;
            var b = (i64) d;
            var a = 10 >= 20 && 30 <= 5 || c > ((i64) d);
            
            while a {
                println("b: {b}, c: {c}");
            }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;
    println!("{mir:#?}");

    Ok(())
}

#[test]
fn fn_block_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
        
            var b = 20;
            fn hello() -> f64 {
                
                return (f64) 10;
            }
        
            var a = 10;
            {
                a += 20;
            }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;
    println!("{mir:#?}");

    Ok(())
}

#[test]
fn sys_fn_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
        
            var v: Vec<&i64> = [];
            var ref: &f64 = null;
            var a = 0;
            println("10");
            vec_push(&v, &a);
            vec_pop(&v);
            
            fn main() -> void {
                var a = 10;
                return;
            }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;
    println!("{mir:#?}");

    Ok(())
}

#[test]
fn while_test<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"        
        fn main() -> void {
        
            
            while 10 < 20 {
                var a = 20;
                if a > 9 {
                    break;
                } else {
                    print("hello");
                    break;
                }
                
                return;
            }
            
            fn hello() -> bool {
                return true;
            }
            
            return;
        }
    "#;

    let hir = hir::compile_hir(&inp)?;
    let mir = check_types(hir)?;
    println!("{mir:#?}");

    Ok(())
}
