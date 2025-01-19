use crate::{FuncProto, Mir, Scope, ScopeType, StructProto, Variable};
use crate::stages::sys_funs::*;
use ast::{Block, Expr, FuncDecl, Literal, Stmt, StructDecl, Type, FuncCall};
use err::CompileError;
use std::collections::{HashMap, HashSet};

use crate::GLOBAL_SCOPE_ID;

pub fn check_types<'src>(hir: hir::Hir<'src>) -> Result<Mir<'src>, CompileError<'src>> {
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
        mir.scopes.get_mut(&GLOBAL_SCOPE_ID).unwrap().funs.insert(f.ident, f);
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
    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
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

    // 3 check fields for no having inner type as itself (possible as ref only)
    if struct_obj.fields.iter().any(|x| {
        if let ast::Type::Custom(ref x) = x.ty {
            if x.ident == struct_obj.ident {
                return true;
            }
        }
        false
    }) {
        return Err(
            CompileError::new(
                struct_obj.span,
                "Structs can't be recursive, try to use reference instead:\n\tstruct Example {\n\t\tfield: &Example\n\t}".to_owned()
            ));
    }

    // 4 add struct to scope
    scope_ref.structs.insert(struct_obj.ident, StructProto::from(scope_id, struct_obj));

    Ok(())
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
    let (fn_proto, logic_block) = func_decl_split(scope_id, func_obj);
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
                is_used: false,
                decl_span: arg.span,
            },
        )?;
    }

    // 7 push instructions of function to this scope
    let mut structs = vec![];
    let mut funs = vec![];
    let mut vars = vec![];
    let mut instrs = vec![];
    for stmt in logic_block.stmts {
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
                    parent_id: fn_id,
                    node_id,
                    ident,
                    ty: ty.clone(),
                    is_used: false,
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
        scope_push_struct(fn_id, mir, struct_obj)?;
    }

    for fun in funs {
        scope_push_func(fn_id, mir, fun)?;
    }

    // 6 todo variables (check types of exprs)
    for var in vars {
        scope_push_var(fn_id, mir, var)?;
    }

    // 7 todo instructions
    for instr in instrs {
        scope_push_inst(fn_id, mir, instr)?;
    }

    // 8 check funs return exprs
    // todo

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

fn scope_push_inst<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
) -> Result<(), CompileError<'src>> {
    match instr {
        x if matches!(x, ast::Stmt::Assignment { .. }) => scope_push_assignment(scope_id, mir, x)?,
        ast::Stmt::FuncCall(fc) => scope_push_func_call(scope_id, mir, fc)?,
        _ => unimplemented!(),
    }

    Ok(())
}

fn scope_push_func_call<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    mut fc: FuncCall<'src>,
) -> Result<(), CompileError<'src>> {
    
    check_fn_call_args(scope_id, mir, &mut fc)?;
    mir.scopes.get_mut(&scope_id).unwrap().instrs.push(
        ast::Stmt::FuncCall(fc)
    );
    
    Ok(())
}

fn scope_push_assignment<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    instr: Stmt<'src>,
) -> Result<(), CompileError<'src>> {
    let ast::Stmt::Assignment {
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
    
    debug_assert_eq!(var.ty, ty);
    let expr_type = resolv_expr_type(scope_id, mir, &mut expr, ty)?;
    
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

pub(crate) fn resolv_expr_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    expr: &mut Expr<'src>,
    opt_type: Type<'src>,
) -> Result<Type<'src>, CompileError<'src>> {
    let out_type = match expr {
        Expr::Identifier(x) => {
            let opt_type = resolv_ident_type(scope_id, mir, x.ident);
            if opt_type.is_none() {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find identifier '{}'", x.ident),
                ));
            }
            opt_type.unwrap()
        }
        
        Expr::Literal(x) => {
            resolv_lit_type(x, opt_type)?
        }
        
        Expr::FuncCall(x) => {
            // 1 check fn args
            check_fn_call_args(scope_id, mir, x)?;
            
            // 2 check fn return type
            resolv_fn_ret_type(mir, x)
        }

        x => {println!("{x:?}"); unimplemented!()},
    };

    Ok(out_type)
}

fn check_fn_call_args<'src>(
    scope_id: usize, 
    mir: &mut Mir<'src>,
    func: &mut FuncCall<'src>,
) -> Result<(), CompileError<'src>> {
    
    if func.decl_scope_id.is_none() {
        let mut current_scope_id = scope_id;
    
        while let Some(scope) = mir.scopes.get(&current_scope_id) {
            if let Some(_) = scope.funs.get(func.ident.ident) {
                func.decl_scope_id = Some(scope.node_id);
                break;
            }
    
            if current_scope_id == 0 {
                return Err(CompileError::new(func.span,format!("Can not find declaration of function '{}'", func.ident.ident)));
            }
    
            current_scope_id = scope.parent_id;
        }
    }
    
    // check argumetns
    let proto = mir.scopes.get_mut(&func.decl_scope_id.unwrap()).unwrap().funs.get(func.ident.ident).unwrap();
    if func.args.len() != proto.args.len() {
        return Err(CompileError::new(func.span,format!("Calling function '{}' with wrong arguments", func.ident.ident)));
    }
    
    // check system funs
    if mir.sys_funs.contains(&func.ident.ident) {
        let v = func
             .args
             .iter_mut()
             .map(
                 |a| resolv_expr_type(scope_id, mir, a, Type::Unresolved).unwrap()
             )
             .collect::<Vec<_>>();
        
        return check_sys_fn_args_types(
            func,
           &v
        );
    }
    
    for (i, expr) in func.args.iter_mut().enumerate() {
        let actual_type = mir.scopes.get_mut(&func.decl_scope_id.unwrap()).unwrap().funs.get(func.ident.ident).unwrap().args[i].ty.clone();
        if resolv_expr_type(scope_id, mir, expr, Type::Unresolved)? != actual_type {
            return Err(CompileError::new(func.span,format!("Calling function '{}' with wrong arguments", func.ident.ident)));
        }
    }
    
    Ok(())
}

fn resolv_fn_ret_type<'src>(
    mir: &mut Mir<'src>,
    func: &mut FuncCall<'src>,
) -> Type<'src> {
    mir.scopes.get(&func.decl_scope_id.unwrap()).unwrap().funs.get(&func.ident.ident).unwrap().return_type.clone()
}


fn resolv_ident_type<'src>(
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

fn resolv_lit_type<'src>(lit: &Literal<'src>, possible_type: Type<'src>) -> Result<Type<'src>, CompileError<'src>> {
    match lit {
        Literal::Int { .. } => Ok(Type::I64),
        Literal::Float { .. } => Ok(Type::F64), 
        Literal::Str { .. } => Ok(Type::Str),
        Literal::Bool { .. } => Ok(Type::Bool),
        Literal::Char { .. } => Ok(Type::Char),
        Literal::NullRef { node_id: _, span } => {
            if possible_type == Type::Unresolved { return Err(CompileError::new(
                *span, "Must specify type of reference\n\tExample: var a: &A = null;".to_owned()
            )); }
            if let Type::Ref(x) = possible_type { 
                return Ok(Type::Ref(x));
            }
            
            return Err(CompileError::new(
                *span, "Null can be used as refrence only".to_owned()
            ));
        }
    }
}

fn func_decl_split<'src>(scope_id: usize, func: FuncDecl<'src>) -> (FuncProto<'src>, Block<'src>) {
    (FuncProto {
            parent_id: scope_id,
            node_id: func.node_id,
            ident: func.ident,
            args: func.args,
            return_type: func.return_type,
            is_used: false,
            span: func.span,
        },
        func.body,)
}

fn get_span_line_index<'src>(span: pest::Span<'src>) -> usize {
    let input = span.get_input();
    let start = span.start();
    input[..start].chars().filter(|&c| c == '\n').count() + 1
}

#[test]
fn main_test<'src>() -> Result<(), CompileError<'src>> {
    // для null нужно проверять, что тип переменной определен и ссылочный
    let inp = r#"
    
    fn main() -> i64 {
        var a: &i64 = null;
    }
    
    "#;
    
    let hir = hir::parser::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");
    
    Ok(())
}

#[test]
fn test_index_fn<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn hello(a: i64, b: str) -> i64 {
        
            var c = hello(a, b);
        }
    "#;

    let hir = hir::parser::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_inner_fn<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn tt(a: i64) -> void {}
        fn hello(a: i64, b: str) -> i64 {
        
            var c = a;
            tt(c);
            
            print("hello");
        }
    "#;

    let hir = hir::parser::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_struct<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        struct Hello {
            a: str,
            b: &Hello,
        }
    "#;

    let hir = hir::parser::compile_hir(&inp)?;
    let mir = check_types(hir)?;

    println!("{mir:#?}");

    Ok(())
}
