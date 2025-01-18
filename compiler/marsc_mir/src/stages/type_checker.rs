use std::collections::{HashMap, HashSet};
use err::CompileError;
use crate::{FuncProto, Mir, Scope, Variable};
use ast::{Block, Expr, FuncDecl, StructDecl, Type, Stmt};

use crate::GLOBAL_SCOPE_ID;

static mut SYS_FN_COUNTER: usize = 0;

fn sys_funs_init<'sf, 'src>() -> HashMap<&'sf str, usize> {
    fn gen_id() -> usize {
        unsafe {
            SYS_FN_COUNTER += 1;
            SYS_FN_COUNTER
        }
    }
    
    HashMap::from([
        ( "print", gen_id(), ),
    ])
}

pub fn check_types<'src, 'sf>(
    hir: hir::Hir<'src>
) -> Result<Mir<'src, 'sf>, CompileError<'src>> {
    
    let mut mir = Mir { code: hir.code, scopes: HashMap::new(), sys_funs: sys_funs_init() };
    mir.scopes.insert(GLOBAL_SCOPE_ID, Scope {
        parent_id: GLOBAL_SCOPE_ID,
        node_id: GLOBAL_SCOPE_ID,
        structs: HashMap::new(),
        funs: HashMap::new(),
        vars: HashMap::new(),
        instrs: vec![],
    });
    
    for stmt in hir.ast.program {
        match stmt {
            ast::ProgStmt::StructDecl(x) => {
                scope_push_struct(GLOBAL_SCOPE_ID, &mut mir, x)?;
            },
            ast::ProgStmt::FuncDecl(x) => {
                scope_push_func(GLOBAL_SCOPE_ID, &mut mir, x)?;
            },
        }
    }
    
    Ok(mir)
}

fn scope_push_struct<'src, 'sf>(
    scope_id: usize,
    mir: &mut Mir<'src, 'sf>,
    struct_obj: StructDecl<'src>
) -> Result<(), CompileError<'src>> {
    
    // 1 check if same named structure already been declarated
    let scope_ref = mir.scopes.get_mut(&scope_id).unwrap();
    if scope_ref.structs.contains_key(struct_obj.ident) {
        return Err(
            CompileError::new(
                struct_obj.span,
                format!(
                    "Struct with name '{}' has already been declarated in this scope (line number {})",
                    struct_obj.ident,
                    get_span_line_index(scope_ref.structs.get(struct_obj.ident).unwrap().span)
                ))
        );
    }
    
    // 2 check struct fields for uniq names
    let mut set = HashSet::new();
    if !struct_obj.fields.iter().all(|x| set.insert(x.ident)) {
        return Err(CompileError::new(struct_obj.span, format!("Struct '{}' fields has duplicated names", struct_obj.ident)));
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
    scope_ref.structs.insert(struct_obj.ident, struct_obj);
    
    Ok(())
}

fn scope_push_func<'src, 'sf>(
    scope_id: usize,
    mir: &mut Mir<'src, 'sf>,
    func_obj: FuncDecl<'src>
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
    if mir.sys_funs.contains_key(func_obj.ident) {
        return Err(
            CompileError::new(
                func_obj.span,
                format!(
                    "Can not define function with name '{}'\n\tIt's already defined in list of system functions: \n\t\t{:#?}",
                    func_obj.ident,
                    mir.sys_funs.iter().map(|x| x.0).collect::<Vec<_>>()
                )
            ));
    }
    
    // 3 check func args
    let mut set = HashSet::new();
    if !func_obj.args.iter().all(|x| set.insert(x.ident)) {
        return Err(CompileError::new(func_obj.span, format!("Function '{}' arguments has duplicated names", func_obj.ident)));
    }
    
    // 4 add func to scope
    let (fn_proto, logic_block) = func_decl_split(func_obj);
    let fn_id = fn_proto.node_id;
    scope_ref.funs.insert(fn_proto.ident, fn_proto);
    
    // 5 procceed with function body
    // create new scope
    // push fn args to new scope
    // push instructions of function to this scope
    mir.scopes.insert(fn_id, Scope {
        parent_id: scope_id,
        node_id: fn_id,
        structs: HashMap::new(),
        funs: HashMap::new(),
        vars: HashMap::new(),
        instrs: vec![],
    });
    
    let mut structs = vec![];
    let mut funs = vec![];
    let mut vars = vec![];
    let mut instrs = vec![];
    for stmt in logic_block.stmts {
        match stmt {
            ast::Stmt::StructDecl(x) => structs.push(x),
            ast::Stmt::FuncDecl(x) => funs.push(x),
            ast::Stmt::Assignment { node_id, ident, ty, expr, span } => { 
                vars.push(Variable { 
                    node_id,
                    ident,
                    ty: if let Some(x) = ty.clone() { x } else { Type::Unresolved },
                    is_used: false,
                });
                instrs.push(ast::Stmt::Assignment { node_id, ident, ty, expr, span })
            },
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
        scope_push_var(fn_id, mir, var, &instrs)?;
    }
    
    // 7 todo instructions + check funs return exprs
    
    Ok(())
}

fn scope_push_var<'src>(
    scope_id: usize,
    mir: &mut Mir,
    var_obj: Variable,
    instrs: &Vec<Stmt>
) -> Result<(), CompileError<'src>> {
    
    todo!()
    
    Ok(())
}

fn _scope_push_inst() {}

fn _resolv_expr_type<'src, 'e>(_expr: &'e Expr) -> Result<Type<'src>, CompileError<'src>> {
    unimplemented!()
}

fn func_decl_split<'src>(func: FuncDecl<'src>) -> (FuncProto<'src>, Block<'src>) {
    (FuncProto { 
        node_id: func.node_id,
        ident: func.ident,
        args: func.args,
        return_type: func.return_type,
        span: func.span,
    }, 
    func.body)
}

fn get_span_line_index<'src>(span: pest::Span<'src>) -> usize {
    let input = span.get_input();
    let start = span.start();
    input[..start].chars().filter(|&c| c == '\n').count() + 1
}


#[test]
fn test_index1<'src>() -> Result<(), CompileError<'src>> {
    
    let inp = r#"
        fn hello(a: i64, b: str) -> i64 {
        
            fn h2() -> void {
                var a = null;
                return;
            }  
        
            return 10;
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