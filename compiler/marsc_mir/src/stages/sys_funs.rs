use crate::{FuncProto, Mir, GLOBAL_SCOPE_ID};
use ast::*;
use err::CompileError;
use pest::Span;
use regex::Regex;

use super::type_checker::resolve_expr_type;

pub(crate) fn sys_funs_init<'src>() -> Vec<FuncProto<'src>> {
    static mut SYS_FN_COUNTER: usize = GLOBAL_SCOPE_ID;

    fn gen_id() -> usize {
        unsafe {
            SYS_FN_COUNTER += 1;
            SYS_FN_COUNTER
        }
    }

    Vec::from([
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "println",
            args: vec![ArgDecl {
                node_id: gen_id(),
                ident: "s",
                ty: Type::Str, // example: println("{x}"); where x type is i64/f64/char/bool/str
                span: Span::new("external fn arg", 0, 14).unwrap(),
            }],
            return_type: Type::Void,
            is_used: true,
            span: Span::new("external fn", 0, 10).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "print",
            args: vec![ArgDecl {
                node_id: gen_id(),
                ident: "s",
                ty: Type::Str, // example: print("{x}"); where x type is i64/f64/char/bool/str
                span: Span::new("external fn arg", 0, 14).unwrap(),
            }],
            return_type: Type::Void,
            is_used: true,
            span: Span::new("external fn", 0, 10).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "len",
            args: vec![ArgDecl {
                node_id: gen_id(),
                ident: "obj",
                ty: Type::Any, // &str, &array and &vec
                span: Span::new("external fn arg", 0, 15).unwrap(),
            }],
            return_type: Type::I64,
            is_used: true,
            span: Span::new("external fn", 0, 11).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "capacity",
            args: vec![ArgDecl {
                node_id: gen_id(),
                ident: "v",
                ty: Type::Any, // &vec
                span: Span::new("external fn arg", 0, 15).unwrap(),
            }],
            return_type: Type::I64,
            is_used: true,
            span: Span::new("external fn", 0, 11).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "vec_push",
            args: vec![
                ArgDecl {
                    node_id: gen_id(),
                    ident: "v",
                    ty: Type::Any, // &vec
                    span: Span::new("external fn arg", 0, 15).unwrap(),
                },
                ArgDecl {
                    node_id: gen_id(),
                    ident: "obj",
                    ty: Type::Any, // inner type
                    span: Span::new("external fn arg", 0, 15).unwrap(),
                },
            ],
            return_type: Type::Void,
            is_used: true,
            span: Span::new("external fn", 0, 11).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "vec_pop",
            args: vec![ArgDecl {
                node_id: gen_id(),
                ident: "v",
                ty: Type::Any, // &vec
                span: Span::new("external fn arg", 0, 15).unwrap(),
            }],
            return_type: Type::Void,
            is_used: true,
            span: Span::new("external fn", 0, 11).unwrap(),
        },
        
        FuncProto {
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "now",   // todo now_sec, now_milis, now_nanos
            args: vec![],
            return_type: Type::I64,
            is_used: true,
            span: Span::new("external fn", 0, 11).unwrap(),
        },
    ])
}

const PRIMITIVES: [Type; 5] = [Type::I64, Type::F64, Type::Bool, Type::Char, Type::Str];

pub(crate) fn check_sys_fn_args_types<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    mut func: FuncCall<'src>,
) -> Result<(), CompileError<'src>> {
    return match func.ident.ident {
        "print" | "println" => {
            let Expr::Literal(x) = func.args.pop().unwrap() else {
                return Err(CompileError::new(
                    func.span,
                    "Can call print/println function with string literal only".to_owned(),
                ));
            };
            let Literal::Str {
                node_id: _,
                lit,
                span,
            } = x
            else {
                return Err(CompileError::new(
                    func.span,
                    "Can call print/println function with string literal only".to_owned(),
                ));
            };

            let var_list = extract_variable_names(lit);
            dbg!(&var_list);

            for x in var_list {
                let Some(inner) = resolve_ident_type(scope_id, mir, x.clone()) else {
                    return Err(CompileError::new(
                        span,
                        format!("Can not find variable with name {:?}", x),
                    ));
                };

                if !PRIMITIVES.contains(&inner) {
                    return Err(CompileError::new(
                        span,
                        format!(
                            "Output string contains variable '{:?}' that has type '{:?}', but print/println can compute only {:?} types",
                            x,
                            inner,
                            PRIMITIVES
                        )
                    ));
                }
            }

            Ok(())
        }
        
        "len" => {
            let Expr::Reference { node_id: _, inner, span } = func.args.pop().unwrap() else {
                return Err(CompileError::new(
                    func.span,
                    "Can call 'len' function with refrence only".to_owned(),
                ));
            };
            let Expr::Identifier(x) = *inner else {
                return Err(CompileError::new(
                    span,
                    "Use refrence to identifier".to_owned(),
                ));
            };
            let Some(ty) = resolve_ident_type(scope_id, mir, x.ident.to_owned()) else {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find variable with name {:?}", x.ident),
                ));
            };
            if !matches!(ty, Type::Str | Type::Array(_,_) | Type::Vec(_)) {
                return Err(CompileError::new(
                    x.span,
                    format!("Function 'len' can take argument of types '&str', '&[ _ ; _ ]' and '&Vec<_>' only"),
                ));
            }
            
            Ok(())
        },
        
        "capacity" => {
            let Expr::Reference { node_id: _, inner, span } = func.args.pop().unwrap() else {
                return Err(CompileError::new(
                    func.span,
                    "Can call 'capacity' function with refrence only".to_owned(),
                ));
            };
            let Expr::Identifier(x) = *inner else {
                return Err(CompileError::new(
                    span,
                    "Use refrence to identifier".to_owned(),
                ));
            };
            let Some(ty) = resolve_ident_type(scope_id, mir, x.ident.to_owned()) else {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find variable with name {:?}", x.ident),
                ));
            };
            if !matches!(ty, Type::Vec(_)) {
                return Err(CompileError::new(
                    x.span,
                    format!("Function len 'capacity' take argument of '&Vec<_>' type only"),
                ));
            }
            
            Ok(())
        },
        
        "vec_push" => {
            func.args.reverse();
            let Expr::Reference { node_id: _, inner, span } = func.args.pop().unwrap() else {
                return Err(CompileError::new(
                    func.span,
                    "Can call 'vec_push' function with refrence to vec only".to_owned(),
                ));
            };
            let Expr::Identifier(x) = *inner else {
                return Err(CompileError::new(
                    span,
                    "Use refrence to identifier".to_owned(),
                ));
            };
            let Some(ty) = resolve_ident_type(scope_id, mir, x.ident.to_owned()) else {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find variable with name {:?}", x.ident),
                ));
            };
            if !matches!(ty, Type::Vec(_)) {
                return Err(CompileError::new(
                    x.span,
                    format!("Expected: '&Vec<_>', actual: '{ty:?}'"),
                ));
            }
            let Type::Vec(vec_inner_ty) = ty else {
                panic!("Somethin went wrong");
            };
            let mut inner_expr = func.args.pop().unwrap();
            let expr_ty = resolve_expr_type(scope_id, mir, &mut inner_expr, *vec_inner_ty.clone())?;
            
            if *vec_inner_ty != expr_ty {
                return Err(CompileError::new(
                    x.span,
                    format!("Expected: '{vec_inner_ty:?}', actual: '{expr_ty:?}'"),
                ));
            }
            
            Ok(())
        },
        
        "vec_pop" => {
            let Expr::Reference { node_id: _, inner, span } = func.args.pop().unwrap() else {
                return Err(CompileError::new(
                    func.span,
                    "Can call 'vec_pop' function with refrence to vec only".to_owned(),
                ));
            };
            let Expr::Identifier(x) = *inner else {
                return Err(CompileError::new(
                    span,
                    "Use refrence to identifier".to_owned(),
                ));
            };
            let Some(ty) = resolve_ident_type(scope_id, mir, x.ident.to_owned()) else {
                return Err(CompileError::new(
                    x.span,
                    format!("Can not find variable with name {:?}", x.ident),
                ));
            };
            if !matches!(ty, Type::Vec(_)) {
                return Err(CompileError::new(
                    x.span,
                    format!("Expected: '&Vec<_>', actual: '{ty:?}'"),
                ));
            }

            Ok(())
        },
        
        "now" => {
            Ok(())
        },
        
        x => { panic!("System fn '{x:?}' args check not implemented")},
    };
}

pub fn extract_variable_names(input: String) -> Vec<String> {
    let re = Regex::new(r"\{([^\{\}]+)\}").unwrap();
    let mut variables = Vec::new();

    for cap in re.captures_iter(&input) {
        if let Some(var_content) = cap.get(1) {
            let var = var_content.as_str();
            if !var.is_empty() {
                variables.push(var.to_string());
            }
        }
    }

    variables
}

fn resolve_ident_type<'src>(
    scope_id: usize,
    mir: &mut Mir<'src>,
    ident: String,
) -> Option<Type<'src>> {
    let mut current_scope_id = scope_id;
    loop {
        let scope = mir.scopes.get(&current_scope_id)?;

        if let Some(var) = scope.vars.get(ident.trim()) {
            return Some(var.ty.clone());
        }

        if scope.scope_type == crate::ScopeType::Function {
            return None;
        }

        current_scope_id = scope.parent_id;
    }
}
