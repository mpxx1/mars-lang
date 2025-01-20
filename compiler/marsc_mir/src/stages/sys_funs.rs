use crate::{GLOBAL_SCOPE_ID, FuncProto};
use ast::{ArgDecl, Type, FuncCall};
use err::CompileError;
use pest::Span;

pub (crate) fn _sys_funs_init<'src>() -> Vec<FuncProto<'src>> {
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
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "obj",
                    ty: Type::Any,  // primitives and string (also possible a + "hello" + 22)
                    span: Span::new("external fn arg", 0, 14).unwrap(), 
                }], 
            return_type: Type::Void, 
            is_used: true, 
            span: Span::new("external fn", 0, 10).unwrap()
        },
        FuncProto { 
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "print", 
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "obj",
                    ty: Type::Any,  // primitives and string (also possible a + "hello" + 22)
                    span: Span::new("external fn arg", 0, 14).unwrap(), 
                }], 
            return_type: Type::Void, 
            is_used: true, 
            span: Span::new("external fn", 0, 10).unwrap()
        },
        FuncProto { 
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "len", 
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "obj",
                    ty: Type::Any,  // str, array and vec
                    span: Span::new("external fn arg", 0, 15).unwrap(), 
                }], 
            return_type: Type::I64, 
            is_used: true, 
            span: Span::new("external fn", 0, 11).unwrap()
        },
        FuncProto { 
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "capacity", 
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "v",
                    ty: Type::Any,  // vec
                    span: Span::new("external fn arg", 0, 15).unwrap(), 
                }], 
            return_type: Type::I64, 
            is_used: true, 
            span: Span::new("external fn", 0, 11).unwrap()
        },
        FuncProto { 
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "push", 
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "v",
                    ty: Type::Any,  // vec
                    span: Span::new("external fn arg", 0, 15).unwrap(), 
                },
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "obj",
                    ty: Type::Any,  // inner type
                    span: Span::new("external fn arg", 0, 15).unwrap(), 
                }
            ], 
            return_type: Type::Void, 
            is_used: true, 
            span: Span::new("external fn", 0, 11).unwrap()
        },
        FuncProto { 
            parent_id: GLOBAL_SCOPE_ID,
            node_id: gen_id(),
            ident: "pop", 
            args: vec![
                ArgDecl { 
                    node_id: gen_id(),
                    ident: "v",
                    ty: Type::Any,  // vec
                    span: Span::new("external fn arg", 0, 15).unwrap(), 
                }], 
            return_type: Type::Any, // inner 
            is_used: true, 
            span: Span::new("external fn", 0, 11).unwrap()
        },
    ])
}

static PRIMITIVE: [Type; 6] = [Type::I64, Type::F64, Type::Bool, Type::Char, Type::Str, Type::ToStr];

pub(crate) fn check_sys_fn_args_types<'src>(
    func: &FuncCall<'src>,
    arg_types: &Vec<Type<'src>>,
) -> Result<(), CompileError<'src>> {
    
    match func.ident.ident {
        "print" | "println" => {
            if PRIMITIVE.contains(&arg_types[0]) {
                return Ok(());
            }
        },
        _ => unimplemented!(),
    }
    
    Err(CompileError::new(func.span, format!("Calling function '{}' with incorrect typed arguments", func.ident.ident )))
}