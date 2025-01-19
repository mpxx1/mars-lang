use crate::{FuncProto, Mir, Scope, ScopeType, StructProto, Variable};
use ast::{Block, Expr, FuncDecl, Stmt, StructDecl, Type, Literal};
use err::CompileError;
use std::collections::{HashMap, HashSet};
use marsc_session::session::{CompilerIO, Session};
use crate::context::{ExternalFunctions, GlobalContext, TypeContext};
use crate::GLOBAL_SCOPE_ID;

pub struct TypeChecker<'src, 'tcx> {
    type_context: &'tcx TypeContext<'tcx>,
    mir: Mir<'src>,
}

impl<'src, 'tcx> TypeChecker<'src, 'tcx> {
    pub fn check_types(
        type_context: &'tcx TypeContext<'tcx>,
        hir: hir::Hir<'src>) -> Result<Mir<'src>, CompileError<'src>> {
       
        let mut type_checker = TypeChecker {
            type_context: type_context,
            mir: Mir {
                code: hir.code,
                scopes: Default::default(),
            }
        };

        type_checker.mir.scopes.insert(
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

            for stmt in hir.ast.program {
            match stmt {
                ast::ProgStmt::StructDecl(x) => {
                    type_checker.scope_push_struct(GLOBAL_SCOPE_ID, x)?;
                }
                ast::ProgStmt::FuncDecl(x) => {
                    type_checker.scope_push_func(GLOBAL_SCOPE_ID, x)?;
                }
            }
        }

        Ok(type_checker.mir)
    }

    fn scope_push_struct(
        &mut self,
        scope_id: usize,
        struct_obj: StructDecl<'src>,
    ) -> Result<(), CompileError<'src>> {
        // 1 check if same named structure already been declarated
        let scope_ref = self.mir.scopes.get_mut(&scope_id).unwrap();
        if scope_ref.structs.contains_key(struct_obj.ident) {
            return Err(CompileError::new(
                struct_obj.span,
                format!(
                    "Struct with name '{}' has already been declarated in this scope (line number {})",
                    struct_obj.ident,
                    Self::get_span_line_index(scope_ref.structs.get(struct_obj.ident).unwrap().span)
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

    fn scope_push_func(
        &mut self,
        scope_id: usize,
        func_obj: FuncDecl<'src>,
    ) -> Result<(), CompileError<'src>> {
        // 1 check if same named func already been declarated
        let scope_ref = self.mir.scopes.get_mut(&scope_id).unwrap();
        if scope_ref.funs.contains_key(func_obj.ident) {
            return Err(
                CompileError::new(
                    func_obj.span,
                    format!(
                        "Function with name '{}' has already been declarated in this scope (line number {})",
                        func_obj.ident,
                        Self::get_span_line_index(scope_ref.funs.get(func_obj.ident).unwrap().span)
                    ))
            );
        }

        // 2 check if same named func is sys func TODO
        if let Some(_) = self.type_context.global_context.external_functions.get(func_obj.ident) {
            return Err(
                CompileError::new(
                    func_obj.span,
                    format!(
                        "Can not define function with name '{}'\n\tIt's already defined as system function",
                        func_obj.ident
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
        let (fn_proto, logic_block) = Self::func_decl_split(scope_id, func_obj);
        let fn_id = fn_proto.node_id;
        let fn_args = fn_proto.args.clone();
        scope_ref.funs.insert(fn_proto.ident, fn_proto);

        // 5 procceed with function body
        // create new scope
        self.mir.scopes.insert(
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
            self.scope_push_var(
                fn_id,
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
            self.scope_push_struct(fn_id, struct_obj)?;
        }

        for fun in funs {
            self.scope_push_func(fn_id, fun)?;
        }

        // 6 todo variables (check types of exprs)
        for var in vars {
            self.scope_push_var(fn_id, var)?;
        }

        // 7 todo instructions
        for instr in instrs {
            self.scope_push_inst(fn_id, instr)?;
        }

        // 8 check funs return exprs
        // todo

        Ok(())
    }

    fn scope_push_var(
        &mut self,
        scope_id: usize,
        var_obj: Variable<'src>,
    ) -> Result<(), CompileError<'src>> {
        let scope_ref = self.mir.scopes.get_mut(&scope_id).unwrap();
        if scope_ref.vars.contains_key(var_obj.ident) {
            let idx = Self::get_span_line_index(scope_ref.vars.get(var_obj.ident).unwrap().decl_span);
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

    fn scope_push_inst(
        &mut self,
        scope_id: usize,
        instr: Stmt<'src>,
    ) -> Result<(), CompileError<'src>> {
        match instr {
            x if matches!(x, ast::Stmt::Assignment { .. }) => self.scope_push_assignment(scope_id, x)?,
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn scope_push_assignment(
        &mut self,
        scope_id: usize,
        instr: Stmt<'src>,
    ) -> Result<(), CompileError<'src>> {
        let ast::Stmt::Assignment {
            node_id,
            ident,
            ty,
            expr,
            span,
        } = instr
        else {
            panic!("Something went wrong");
        };

        let mut var = self.mir
            .scopes
            .get_mut(&scope_id)
            .unwrap()
            .vars
            .remove(ident)
            .unwrap();

        debug_assert_eq!(var.ty, ty);
        let expr_type = self.resolv_expr_type(scope_id, &expr, ty)?;

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
        let scope_ref = self.mir.scopes.get_mut(&scope_id).unwrap();
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

    fn resolv_expr_type(
        &mut self,
        scope_id: usize,
        expr: &Expr<'src>,
        opt_type: Type<'src>,
    ) -> Result<Type<'src>, CompileError<'src>> {
        let out_type = match expr {
            Expr::Identifier(x) => {
                let opt_type = self.resolv_ident_type(scope_id, x.ident);
                if opt_type.is_none() {
                    return Err(CompileError::new(
                        x.span,
                        format!("Can not find identifier '{}'", x.ident),
                    ));
                }
                opt_type.unwrap()
            }

            Expr::Literal(x) => {
                Self::resolv_lit_type(x, opt_type)?
            }

            Expr::FuncCall(x) => {
                // 1 check fn args


                // 2 check fn return type
                let opt_type = self.resolv_fn_ret_type(scope_id, x.ident.ident);
                if opt_type.is_none() {
                    return Err(CompileError::new(
                        x.span,
                        format!("Can not find function '{}'", x.ident.ident),
                    ));
                }
                opt_type.unwrap()
            }

            _ => unimplemented!("{:#?}", expr),
        };

        Ok(out_type)
    }

    fn resolv_fn_ret_type(
        &mut self,
        scope_id: usize,
        fn_name: &'src str,
    ) -> Option<Type<'src>> {
        let mut current_scope_id = scope_id;

        while let Some(scope) = self.mir.scopes.get(&current_scope_id) {
            if let Some(func) = scope.funs.get(fn_name) {
                return Some(func.return_type.clone());
            }

            if current_scope_id == 0 {
                break;
            }

            current_scope_id = scope.parent_id;
        }

        None
    }


    fn resolv_ident_type(
        &mut self,
        scope_id: usize,
        ident: &'src str,
    ) -> Option<Type<'src>> {
        let mut current_scope_id = scope_id;
        loop {
            let scope = self.mir.scopes.get(&current_scope_id)?;

            if let Some(var) = scope.vars.get(ident) {
                return Some(var.ty.clone());
            }

            if scope.scope_type == ScopeType::Function {
                return None;
            }

            current_scope_id = scope.parent_id;
        }
    }

    fn resolv_lit_type(lit: &Literal<'src>, possible_type: Type<'src>) -> Result<Type<'src>, CompileError<'src>> {
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

    fn func_decl_split(scope_id: usize, func: FuncDecl<'src>) -> (FuncProto<'src>, Block<'src>) {
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

    fn get_span_line_index(span: pest::Span<'src>) -> usize {
        let input = span.get_input();
        let start = span.start();
        input[..start].chars().filter(|&c| c == '\n').count() + 1
    }
}

#[test]
fn main_test() -> Result<(), CompileError<'static>> {
    // для null нужно проверять, что тип переменной определен и ссылочный
    let inp = r#"
    
    fn main() -> i64 {
        var a: &i64 = null;
    }
    
    "#;

    let session = Session {
        io: CompilerIO { input_file: Default::default(), output_file: Default::default() },
    };

    let global_context = GlobalContext {
        session: &session,
        external_functions: ExternalFunctions::new(),
    };

    let type_context = TypeContext::new(&global_context);

    let hir = hir::parser::compile_hir(inp)?;

    let mir = TypeChecker::check_types(&type_context, hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_fn<'src>() -> Result<(), CompileError<'src>> {
    let inp = r#"
        fn hello(a: i64, b: str) -> i64 {
        
            var c = a;        
        }
    "#;

    let session = Session {
        io: CompilerIO { input_file: Default::default(), output_file: Default::default() },
    };

    let global_context = GlobalContext {
        session: &session,
        external_functions: ExternalFunctions::new(),
    };

    let type_context = TypeContext::new(&global_context);

    let hir = hir::parser::compile_hir(inp)?;

    let mir = TypeChecker::check_types(&type_context, hir)?;

    println!("{mir:#?}");

    Ok(())
}

#[test]
fn test_index_inner_fn() -> Result<(), CompileError<'static>> {
    let inp = r#"
        fn hello(a: i64, b: str) -> i64 {
        
            fn h2() -> void {
                var a = null;
                return;
            }  
        
            return 10;
        }
    "#;

    let session = Session {
        io: CompilerIO { input_file: Default::default(), output_file: Default::default() },
    };

    let global_context = GlobalContext {
        session: &session,
        external_functions: ExternalFunctions::new(),
    };

    let type_context = TypeContext::new(&global_context);

    let hir = hir::parser::compile_hir(inp)?;

    let mir = TypeChecker::check_types(&type_context, hir)?;

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

    let session = Session {
        io: CompilerIO { input_file: Default::default(), output_file: Default::default() },
    };

    let global_context = GlobalContext {
        session: &session,
        external_functions: ExternalFunctions::new(),
    };

    let type_context = TypeContext::new(&global_context);

    let hir = hir::parser::compile_hir(inp)?;

    let mir = TypeChecker::check_types(&type_context, hir)?;

    println!("{mir:#?}");

    Ok(())
}
