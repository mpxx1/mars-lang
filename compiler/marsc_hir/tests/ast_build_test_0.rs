#[cfg(test)]
mod tests {
    use hir::ast::*;
    use hir::parser::*;

    #[test]
    fn test_parse_i64_assignment() {
        let input = r#"
        fn main() -> void {
            var x: i64 = 42;
        }
    "#;

        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            if let Some(Stmt::Assignment(assign)) = func_decl.body.stmts.get(0) {
                assert_eq!(assign.var_name, "x");

                if let Some(Type::I64) = assign.typ {
                } else {
                    panic!("Expected type to be I64");
                }

                if let Expr::LogicalExpr(logical_expr) = &assign.expr {
                    if let LogicalExpr::Primary(primary_expr) = &logical_expr {
                        if let Expr::MathExpr(math_expr) = &**primary_expr {
                            if let MathExpr::Primary(inner_expr) = math_expr {
                                if let Expr::Literal(Literal::Int(42)) = **inner_expr {
                                } else {
                                    panic!("Expected literal 42");
                                }
                            } else {
                                panic!("Expected primary expression in MathExpr");
                            }
                        } else {
                            panic!("Expected MathExpr");
                        }
                    } else {
                        panic!("Expected primary logical expression");
                    }
                } else {
                    panic!("Expected logical expression");
                }
            } else {
                panic!("Expected assignment in main");
            }
        } else {
            panic!("Expected function declaration for main");
        }
    }

    #[test]
    fn test_custom_type_assignment() {
        let input = r#"
        struct MyType {
            value: i64
        }

        fn main() -> void {
            var customVar: MyType = MyType { value: 10 };
        }
    "#;

        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();

        assert_eq!(ast.program.len(), 2);

        if let ProgStmt::StructDecl(struct_decl) = &ast.program[0] {
            assert_eq!(struct_decl.name, "MyType");
            assert_eq!(struct_decl.fields.len(), 1);
            if let ArgDecl { name, typ: Type::I64 } = &struct_decl.fields[0] {
                assert_eq!(name, "value");
            } else {
                panic!("Expected 'value' field of type I64 in MyType struct");
            }
        } else {
            panic!("Expected a StructDecl for MyType");
        }

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[1] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            match &func_decl.return_type {
                Type::Void => (),
                _ => panic!("Expected return type to be Void"),
            }

            if let Some(Stmt::Assignment(assign)) = func_decl.body.stmts.first() {
                assert_eq!(assign.var_name, "customVar");

                if let Some(Type::Custom(custom_type)) = &assign.typ {
                    assert_eq!(custom_type, "MyType");
                } else {
                    panic!("Expected type to be MyType");
                }

                if let Expr::StructInit(struct_init) = &assign.expr {
                    assert_eq!(struct_init.name, "MyType");
                    assert_eq!(struct_init.fields.len(), 1);

                    if let (field_name, Expr::LogicalExpr(logical_expr)) = &struct_init.fields[0] {
                        assert_eq!(field_name, "value");

                        if let LogicalExpr::Primary(primary_expr) = logical_expr {
                            if let Expr::MathExpr(math_expr) = &**primary_expr {
                                if let MathExpr::Primary(inner_expr) = math_expr {
                                    if let Expr::Literal(Literal::Int(10)) = **inner_expr {
                                    } else {
                                        panic!("Expected literal 10");
                                    }
                                } else {
                                    panic!("Expected primary expression in MathExpr");
                                }
                            } else {
                                panic!("Expected MathExpr");
                            }
                        } else {
                            panic!("Expected primary logical expression");
                        }
                    } else {
                        panic!("Expected field 'value' in MyType struct initialization");
                    }
                } else {
                    panic!("Expected struct initialization for MyType");
                }
            } else {
                panic!("Expected assignment in main");
            }
        } else {
            panic!("Expected function declaration for main");
        }
    }

    #[test]
    fn test_i64_assign() {
        let input = r#"
        fn main() -> void {
            var x: i64 = 10;
            x = 20;
        }
    "#;

        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            let stmts = &func_decl.body.stmts;
            assert_eq!(stmts.len(), 2);

            if let Stmt::Assignment(assignment) = &stmts[0] {
                assert_eq!(assignment.var_name, "x");

                match &assignment.typ {
                    Some(Type::I64) => (),
                    _ => panic!("Expected type I64 for variable x, but got something else."),
                }

                if let Expr::LogicalExpr(logical_expr) = &assignment.expr {
                    if let LogicalExpr::Primary(ref inner_expr) = *logical_expr {
                        if let Expr::MathExpr(MathExpr::Primary(ref primary_expr)) = **inner_expr {
                            if let Expr::Literal(Literal::Int(10)) = **primary_expr {
                            } else {
                                panic!("Expected literal 10 in the expression.");
                            }
                        } else {
                            panic!("Expected a MathExpr as the primary expression.");
                        }
                    } else {
                        panic!("Expected a Primary expression inside the logical expression.");
                    }
                } else {
                    panic!("Expected a logical expression in assignment.");
                }
            } else {
                panic!("Expected an assignment statement for variable x");
            }

            if let Stmt::Assign(assign) = &stmts[1] {
                if let Expr::Identifier(ref ident) = assign.lhs {
                    assert_eq!(ident, "x");
                } else {
                    panic!("Expected lhs to be an identifier 'x'");
                }

                if let Expr::LogicalExpr(logical_expr) = &assign.rhs {
                    if let LogicalExpr::Primary(ref inner_expr) = *logical_expr {
                        if let Expr::MathExpr(MathExpr::Primary(ref primary_expr)) = **inner_expr {
                            if let Expr::Literal(Literal::Int(20)) = **primary_expr {
                            } else {
                                panic!("Expected literal 20 in the expression.");
                            }
                        } else {
                            panic!("Expected a MathExpr as the primary expression.");
                        }
                    } else {
                        panic!("Expected a Primary expression inside the logical expression.");
                    }
                } else {
                    panic!("Expected a logical expression in assign statement.");
                }
            } else {
                panic!("Expected an assign statement for variable x");
            }
        } else {
            panic!("Expected a function declaration for main");
        }
    }

    #[test]
    fn test_custom_type_assign() {
        let input = r#"
        struct MyType {
            value: i64
        }

        fn main() -> void {
            var customVar: MyType = MyType { value: 10 };
            customVar = MyType { value: 20 };
        }
    "#;

        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 2);


        if let ProgStmt::StructDecl(struct_decl) = &ast.program[0] {
            assert_eq!(struct_decl.name, "MyType");
            assert_eq!(struct_decl.fields.len(), 1);
            assert_eq!(struct_decl.fields[0].name, "value");
            match struct_decl.fields[0].typ {
                Type::I64 => (),
                _ => panic!("Expected type I64 for field 'value', but got something else."),
            }
        } else {
            panic!("Expected a structure declaration for MyType.");
        }

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[1] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            let stmts = &func_decl.body.stmts;
            assert_eq!(stmts.len(), 2);

            if let Stmt::Assignment(assignment) = &stmts[0] {
                assert_eq!(assignment.var_name, "customVar");

                assert!(matches!(assignment.typ, Some(Type::Custom(ref name)) if name == "MyType"));

                if let Expr::StructInit(struct_init) = &assignment.expr {
                    assert_eq!(struct_init.name, "MyType");
                    assert_eq!(struct_init.fields.len(), 1);
                    if let Expr::LogicalExpr(logical_expr) = &struct_init.fields[0].1 {
                        match logical_expr {
                            LogicalExpr::Primary(expr) => {
                                if let Expr::MathExpr(math_expr) = &**expr {
                                    if let MathExpr::Primary(primary_expr) = &math_expr {
                                        if let Expr::Literal(Literal::Int(value)) = &**primary_expr {
                                            println!("Found value for field 'value': {}", value);
                                            assert_eq!(*value, 10);
                                        }
                                    }
                                }
                            }
                            _ => panic!("Expected a Primary logical expression."),
                        }
                    } else {
                        panic!("Expected a LogicalExpr for field 'value'.");
                    }
                } else {
                    panic!("Expected a StructInit expression for MyType.");
                }
            } else {
                panic!("Expected an assignment statement for customVar.");
            }

            if let Stmt::Assign(assign) = &stmts[1] {
                if let Expr::Identifier(ref ident) = assign.lhs {
                    assert_eq!(ident, "customVar");
                } else {
                    panic!("Expected an identifier for the left-hand side of the assignment.");
                }

                if let Expr::StructInit(struct_init) = &assign.rhs {
                    assert_eq!(struct_init.name, "MyType");
                    assert_eq!(struct_init.fields.len(), 1);
                    if let Expr::LogicalExpr(logical_expr) = &struct_init.fields[0].1 {
                        match logical_expr {
                            LogicalExpr::Primary(expr) => {
                                if let Expr::MathExpr(math_expr) = &**expr {
                                    if let MathExpr::Primary(primary_expr) = &math_expr {
                                        if let Expr::Literal(Literal::Int(value)) = &**primary_expr {
                                            println!("Found value for field 'value': {}", value);
                                            assert_eq!(*value, 20);
                                        }
                                    }
                                }
                            }
                            _ => panic!("Expected a Primary logical expression."),
                        }
                    } else {
                        panic!("Expected a LogicalExpr for field 'value'.");
                    }
                } else {
                    panic!("Expected a StructInit expression for MyType.");
                }
            } else {
                panic!("Expected an assignment statement for customVar.");
            }
        } else {
            panic!("Expected a function declaration for main.");
        }
    }


    #[test]
    fn test_array_declaration() {
        let input = r#"
        fn main() -> void {
            var arr: [i64; 3] = [10, 20, 30];
        }
    "#;

        let result = build_ast(input);

        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            if let Some(Stmt::Assignment(assign)) = func_decl.body.stmts.get(0) {
                if let Some(Type::Array(inner_type, size)) = &assign.typ {
                    assert_eq!(*size, 3);
                    if let Type::I64 = **inner_type {
                    } else {
                        panic!("Expected array of type I64");
                    }
                } else {
                    panic!("Expected assignment with array type");
                }
            } else {
                panic!("Expected assignment in main");
            }
        } else {
            panic!("Expected function declaration for main");
        }
    }


    #[test]
    fn test_single_element_array_declaration() {
        let input = r#"
        fn main() -> void {
            var arr: [i64; 1] = [10];
        }
    "#;

        let result = build_ast(input);
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            if let Some(Stmt::Assignment(assign)) = func_decl.body.stmts.get(0) {
                if let Some(Type::Array(inner_type, size)) = &assign.typ {
                    assert_eq!(*size, 1);
                    if let Type::I64 = **inner_type {
                    } else {
                        panic!("Expected array of type I64");
                    }
                } else {
                    panic!("Expected assignment with array type");
                }

                if let Expr::ArrayDecl(arr_decl) = &assign.expr {
                    assert_eq!(arr_decl.len(), 1);
                    if let Expr::LogicalExpr(LogicalExpr::Primary(_)) = &arr_decl[0] {
                        if let Expr::MathExpr(MathExpr::Primary(_)) = &arr_decl[0] {
                            if let Expr::Literal(Literal::Int(10)) = &arr_decl[0] {
                            } else {
                                panic!("Expected array element to be 10");
                            }
                        }
                    } else {
                        panic!("Expected array element to be a logical expression");
                    }
                } else {
                    panic!("Expected array declaration");
                }
            } else {
                panic!("Expected assignment in main");
            }
        } else {
            panic!("Expected function declaration for main");
        }
    }

    #[test]
    fn test_mem_lookup() {
        let input = r#"
        fn main() -> void {
            var arr: [i64; 3] = [10, 20, 30];

            var element = arr[1];

            return element;
        }
    "#;

        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
            assert_eq!(func_decl.name, "main");
            assert_eq!(func_decl.args.len(), 0);

            if let Some(Stmt::Assignment(assign)) = func_decl.body.stmts.get(0) {
                if let Some(Type::Array(inner_type, size)) = &assign.typ {
                    assert_eq!(*size, 3);
                    if let Type::I64 = **inner_type {
                    } else {
                        panic!("Expected array of type I64");
                    }
                } else {
                    panic!("Expected assignment with array type");
                }

                if let Expr::MemLookup(mem_lookup) = &assign.expr {
                    if let Type::I64 = assign.typ.clone().unwrap() {
                    } else {
                        panic!("Expected element type to be I64");
                    }
                }
            } else {
                panic!("Expected assignment in main");
            }
        } else {
            panic!("Expected function declaration for main");
        }
    }
}