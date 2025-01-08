#[cfg(test)]
mod tests {
    use hir::ast::*;
    use hir::parser::*;

    #[test]
    fn test_parse_empty_program() {
        let input = "";
        let result = build_ast(input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().program.len(), 0);
    }

    #[test]
    fn test_parse_struct_decl() {
        let input = r#"
            struct Point {
                x: i64,
                y: i64,
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::StructDecl(decl) = &ast.program[0] {
            assert_eq!(decl.name, "Point");
            assert_eq!(decl.fields.len(), 2);
            assert_eq!(decl.fields[0].name, "x");
            assert!(matches!(decl.fields[0].typ, Type::I64));
            assert_eq!(decl.fields[1].name, "y");
            assert!(matches!(decl.fields[1].typ, Type::I64));
        } else {
            panic!("Expected struct declaration");
        }
    }

    #[test]
    fn test_parse_func_decl_no_args_no_return() {
        let input = r#"
            fn main() -> void {
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
            assert_eq!(decl.name, "main");
            assert!(decl.args.is_empty());
            assert!(matches!(decl.return_type, Type::Void));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_func_decl_with_args_and_return() {
        let input = r#"
            fn add(a: i64, b: i64) -> i64 {
                return a + b;
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
            assert_eq!(decl.name, "add");
            assert_eq!(decl.args.len(), 2);
            assert_eq!(decl.args[0].name, "a");
            assert!(matches!(decl.args[0].typ, Type::I64));
            assert_eq!(decl.args[1].name, "b");
            assert!(matches!(decl.args[1].typ, Type::I64));
            assert!(matches!(decl.return_type, Type::I64));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_func_decl_with_args_and_no_return() {
        let input = r#"
            fn add(a: i64, b: i64) -> void {
                return;
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
            assert_eq!(decl.name, "add");
            assert_eq!(decl.args.len(), 2);
            assert_eq!(decl.args[0].name, "a");
            assert!(matches!(decl.args[0].typ, Type::I64));
            assert_eq!(decl.args[1].name, "b");
            assert!(matches!(decl.args[1].typ, Type::I64));
            assert!(matches!(decl.return_type, Type::Void));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_func_decl_with_no_args_and_return() {
        let input = r#"
            fn add() -> i64 {
                return 0;
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 1);

        if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
            assert_eq!(decl.name, "add");
            assert!(matches!(decl.return_type, Type::I64));
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_parse_nested_struct() {
        let input = r#"
            struct Point {
                x: i64,
                y: i64,
            }

            struct Line {
                start: Point,
                end: Point,
            }
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        assert_eq!(ast.program.len(), 2);

        if let ProgStmt::StructDecl(decl) = &ast.program[1] {
            assert_eq!(decl.name, "Line");
            assert_eq!(decl.fields.len(), 2);
            assert_eq!(decl.fields[0].name, "start");
            assert!(matches!(decl.fields[0].typ, Type::Custom(ref name) if name == "Point"));

            assert_eq!(decl.fields[1].name, "end");
            assert!(matches!(decl.fields[1].typ, Type::Custom(ref name) if name == "Point"));
        } else {
            panic!("Expected struct declaration");
        }
    }

    #[test]
    fn test_parse_invalid_syntax() {
        let input = "invalid syntax";
        let result = build_ast(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_struct() {
        let input = r#"
            struct Empty {}
        "#;
        let result = build_ast(input);
        assert!(result.is_ok());
        let ast = result.unwrap();
        match &ast.program[0] {
            ProgStmt::StructDecl(decl) => {
                assert_eq!(decl.name, "Empty");
                assert!(decl.fields.is_empty());
            }
            _ => panic!("Expected a struct declaration"),
        }
    }
}
