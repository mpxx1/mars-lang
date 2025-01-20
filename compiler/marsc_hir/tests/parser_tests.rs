use std::ops::Deref;
use ast::*;
use hir::compile_hir;

#[test]
fn test_parse_empty_program() {
    let input = "";
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 0);
}

#[test]
fn test_parse_struct_decl() {
    let input = r#"
        struct Point {
            x: i64,
            y: i64,
        }
    "#;
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::StructDecl(decl) = &ast.program[0] {
        assert_eq!(decl.ident, "Point");
        assert_eq!(decl.fields.len(), 2);
        assert_eq!(decl.fields[0].ident, "x");
        assert!(matches!(decl.fields[0].ty, Type::I64));
        assert_eq!(decl.fields[1].ident, "y");
        assert!(matches!(decl.fields[1].ty, Type::I64));
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
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
        assert_eq!(decl.ident, "main");
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
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
        assert_eq!(decl.ident, "add");
        assert_eq!(decl.args.len(), 2);
        assert_eq!(decl.args[0].ident, "a");
        assert!(matches!(decl.args[0].ty, Type::I64));
        assert_eq!(decl.args[1].ident, "b");
        assert!(matches!(decl.args[1].ty, Type::I64));
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
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
        assert_eq!(decl.ident, "add");
        assert_eq!(decl.args.len(), 2);
        assert_eq!(decl.args[0].ident, "a");
        assert!(matches!(decl.args[0].ty, Type::I64));
        assert_eq!(decl.args[1].ident, "b");
        assert!(matches!(decl.args[1].ty, Type::I64));
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
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(decl) = &ast.program[0] {
        assert_eq!(decl.ident, "add");
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
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 2);

    if let ProgStmt::StructDecl(decl) = &ast.program[1] {
        assert_eq!(decl.ident, "Line");
        assert_eq!(decl.fields.len(), 2);
        assert_eq!(decl.fields[0].ident, "start");
        assert!(matches!(decl.fields[0].ty, Type::Custom(ref name) if name.ident == "Point"));

        assert_eq!(decl.fields[1].ident, "end");
        assert!(matches!(decl.fields[1].ty, Type::Custom(ref name) if name.ident == "Point"));
    } else {
        panic!("Expected struct declaration");
    }
}

#[test]
fn test_parse_empty_struct() {
    let input = r#"
        struct Empty {}
    "#;
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    match &ast.program[0] {
        ProgStmt::StructDecl(decl) => {
            assert_eq!(decl.ident, "Empty");
            assert!(decl.fields.is_empty());
        }
        _ => panic!("Expected a struct declaration"),
    }
}

#[test]
fn test_parse_if_else_in_main() {
    let input = r#"
        fn main() -> void {
            if x > 10 {
                return;
            } else {
                x = 20;
            }
        }
    "#;
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
        assert_eq!(func_decl.ident, "main");
        assert_eq!(func_decl.args.len(), 0);

        if let Some(Stmt::IfElse { cond, then_block, else_block, .. }) = func_decl.body.stmts.first() {
            assert!(matches!(*cond.deref(), Expr::LogicalExpr(_)));
            assert_eq!(then_block.stmts.len(), 1);
            assert_eq!(else_block.as_ref().unwrap().stmts.len(), 1);
        } else {
            panic!("Expected an if-else statement in main");
        }
    } else {
        panic!("Expected a function declaration for main");
    }
}

#[test]
fn test_parse_if_in_main() {
    let input = r#"
        fn main() -> void {
            if x > 10 {
                return;
            }
        }
    "#;
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
        assert_eq!(func_decl.ident, "main");
        assert_eq!(func_decl.args.len(), 0);

        if let Some(Stmt::IfElse { cond, then_block, else_block, .. }) = func_decl.body.stmts.first() {
            assert!(matches!(*cond.deref(), Expr::LogicalExpr(_)));
            assert_eq!(then_block.stmts.len(), 1);
            assert!(else_block.is_none(), "Expected no else block");
        } else {
            panic!("Expected an if statement in main");
        }
    } else {
        panic!("Expected a function declaration for main");
    }
}


#[test]
fn test_parse_while_in_main() {
    let input = r#"
        fn main() -> void {
            while x < 5 {
                x = x + 1;
            }
        }
    "#;
    let result = compile_hir(input);
    assert!(result.is_ok());
    let ast = result.unwrap().ast;
    assert_eq!(ast.program.len(), 1);

    if let ProgStmt::FuncDecl(func_decl) = &ast.program[0] {
        assert_eq!(func_decl.ident, "main");
        assert_eq!(func_decl.args.len(), 0);

        if let Some(Stmt::WhileLoop { cond, body, .. }) = func_decl.body.stmts.first() {
            assert!(matches!(*cond.deref(), Expr::LogicalExpr(_))); // Разыменовываем cond
            assert_eq!(body.stmts.len(), 1);
        } else {
            panic!("Expected a while statement in main");
        }
    } else {
        panic!("Expected a function declaration for main");
    }
}

