use hir::ast::*;
use anyhow::Result;

pub fn simplify(ast: AST) -> Result<AST> {
    fn simplify_expr(expr: Expr) -> Expr {
        match expr {
            Expr::LogicalExpr(LogicalExpr::Primary(inner_expr)) |
            Expr::MathExpr(MathExpr::Primary(inner_expr))
            => simplify_expr(*inner_expr),

            Expr::MathExpr(MathExpr::Additive(lhs, op, rhs)) => Expr::MathExpr(MathExpr::Additive(
                Box::new(simplify_math_expr(*lhs)),
                op,
                Box::new(simplify_math_expr(*rhs)),
            )),

            Expr::MathExpr(MathExpr::Multiplicative(lhs, op, rhs)) => Expr::MathExpr(MathExpr::Multiplicative(
                Box::new(simplify_math_expr(*lhs)),
                op,
                Box::new(simplify_math_expr(*rhs)),
            )),

            Expr::MathExpr(MathExpr::Power(base, exp)) => Expr::MathExpr(MathExpr::Power(
                Box::new(simplify_math_expr(*base)),
                Box::new(simplify_math_expr(*exp)),
            )),

            Expr::ArrayDecl(elements) => Expr::ArrayDecl(
                elements.into_iter().map(simplify_expr).collect(),
            ),

            Expr::FuncCall(mut func_call) => {
                func_call.args = func_call.args.into_iter().map(simplify_expr).collect();
                Expr::FuncCall(func_call)
            }

            Expr::StructInit(mut struct_init) => {
                struct_init.fields = struct_init
                    .fields
                    .into_iter()
                    .map(|(name, field_expr)| (name, simplify_expr(field_expr)))
                    .collect();
                Expr::StructInit(struct_init)
            }

            Expr::IfElse(mut if_else) => {
                if_else.condition = Box::new(simplify_expr(*if_else.condition));
                if_else.then_block = simplify_block(if_else.then_block);
                if_else.else_block = if_else.else_block.map(simplify_block);
                Expr::IfElse(if_else)
            }

            Expr::Loop(mut while_loop) => {
                while_loop.condition = Box::new(simplify_expr(*while_loop.condition));
                while_loop.body = simplify_block(while_loop.body);
                Expr::Loop(while_loop)
            }

            Expr::MemLookup(mut mem_lookup) => {
                mem_lookup.indices = mem_lookup.indices.into_iter().map(simplify_expr).collect();
                Expr::MemLookup(mem_lookup)
            }

            other => other,
        }
    }

    fn simplify_math_expr(expr: MathExpr) -> MathExpr {
        match expr {
            MathExpr::Primary(inner_expr) => {
                if let Expr::MathExpr(inner_math) = *inner_expr {
                    simplify_math_expr(inner_math)
                } else {
                    MathExpr::Primary(Box::new(simplify_expr(*inner_expr)))
                }
            }
            MathExpr::Additive(lhs, op, rhs) => MathExpr::Additive(
                Box::new(simplify_math_expr(*lhs)),
                op,
                Box::new(simplify_math_expr(*rhs)),
            ),
            MathExpr::Multiplicative(lhs, op, rhs) => MathExpr::Multiplicative(
                Box::new(simplify_math_expr(*lhs)),
                op,
                Box::new(simplify_math_expr(*rhs)),
            ),
            MathExpr::Power(base, exp) => MathExpr::Power(
                Box::new(simplify_math_expr(*base)),
                Box::new(simplify_math_expr(*exp)),
            ),
        }
    }

    fn simplify_stmt(stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::Block(block) => Stmt::Block(Block {
                stmts: block.stmts.into_iter().map(simplify_stmt).collect(),
            }),
            Stmt::Return(Some(expr)) => Stmt::Return(Some(simplify_expr(expr))),
            Stmt::Assignment(mut assignment) => {
                assignment.expr = simplify_expr(assignment.expr);
                Stmt::Assignment(assignment)
            }
            Stmt::Assign(mut assign) => {
                assign.lhs = simplify_expr(assign.lhs);
                assign.rhs = simplify_expr(assign.rhs);
                Stmt::Assign(assign)
            }
            Stmt::FuncCall(mut func_call) => {
                func_call.args = func_call.args.into_iter().map(simplify_expr).collect();
                Stmt::FuncCall(func_call)
            }
            Stmt::IfElse(mut if_else) => {
                if_else.condition = Box::new(simplify_expr(*if_else.condition));
                if_else.then_block = simplify_block(if_else.then_block);
                if_else.else_block = if_else.else_block.map(simplify_block);
                Stmt::IfElse(if_else)
            }
            Stmt::Loop(mut while_loop) => {
                while_loop.condition = Box::new(simplify_expr(*while_loop.condition));
                while_loop.body = simplify_block(while_loop.body);
                Stmt::Loop(while_loop)
            }
            _ => stmt,
        }
    }

    fn simplify_block(block: Block) -> Block {
        Block {
            stmts: block.stmts.into_iter().map(simplify_stmt).collect(),
        }
    }

    fn simplify_func_decl(func: FuncDecl) -> FuncDecl {
        FuncDecl {
            name: func.name,
            args: func.args,
            return_type: func.return_type,
            body: simplify_block(func.body),
        }
    }

    fn simplify_prog_stmt(stmt: ProgStmt) -> ProgStmt {
        match stmt {
            ProgStmt::StructDecl(struct_decl) => ProgStmt::StructDecl(struct_decl),
            ProgStmt::FuncDecl(func_decl) => ProgStmt::FuncDecl(simplify_func_decl(func_decl)),
        }
    }

    Ok(AST {
        program: ast.program.into_iter().map(simplify_prog_stmt).collect(),
    })
}



#[test]
fn simple_test() {
    use hir::parser::build_ast;

    let inp = r#"
    fn main() -> i64 {
        var a = if true {
            print("10");
            return hello;
        } else {
            print("20");
            return 50;
        };
        
        var a: i64 = b;

        return 0;
    }
    "#;

    let simple_ast = simplify(build_ast(inp).unwrap());

    dbg!(simple_ast).unwrap();
}