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
                if_else.then_block = simplify_top_block(if_else.then_block);
                if_else.else_block = if_else.else_block.map(simplify_top_block);
                if if_else.else_block.as_ref().map_or(false, |b| b.stmts.is_empty()) {
                    if_else.else_block = None;
                }
                Expr::IfElse(if_else)
            }

            Expr::Loop(mut while_loop) => {
                while_loop.condition = Box::new(simplify_expr(*while_loop.condition));
                while_loop.body = simplify_top_block(while_loop.body);
                Expr::Loop(while_loop)
            }

            Expr::MemLookup(mut mem_lookup) => {
                mem_lookup.indices = mem_lookup.indices.into_iter().map(simplify_expr).collect();
                Expr::MemLookup(mem_lookup)
            }

            Expr::Dereference(inner) => {
                if let Expr::Reference(inner_inner) = *inner {
                    simplify_expr(*inner_inner)
                } else {
                    Expr::Dereference(Box::new(simplify_expr(*inner)))
                }
            }

            Expr::Reference(inner) => {
                if let Expr::Dereference(inner_inner) = *inner {
                    simplify_expr(*inner_inner)
                } else {
                    Expr::Reference(Box::new(simplify_expr(*inner)))
                }
            }

            _ => expr,
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
                if_else.then_block = simplify_top_block(if_else.then_block);
                if_else.else_block = if_else.else_block.map(simplify_top_block);
                if if_else.else_block.as_ref().map_or(false, |b| b.stmts.is_empty()) {
                    if_else.else_block = None;
                }
                Stmt::IfElse(if_else)
            }

            Stmt::Loop(mut while_loop) => {
                while_loop.condition = Box::new(simplify_expr(*while_loop.condition));
                while_loop.body = simplify_top_block(while_loop.body);
                Stmt::Loop(while_loop)
            }

            _ => stmt,
        }
    }

    fn simplify_block(block: Block) -> Vec<Stmt> {
        if block.stmts.is_empty() {
            return vec![];
        }

        let mut contains_stmts = false;
        let mut block_count = 0;
        for stmt in &block.stmts {
            if !matches!(stmt, Stmt::Block(_) | Stmt::Return(_)) {
                contains_stmts = true;
            } else if matches!(stmt, Stmt::Block(_)) {
                block_count += 1;
            } else {
                continue
            }
        }
        let mut stmts = vec![];

        for stmt in block.stmts {
            match stmt {
                Stmt::Block(block) => {
                    if block.stmts.is_empty() { continue; }
                    let inner = simplify_block(block);

                    if contains_stmts {
                        if inner.is_empty() { continue; }
                        stmts.push(Stmt::Block(Block{ stmts: inner }))
                    } else {
                        for s in inner {
                            stmts.push(s);
                        }
                    }
                },
                x => stmts.push(simplify_stmt(x)),
            }
        }

        stmts
    }

    fn simplify_top_block(block: Block) -> Block {
        Block { stmts: simplify_block(block) }
    }

    fn simplify_func_decl(func: FuncDecl) -> FuncDecl {
        FuncDecl {
            name: func.name,
            args: func.args,
            return_type: func.return_type,
            body: simplify_top_block(func.body),
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
        var a = 10 + b;
        {
            {}
            {
                {}
                var hello = hello();
                {
                    a += hello(halo(10, 30 + 4));
                }
            }
        }

        {
            { a += 10; }
        }

        { var a = 10; }

        return 0;
    }
    "#;

    let simple_ast = simplify(build_ast(inp).unwrap());

    dbg!(simple_ast).unwrap();
}