use ast::*;
use err::CompileError;

pub(crate) fn simplify(ast: Ast) -> Result<Ast, CompileError> {
    fn simplify_logical_expr(expr: LogicalExpr) -> LogicalExpr {
        match expr {
            LogicalExpr::Or {
                node_id,
                left,
                right,
                span,
            } => LogicalExpr::Or {
                node_id,
                left: Box::new(simplify_logical_expr(*left)),
                right: Box::new(simplify_logical_expr(*right)),
                span,
            },

            LogicalExpr::And {
                node_id,
                left,
                right,
                span,
            } => LogicalExpr::And {
                node_id,
                left: Box::new(simplify_logical_expr(*left)),
                right: Box::new(simplify_logical_expr(*right)),
                span,
            },

            LogicalExpr::Not {
                node_id,
                inner,
                span,
            } => LogicalExpr::Not {
                node_id,
                inner: Box::new(simplify_logical_expr(*inner)),
                span,
            },

            LogicalExpr::Primary(x) => LogicalExpr::Primary(Box::new(simplify_expr(*x))),

            LogicalExpr::Comparison {
                node_id,
                left,
                right,
                op,
                span,
            } => LogicalExpr::Comparison {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            },
        }
    }

    fn simplify_expr(expr: Expr) -> Expr {
        match expr {
            Expr::LogicalExpr(LogicalExpr::Primary(inner_expr))
            | Expr::MathExpr(MathExpr::Primary(inner_expr)) => simplify_expr(*inner_expr),

            Expr::LogicalExpr(LogicalExpr::Or {
                node_id,
                left,
                right,
                span,
            }) => Expr::LogicalExpr(LogicalExpr::Or {
                node_id,
                left: Box::new(simplify_logical_expr(*left)),
                right: Box::new(simplify_logical_expr(*right)),
                span,
            }),

            Expr::LogicalExpr(LogicalExpr::And {
                node_id,
                left,
                right,
                span,
            }) => Expr::LogicalExpr(LogicalExpr::And {
                node_id,
                left: Box::new(simplify_logical_expr(*left)),
                right: Box::new(simplify_logical_expr(*right)),
                span,
            }),

            Expr::LogicalExpr(LogicalExpr::Not {
                node_id,
                inner,
                span,
            }) => Expr::LogicalExpr(LogicalExpr::Not {
                node_id,
                inner: Box::new(simplify_logical_expr(*inner)),
                span,
            }),

            Expr::LogicalExpr(LogicalExpr::Comparison {
                node_id,
                left,
                right,
                op,
                span,
            }) => Expr::LogicalExpr(LogicalExpr::Comparison {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            }),

            Expr::MathExpr(MathExpr::Additive {
                node_id,
                left,
                right,
                op,
                span,
            }) => Expr::MathExpr(MathExpr::Additive {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            }),

            Expr::MathExpr(MathExpr::Multiplicative {
                node_id,
                left,
                right,
                op,
                span,
            }) => Expr::MathExpr(MathExpr::Multiplicative {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            }),

            Expr::MathExpr(MathExpr::Power {
                node_id,
                base,
                exp,
                span,
            }) => Expr::MathExpr(MathExpr::Power {
                node_id,
                base: Box::new(simplify_math_expr(*base)),
                exp: Box::new(simplify_math_expr(*exp)),
                span,
            }),

            Expr::ArrayDecl {
                node_id,
                list,
                span,
            } => Expr::ArrayDecl {
                node_id,
                list: list.into_iter().map(simplify_expr).collect(),
                span,
            },

            Expr::FuncCall(mut func_call) => {
                func_call.args = func_call.args.into_iter().map(simplify_expr).collect();
                Expr::FuncCall(func_call)
            }

            Expr::StructInit {
                decl_scope_id,
                struct_id,
                node_id,
                ident,
                fields,
                span,
            } => {
                let fields = fields
                    .into_iter()
                    .map(|decl| StructFieldDecl {
                        node_id: decl.node_id,
                        ident: decl.ident,
                        expr: simplify_expr(decl.expr),
                        span: decl.span,
                    })
                    .collect();
                Expr::StructInit {
                    decl_scope_id,
                    struct_id,
                    node_id,
                    ident,
                    fields,
                    span,
                }
            }

            Expr::MemLookup {
                node_id,
                ident,
                indices,
                span,
            } => Expr::MemLookup {
                node_id,
                ident,
                indices: indices.into_iter().map(simplify_expr).collect(),
                span,
            },

            Expr::Dereference {
                node_id,
                inner,
                span,
            } => {
                if let Expr::Reference {
                    inner: inner_inner, ..
                } = *inner
                {
                    simplify_expr(*inner_inner)
                } else {
                    Expr::Dereference {
                        node_id,
                        inner: Box::new(simplify_expr(*inner)),
                        span,
                    }
                }
            }

            Expr::Reference {
                node_id,
                inner,
                span,
            } => {
                if let Expr::Dereference {
                    inner: inner_inner, ..
                } = *inner
                {
                    simplify_expr(*inner_inner)
                } else {
                    Expr::Reference {
                        node_id,
                        inner: Box::new(simplify_expr(*inner)),
                        span,
                    }
                }
            }

            Expr::CastType {
                node_id,
                cast_to,
                expr,
                span,
            } => Expr::CastType {
                node_id,
                cast_to,
                expr: Box::new(simplify_expr(*expr)),
                span,
            },

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

            MathExpr::Additive {
                node_id,
                left,
                right,
                op,
                span,
            } => MathExpr::Additive {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            },

            MathExpr::Multiplicative {
                node_id,
                left,
                right,
                op,
                span,
            } => MathExpr::Multiplicative {
                node_id,
                left: Box::new(simplify_math_expr(*left)),
                right: Box::new(simplify_math_expr(*right)),
                op,
                span,
            },

            MathExpr::Power {
                node_id,
                base,
                exp,
                span,
            } => MathExpr::Power {
                node_id,
                base: Box::new(simplify_math_expr(*base)),
                exp: Box::new(simplify_math_expr(*exp)),
                span,
            },
        }
    }

    fn simplify_stmt(stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::FuncDecl(mut x) => {
                x.body.stmts = x.body.stmts.into_iter().map(|s| simplify_stmt(s)).collect();
                Stmt::FuncDecl(x)
            }

            Stmt::Block(mut x) => {
                x.stmts = x.stmts.into_iter().map(|s| simplify_stmt(s)).collect();
                Stmt::Block(x)
            }

            Stmt::Return {
                node_id,
                expr: Some(inner),
                span,
            } => Stmt::Return {
                node_id,
                expr: Some(simplify_expr(inner)),
                span,
            },

            Stmt::Assignment {
                node_id,
                ident,
                ty,
                expr,
                span,
            } => Stmt::Assignment {
                node_id,
                ident,
                ty,
                expr: simplify_expr(expr),
                span,
            },

            Stmt::Assign {
                node_id,
                lhs,
                rhs,
                span,
            } => Stmt::Assign {
                node_id,
                lhs: simplify_expr(lhs),
                rhs: simplify_expr(rhs),
                span,
            },

            Stmt::FuncCall(mut func_call) => {
                func_call.args = func_call.args.into_iter().map(simplify_expr).collect();
                Stmt::FuncCall(func_call)
            }

            Stmt::IfElse {
                node_id,
                else_id,
                cond,
                then_block,
                else_block,
                span,
            } => Stmt::IfElse {
                node_id,
                else_id,
                cond: Box::new(simplify_expr(*cond)),
                then_block: simplify_top_block(then_block),
                else_block: else_block.map(simplify_top_block),
                span,
            },

            Stmt::WhileLoop {
                node_id,
                cond,
                body,
                span,
            } => Stmt::WhileLoop {
                node_id,
                cond: Box::new(simplify_expr(*cond)),
                body: simplify_top_block(body),
                span,
            },

            _ => stmt,
        }
    }

    fn simplify_block(block: Block) -> Vec<Stmt> {
        if block.stmts.is_empty() {
            return vec![];
        }

        let mut contains_stmts = false;
        for stmt in &block.stmts {
            if !matches!(stmt, Stmt::Block(_) | Stmt::Return { .. }) {
                contains_stmts = true;
                break;
            }
        }
        let mut stmts = vec![];

        for stmt in block.stmts {
            match stmt {
                Stmt::Block(block) => {
                    if block.stmts.is_empty() {
                        continue;
                    }
                    let node_id = block.node_id;
                    let span = block.span;
                    let inner = simplify_block(block);

                    if contains_stmts {
                        if inner.is_empty() {
                            continue;
                        }
                        stmts.push(Stmt::Block(Block {
                            node_id,
                            stmts: inner,
                            span,
                        }))
                    } else {
                        for s in inner {
                            stmts.push(s);
                        }
                    }
                }

                x => stmts.push(simplify_stmt(x)),
            }
        }

        stmts
    }

    fn simplify_top_block(block: Block) -> Block {
        let span = block.span;
        Block {
            node_id: block.node_id,
            stmts: simplify_block(block),
            span,
        }
    }

    fn simplify_func_decl(func: FuncDecl) -> FuncDecl {
        FuncDecl {
            node_id: func.node_id,
            ident: func.ident,
            args: func.args,
            return_type: func.return_type,
            body: simplify_top_block(func.body),
            span: func.span,
        }
    }

    fn simplify_prog_stmt(stmt: ProgStmt) -> ProgStmt {
        match stmt {
            ProgStmt::StructDecl(struct_decl) => ProgStmt::StructDecl(struct_decl),
            ProgStmt::FuncDecl(func_decl) => ProgStmt::FuncDecl(simplify_func_decl(func_decl)),
        }
    }

    Ok(Ast {
        program: ast.program.into_iter().map(simplify_prog_stmt).collect(),
    })
}

#[test]
fn test1() -> () {
    let inp = r#"
        fn main() -> i64 {
            var a = 10;
            if a == 0 {
                a += 10;
            } else {
                a += 20;
            }
            
            println("{a}");
        }
    "#;

    let hir = crate::stages::parser::parse(inp);
    println!("{:#?}", hir.unwrap().ast)
}
