use ast::Ast;
use ast::ProgStmt;
use ast::Block;
use ast::Expr;
use ast::Type::*;
use ast::Stmt::{self, *};

pub(crate) fn arr_expand<'src>(mut ast: Ast<'src>) -> Ast<'src> {

    fn process_stmt<'src>(stmt: &mut Stmt<'src>) {
        match stmt {
            Assignment {
                ty: Array(_, len),
                expr,
                ..
            } => {
                if *len <= 1 {
                    return;
                }
                
                let Expr::ArrayDecl { node_id: _, list, span: _ } = expr else {
                    panic!("Something went wrong")
                };
                
                let act_len = list.len();
                if act_len > 1 || act_len == 0 { return; }
                
                let obj = list.pop().unwrap();
                for _ in 0..*len {
                    list.push(obj.clone());
                }
            }
            Block(block) => {
                for stmt in block.stmts.iter_mut() {
                    process_stmt(stmt);
                }
            }
            FuncDecl(func) => {
                process_block(&mut func.body);
            }
            _ => {}
        }
    }

    fn process_block<'src>(block: &mut Block<'src>) {
        for stmt in block.stmts.iter_mut() {
            process_stmt(stmt);
        }
    }

    for prog_stmt in ast.program.iter_mut() {
        if let ProgStmt::FuncDecl(func_decl) = prog_stmt {
            process_block(&mut func_decl.body);
        }
    }

    ast
}


#[test]
fn test_arr_exp() {
    let inp = r#"
        fn main() -> void {
            var a: [i64; 10] = [0];
        }
    "#;
    
    let mut hir = crate::stages::parser::parse(inp).unwrap();
    
    hir.ast = crate::stages::simplifier::simplify(hir.ast).unwrap();
    hir.ast = arr_expand(hir.ast);
    
    println!("{:#?}", hir.ast.program);
}