use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{FunctionValue, StructValue};
use marsc_hir::ast;
use marsc_hir::ast::{Block, Expr, FuncDecl, ProgStmt, Stmt, StructDecl, Type, AST};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    
    fn codegen_ast(&self, root_ast: &'ctx ast::AST) -> String {
        for prog_stmt in &root_ast.program {
            match prog_stmt {
                ProgStmt::StructDecl(struct_decl) => self.codegen_struct(&struct_decl),
                ProgStmt::FuncDecl(func_decl) => self.codegen_func(&func_decl),
            };
        }
        
        self.module.print_to_string().to_string()
    }
    
    fn codegen_struct(&self, struct_decl: &StructDecl) {
        todo!()
    }

    fn codegen_func(&self, func_decl: &FuncDecl) {
        // let arg_types: &[BasicMetadataTypeEnum] = func_decl
        //     .args
        //     .iter()
        //     .map(|arg| self.codegen_type(&arg.typ))
        //     .collect();
        
        // let return_type = self.codegen_type(&func_decl.return_type);
        // let fn_type = return_type.fn_type(&[], false);
        // let function = self.module.add_function(&func_decl.name, fn_type, None);
        // let entry = self.context.append_basic_block(function, "entry");
        // self.builder.position_at_end(entry);
        
        // self.codegen_block(&func_decl.body);
    }
    
    fn codegen_block(&self, block: &Block) {
        todo!()
    }
    
    fn codegen_stmt(&self, stmt: &Stmt) {
        todo!()
    }
    
    fn codegen_expr(&self, expr: Expr) -> BasicTypeEnum {
        todo!()
    }
    
    fn codegen_type(&self, type_ast: &Type) -> BasicTypeEnum<'ctx> {
        match type_ast {
            Type::I64 => self.context.i64_type().as_basic_type_enum(),
            Type::F64 => self.context.f64_type().as_basic_type_enum(),
            Type::Str => todo!(),
            Type::Char => todo!(),
            Type::Bool => todo!(),
            Type::Void => todo!(),
            Type::Custom(_) => todo!(),
            Type::Array(_, _) => todo!(),
            Type::Vec(_) => todo!(),
            Type::Ref(_) => todo!(),
        }
    }
    
    fn test(&self) -> String {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let z = function.get_nth_param(2).unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        let sum = self.builder.build_int_add(sum, z, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        self.module.print_to_string().to_string()
    }
}

pub fn codegen(ast: AST) {
    let context = Context::create();
    let module = context.create_module("test_module");
    let codegen = Codegen {
        context: &context,
        module,
        builder: context.create_builder(),
    };
    
    println!("{}", codegen.codegen_ast(&ast))
}