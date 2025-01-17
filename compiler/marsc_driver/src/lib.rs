extern crate marsc_proc_macro;
extern crate marsc_query_system;
extern crate marsc_resolve;

mod args;
mod config;

use crate::args::Args;
use crate::config::build_config;
use clap::Parser;
use marsc_interface::linker::Linker;
use marsc_interface::passes::create_and_enter_global_context;
use marsc_interface::{interface, passes};
use marsc_query_system::context::TypeContextProviders;
use marsc_resolve::ResolveNamesProvider;
use std::process;
use anyhow::Result;

pub struct RunCompiler<'a> {
    args: &'a Args,
    // file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
    // make_codegen_backend: Option<Box<dyn FnOnce(&config::Options) -> Box<dyn CodegenBackend> + Send>>,
}

impl<'a> RunCompiler<'a> {
    pub fn new(args: &'a Args) -> Self {
        RunCompiler { args }
    }

    pub fn run(&self) -> Result<()> {
        run_compiler(self.args)
    }
}

fn run_compiler(args: &Args) -> Result<()> {
    let config = build_config(args);

    interface::run_compiler(config, |compiler| {
        let session = &compiler.session;
        // let codegen_backend = &*compiler.codegen_backend;
        
        let ast = passes::parse(session);
        
        let linker = create_and_enter_global_context(compiler, |type_context| {
            let mir = type_context.providers().resolve_names(ast.unwrap());
            
            println!("{:#?}", mir);
            
            Some(Linker {})
        });

        if let Some(_linker) = linker {
            // linker.link(session, codegen_backend) TODO
        }

        Ok(())
    })
}

pub fn main() -> ! {
    let args = Args::parse();

    RunCompiler::new(&args).run().expect(""); // TODO

    process::exit(0); // TODO
}