extern crate marsc_proc_macro;
extern crate marsc_resolve;
extern crate marsc_query_system;

mod args;
mod config;

use crate::args::Args;
use crate::config::build_config;
use clap::builder::TypedValueParser;
use clap::Parser;
use marsc_interface::{interface, passes};
use marsc_interface::linker::Linker;
use std::process;
use marsc_interface::passes::create_and_enter_global_context;
use marsc_resolve::resolve_names;
use marsc_query_system::provider;
use marsc_query_system::{context::TypeContextProviders};
use marsc_resolve::ResolveNamesProvider;
use marsc_proc_macro::provider_method;

pub struct RunCompiler<'a> {
    args: &'a Args,
    // file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
    // make_codegen_backend: Option<Box<dyn FnOnce(&config::Options) -> Box<dyn CodegenBackend> + Send>>,
}

impl<'a> RunCompiler<'a> {
    pub fn new(args: &'a Args) -> Self {
        RunCompiler { args }
    }

    pub fn run(&self) -> interface::Result<()> {
        run_compiler(self.args)
    }
}

fn run_compiler(args: &Args) -> interface::Result<()> {
    let config = build_config(args);

    interface::run_compiler(config, |compiler| {
        let session = &compiler.session;
        // let codegen_backend = &*compiler.codegen_backend;
        
        let ast = passes::parse(session);
        
        let linker = create_and_enter_global_context(compiler, |type_context| {
            println!("{:#?}", ast);
            
            type_context.providers().resolve_names(&ast.unwrap());
            
            Some(Linker {})
        });

        if let Some(linker) = linker {
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