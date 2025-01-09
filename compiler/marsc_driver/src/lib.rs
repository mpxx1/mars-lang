mod args;
mod config;

use clap::{Arg, Parser};
use marsc_interface::interface;
use std::process;
use std::time::Instant;
use clap::builder::TypedValueParser;
use marsc_interface::queries::Linker;
use crate::args::Args;
use crate::config::build_config;

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

        let linker = compiler.enter(|queries| {
            println!("{:#?}", queries.global_context());
            
            let ast = queries.parse().unwrap();
            println!("{:#?}", ast);

            // queries.enter
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
