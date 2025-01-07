mod args;
mod config;

use clap::{Arg, Parser};
use marsc_interface::interface;
use std::process;
use std::time::Instant;

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
        let codegen_backend = &*compiler.codegen_backend;

        let linker = compiler.enter(|queries| {
            // queries.parse()?;

            // queries.enter
            Ok(())
        });

        if let Some(linker) = linker {
            // linker.link(session, codegen_backend) TODO
        }

        Ok(())
    })
}

pub fn main() -> ! {
    let start_time = Instant::now();

    let args = Args::parse();

    RunCompiler::new(&args).run().expect(""); // TODO

    process::exit(0); // TODO
}
