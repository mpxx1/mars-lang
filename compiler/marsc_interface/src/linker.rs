use std::env;
use std::process::Command;
use marsc_session::session::Session;

pub struct Linker {
//    dep_graph: DepGraph,
//    output_filenames: Arc<OutputFilenames>,
//    // module_hash: Option<Svh>,
//    ongoing_codegen: Box<dyn Any>,
}

impl Linker {
    pub fn link(&self, session: &Session) {
        let out_dir = env::var("OUT_DIR").unwrap();
        let status = Command::new("clang")
            .args([format!("{}/libio.so", out_dir), format!("{}/program.o", out_dir)])
            .arg("-o")
            .arg(format!("{}", session.io.output_file.to_str().unwrap()))
            .status()
            .expect("Failed to invoke C compiler and build shared library for external C functions!");

        if !status.success() {
            panic!("Compilation of C add-on libraries failed!");
        }
    }
//    pub fn codegen_and_build_linker(
//        type_context: TypeContext<'_>,
//        codegen_backend: &dyn CodegenBackend,
//    ) -> Result<Linker> {
//        let ongoing_codegen = passes::start_codegen(codegen_backend, type_context)?;
//
//        Ok(Linker {
//            dep_graph: type_context.gep_graph.clone(),
//            output_filenames: type_context.output_filenames(()).clone(),
//            ongoing_codegen,
//        })
//    }
}
