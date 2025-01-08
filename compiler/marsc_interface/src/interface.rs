use marsc_codegen::codegen::CodegenBackend;
use marsc_context::context::{CurrentGlobalContext, GlobalContext};
use marsc_session::session::Session;
use marsc_span::ErrorGuaranteed;
use std::path::PathBuf;
use std::process::Output;
use std::result;
use std::sync::Arc;

pub type Result<T> = result::Result<T, ErrorGuaranteed>;

pub struct Config {
    pub input: String,
    pub output: String,
    /*pub opts: config::Options,

    pub crate_cfg: Vec<String>,
    pub crate_check_cfg: Vec<String>,

    pub input: Input,
    pub output_dir: Option<PathBuf>,
    pub output_file: Option<OutFileName>,
    pub ice_file: Option<PathBuf>,
    pub file_loader: Option<Box<dyn FileLoader + Send + Sync>>,
    pub locale_resources: Vec<&'static str>,

    pub lint_caps: FxHashMap<lint::LintId, lint::Level>,

    pub psess_created: Option<Box<dyn FnOnce(&mut ParseSess) + Send>>,

    pub hash_untracked_state: Option<Box<dyn FnOnce(&Session, &mut StableHasher) + Send>>,

    pub register_lints: Option<Box<dyn Fn(&Session, &mut LintStore) + Send + Sync>>,

    pub override_queries: Option<fn(&Session, &mut Providers)>,

    pub make_codegen_backend:
        Option<Box<dyn FnOnce(&config::Options) -> Box<dyn CodegenBackend> + Send>>,

    pub registry: Registry,

    pub using_internal_features: Arc<std::sync::atomic::AtomicBool>,

    pub expanded_args: Vec<String>,*/
}

pub struct Compiler {
    pub session: Session,
    pub codegen_backend: Box<dyn CodegenBackend>,
    // pub(crate) override_queries: Option<fn(&Session, &mut Providers)>,
    pub(crate) current_gcx: CurrentGlobalContext,
}

//pub fn run_compiler<R: Send>(config: Config, f: impl FnOnce(&Compiler) -> R + Send) -> R {
//    // setup callbacks
//
//    // resolve target
//    // create file loader
//
//    // run with new GlobalContext
//    run_with_global_context(|global_context: GlobalContext| {
//        // create codegen backend
//
//        // create session
//        let mut session = Session {};
//
//        // init codegen with session
//
//        // create compiler
//        let compiler = Compiler {};
//
//        let res = {
//            let res = f(&compiler);
//
//            res
//        };
//
//        res
//    })
//}

//pub fn run_with_global_context<F: FnOnce(GlobalContext) -> R + Send, R: Send>(f: F) -> R {
//    f(GlobalContext::new())
//}
