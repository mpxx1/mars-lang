use crate::interface::{Compiler, Result};
use crate::passes;
use marsc_core::dep_graph::{DepGraph};
use marsc_data_structures::steal::Steal;
use marsc_hir::ast;
use marsc_mir::types::context::{GlobalContext, TypeContext};
use std::any::Any;
use std::cell::{RefCell, RefMut};
use std::sync::{Arc, OnceLock};

pub struct Query<T> {
    result: RefCell<Option<Result<Steal<T>>>>,
}

impl<T> Query<T> {
    fn compute<F: FnOnce() -> Result<T>>(&self, f: F) -> Result<QueryResult<'_, T>> {
        RefMut::filter_map(
            self.result.borrow_mut(),
            |r: &mut Option<Result<Steal<T>>>| -> Option<&mut Steal<T>> {
                r.get_or_insert_with(|| f().map(Steal::new)).as_mut().ok()
            },
        )
        .map_err(|r| *r.as_ref().unwrap().as_ref().map(|_| ()).unwrap_err())
        .map(QueryResult)
    }
}

pub struct QueryResult<'a, T>(RefMut<'a, Steal<T>>);

pub struct Queries<'tcx> {
    compiler: &'tcx Compiler,
    global_context_cell: OnceLock<GlobalContext<'tcx>>,

    /* arena: WorkerLocal<Arena<'tcx>>,
    hir_arena: WorkerLocal<rustc_hir::Arena<'tcx>>,*/
    parse: Query<ast::AST>,
    global_context: Query<&'tcx GlobalContext<'tcx>>,
}

impl<'tcx> Queries<'tcx> {
    pub fn new(compiler: &'tcx Compiler) -> Queries<'tcx> {
        Queries {
            compiler,
            global_context_cell: OnceLock::new(),
            /*arena: WorkerLocal::new(|_| Arena::default()),
            hir_arena: WorkerLocal::new(|_| rustc_hir::Arena::default()),*/
            parse: Query {
                result: RefCell::new(None),
            },
            global_context: Query {
                result: RefCell::new(None),
            },
        }
    }

    pub fn parse(&self) -> Result<QueryResult<'_, ast::AST>> {
        self.parse.compute(|| passes::parse(&self.compiler.session))
    }

    pub fn global_context(&'tcx self) -> Result<QueryResult<'tcx, &'tcx GlobalContext<'tcx>>> {
        self.global_context.compute(|| {
            let project = self.parse()?.steal();

            passes::create_global_context(
                self.compiler,
                project,
                &self.global_context_cell,
                /*&self.arena,
                &self.hir_arena,*/
            )
        })
    }
}

pub struct Linker {
    dep_graph: DepGraph,
    output_filenames: Arc<OutputFilenames>,
    // module_hash: Option<Svh>,
    ongoing_codegen: Box<dyn Any>,
}

impl Linker {
    pub fn codegen_and_build_linker(
        type_context: TypeContext<'_>,
        codegen_backend: &dyn CodegenBackend,
    ) -> Result<Linker> {
        let ongoing_codegen = passes::start_codegen(codegen_backend, type_context)?;

        Ok(Linker {
            dep_graph: type_context.gep_graph.clone(),
            output_filenames: type_context.output_filenames(()).clone(),
            ongoing_codegen,
        })
    }
}

impl Compiler {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'tcx> FnOnce(&'tcx Queries<'tcx>) -> T,
    {
        let queries = Queries::new(self);
        let ret = f(&queries);

        ret
    }
}
