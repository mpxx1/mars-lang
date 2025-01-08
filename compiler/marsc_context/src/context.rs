use core::cell::Cell;
use core::ptr;
use marsc_session::session::Session;
use std::cell::RefCell;
use std::rc::Rc;

pub struct TypeContext<'tcx> {
    global_context: &'tcx GlobalContext<'tcx>,
}

impl<'tcx> TypeContext<'tcx> {
    pub fn new(global_context: &'tcx GlobalContext) -> Self {
        TypeContext::<'tcx> {
            global_context
        }
    }
}

#[warn(dead_code)]
pub struct GlobalContext<'tcx> {
    pub session: &'tcx Session,
    // pub dep_graph: DepGraph,
    current_global_context: CurrentGlobalContext,
}

impl<'tcx> GlobalContext<'tcx> {
    pub fn new(session: &'tcx Session) -> Self {
        GlobalContext {
            session,
            current_global_context: CurrentGlobalContext::default(),
        }
    }
}

#[derive(Clone, Default)]
pub struct CurrentGlobalContext {
    value: Rc<RefCell<Option<*const ()>>>,
}

impl<'tcx> GlobalContext<'tcx> {
    pub fn enter<F, R>(&'tcx self, f: F) -> R
    where
        F: FnOnce(TypeContext) -> R,
    {
        let implicit_context = ImplicitContext::new(self);

        /*let _on_drop = defer(move || {
            *self.current_global_context.value.write() = None;
        });*/

        // enter_implicit_context(&implicit_context, || f(implicit_context.type_context))
        panic!()
    }
}

pub struct ImplicitContext<'tcx> {
    pub type_context: TypeContext<'tcx>,
    // pub query: Option<QueryJobId>,
    pub query_depth: usize,
}

impl<'tcx> ImplicitContext<'tcx> {
    pub fn new(global_context: &'tcx GlobalContext) -> Self {
        let tcx = TypeContext::<'tcx> { global_context };
        ImplicitContext::<'tcx> {
            type_context: tcx,
            // query: None,
            query_depth: 0,
        }
    }
}

#[cfg(not(parallel_compiler))]
thread_local! {
    static TREAD_LOCAL_IMPLICIT_CONTEXT: Cell<*const ()> = const { Cell::new(ptr::null()) };
}

#[inline]
fn erase(context: &ImplicitContext) -> *const () {
    context as *const _ as *const ()
}

pub fn enter_implicit_context<'a, 'tcx, F, R>(context: &ImplicitContext, f: F) -> R
where
    F: FnOnce() -> R,
{
    TREAD_LOCAL_IMPLICIT_CONTEXT.with(|tl_ctxt| {
        let old_context = tl_ctxt.replace(erase(context));
        /*let _reset = rustc_data_structures::defer(move || tl_ctxt.set(old_context));*/
        f()
    })
}
