use marsc_session::session::Session;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct CommonTypes {
    
}

impl CommonTypes{
    pub fn new() -> Self {
        CommonTypes {}
    }
}

pub struct TypeContext<'tcx> {
    global_context: &'tcx GlobalContext<'tcx>,
}

impl<'tcx> TypeContext<'tcx> {
    pub fn new(global_context: &'tcx GlobalContext) -> Self {
        TypeContext::<'tcx> {
            global_context,
        }
    }
    
    pub fn create_global_context(
        session: &'tcx Session,
    ) -> GlobalContext<'tcx> {
        let common_types = CommonTypes::new();
        
        GlobalContext {
            session,
            types: common_types,
        }
    }
}

#[derive(Debug)]
pub struct GlobalContext<'tcx> {
    pub session: &'tcx Session,
    // pub dep_graph: DepGraph,
    types: CommonTypes,
}

impl<'tcx> GlobalContext<'tcx> {
    pub fn enter<F, R>(&'tcx self, f: F) -> R
    where
        F: FnOnce(TypeContext) -> R,
    {
        let type_context = TypeContext::new(self);
        f(type_context)
    }
}

#[derive(Clone, Default)]
pub struct CurrentGlobalContext {
    value: Rc<RefCell<Option<*const ()>>>
}

impl CurrentGlobalContext {
    pub fn new() -> Self {
        Self { value: Rc::new(RefCell::new(None)) }
    }

    pub fn access<R>(&self, f: impl for<'tcx> FnOnce(&'tcx GlobalContext<'tcx>) -> R) -> R {
        let borrow = self.value.borrow();

        let gcx = borrow.unwrap() as *const _;

        f(unsafe { &*gcx })
    }
}
