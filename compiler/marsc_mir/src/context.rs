use ast::Type;
use marsc_session::session::Session;
use crate::ExternalFunction;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::OnceLock;

#[derive(Debug)]
pub struct ExternalFunctions<'tcx> {
    pub println: ExternalFunction<'tcx>,
    pub print: ExternalFunction<'tcx>,
}

impl<'tcx> ExternalFunctions<'tcx> {
    pub fn new() -> Self {
        ExternalFunctions {
            println: ExternalFunction {
                ident: "println",
                args: vec![Type::Str],
                return_type: Type::Void,
            },
            print: ExternalFunction {
                ident: "print",
                args: vec![Type::Str],
                return_type: Type::Void,
            }
        }
    }
}

impl<'tcx> ExternalFunctions<'tcx> {
    pub fn get(&self, name: &str) -> Option<&ExternalFunction> {
        match name {
            "println" => Some(&self.println),
            _ => None
        }
    }
    
    pub fn get_all(&self) -> Box<[&ExternalFunction]> {
        Box::new([
            &self.println,
            &self.print,
        ])
    }
}

pub struct TypeContext<'tcx> {
    pub global_context: &'tcx GlobalContext<'tcx>,
}

impl<'tcx> TypeContext<'tcx> {
    pub fn new(global_context: &'tcx GlobalContext) -> Self {
        TypeContext::<'tcx> {
            global_context,
        }
    }
    
    pub fn create_global_context<T>(
        global_context_cell: &'tcx OnceLock<GlobalContext<'tcx>>,
        session: &'tcx Session,
        f: impl FnOnce(TypeContext<'tcx>) -> T,
    ) -> T {
        let external_functions = ExternalFunctions::new();
        
        let global_context = global_context_cell.get_or_init(|| GlobalContext {
            session,
            external_functions,
        });
        
        let type_context = TypeContext::new(global_context);
        
        f(type_context)
    }
}

impl<'tcx> Deref for TypeContext<'tcx> {
    type Target = &'tcx GlobalContext<'tcx>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.global_context
    }
}

#[derive(Debug)]
pub struct GlobalContext<'tcx> {
    pub session: &'tcx Session,
    pub external_functions: ExternalFunctions<'tcx>,
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
