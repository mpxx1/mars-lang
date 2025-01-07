use std::rc::Rc;
use std::sync::Arc;

pub trait Deps {}

pub struct DepGraph<D: Deps> {
    data: Option<Rc<DepGraphData<D>>>,
}

pub struct DepGraphData<D: Deps> {
    current: CurrentDepGraph<D>,
    previous: Arc<SerializedDepGraph>,
    colors: DepNodeColorMap,
}

pub struct CurrentDepGraph<D: Deps> {}

pub struct SerializedDepGraph {}

pub struct DepNodeColorMap {}
