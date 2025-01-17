use std::sync::Arc;

pub trait Deps {}

pub struct DepGraph {
    // data: Option<Rc<DepGraphData<D>>>,
}

pub struct DepGraphData {
    current: CurrentDepGraph,
    previous: Arc<SerializedDepGraph>,
    colors: DepNodeColorMap,
}

pub struct CurrentDepGraph {}

pub struct SerializedDepGraph {}

pub struct DepNodeColorMap {}
