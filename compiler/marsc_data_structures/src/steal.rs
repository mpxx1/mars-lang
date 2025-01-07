use std::sync::RwLock;

#[derive(Debug)]
pub struct Steal<T> {
    value: RwLock<Option<T>>,
}

impl<T> Steal<T> {
    pub fn new(value: T) -> Self {
        Steal {
            value: RwLock::new(Some(value)),
        }
    }
}
