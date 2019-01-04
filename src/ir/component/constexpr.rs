use std::sync;

use specs::Component;
use specs::VecStorage;

use crate::value;

#[derive(Component, Clone, Debug, VisitEntities)]
#[storage(VecStorage)]
pub struct Constexpr {
    pub value: sync::Arc<value::Value>,
}
