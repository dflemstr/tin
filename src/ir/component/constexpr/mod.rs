use std::sync;

use specs::Component;
use specs::VecStorage;

use crate::value;

pub mod error;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Constexpr {
    pub value: sync::Arc<value::Value>,
}
