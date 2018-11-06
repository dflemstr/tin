use std::collections;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, VisitEntities)]
#[storage(VecStorage)]
pub struct Scope {
    pub definitions: collections::HashMap<String, specs::Entity>,
}
