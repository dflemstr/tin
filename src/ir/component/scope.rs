use std::collections;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, VisitEntities)]
#[storage(VecStorage)]
pub struct Scope {
    pub variables: collections::HashMap<String, specs::Entity>,
}
