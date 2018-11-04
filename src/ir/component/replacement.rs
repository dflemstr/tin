use specs;

use specs::Component;
use specs::HashMapStorage;

#[derive(Component, Debug, VisitEntities)]
#[storage(HashMapStorage)]
pub struct Replacement {
    pub to: specs::Entity,
}
