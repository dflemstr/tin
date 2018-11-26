use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, VisitEntities)]
#[storage(VecStorage)]
pub struct Constexpr;
