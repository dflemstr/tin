use specs;

pub mod element;
pub mod replacement;
pub mod ty;

pub fn register_all(world: &mut specs::World) {
    world.register::<element::Element>();
    world.register::<replacement::Replacement>();
    world.register::<ty::Type>();
}
