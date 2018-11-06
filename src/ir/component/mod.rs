use specs;

pub mod element;
pub mod replacement;
pub mod scope;
pub mod ty;

pub fn register_all(world: &mut specs::World) {
    world.register::<element::Element>();
    world.register::<scope::Scope>();
    world.register::<replacement::Replacement>();
    world.register::<ty::Type>();
}
