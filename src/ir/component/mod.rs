use specs;

pub mod element;
pub mod replacement;

pub fn register_all(world: &mut specs::World) {
    world.register::<element::Element>();
    world.register::<replacement::Replacement>();
}
