use specs;

pub mod constexpr;
pub mod element;
pub mod layout;
pub mod replacement;
pub mod symbol;
pub mod ty;

macro_rules! for_each_component {
    (|$name:ident| $body:expr) => {
        apply_component!(constexpr::Constexpr, $name, $body);
        apply_component!(element::Element, $name, $body);
        apply_component!(layout::Layout, $name, $body);
        apply_component!(replacement::Replacement, $name, $body);
        apply_component!(symbol::Symbol, $name, $body);
        apply_component!(ty::Type, $name, $body);
    };
}

macro_rules! apply_component {
    ($component:ty, $name:ident, $body:expr) => {{
        type $name = $component;
        $body
    }};
}

pub fn register_all(world: &mut specs::World) {
    for_each_component!(|C| world.register::<C>());
}
