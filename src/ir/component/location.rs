use specs::Component;
use specs::VecStorage;

#[derive(Clone, Component, Debug)]
#[storage(VecStorage)]
pub struct Location(pub codespan::ByteSpan);
