use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::ir::symbol;

pub trait World {
    fn create_entity(&mut self) -> ir::Entity;
    fn set_element(&mut self, entity: ir::Entity, element: element::Element);
    fn set_location(&mut self, entity: ir::Entity, location: location::Location);
    fn set_symbol(&mut self, entity: ir::Entity, symbol: symbol::Symbol);
    fn replace(&mut self, from: ir::Entity, to: ir::Entity);
}

impl<'a, W> World for &'a mut W
where
    W: World,
{
    fn create_entity(&mut self) -> ir::Entity {
        (*self).create_entity()
    }

    fn set_element(&mut self, entity: ir::Entity, element: element::Element) {
        (*self).set_element(entity, element)
    }

    fn set_location(&mut self, entity: ir::Entity, location: location::Location) {
        (*self).set_location(entity, location)
    }

    fn set_symbol(&mut self, entity: ir::Entity, symbol: symbol::Symbol) {
        (*self).set_symbol(entity, symbol)
    }

    fn replace(&mut self, from: ir::Entity, to: ir::Entity) {
        (*self).replace(from, to)
    }
}

impl World for ir::Entities {
    fn create_entity(&mut self) -> ir::Entity {
        let entity = ir::Entity::new(self.next_entity);
        self.next_entity += 1;
        entity
    }

    fn set_element(&mut self, entity: ir::Entity, element: element::Element) {
        unimplemented!()
    }

    fn set_location(&mut self, entity: ir::Entity, location: location::Location) {
        unimplemented!()
    }

    fn set_symbol(&mut self, entity: ir::Entity, symbol: symbol::Symbol) {
        unimplemented!()
    }

    fn replace(&mut self, from: ir::Entity, to: ir::Entity) {
        unimplemented!()
    }
}
