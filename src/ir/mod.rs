//! Intermediate representation variables for the compiler and interpreter.
use std::collections;

// pub mod builder;
pub mod db;
pub mod element;
pub mod error;
pub mod location;
pub mod symbol;
pub mod world;
//#[cfg(test)]
//mod tests;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ident(u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Entity(u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entities {
    elements: collections::HashMap<Entity, element::Element>,
    locations: collections::HashMap<Entity, location::Location>,
    symbols: collections::HashMap<Entity, symbol::Symbol>,
}

impl salsa::InternKey for Ident {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Ident(v.as_u32())
    }

    fn as_intern_id(&self) -> salsa::InternId {
        salsa::InternId::from(self.0)
    }
}

impl salsa::InternKey for Entity {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Entity(v.as_u32())
    }

    fn as_intern_id(&self) -> salsa::InternId {
        salsa::InternId::from(self.0)
    }
}

impl Entities {
    pub fn new() -> Self {
        let elements = collections::HashMap::new();
        let locations = collections::HashMap::new();
        let symbols = collections::HashMap::new();

        Self {
            elements,
            locations,
            symbols,
        }
    }
}
