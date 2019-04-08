//! Intermediate representation variables for the compiler and interpreter.
use std::collections;
use std::fmt;

use crate::syntax::ast;
use crate::syntax::parser;

pub mod builder;
pub mod db;
pub mod element;
pub mod error;
pub mod location;
pub mod symbol;
pub mod world;
//#[cfg(test)]
//mod tests;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Entity(u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entities {
    elements: collections::HashMap<Entity, element::Element>,
    locations: collections::HashMap<Entity, location::Location>,
    symbols: collections::HashMap<Entity, symbol::Symbol>,
    next_entity: u32,
}

impl Entity {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

impl Entities {
    pub fn new() -> Self {
        let elements = collections::HashMap::new();
        let locations = collections::HashMap::new();
        let symbols = collections::HashMap::new();
        let next_entity = 0;

        Self {
            elements,
            locations,
            symbols,
            next_entity,
        }
    }
}
