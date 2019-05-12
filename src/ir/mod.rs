//! Intermediate representation variables for the compiler and interpreter.
use std::collections;
use std::sync;

use crate::source;

pub mod builder;
pub mod db;
pub mod element;
pub mod error;
pub mod location;
#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ident(u32);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Entity(u32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entities {
    pub infos: collections::HashMap<Entity, EntityInfo>,
    pub modules: Vec<Entity>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum EntityRole {
    File(source::FileId),
    Reference(Ident),
    RecordField(Ident),
    TupleField(usize),
    VariableDefinition(Ident),
    VariableInitializer,
    SelectField(Ident),
    AppliedFunction,
    AppliedParameter(usize),
    ParameterSignature,
    ClosureCaptureDefinition(Ident),
    ClosureParameter(Ident),
    ClosureStatement(usize),
    ClosureSignature,
    ClosureResult,
    ModuleDefinition(Ident),
    UnOperand,
    BiLhs,
    BiRhs,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntityInfo {
    pub element: sync::Arc<element::Element>,
    pub location: sync::Arc<location::Location>,
}

impl Ident {
    pub fn id(self) -> u32 {
        self.0
    }
}

impl Entity {
    pub fn id(self) -> u32 {
        self.0
    }
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

impl EntityInfo {
    fn new(element: element::Element, location: location::Location) -> Self {
        let element = sync::Arc::new(element);
        let location = sync::Arc::new(location);

        Self { element, location }
    }
}

impl Entities {
    pub fn new() -> Self {
        let infos = collections::HashMap::new();
        let modules = Vec::new();

        Self { infos, modules }
    }
}
