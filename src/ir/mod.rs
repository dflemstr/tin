//! Intermediate representation variables for the compiler and interpreter.
use std::collections;
use std::sync;

use crate::source;
use crate::syntax;

pub mod builder;
pub mod element;
pub mod error;
pub mod location;
#[cfg(test)]
mod tests;

#[salsa::query_group(IrStorage)]
pub trait Db: salsa::Database + syntax::db::SyntaxDb {
    #[salsa::interned]
    fn ident(&self, id: sync::Arc<String>) -> Ident;

    #[salsa::interned]
    fn entity(&self, parent: Option<Entity>, role: EntityRole) -> Entity;

    fn element(&self, entity: Entity) -> error::Result<sync::Arc<element::Element>>;

    fn location(&self, entity: Entity) -> error::Result<location::Location>;

    fn entities(&self) -> error::Result<sync::Arc<Entities>>;
}

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
    element: sync::Arc<element::Element>,
    location: location::Location,
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

impl EntityInfo {
    pub fn element(&self) -> &element::Element {
        &self.element
    }

    pub fn location(&self) -> location::Location {
        self.location
    }
}

fn element(db: &impl Db, entity: Entity) -> error::Result<sync::Arc<element::Element>> {
    Ok(db.entities()?.infos.get(&entity).unwrap().element.clone())
}

fn location(db: &impl Db, entity: Entity) -> error::Result<location::Location> {
    Ok(db.entities()?.infos.get(&entity).unwrap().location)
}

fn entities(db: &impl Db) -> error::Result<sync::Arc<Entities>> {
    let mut entities = Entities::new();
    for source_root_id in &*db.all_source_roots() {
        for file in db.source_root(*source_root_id).files.values() {
            let result = db.parse(*file).unwrap();

            let entity = db.entity(None, EntityRole::File(*file));
            let builder = builder::Builder::new(db, entity, &mut entities.infos);
            builder.build_module(&*result)?;
            entities.modules.push(entity);
        }
    }
    Ok(sync::Arc::new(entities))
}
