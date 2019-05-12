#![allow(non_camel_case_types)]
use std::sync;

use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::syntax;

#[salsa::query_group(Ir)]
pub trait IrDb: salsa::Database + syntax::db::SyntaxDb {
    #[salsa::interned]
    fn ident(&self, id: sync::Arc<String>) -> ir::Ident;

    #[salsa::interned]
    fn entity(&self, parent: Option<ir::Entity>, kind: EntityKind) -> ir::Entity;

    fn entity_element(&self, entity: ir::Entity) -> sync::Arc<element::Element>;

    fn entity_location(&self, entity: ir::Entity) -> sync::Arc<location::Location>;

    fn entities(&self) -> sync::Arc<ir::Entities>;
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum EntityKind {
    Module(ir::Ident),
    Named(ir::Ident),
    Indexed(usize),
    Initializer,
    UnOperand,
    BiLhs,
    BiRhs,
    Signature,
    LambdaResult,
    CalledFunction,
}

fn entity_element(db: &impl IrDb, entity: ir::Entity) -> sync::Arc<element::Element> {
    db.entities().infos.get(&entity).unwrap().element.clone()
}

fn entity_location(db: &impl IrDb, entity: ir::Entity) -> sync::Arc<location::Location> {
    db.entities().infos.get(&entity).unwrap().location.clone()
}

fn entities(db: &impl IrDb) -> sync::Arc<ir::Entities> {
    let mut entities = ir::Entities::new();
    for source_root_id in &*db.all_source_roots() {
        for file in db.source_root(*source_root_id).files.values() {
            let result = db.parse(*file).unwrap();

            //let mut builder = ir::builder::Builder::new(&mut entities);
            //builder.add_module(&*result).unwrap();
        }
    }
    sync::Arc::new(entities)
}
