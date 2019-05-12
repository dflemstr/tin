#![allow(non_camel_case_types)]
use std::sync;

use crate::ir;
use crate::ir::location;
use crate::ir::{element, EntityRole};
use crate::syntax;

#[salsa::query_group(Ir)]
pub trait IrDb: salsa::Database + syntax::db::SyntaxDb {
    #[salsa::interned]
    fn ident(&self, id: sync::Arc<String>) -> ir::Ident;

    #[salsa::interned]
    fn entity(&self, parent: Option<ir::Entity>, role: EntityRole) -> ir::Entity;

    fn entity_element(
        &self,
        entity: ir::Entity,
    ) -> Result<sync::Arc<element::Element>, ir::error::Error>;

    fn entity_location(
        &self,
        entity: ir::Entity,
    ) -> Result<sync::Arc<location::Location>, ir::error::Error>;

    fn entities(&self) -> Result<sync::Arc<ir::Entities>, ir::error::Error>;
}

fn entity_element(
    db: &impl IrDb,
    entity: ir::Entity,
) -> Result<sync::Arc<element::Element>, ir::error::Error> {
    Ok(db.entities()?.infos.get(&entity).unwrap().element.clone())
}

fn entity_location(
    db: &impl IrDb,
    entity: ir::Entity,
) -> Result<sync::Arc<location::Location>, ir::error::Error> {
    Ok(db.entities()?.infos.get(&entity).unwrap().location.clone())
}

fn entities(db: &impl IrDb) -> Result<sync::Arc<ir::Entities>, ir::error::Error> {
    let mut entities = ir::Entities::new();
    for source_root_id in &*db.all_source_roots() {
        for file in db.source_root(*source_root_id).files.values() {
            let result = db.parse(*file).unwrap();

            let entity = db.entity(None, ir::EntityRole::File(*file));
            let builder = ir::builder::Builder::new(db, entity, &mut entities.infos);
            let module_id = builder.build_module(
                sync::Arc::new(db.file_relative_path(*file).as_str().to_owned()),
                &*result,
            )?;
            entities.modules.push(module_id);
        }
    }
    Ok(sync::Arc::new(entities))
}
