#![allow(non_camel_case_types)]
use std::sync;

use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::ir::symbol;
use crate::syntax;

#[salsa::query_group(Ir)]
pub trait IrDb: salsa::Database + syntax::db::SyntaxDb {
    #[salsa::interned]
    fn ident(&self, id: sync::Arc<String>) -> ir::Ident;

    #[salsa::interned]
    fn entity(&self, parent: Option<ir::Entity>, ident: ir::Ident) -> ir::Entity;

    fn entity_element(&self, entity: ir::Entity) -> sync::Arc<element::Element>;

    fn entity_location(&self, entity: ir::Entity) -> sync::Arc<location::Location>;

    fn entity_symbol(&self, entity: ir::Entity) -> sync::Arc<symbol::Symbol>;

    fn entities(&self) -> sync::Arc<ir::Entities>;
}

fn entity_element(_ir: &impl IrDb, _entity: ir::Entity) -> sync::Arc<element::Element> {
    unimplemented!()
}

fn entity_location(_ir: &impl IrDb, _entity: ir::Entity) -> sync::Arc<location::Location> {
    unimplemented!()
}

fn entity_symbol(_ir: &impl IrDb, _entity: ir::Entity) -> sync::Arc<symbol::Symbol> {
    unimplemented!()
}

fn entities(ir: &impl IrDb) -> sync::Arc<ir::Entities> {
    let mut entities = ir::Entities::new();
    for source_root_id in &*ir.all_source_roots() {
        for file in ir.source_root(*source_root_id).files.values() {
            let result = ir.parse(*file).unwrap();

            //let mut builder = ir::builder::Builder::new(&mut entities);
            //builder.add_module(&*result).unwrap();
        }
    }
    sync::Arc::new(entities)
}
