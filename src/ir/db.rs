#![allow(non_camel_case_types)]
use std::sync;

use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::ir::symbol;
use crate::syntax;

#[salsa::query_group(Ir)]
pub trait IrDb: salsa::Database + syntax::db::SyntaxDb {
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

fn entities(_ir: &impl IrDb) -> sync::Arc<ir::Entities> {
    unimplemented!()
}
