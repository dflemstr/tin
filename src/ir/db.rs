#![allow(non_camel_case_types)]
use std::sync;

use crate::interpreter;
use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::ir::symbol;
use crate::layout;
use crate::syntax;
use crate::ty;
use crate::value;

#[salsa::query_group(IrStorage)]
pub trait IrDb: salsa::Database + syntax::db::SyntaxDb {
    fn entity_constexpr(
        &self,
        entity: ir::Entity,
    ) -> Result<Option<value::Value>, interpreter::error::Error>;
    fn entity_element(&self, entity: ir::Entity) -> sync::Arc<element::Element>;
    fn entity_type(
        &self,
        entity: ir::Entity,
    ) -> Result<sync::Arc<ty::Type>, ty::error::Error<ir::Entity>>;
    fn entity_layout(&self, entity: ir::Entity) -> sync::Arc<layout::Layout>;
    fn entity_location(&self, entity: ir::Entity) -> sync::Arc<location::Location>;
    fn entity_symbol(&self, entity: ir::Entity) -> sync::Arc<symbol::Symbol>;

    fn entities(&self) -> sync::Arc<ir::Entities>;
}

fn entity_constexpr(
    ir: &impl IrDb,
    entity: ir::Entity,
) -> Result<Option<value::Value>, interpreter::error::Error> {
    let element = ir.entity_element(entity);

    Ok(interpreter::eval(&*element, |e| {
        ir.entity_constexpr(e)
            .as_ref()
            .ok()
            .and_then(|o| o.as_ref())
            .map(|c| c.value.clone())
    })?)
}

fn entity_element(_ir: &impl IrDb, _entity: ir::Entity) -> sync::Arc<element::Element> {
    unimplemented!()
}

fn entity_type(
    _ir: &impl IrDb,
    _entity: ir::Entity,
) -> Result<sync::Arc<ty::Type>, ty::error::Error<ir::Entity>> {
    unimplemented!()
}

fn entity_layout(_ir: &impl IrDb, _entity: ir::Entity) -> sync::Arc<layout::Layout> {
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
