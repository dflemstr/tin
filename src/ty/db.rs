use std::sync;

use crate::ir;
use crate::ty;

#[salsa::query_group(Ty)]
pub trait TyDb: salsa::Database + ir::db::IrDb {
    fn entity_type(&self, entity: ir::Entity) -> Result<sync::Arc<ty::Type>, ty::error::Error>;
}

fn entity_type(
    _ir: &impl TyDb,
    _entity: ir::Entity,
) -> Result<sync::Arc<ty::Type>, ty::error::Error> {
    unimplemented!()
}
