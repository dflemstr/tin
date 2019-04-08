use std::sync;

use crate::ir;
use crate::layout;

#[salsa::query_group(IrStorage)]
pub trait LayoutDb: salsa::Database + ir::db::IrDb {
    fn entity_layout(&self, entity: ir::Entity) -> sync::Arc<layout::Layout>;
}
