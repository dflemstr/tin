use std::sync;

use crate::ir;
use crate::ty;

#[salsa::query_group(Ty)]
pub trait TyDb: salsa::Database + ir::Db {
    fn entity_type(&self, entity: ir::Entity) -> Result<sync::Arc<ty::Type>, ty::error::Error>;

    fn bool_type(&self) -> sync::Arc<ty::Type>;
}

fn entity_type(
    _ir: &impl TyDb,
    _entity: ir::Entity,
) -> Result<sync::Arc<ty::Type>, ty::error::Error> {
    Err(ty::error::Error {
        expected: ty::error::ExpectedType::AnyOf(vec![]),
        actual: sync::Arc::new(ty::Type::Placeholder),
        main_entity: _entity,
        aux_entities: vec![],
    })
}

fn bool_type(db: &impl TyDb) -> sync::Arc<ty::Type> {
    let alternatives = vec![
        ty::Symbol {
            label: db.ident(sync::Arc::new("f".to_owned())),
        },
        ty::Symbol {
            label: db.ident(sync::Arc::new("t".to_owned())),
        },
    ];
    sync::Arc::new(ty::Type::Union(ty::Union { alternatives }))
}
