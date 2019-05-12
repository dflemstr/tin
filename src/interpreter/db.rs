use crate::interpreter;
use crate::ir;
use crate::value;

#[salsa::query_group(Interpreter)]
pub trait InterpreterDb: salsa::Database + ir::db::IrDb {
    fn entity_value(
        &self,
        entity: ir::Entity,
    ) -> Result<Option<value::Value>, interpreter::error::Error>;
}

fn entity_value(
    db: &impl InterpreterDb,
    entity: ir::Entity,
) -> Result<Option<value::Value>, interpreter::error::Error> {
    let element = db.entity_element(entity)?;

    Ok(interpreter::eval(&*element, db)?)
}
