use crate::codegen;
use crate::interpreter;
use crate::ir;
use crate::layout;
use crate::source;
use crate::syntax;
use crate::ty;

#[salsa::database(
    codegen::CodegenStorage,
    interpreter::InterpreterStorage,
    ir::IrStorage,
    layout::LayoutStorage,
    source::SourceStorage,
    syntax::SyntaxStorage,
    ty::TyStorage
)]
#[derive(Debug, Default)]
pub struct Db {
    runtime: salsa::Runtime<Db>,
}

impl Db {
    pub fn new() -> Self {
        Self::default()
    }
}

impl salsa::Database for Db {
    fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
        &self.runtime
    }
}
