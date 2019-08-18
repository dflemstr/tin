use std::sync;

use crate::source;

pub mod ast;
pub mod parser;

#[salsa::query_group(SyntaxStorage)]
pub trait Db: salsa::Database + source::Db {
    /// Parses the file into an AST module.
    fn parse(
        &self,
        file_id: source::FileId,
    ) -> Result<sync::Arc<ast::Module<parser::Context>>, parser::Error>;
}

fn parse(
    db: &impl Db,
    file_id: source::FileId,
) -> Result<sync::Arc<ast::Module<parser::Context>>, parser::Error> {
    use crate::syntax::parser::Parse;

    let text = db.file_text(file_id);
    let span = db.file_span(file_id);

    ast::Module::parse(span, &*text).map(sync::Arc::new)
}
