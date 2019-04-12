#![allow(non_camel_case_types)]
use std::sync;

use crate::source;
use crate::syntax;

#[salsa::query_group(Syntax)]
pub trait SyntaxDb: salsa::Database + source::db::SourceDb {
    /// Parses the file into an AST module.
    fn parse(
        &self,
        file_id: source::FileId,
    ) -> Result<sync::Arc<syntax::ast::Module<syntax::parser::Context>>, syntax::parser::Error>;
}

fn parse(
    source: &impl SyntaxDb,
    file_id: source::FileId,
) -> Result<sync::Arc<syntax::ast::Module<syntax::parser::Context>>, syntax::parser::Error> {
    use crate::syntax::parser::Parse;

    let text = source.file_text(file_id);
    let span = source.file_span(file_id);

    syntax::ast::Module::parse(span, &*text).map(sync::Arc::new)
}
