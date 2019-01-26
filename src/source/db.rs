// This code is largely copied from rust-analyzer
#![allow(non_camel_case_types)]
use std::sync;

use crate::source;
use crate::syntax;

#[salsa::query_group(SourceStorage)]
pub trait Source: salsa::Database {
    /// Text of the file.
    #[salsa::input]
    fn file_text(&self, file_id: source::FileId) -> sync::Arc<String>;

    /// Span of the file.
    #[salsa::input]
    fn file_span(&self, file_id: source::FileId) -> codespan::ByteSpan;

    /// Parses the file into an AST module.
    fn parse(
        &self,
        file_id: source::FileId,
    ) -> Result<sync::Arc<syntax::ast::Module<syntax::parser::Context>>, syntax::parser::Error>;

    /// Path to a file, relative to the root of its source root.
    #[salsa::input]
    fn file_relative_path(&self, file_id: source::FileId) -> relative_path::RelativePathBuf;

    /// Source root of the file.
    #[salsa::input]
    fn file_source_root(&self, file_id: source::FileId) -> source::SourceRootId;

    /// Contents of the source root.
    #[salsa::input]
    fn source_root(&self, id: source::SourceRootId) -> sync::Arc<source::SourceRoot>;
}

fn parse(
    source: &impl Source,
    file_id: source::FileId,
) -> Result<sync::Arc<syntax::ast::Module<syntax::parser::Context>>, syntax::parser::Error> {
    use crate::syntax::parser::Parse;

    let text = source.file_text(file_id);
    let span = source.file_span(file_id);

    syntax::ast::Module::parse(span, &*text).map(sync::Arc::new)
}
