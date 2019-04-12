// This code is largely copied from rust-analyzer
#![allow(non_camel_case_types)]
use std::sync;

use crate::source;

#[salsa::query_group(Source)]
pub trait SourceDb: salsa::Database {
    /// Text of the file.
    #[salsa::input]
    fn file_text(&self, file_id: source::FileId) -> sync::Arc<String>;

    /// Span of the file.
    #[salsa::input]
    fn file_span(&self, file_id: source::FileId) -> codespan::ByteSpan;

    /// Path to a file, relative to the root of its source root.
    #[salsa::input]
    fn file_relative_path(&self, file_id: source::FileId) -> relative_path::RelativePathBuf;

    /// Source root of the file.
    #[salsa::input]
    fn file_source_root(&self, file_id: source::FileId) -> source::SourceRootId;

    /// Contents of the source root.
    #[salsa::input]
    fn source_root(&self, id: source::SourceRootId) -> sync::Arc<source::SourceRoot>;

    /// All source roots.
    #[salsa::input]
    fn all_source_roots(&self) -> sync::Arc<Vec<source::SourceRootId>>;
}
