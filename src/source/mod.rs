// This code is largely copied from rust-analyzer

//! This module specifies the input to the compiler.
//!
//! In some sense, this is **the** most important module, because all other fancy stuff is strictly
//! derived from this input.
//!
//! Note that neither this module, nor any other part of the compiler do actual IO.
//!
use std::collections;
use std::sync;

#[salsa::query_group(SourceStorage)]
pub trait Db: salsa::Database {
    /// Text of the file.
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> sync::Arc<String>;

    /// Span of the file.
    #[salsa::input]
    fn file_span(&self, file_id: FileId) -> codespan::ByteSpan;

    /// Path to a file, relative to the root of its source root.
    #[salsa::input]
    fn file_relative_path(&self, file_id: FileId) -> relative_path::RelativePathBuf;

    /// Source root of the file.
    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> RootId;

    /// Contents of the source root.
    #[salsa::input]
    fn source_root(&self, id: RootId) -> sync::Arc<Root>;

    /// All source roots.
    #[salsa::input]
    fn all_source_roots(&self) -> sync::Arc<Vec<RootId>>;
}

/// `FileId` is an integer which uniquely identifies a file.
///
/// File paths are messy and system-dependent, so most of the code should work directly with
/// `FileId`, without inspecting the path. The mapping between `FileId` and path  and `SourceRoot`
/// is constant. A file rename is represented as a pair of deletion/creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

/// Files are grouped into source roots.
///
/// A source root is a directory on the file systems which is watched for changes. Source roots
/// *might* be nested: in this case, a file belongs to the nearest enclosing source root. Paths to
/// files are always relative to a source root, and the compiler does not know the root path of the
/// source root at all. So, a file from one source root can't refer to a file in another source root
/// by path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RootId(pub u32);

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct Root {
    pub files: collections::HashMap<relative_path::RelativePathBuf, FileId>,
}
