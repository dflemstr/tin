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

    #[salsa::dependencies]
    fn code_map(&self) -> sync::Arc<CodeMap>;

    /// Span of the file.
    fn file_span(&self, file_id: FileId) -> codespan::ByteSpan;
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

#[derive(Default, Clone, Debug)]
pub struct CodeMap {
    pub raw: codespan::CodeMap<Text>,
    file_maps: collections::HashMap<FileId, sync::Arc<codespan::FileMap<Text>>>,
}

#[derive(Default, Clone, Debug)]
pub struct Text(sync::Arc<String>);

fn code_map(db: &impl Db) -> sync::Arc<CodeMap> {
    let mut raw = codespan::CodeMap::new();
    let mut file_maps = collections::HashMap::new();

    for root_id in &*db.all_source_roots() {
        let source_root = db.source_root(*root_id);
        for (path, file) in &source_root.files {
            let text = db.file_text(*file);
            let file_map = raw.add_filemap(
                codespan::FileName::Virtual(path.as_str().to_owned().into()),
                Text(text),
            );
            file_maps.insert(*file, file_map);
        }
    }

    sync::Arc::new(CodeMap { raw, file_maps })
}

fn file_span(db: &impl Db, file_id: FileId) -> codespan::ByteSpan {
    db.code_map().file_maps[&file_id].span()
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        &*self.0
    }
}
