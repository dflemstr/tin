use std::result;

use crate::interpreter;
use crate::ir;
use crate::layout;
use crate::ty;

/// Errors that may occur while building and interacting with an [`Ir`].
#[derive(Clone, Debug, Eq, failure::Fail, PartialEq)]
pub enum Error {
    /// Semantic error.
    #[fail(display = "semantic error")]
    Ir(#[cause] ir::error::Error),

    /// Type error.
    #[fail(display = "type error")]
    Ty(#[cause] ty::error::Error),

    /// Constexpr interpreter evaluation failed.
    #[fail(display = "failed to evaluate constexpr")]
    Constexpr(#[cause] interpreter::error::Error),

    /// Failed to determine data layout.
    #[fail(display = "failed to determine data layout")]
    Layout(#[cause] layout::error::Error),

    /// There were multiple codegen errors.
    #[fail(display = "multiple codegen errors")]
    Multiple {
        /// The errors, in the order they were encountered in the codegen.
        errors: Vec<Error>,
    },
}

pub type Result<A> = result::Result<A, Error>;

impl From<ir::error::Error> for Error {
    fn from(error: ir::error::Error) -> Self {
        Error::Ir(error)
    }
}

impl From<ty::error::Error> for Error {
    fn from(error: ty::error::Error) -> Self {
        Error::Ty(error)
    }
}

impl From<interpreter::error::Error> for Error {
    fn from(error: interpreter::error::Error) -> Self {
        Error::Constexpr(error)
    }
}

impl From<layout::error::Error> for Error {
    fn from(error: layout::error::Error) -> Self {
        Error::Layout(error)
    }
}
