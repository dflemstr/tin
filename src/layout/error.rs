use crate::ir;
use crate::ty;
use std::result;

#[derive(Clone, Debug, Eq, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "semantic error: {}", _0)]
    Ir(#[cause] ir::error::Error),
    #[fail(display = "unknown size")]
    UnknownSize,
}

pub type Result<A> = result::Result<A, Error>;

impl From<ir::error::Error> for Error {
    fn from(error: ir::error::Error) -> Self {
        Error::Ir(error)
    }
}
