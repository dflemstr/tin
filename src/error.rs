//! Common error types and utilities.
use std::result;

use parser;

/// An error that occurs while interacting with Norm.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// Parsing the source code failed.
    #[fail(display = "parser error")]
    Parser(#[cause] parser::Error),
}

/// A convenience result wrapper for the [`Error`] type.
pub type Result<A> = result::Result<A, Error>;
