//! Common error types and utilities.
use std::result;

use interpreter;
use parser;

/// An error that occurs while interacting with Norm.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// Parsing the source code failed.
    #[fail(display = "parser error")]
    Parser(#[cause] parser::Error),
    /// There was an error while interpreting the code.
    #[fail(display = "interpreter error")]
    Interpreter(#[cause] interpreter::Error),
}

/// A convenience result wrapper for the [`Error`] type.
pub type Result<A> = result::Result<A, Error>;
