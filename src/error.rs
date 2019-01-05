//! Common error types and utilities.
use std::result;

use crate::diagnostic;
use crate::ir;
use crate::parser;

/// An error that occurs while interacting with Tin.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// Parsing the source code failed.
    #[fail(display = "parser error")]
    Parser(#[cause] parser::Error),
    /// Semantically understanding the source code failed.
    #[fail(display = "IR error")]
    Ir(#[cause] ir::Error),
}

/// A convenience result wrapper for the [`Error`] type.
pub type Result<A> = result::Result<A, Error>;

impl diagnostic::Diagnostic for Error {
    fn to_diagnostics(&self, result: &mut Vec<codespan_reporting::Diagnostic>) {
        match self {
            Error::Parser(e) => e.to_diagnostics(result),
            Error::Ir(_) => {}
        }
    }
}

impl From<ir::Error> for Error {
    fn from(err: ir::Error) -> Self {
        Error::Ir(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parser(err)
    }
}
