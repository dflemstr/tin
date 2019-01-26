//! Common error types and utilities.
use std::result;

use crate::diagnostic;
use crate::interpreter;
use crate::ir;
use crate::syntax::parser;

/// An error that occurs while interacting with Tin.
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    /// Interpreting the code failed.
    ///
    /// This can happen either during an interpreter run, or during compiler constant evaluation
    /// which internally uses the interpreter.
    #[fail(display = "interpreter error")]
    Interpreter(interpreter::error::Error),
    /// Semantically understanding the source code failed.
    #[fail(display = "IR error")]
    Ir(#[cause] ir::error::Error),
    /// Parsing the source code failed.
    #[fail(display = "syntax error")]
    Parser(#[cause] parser::Error),
}

/// A convenience result wrapper for the [`Error`] type.
pub type Result<A> = result::Result<A, Error>;

impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        match *self {
            Error::Parser(ref e) => e.to_diagnostics(builder),
            Error::Interpreter(ref e) => e.to_diagnostics(builder),
            Error::Ir(ref e) => e.to_diagnostics(builder),
        }
    }
}

impl From<interpreter::error::Error> for Error {
    fn from(err: interpreter::error::Error) -> Self {
        Error::Interpreter(err)
    }
}

impl From<ir::error::Error> for Error {
    fn from(err: ir::error::Error) -> Self {
        Error::Ir(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parser(err)
    }
}
