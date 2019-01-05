//! Common error types and utilities.
use std::result;

use crate::diagnostic;
use crate::interpreter;
use crate::ir;
use crate::parser;

/// An error that occurs while interacting with Tin.
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    /// Interpreting the code failed.
    ///
    /// This can happen either during an interpreter run, or during compiler constant evaluation
    /// which internally uses the interpreter.
    #[fail(display = "interpreter error")]
    Interpreter(interpreter::Error),
    /// Semantically understanding the source code failed.
    #[fail(display = "IR error")]
    Ir(#[cause] ir::Error),
    /// Parsing the source code failed.
    #[fail(display = "parser error")]
    Parser(#[cause] parser::Error),
}

/// A convenience result wrapper for the [`Error`] type.
pub type Result<A> = result::Result<A, Error>;

impl diagnostic::Diagnostic for Error {
    fn to_diagnostics(&self, result: &mut Vec<codespan_reporting::Diagnostic>) {
        match self {
            Error::Parser(e) => e.to_diagnostics(result),
            Error::Interpreter(_) => {}
            Error::Ir(e) => e.to_diagnostics(result),
        }
    }
}

impl From<interpreter::Error> for Error {
    fn from(err: interpreter::Error) -> Self {
        Error::Interpreter(err)
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
