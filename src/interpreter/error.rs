use crate::diagnostic;
use crate::ir;
use crate::module;
use std::result;

#[derive(Clone, Debug, Eq, failure::Fail, PartialEq)]
pub enum Error {
    #[fail(display = "semantic error: {}", _0)]
    Ir(#[cause] ir::error::Error),
    #[fail(display = "type conflict: {}", _0)]
    RuntimeTypeConflict(String),
    #[fail(display = "evaluation error: {}", _0)]
    Evaluation(#[cause] module::Error),
}

pub type Result<A> = result::Result<A, Error>;

impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        match *self {
            Error::Ir(_) => {}
            Error::RuntimeTypeConflict(ref cause) => {
                builder.add_diagnostic(codespan_reporting::Severity::Bug, None, cause);
            }
            Error::Evaluation(ref cause) => {
                builder.add_diagnostic(
                    codespan_reporting::Severity::Error,
                    None,
                    &cause.to_string(),
                );
            }
        }
    }
}

impl From<ir::error::Error> for Error {
    fn from(error: ir::error::Error) -> Self {
        Error::Ir(error)
    }
}
