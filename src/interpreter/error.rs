use crate::diagnostic;
use crate::module;

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "type conflict: {}", _0)]
    RuntimeTypeConflict(String),
    #[fail(display = "evaluation error: {}", _0)]
    EvaluationError(#[cause] module::Error),
}

impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        match *self {
            Error::RuntimeTypeConflict(ref cause) => {
                builder.add_diagnostic(codespan_reporting::Severity::Bug, None, cause);
            }
            Error::EvaluationError(ref cause) => {
                builder.add_diagnostic(
                    codespan_reporting::Severity::Error,
                    None,
                    &cause.to_string(),
                );
            }
        }
    }
}
