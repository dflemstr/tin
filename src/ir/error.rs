use crate::diagnostic;
use std::result;

/// Errors that may occur while building and interacting with an [`Ir`].
#[derive(Clone, Debug, Eq, Fail, PartialEq)]
pub enum Error {
    /// The IR cannot be built because something refers to an identifier that is not defined.
    #[fail(display = "undefined reference to `{}`", reference)]
    UndefinedReference {
        /// The identifier of the undefined reference.
        reference: String,
        /// The location of the reference.
        location: codespan::ByteSpan,
    },

    /// There were multiple IR errors.
    #[fail(display = "multiple IR errors")]
    Multiple {
        /// The errors, in the order they were encountered in the source code.
        errors: Vec<Error>,
    },
}

pub type Result<A> = result::Result<A, Error>;

impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        match *self {
            Error::UndefinedReference { location, .. } => {
                builder.add_label(codespan_reporting::Label {
                    span: location,
                    message: None,
                    style: codespan_reporting::LabelStyle::Primary,
                });
                builder.add_diagnostic(
                    codespan_reporting::Severity::Error,
                    None,
                    &self.to_string(),
                );
            }
            Error::Multiple { ref errors } => {
                for error in errors {
                    error.to_diagnostics(builder);
                }
            }
        }
    }
}
