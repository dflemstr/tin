use crate::diagnostic;
use crate::ir::component::constexpr;
use crate::ir::component::ty;

/// Errors that may occur while building and interacting with an [`Ir`].
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    /// The IR cannot be built because something refers to an identifier that is not defined.
    #[fail(display = "undefined reference to {:?}", reference)]
    UndefinedReference {
        /// The identifier of the undefined reference.
        reference: String,
        /// The location of the reference.
        location: codespan::ByteSpan,
    },

    /// The IR has a type error.
    #[fail(display = "type error")]
    Type(
        codespan::ByteSpan,
        #[cause] ty::error::Error<codespan::ByteSpan>,
    ),

    /// The IR has a constexpr that cannot be evaluated.
    #[fail(display = "constexpr error")]
    Constexpr(codespan::ByteSpan, #[cause] constexpr::error::Error),

    /// There were multiple IR errors.
    #[fail(display = "multiple IR errors")]
    Multiple {
        /// The errors, in the order they were encountered in the source code.
        errors: Vec<Error>,
    },
}

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
                    format!("{}", self),
                );
            }
            Error::Type(entity, ref type_error) => match type_error {
                ty::error::Error {
                    ref main_entity,
                    ref aux_entities,
                    ..
                } => {
                    builder.add_label(codespan_reporting::Label {
                        span: entity,
                        message: None,
                        style: codespan_reporting::LabelStyle::Primary,
                    });

                    builder.add_label(codespan_reporting::Label {
                        span: *main_entity,
                        message: Some(type_error.to_string()),
                        style: codespan_reporting::LabelStyle::Primary,
                    });

                    for aux_entity in aux_entities {
                        builder.add_label(codespan_reporting::Label {
                            span: aux_entity.entity,
                            message: Some(aux_entity.label.clone()),
                            style: codespan_reporting::LabelStyle::Secondary,
                        });
                    }

                    builder.add_diagnostic(
                        codespan_reporting::Severity::Error,
                        None,
                        format!("{}", self),
                    );
                }
            },
            Error::Constexpr(entity, ref constexpr_error) => {
                builder.add_label(codespan_reporting::Label {
                    span: entity,
                    message: Some("while evaluating this constexpr".to_owned()),
                    style: codespan_reporting::LabelStyle::Primary,
                });
                match constexpr_error {
                    constexpr::error::Error::Evaluation(ref interpreter_error) => {
                        interpreter_error.to_diagnostics(builder);
                    }
                }
            }
            Error::Multiple { ref errors } => {
                for error in errors {
                    error.to_diagnostics(builder);
                }
            }
        }
    }
}
