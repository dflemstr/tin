//! Utilities for producing human-readable diagnostics out of errors.
use std::mem;

/// A trait for types that have the ability to emit diagnostic information.
pub trait Diagnostics {
    /// Emits diagnostics from an instance of this type.
    fn to_diagnostics(&self, builder: &mut DiagnosticsBuilder);
}

/// A builder for creating structured diagnostics.
#[derive(Clone, Debug, Default)]
pub struct DiagnosticsBuilder {
    message: Option<String>,
    labels: Vec<codespan_reporting::Label>,
    result: Vec<codespan_reporting::Diagnostic>,
}

impl DiagnosticsBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        let message = None;
        let labels = Vec::new();
        let result = Vec::new();

        DiagnosticsBuilder {
            message,
            labels,
            result,
        }
    }

    /// Add a label to be added to the following diagnostic.
    pub fn add_label(&mut self, label: codespan_reporting::Label) {
        self.labels.push(label);
    }

    /// Add a message to be added to the following diagnostic.
    pub fn add_message(&mut self, message: String) {
        // Needed because of ownership transfer
        #[allow(clippy::option_map_unwrap_or)]
        let message = Some(
            self.message
                .as_ref()
                .map(|old_msg| format!("{}: {}", old_msg, message))
                .unwrap_or(message),
        );
        self.message = message;
    }

    /// Add a new diagnostic, using the labels and messages accumulated so far.
    pub fn add_diagnostic(
        &mut self,
        severity: codespan_reporting::Severity,
        code: Option<String>,
        message: &str,
    ) {
        let code = code.map(Into::into);
        let message = self.message.as_ref().map_or_else(
            || message.to_owned(),
            |old_msg| format!("{}: {}", old_msg, message),
        );
        let labels = mem::replace(&mut self.labels, Vec::new());
        let diagnostic = codespan_reporting::Diagnostic {
            severity,
            code,
            message,
            labels,
        };
        self.result.push(diagnostic)
    }

    /// Builds a sequence of diagnostics out of this builder.
    pub fn build(self) -> Vec<codespan_reporting::Diagnostic> {
        self.result
    }
}
