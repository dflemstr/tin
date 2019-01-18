//! Utilities for producing human-readable diagnostics out of errors.
//!
//! # Examples
//!
//! ```
//! # extern crate failure;
//! # extern crate tin;
//! # fn main() -> Result<(), failure::Error> {
//! let source = r#"
//! /* Intentional type error: */
//! main = || -> i32 { 42f32 + 35f32 };
//! "#;
//!
//! let mut tin = tin::Tin::new();
//! tin.load("main.tn", source)?;
//!
//! // We know that this will yield a type error in this example:
//! let error = tin.compile().unwrap_err();
//!
//! // Emit the error as a diagnostic string without colors:
//! let diagnostic_string = tin::diagnostic::to_string(tin.codemap(), &error);
//!
//! assert_eq!(&diagnostic_string, r#"error: type error
//! - <main.tn>:3:8
//! 3 | main = || -> i32 { 42f32 + 35f32 };
//!   |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
//! - <main.tn>:3:20
//! 3 | main = || -> i32 { 42f32 + 35f32 };
//!   |                    ^^^^^^^^^^^^^ expected `i32` but got `f32`
//! - <main.tn>:3:14
//! 3 | main = || -> i32 { 42f32 + 35f32 };
//!   |              --- declared return type is `i32`
//! "#);
//! # Ok(())
//! # }
//! ```
use std::io;
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

/// Writes the specified [`Diagnostics`] to a string.
///
/// The resulting string is not color coded.  This is a convenience method; you can use
/// `Diagnostics::to_diagnostics` for lower-level control over how diagnostics are captured.
pub fn to_string<D>(code_map: &codespan::CodeMap, diagnostics: &D) -> String
where
    D: Diagnostics,
{
    let mut output = Vec::new();
    emit(
        codespan_reporting::termcolor::NoColor::new(&mut output),
        code_map,
        diagnostics,
    )
    .unwrap();
    String::from_utf8(output).unwrap()
}

/// Writes the specified [`Diagnostics`] to a string.
///
/// This is a convenience method; you can use `Diagnostics::to_diagnostics` for lower-level control
/// over how diagnostics are captured.
pub fn emit_to_stderr<D>(
    color_choice: codespan_reporting::termcolor::ColorChoice,
    code_map: &codespan::CodeMap,
    diagnostics: &D,
) -> io::Result<()>
where
    D: Diagnostics,
{
    let stderr = codespan_reporting::termcolor::StandardStream::stderr(color_choice);
    let stderr = stderr.lock();

    emit(stderr, code_map, diagnostics)
}

/// Emits the specified [`Diagnostics`] to an output that potentially supports color.
///
/// Use `termcolor::NoColor::new(...)` to wrap an ordinary `Write` if color is not desired.  This is
/// a convenience method; you can use `Diagnostics::to_diagnostics` for lower-level control over how
/// diagnostics are captured.
pub fn emit<W, D>(mut writer: W, code_map: &codespan::CodeMap, diagnostics: &D) -> io::Result<()>
where
    D: Diagnostics,
    W: codespan_reporting::termcolor::WriteColor,
{
    let mut builder = DiagnosticsBuilder::new();
    diagnostics.to_diagnostics(&mut builder);
    for diagnostic in builder.result {
        codespan_reporting::emit(&mut writer, code_map, &diagnostic)?;
    }
    Ok(())
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
