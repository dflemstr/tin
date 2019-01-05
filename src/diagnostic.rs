//! Utilities for producing human-readable diagnostics out of errors.

/// A trait for types that have the ability to emit diagnostic information.
pub trait Diagnostic {
    /// Emits diagnostics from an instance of this type.
    fn to_diagnostics(&self, result: &mut Vec<codespan_reporting::Diagnostic>);
}
