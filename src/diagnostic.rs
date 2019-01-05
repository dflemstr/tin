pub trait Diagnostic {
    fn into_diagnostics(self, result: &mut Vec<codemap_diagnostic::Diagnostic>);
}

#[cfg(test)]
pub fn format_string(
    code_map: &codemap::CodeMap,
    diagnostics: &[codemap_diagnostic::Diagnostic],
) -> String {
    let mut output = Vec::new();
    {
        let mut emitter = codemap_diagnostic::Emitter::vec(&mut output, Some(code_map));
        emitter.emit(diagnostics);
    }
    String::from_utf8(output).unwrap()
}
