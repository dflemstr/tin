use failure;

use crate::graph;
use crate::ir;

pub fn render_graph(name: &str, ir: &ir::Ir) -> Result<(), failure::Error> {
    if cfg!(feature = "test-render-graphs") {
        let graph = graph::Graph::new(ir);

        let dot_path = format!("/tmp/{}.dot", name);
        let img_path = format!("/tmp/{}.svg", name);

        let mut file = ::std::fs::File::create(&dot_path)?;
        dot::render(&graph, &mut file)?;
        ::std::process::Command::new("dot")
            .args(&["-Tsvg", "-o", &img_path, &dot_path])
            .spawn()?
            .wait()?;
    }

    Ok(())
}

#[cfg(test)]
pub fn format_diagnostics(
    code_map: &codespan::CodeMap,
    diagnostics: &[codespan_reporting::Diagnostic],
) -> String {
    let mut output = Vec::new();
    for diagnostic in diagnostics {
        codespan_reporting::emit(
            codespan_reporting::termcolor::NoColor::new(&mut output),
            code_map,
            diagnostic,
        )
        .unwrap();
    }
    String::from_utf8(output).unwrap()
}
