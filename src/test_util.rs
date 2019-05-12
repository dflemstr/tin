use failure;

use crate::db;
use crate::graph;

pub fn render_graph(name: &str, db: &db::Db) -> Result<(), failure::Error> {
    if cfg!(feature = "test-render-graphs") {
        let graph = graph::Graph::new(db);

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
