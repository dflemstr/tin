#![allow(warnings, unused)]

use specs;

use ast;
use parser;

pub mod component;
mod graph;
mod jit_compiler;
pub mod system;

#[derive(Clone, Copy, Debug)]
pub struct AstContext {
    entity: specs::Entity,
    span: parser::Span,
    kind: ast::Kind,
}

pub struct Ir {
    world: specs::World,
}

impl Ir {
    pub fn new() -> Ir {
        let mut world = specs::World::new();
        component::register_all(&mut world);
        Ir { world }
    }

    pub fn add_module(&mut self, ast: ast::Module<parser::Context>) {
        let ast = ast.map_context(&mut |c| {
            use specs::Builder;

            let entity = self.world.entities().create();
            let parser::Context { span, kind } = c;

            AstContext { entity, span, kind }
        });

        {
            let mut dispatcher = specs::DispatcherBuilder::new()
                .with(
                    system::assign_element::AssignElementSystem::new(&ast),
                    "assign_element",
                    &[],
                ).with(
                    system::resolve_references::ResolveReferencesSystem::new(&ast),
                    "resolve_references",
                    &["assign_element"],
                ).with(
                    system::apply_replacements::ApplyReplacementsSystem,
                    "apply_replacements",
                    &["resolve_references"],
                ).build();
            dispatcher.dispatch(&mut self.world.res);
        }
        self.world.maintain();
    }

    pub fn check_types(&mut self) {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(system::infer_types::InferTypesSystem, "infer_types", &[])
            .build();
        dispatcher.dispatch(&mut self.world.res);
        self.world.maintain();
    }
}

#[cfg(test)]
mod tests {
    use dot;
    use env_logger;
    use failure;

    use super::*;
    use ast;

    #[test]
    fn entity_assignments() -> Result<(), failure::Error> {
        use ast::AstNode;
        use ir::component::element;
        use ir::component::replacement;
        use parser::Parse;

        let _ = env_logger::try_init();

        let source = r#"
Int = 0;
main = ||: Int {0};
"#;

        let ast_module = ast::Module::parse(source)?;

        let mut ir = Ir::new();
        let ast_module = ir.add_module(ast_module);
        ir.check_types();

        let graph = graph::Graph::new(&ir.world);

        let mut file = ::std::fs::File::create("/tmp/ir.dot")?;
        dot::render(&graph, &mut file)?;
        drop(file);
        ::std::process::Command::new("dot")
            .args(&["-Tpng", "-o/tmp/ir.png", "/tmp/ir.dot"])
            .spawn()?
            .wait()?;
        Ok(())
    }
}
