#![allow(warnings, unused)]

use specs;

use ast;
use parser;

pub mod component;
pub mod system;

#[derive(Clone, Copy, Debug)]
pub struct AstContext {
    entity: specs::Entity,
    span: parser::Span,
    kind: ast::Kind,
}

pub struct Ir {
    #[cfg(not(test))]
    world: specs::World,
    #[cfg(test)]
    pub world: specs::World,
}

impl Ir {
    fn new() -> Ir {
        let mut world = specs::World::new();
        component::register_all(&mut world);
        Ir { world }
    }

    fn add_module(&mut self, ast: ast::Module<parser::Context>) -> ast::Module<AstContext> {
        let ast = ast.map_context(&mut |c| {
            use specs::Builder;

            let entity = self.world.create_entity().build();
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

        ast
    }

    fn check_types(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast;

    use failure;

    #[test]
    fn entity_assignments() -> Result<(), failure::Error> {
        use ast::AstNode;
        use ir::component::element;
        use ir::component::replacement;
        use parser::Parse;

        let source = r#"
/* A record describing a person */
Person = { name: String, age: Int };

/* Makes any person old */
makeOld = |person: Person| {
  getName = |person: Person| { person.name };
  /* Some convoluted lambda mess to confuse the symbol generator */
  getOldAge = |result|{|a|{0}();|b|{0}();|c|{|d|{0}}()();result}(90);
  { name: getName(person), age: getOldAge(0) }
};

/* Application main entry point */
main = || {
  /* Print a debug representation of the old person */
  print(makeOld({ name: "David", age: 27 }))
};
"#;

        let ast_module = ast::Module::parse(source)?;

        let mut ir = Ir::new();
        let ast_module = ir.add_module(ast_module);

        struct ContextVisitor<'a>(&'a mut Vec<AstContext>);

        impl<'a> ast::visitor::Visitor<AstContext> for ContextVisitor<'a> {
            fn visit_context(&mut self, context: &AstContext) {
                self.0.push(context.clone());
            }
        }

        let mut contexts = Vec::new();
        ast_module.visit(&mut ContextVisitor(&mut contexts));

        let elements = ir.world.read_storage::<element::Element>();
        let replacements = ir.world.read_storage::<replacement::Replacement>();

        use std::fmt::Write;
        let mut dot = String::new();
        writeln!(dot, "digraph IR {{")?;
        for context in contexts {
            let id = context.entity.id();

            if let Some(element) = elements.get(context.entity) {
                let color = if replacements.get(context.entity).is_some() {
                    "red"
                } else {
                    "black"
                };
                match element {
                    element::Element::Number(v) => writeln!(
                        dot,
                        "{} [label={:?}, color={:?}];",
                        id,
                        &format!("number {:?}", v),
                        color
                    )?,
                    element::Element::String(v) => writeln!(
                        dot,
                        "{} [label={:?}, color={:?}];",
                        id,
                        &format!("string {:?}", v),
                        color
                    )?,
                    element::Element::Tuple { fields } => {
                        writeln!(
                            dot,
                            "{} [label={:?}, color={:?}];",
                            id,
                            &format!("tuple with {:?} fields", fields.len()),
                            color
                        )?;
                        for (idx, field) in fields.iter().enumerate() {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                field.id(),
                                format!("field {}", idx)
                            )?;
                        }
                    }
                    element::Element::Record { fields } => {
                        writeln!(
                            dot,
                            "{} [label={:?}, color={:?}];",
                            id,
                            &format!("record with {:?} fields", fields.len()),
                            color
                        )?;
                        for (name, field) in fields {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                field.id(),
                                format!("field {}", name)
                            )?;
                        }
                    }
                    element::Element::Reference(v) => writeln!(
                        dot,
                        "{} [label={:?}, color={:?}];",
                        id,
                        &format!("reference to {:?}", v),
                        color
                    )?,
                    element::Element::Select { record, field } => {
                        writeln!(
                            dot,
                            "{} [label={:?}, color={:?}];",
                            id, "select field on record", color
                        )?;
                        writeln!(
                            dot,
                            "{} -> {} [label={:?}];",
                            id,
                            record.id(),
                            format!("select field {}", field)
                        )?;
                    }
                    element::Element::Apply {
                        function,
                        parameters,
                    } => {
                        writeln!(
                            dot,
                            "{} [label={:?}, color={:?}];",
                            id,
                            &format!(
                                "function application with {:?} parameters",
                                parameters.len()
                            ),
                            color
                        )?;
                        writeln!(dot, "{} -> {} [label=\"function\"];", id, function.id())?;
                        for (idx, parameter) in parameters.iter().enumerate() {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                parameter.id(),
                                format!("parameter {}", idx)
                            )?;
                        }
                    }
                    element::Element::Parameter { name, signature } => {
                        writeln!(
                            dot,
                            "{} [label={:?}, color={:?}];",
                            id,
                            &format!("parameter {:?}", name),
                            color
                        )?;
                        if let Some(signature) = signature {
                            writeln!(dot, "{} -> {} [label=\"signature\"];", id, signature.id())?;
                        }
                    }
                    element::Element::Closure {
                        captures,
                        parameters,
                        statements,
                        signature,
                    } => {
                        writeln!(dot, "{} [label={:?}, color={:?}];", id, "closure", color)?;
                        for (name, capture) in captures {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                capture.id(),
                                format!("capture {}", name)
                            )?;
                        }
                        for (idx, parameter) in parameters.iter().enumerate() {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                parameter.id(),
                                format!("parameter {}", idx)
                            )?;
                        }
                        for (idx, statement) in statements.iter().enumerate() {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                statement.id(),
                                format!("statement {}", idx)
                            )?;
                        }
                        if let Some(signature) = signature {
                            writeln!(dot, "{} -> {} [label=\"signature\"];", id, signature.id())?;
                        }
                    }
                    element::Element::Module { definitions } => {
                        writeln!(dot, "{} [label={:?}, color={:?}];", id, "module", color)?;
                        for (name, definition) in definitions {
                            writeln!(
                                dot,
                                "{} -> {} [label={:?}];",
                                id,
                                definition.id(),
                                format!("definition {}", name)
                            )?;
                        }
                    }
                }
            }
        }
        writeln!(dot, "}}")?;

        {
            use std::io::Write;
            ::std::fs::File::create("/tmp/ir.dot")?.write_all(dot.as_bytes())?;
            ::std::process::Command::new("dot")
                .args(&["-Tpng", "-o/tmp/ir.png", "/tmp/ir.dot"])
                .spawn()?
                .wait()?;
        }
        Ok(())
    }
}
