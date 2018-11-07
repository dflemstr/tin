//! Intermediate representation definitions for the compiler and interpreter.

use std::collections;
use std::fmt;

use specs;

use ast;
use parser;

mod component;
pub mod graph;
pub mod compiler;
mod system;

/// A separate universe of the Norm intermediate representation.
pub struct Ir {
    world: specs::World,
}

impl Ir {
    /// Creates a new IR instance.
    pub fn new() -> Ir {
        let mut world = specs::World::new();
        component::register_all(&mut world);
        Ir { world }
    }

    /// Adds all of the definitions in the specified AST module to the IR world.
    pub fn add_module(&mut self, ast: &ast::Module<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let mut definitions = collections::HashMap::new();
        for (name, expr) in &ast.definitions {
            let entity = self.add_expression(expr);
            definitions.insert(name.value.clone(), entity);
        }

        let element = component::element::Element::Module {
            definitions: definitions.clone(),
        };

        for (name, expr) in &ast.definitions {
            Ir::set_expression_scope(
                &self.world.read_storage(),
                &mut self.world.write_storage(),
                definitions[&name.value],
                expr,
                &definitions,
            );
        }

        self.world
            .create_entity()
            .with(element)
            .with(component::scope::Scope { definitions })
            .build()
    }

    /// Resolves references between variables.
    ///
    /// This should be called once when all modules have been loaded, so that no unresolved
    /// references remain.
    pub fn resolve_references(&mut self) {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(
                system::resolve_references::ResolveReferencesSystem,
                "resolve_references",
                &[],
            ).with(
            system::apply_replacements::ApplyReplacementsSystem,
            "apply_replacements",
            &["resolve_references"],
        ).build();
        dispatcher.dispatch(&mut self.world.res);
        self.world.maintain();
    }

    /// Checks and infers types for all known definitions.
    ///
    /// `resolve_references` should be called before this; types will not be inferred for unresolved
    /// references.
    pub fn check_types(&mut self) {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(system::infer_types::InferTypesSystem, "infer_types", &[])
            .build();
        dispatcher.dispatch(&mut self.world.res);
        self.world.maintain();
    }

    fn add_identifier(&mut self, identifier: &ast::Identifier<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::Reference(
                identifier.value.clone(),
            )).build()
    }

    fn set_identifier_scope(
        _elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        _identifier: &ast::Identifier<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_expression(&mut self, expression: &ast::Expression<parser::Context>) -> specs::Entity {
        match *expression {
            ast::Expression::Number(ref v) => self.add_number(v),
            ast::Expression::String(ref v) => self.add_string(v),
            ast::Expression::Tuple(ref v) => self.add_tuple(v),
            ast::Expression::Record(ref v) => self.add_record(v),
            ast::Expression::Identifier(ref v) => self.add_identifier(v),
            ast::Expression::Lambda(ref v) => self.add_lambda(v),
            ast::Expression::Select(ref v) => self.add_select(v),
            ast::Expression::Apply(ref v) => self.add_apply(v),
        }
    }

    fn set_expression_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        expression: &ast::Expression<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match *expression {
            ast::Expression::Number(ref v) => {
                Ir::set_number_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::String(ref v) => {
                Ir::set_string_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Tuple(ref v) => {
                Ir::set_tuple_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Record(ref v) => {
                Ir::set_record_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Identifier(ref v) => {
                Ir::set_identifier_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Lambda(ref v) => {
                Ir::set_lambda_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Select(ref v) => {
                Ir::set_select_scope(elements, scopes, entity, v, definitions)
            }
            ast::Expression::Apply(ref v) => {
                Ir::set_apply_scope(elements, scopes, entity, v, definitions)
            }
        }
    }

    fn add_number(&mut self, number: &ast::NumberLiteral<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::Number(number.value))
            .build()
    }

    fn set_number_scope(
        _elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        _number: &ast::NumberLiteral<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_string(&mut self, string: &ast::StringLiteral<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::String(string.value.clone()))
            .build()
    }

    fn set_string_scope(
        _elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        _string: &ast::StringLiteral<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_tuple(&mut self, tuple: &ast::Tuple<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let fields = tuple
            .fields
            .iter()
            .map(|f| self.add_expression(f))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Tuple { fields })
            .build()
    }

    fn set_tuple_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        tuple: &ast::Tuple<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Tuple { ref fields }) => {
                for (idx, field) in tuple.fields.iter().enumerate() {
                    Ir::set_expression_scope(elements, scopes, fields[idx], field, definitions);
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_record(&mut self, record: &ast::Record<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let fields = record
            .fields
            .iter()
            .map(|(i, e)| (i.value.clone(), self.add_expression(e)))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Record { fields })
            .build()
    }

    fn set_record_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        record: &ast::Record<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Record { ref fields }) => {
                for (name, field) in &record.fields {
                    Ir::set_expression_scope(
                        elements,
                        scopes,
                        fields[&name.value],
                        field,
                        definitions,
                    );
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_lambda(&mut self, lambda: &ast::Lambda<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let captures = collections::HashMap::new();
        let parameters = lambda
            .parameters
            .iter()
            .map(|p| self.add_parameter(p))
            .collect();
        let statements = lambda
            .statements
            .iter()
            .map(|s| self.add_statement(s))
            .collect();
        let signature = lambda.signature.as_ref().map(|s| self.add_expression(s));

        self.world
            .create_entity()
            .with(component::element::Element::Closure {
                captures,
                parameters,
                statements,
                signature,
            }).build()
    }

    fn set_lambda_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        lambda: &ast::Lambda<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Closure {
                ref captures,
                ref parameters,
                ref statements,
                ref signature,
            }) => {
                for (idx, parameter) in lambda.parameters.iter().enumerate() {
                    Ir::set_parameter_scope(
                        elements,
                        scopes,
                        parameters[idx],
                        parameter,
                        definitions,
                    );
                }

                if let Some(signature) = signature {
                    Ir::set_expression_scope(
                        elements,
                        scopes,
                        *signature,
                        lambda.signature.as_ref().unwrap(),
                        definitions,
                    );
                }

                let mut definitions = definitions.clone();

                for (idx, parameter) in lambda.parameters.iter().enumerate() {
                    definitions.insert(parameter.name.value.clone(), parameters[idx]);
                }

                for (name, capture) in captures {
                    definitions.insert(name.clone(), *capture);
                }

                for (idx, statement) in statements.iter().enumerate() {
                    match lambda.statements[idx] {
                        ast::Statement::Definition(ref ident, ref expr) => {
                            Ir::set_expression_scope(
                                elements,
                                scopes,
                                *statement,
                                expr,
                                &definitions,
                            );
                            definitions.insert(ident.value.clone(), *statement);
                        }
                        ast::Statement::Expression(ref expr) => {
                            Ir::set_expression_scope(
                                elements,
                                scopes,
                                *statement,
                                expr,
                                &definitions,
                            );
                        }
                    }
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_statement(&mut self, statement: &ast::Statement<parser::Context>) -> specs::Entity {
        match *statement {
            ast::Statement::Definition(_, ref expression) => self.add_expression(expression),
            ast::Statement::Expression(ref expression) => self.add_expression(expression),
        }
    }

    fn add_select(&mut self, select: &ast::Select<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let record = self.add_expression(&*select.record);
        let field = select.field.value.clone();

        self.world
            .create_entity()
            .with(component::element::Element::Select { record, field })
            .build()
    }

    fn set_select_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        select: &ast::Select<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Select { record, .. }) => {
                Ir::set_expression_scope(elements, scopes, *record, &*select.record, definitions);
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_apply(&mut self, apply: &ast::Apply<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let function = self.add_expression(&*apply.function);
        let parameters = apply
            .parameters
            .iter()
            .map(|p| self.add_expression(p))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Apply {
                function,
                parameters,
            }).build()
    }

    fn set_apply_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        apply: &ast::Apply<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Apply {
                function,
                parameters,
            }) => {
                Ir::set_expression_scope(
                    elements,
                    scopes,
                    *function,
                    &*apply.function,
                    definitions,
                );

                for (i, parameter) in parameters.iter().enumerate() {
                    Ir::set_expression_scope(
                        elements,
                        scopes,
                        *parameter,
                        &apply.parameters[i],
                        definitions,
                    );
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_parameter(&mut self, parameter: &ast::Parameter<parser::Context>) -> specs::Entity {
        use specs::world::Builder;

        let name = parameter.name.value.clone();
        let signature = parameter.signature.as_ref().map(|s| self.add_expression(s));

        self.world
            .create_entity()
            .with(component::element::Element::Parameter { name, signature })
            .build()
    }

    fn set_parameter_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        parameter: &ast::Parameter<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Parameter { name: _, signature }) => {
                if let Some(signature) = signature {
                    Ir::set_expression_scope(
                        elements,
                        scopes,
                        *signature,
                        parameter.signature.as_ref().unwrap(),
                        definitions,
                    );
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn set_scope(
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        let definitions = definitions.clone();
        scopes.insert(entity, component::scope::Scope { definitions }).unwrap();
    }
}

impl fmt::Debug for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Ir").finish()
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
        use parser::Parse;

        let _ = env_logger::try_init();

        let source = r#"
Int = 0;
pickFirst = |a: Int, b: Int|: Int {
  capture = |x: Int|: Int { a };
  capture(b)
};
main = ||: Int { pickFirst(1, 2) };
"#;

        let ast_module = ast::Module::parse(source)?;

        let mut ir = Ir::new();
        ir.add_module(&ast_module);
        ir.resolve_references();
        ir.check_types();

        let graph = graph::Graph::new(&ir);

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
