//! Intermediate representation definitions for the compiler and interpreter.

use std::collections;
use std::fmt;

use specs;

use ast;
use parser;

pub(crate) mod component;
pub mod graph;
mod system;

/// A separate universe of the Norm intermediate representation.
pub struct Ir {
    pub(crate) world: specs::World,
}

impl Ir {
    /// Creates a new IR instance.
    pub fn new() -> Ir {
        let mut world = specs::World::new();
        component::register_all(&mut world);
        Ir { world }
    }

    /// Adds all of the definitions in the specified AST module to the IR world.
    pub fn add_module(
        &mut self,
        ast: &ast::Module<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let mut definitions = collections::HashMap::new();
        for variable in &ast.definitions {
            let entity = self.add_variable(variable, &symbol);
            definitions.insert(variable.name.value.clone(), entity);
        }

        for variable in &ast.definitions {
            Ir::set_variable_scope(
                &self.world.read_storage(),
                &mut self.world.write_storage(),
                definitions[&variable.name.value],
                variable,
                &definitions,
            );
        }

        let element = component::element::Element::Module(component::element::Module {
            definitions: definitions.clone(),
        });

        self.world
            .create_entity()
            .with(element)
            .with(component::symbol::Symbol::new(symbol.to_vec()))
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

    fn add_identifier(
        &mut self,
        identifier: &ast::Identifier<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::Reference(
                component::element::Reference(identifier.value.clone()),
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
            .build()
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

    fn add_expression(
        &mut self,
        expression: &ast::Expression<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        match *expression {
            ast::Expression::Number(ref v) => self.add_number(v, symbol),
            ast::Expression::String(ref v) => self.add_string(v, symbol),
            ast::Expression::Tuple(ref v) => self.add_tuple(v, symbol),
            ast::Expression::Record(ref v) => self.add_record(v, symbol),
            ast::Expression::Identifier(ref v) => self.add_identifier(v, symbol),
            ast::Expression::Lambda(ref v) => self.add_lambda(v, symbol),
            ast::Expression::Select(ref v) => self.add_select(v, symbol),
            ast::Expression::Apply(ref v) => self.add_apply(v, symbol),
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

    fn add_number(
        &mut self,
        number: &ast::NumberLiteral<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::NumberValue(
                Ir::from_ast_number(number.value),
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
            .build()
    }

    fn from_ast_number(number: ast::NumberValue) -> component::element::NumberValue {
        match number {
            ast::NumberValue::U8(n) => component::element::NumberValue::U8(n),
            ast::NumberValue::U16(n) => component::element::NumberValue::U16(n),
            ast::NumberValue::U32(n) => component::element::NumberValue::U32(n),
            ast::NumberValue::U64(n) => component::element::NumberValue::U64(n),
            ast::NumberValue::I8(n) => component::element::NumberValue::I8(n),
            ast::NumberValue::I16(n) => component::element::NumberValue::I16(n),
            ast::NumberValue::I32(n) => component::element::NumberValue::I32(n),
            ast::NumberValue::I64(n) => component::element::NumberValue::I64(n),
            ast::NumberValue::F32(n) => component::element::NumberValue::F32(n),
            ast::NumberValue::F64(n) => component::element::NumberValue::F64(n),
        }
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

    fn add_string(
        &mut self,
        string: &ast::StringLiteral<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        self.world
            .create_entity()
            .with(component::element::Element::StringValue(
                component::element::StringValue(string.value.clone()),
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
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

    fn add_tuple(
        &mut self,
        tuple: &ast::Tuple<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let fields = tuple
            .fields
            .iter()
            .map(|f| self.add_expression(f, symbol))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Tuple(
                component::element::Tuple { fields },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
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
            Some(component::element::Element::Tuple(component::element::Tuple { ref fields })) => {
                for (idx, field) in tuple.fields.iter().enumerate() {
                    Ir::set_expression_scope(elements, scopes, fields[idx], field, definitions);
                }
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_record(
        &mut self,
        record: &ast::Record<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let fields = record
            .fields
            .iter()
            .map(|(i, e)| (i.value.clone(), self.add_expression(e, symbol)))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Record(
                component::element::Record { fields },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
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
            Some(component::element::Element::Record(component::element::Record {
                ref fields,
            })) => {
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

    fn add_lambda(
        &mut self,
        lambda: &ast::Lambda<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        // TODO generate unique symbol for anonymous lambdas

        let captures = Vec::new();
        let parameters = lambda
            .parameters
            .iter()
            .map(|p| self.add_parameter(p, symbol))
            .collect();
        let statements = lambda
            .statements
            .iter()
            .map(|s| self.add_statement(s, symbol))
            .collect::<Vec<_>>();
        let signature = lambda
            .signature
            .as_ref()
            .map(|s| self.add_expression(s, symbol));
        let result = self.add_expression(&*lambda.result, symbol);

        self.world
            .create_entity()
            .with(component::element::Element::Closure(
                component::element::Closure {
                    captures,
                    parameters,
                    statements,
                    signature,
                    result,
                },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
            .build()
    }

    fn set_lambda_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        lambda: &ast::Lambda<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Closure(component::element::Closure {
                captures: _,
                ref parameters,
                ref statements,
                ref signature,
                result,
            })) => {
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

                // TODO: Captures
                //for capture in captures {
                //    definitions.insert(name.clone(), *capture);
                //}

                for (idx, statement) in statements.iter().enumerate() {
                    match lambda.statements[idx] {
                        ast::Statement::Definition(ref variable) => {
                            Ir::set_variable_scope(
                                elements,
                                scopes,
                                *statement,
                                variable,
                                &definitions,
                            );
                            definitions.insert(variable.name.value.clone(), *statement);
                        }
                        ast::Statement::Evaluation(ref expr) => {
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
                Ir::set_expression_scope(elements, scopes, *result, &*lambda.result, &definitions)
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_statement(
        &mut self,
        statement: &ast::Statement<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        match *statement {
            ast::Statement::Definition(ref variable) => self.add_variable(variable, symbol),
            ast::Statement::Evaluation(ref expression) => self.add_expression(expression, symbol),
        }
    }

    fn add_variable(
        &mut self,
        variable: &ast::Variable<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let mut symbol = symbol.to_vec();
        symbol.push(component::symbol::Part::Named(variable.name.value.clone()));

        let name = variable.name.value.clone();
        let initializer = self.add_expression(&variable.initializer, &symbol);

        self.world
            .create_entity()
            .with(component::element::Element::Variable(
                component::element::Variable { name, initializer },
            )).with(component::symbol::Symbol::new(symbol))
            .build()
    }

    fn set_variable_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        variable: &ast::Variable<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Variable(component::element::Variable {
                initializer,
                ..
            })) => {
                Ir::set_expression_scope(
                    elements,
                    scopes,
                    *initializer,
                    &variable.initializer,
                    definitions,
                );
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_select(
        &mut self,
        select: &ast::Select<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let record = self.add_expression(&*select.record, symbol);
        let field = select.field.value.clone();

        self.world
            .create_entity()
            .with(component::element::Element::Select(
                component::element::Select { record, field },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
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
            Some(component::element::Element::Select(component::element::Select {
                record,
                ..
            })) => {
                Ir::set_expression_scope(elements, scopes, *record, &*select.record, definitions);
            }
            _ => unreachable!(),
        }

        Ir::set_scope(scopes, entity, definitions);
    }

    fn add_apply(
        &mut self,
        apply: &ast::Apply<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let function = self.add_expression(&*apply.function, symbol);
        let parameters = apply
            .parameters
            .iter()
            .map(|p| self.add_expression(p, symbol))
            .collect();

        self.world
            .create_entity()
            .with(component::element::Element::Apply(
                component::element::Apply {
                    function,
                    parameters,
                },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
            .build()
    }

    fn set_apply_scope(
        elements: &specs::ReadStorage<component::element::Element>,
        scopes: &mut specs::WriteStorage<component::scope::Scope>,
        entity: specs::Entity,
        apply: &ast::Apply<parser::Context>,
        definitions: &collections::HashMap<String, specs::Entity>,
    ) {
        match elements.get(entity) {
            Some(component::element::Element::Apply(component::element::Apply {
                function,
                parameters,
            })) => {
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

    fn add_parameter(
        &mut self,
        parameter: &ast::Parameter<parser::Context>,
        symbol: &[component::symbol::Part],
    ) -> specs::Entity {
        use specs::world::Builder;

        let name = parameter.name.value.clone();
        let signature = parameter
            .signature
            .as_ref()
            .map(|s| self.add_expression(s, symbol));

        self.world
            .create_entity()
            .with(component::element::Element::Parameter(
                component::element::Parameter { name, signature },
            )).with(component::symbol::Symbol::new(symbol.to_vec()))
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
            Some(component::element::Element::Parameter(component::element::Parameter {
                name: _,
                signature,
            })) => {
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
        scopes
            .insert(entity, component::scope::Scope { definitions })
            .unwrap();
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
        ir.add_module(&ast_module, &[]);
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
