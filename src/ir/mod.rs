//! Intermediate representation variables for the compiler and interpreter.

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

    /// Adds the specified AST module to the IR world.
    pub fn module(&mut self, module: &ast::Module<parser::Context>) {
        use specs::world::Builder;

        let entity = self.world.create_entity().build();
        self.add_module(entity, module, &[], &collections::HashMap::new());

        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(
                system::apply_replacements::ApplyReplacementsSystem,
                "apply_replacements",
                &[],
            )
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.world.maintain();
    }

    fn add_module(
        &mut self,
        entity: specs::Entity,
        ast: &ast::Module<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let mut variables = variables.clone();

        for variable in &ast.variables {
            variables.insert(
                variable.name.value.clone(),
                self.world.create_entity().build(),
            );
        }

        for variable in &ast.variables {
            self.add_variable(
                variables[&variable.name.value],
                variable,
                symbol,
                &variables,
            );
        }

        self.world.write_storage().insert(
            entity,
            component::element::Element::Module(component::element::Module {
                variables: variables.clone(),
            }),
        ).unwrap();

        self.world
            .write_storage()
            .insert(entity, component::symbol::Symbol::new(symbol.to_vec())).unwrap();
    }

    /// Checks and infers types for all known variables.
    ///
    /// `resolve_references` should be called before this; types will not be inferred for unresolved
    /// references.
    pub fn check_types(&mut self) {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(system::infer_types::InferTypesSystem, "infer_types", &[])
            .with(
                system::infer_constexpr::InferConstexprSystem,
                "infer_constexpr",
                &[],
            )
            .with(
                system::infer_layouts::InferLayoutsSystem::new(8),
                "infer_layouts",
                &[],
            )
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.world.maintain();
    }

    fn add_identifier(
        &mut self,
        entity: specs::Entity,
        identifier: &ast::Identifier<parser::Context>,
        _symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        // TODO: handle undefined variable
        self.world.write_storage().insert(
            entity,
            component::replacement::Replacement {
                to: variables[&identifier.value],
            },
        ).unwrap();
    }

    fn add_expression(
        &mut self,
        entity: specs::Entity,
        expression: &ast::Expression<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        match *expression {
            ast::Expression::Number(ref v) => self.add_number(entity, v, symbol, variables),
            ast::Expression::String(ref v) => self.add_string(entity, v, symbol, variables),
            ast::Expression::Tuple(ref v) => self.add_tuple(entity, v, symbol, variables),
            ast::Expression::Record(ref v) => self.add_record(entity, v, symbol, variables),
            ast::Expression::Identifier(ref v) => self.add_identifier(entity, v, symbol, variables),
            ast::Expression::Lambda(ref v) => self.add_lambda(entity, v, symbol, variables),
            ast::Expression::Select(ref v) => self.add_select(entity, v, symbol, variables),
            ast::Expression::Apply(ref v) => self.add_apply(entity, v, symbol, variables),
        }
    }

    fn add_number(
        &mut self,
        entity: specs::Entity,
        number: &ast::NumberLiteral<parser::Context>,
        _symbol: &[component::symbol::Part],
        _variables: &collections::HashMap<String, specs::Entity>,
    ) {
        self.world.write_storage().insert(
            entity,
            component::element::Element::NumberValue(Ir::from_ast_number(number.value)),
        ).unwrap();
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

    fn add_string(
        &mut self,
        entity: specs::Entity,
        string: &ast::StringLiteral<parser::Context>,
        _symbol: &[component::symbol::Part],
        _variables: &collections::HashMap<String, specs::Entity>,
    ) {
        self.world.write_storage().insert(
            entity,
            component::element::Element::StringValue(component::element::StringValue(
                string.value.clone(),
            )),
        ).unwrap();
    }

    fn add_tuple(
        &mut self,
        entity: specs::Entity,
        tuple: &ast::Tuple<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let fields = tuple
            .fields
            .iter()
            .map(|f| {
                let e = self.world.create_entity().build();
                self.add_expression(e, f, symbol, variables);
                e
            })
            .collect();

        self.world.write_storage().insert(
            entity,
            component::element::Element::Tuple(component::element::Tuple { fields }),
        ).unwrap();
    }

    fn add_record(
        &mut self,
        entity: specs::Entity,
        record: &ast::Record<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let fields = record
            .fields
            .iter()
            .map(|(i, e)| {
                let en = self.world.create_entity().build();
                self.add_expression(en, e, symbol, variables);
                (i.value.clone(), en)
            })
            .collect();

        self.world.write_storage().insert(
            entity,
            component::element::Element::Record(component::element::Record { fields }),
        ).unwrap();
    }

    fn add_lambda(
        &mut self,
        entity: specs::Entity,
        lambda: &ast::Lambda<parser::Context>,
        symbol: &[component::symbol::Part],
        outer_variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        // TODO generate unique symbol for anonymous lambdas

        let captures = Vec::new();
        let mut variables = outer_variables.clone();
        let parameters = lambda
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity().build();
                variables.insert(p.name.value.clone(), e);
                self.add_parameter(e, p, symbol, outer_variables);
                e
            })
            .collect();

        let statements = lambda
            .statements
            .iter()
            .map(|s| {
                let e = self.world.create_entity().build();

                match s {
                    ast::Statement::Variable(ref variable) => {
                        variables.insert(variable.name.value.clone(), e);
                        self.add_variable(e, variable, symbol, &variables);
                    }
                    ast::Statement::Expression(ref expression) => {
                        self.add_expression(e, expression, symbol, &variables);
                    }
                }

                e
            })
            .collect::<Vec<_>>();

        let signature = lambda.signature.as_ref().map(|s| {
            let e = self.world.create_entity().build();
            self.add_expression(e, s, symbol, &variables);
            e
        });

        let result = self.world.create_entity().build();
        self.add_expression(result, &*lambda.result, symbol, &variables);

        self.world.write_storage().insert(
            entity,
            component::element::Element::Closure(component::element::Closure {
                captures,
                parameters,
                statements,
                signature,
                result,
            }),
        ).unwrap();

        self.world
            .write_storage()
            .insert(entity, component::symbol::Symbol::new(symbol.to_vec())).unwrap();
    }

    fn add_variable(
        &mut self,
        entity: specs::Entity,
        variable: &ast::Variable<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let mut symbol = symbol.to_vec();
        symbol.push(component::symbol::Part::Named(variable.name.value.clone()));

        let name = variable.name.value.clone();
        let initializer = self.world.create_entity().build();

        self.add_expression(initializer, &variable.initializer, &symbol, variables);

        self.world.write_storage().insert(
            entity,
            component::element::Element::Variable(component::element::Variable {
                name,
                initializer,
            }),
        ).unwrap();

        self.world
            .write_storage()
            .insert(entity, component::symbol::Symbol::new(symbol)).unwrap();
    }

    fn add_select(
        &mut self,
        entity: specs::Entity,
        select: &ast::Select<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let record = self.world.create_entity().build();
        self.add_expression(record, &*select.record, symbol, variables);

        let field = select.field.value.clone();

        self.world.write_storage().insert(
            entity,
            component::element::Element::Select(component::element::Select { record, field }),
        ).unwrap();
    }

    fn add_apply(
        &mut self,
        entity: specs::Entity,
        apply: &ast::Apply<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let function = self.world.create_entity().build();
        self.add_expression(function, &*apply.function, symbol, variables);

        let parameters = apply
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity().build();
                self.add_expression(e, p, symbol, variables);
                e
            })
            .collect();

        self.world.write_storage().insert(
            entity,
            component::element::Element::Apply(component::element::Apply {
                function,
                parameters,
            }),
        ).unwrap();
    }

    fn add_parameter(
        &mut self,
        entity: specs::Entity,
        parameter: &ast::Parameter<parser::Context>,
        symbol: &[component::symbol::Part],
        variables: &collections::HashMap<String, specs::Entity>,
    ) {
        use specs::world::Builder;

        let name = parameter.name.value.clone();
        let signature = parameter.signature.as_ref().map(|s| {
            let e = self.world.create_entity().build();
            self.add_expression(e, s, symbol, variables);
            e
        });

        self.world.write_storage().insert(
            entity,
            component::element::Element::Parameter(component::element::Parameter {
                name,
                signature,
            }),
        ).unwrap();
    }
}

impl fmt::Debug for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Ir").finish()
    }
}

#[cfg(test)]
mod tests {
    use env_logger;
    use failure;

    use super::*;
    use ast;
    use test_util;

    #[test]
    fn entity_assignments() -> Result<(), failure::Error> {
        use parser::Parse;

        let _ = env_logger::try_init();

        let source = r#"
Int = 0u32;
pickFirst = |a: Int, b: Int| Int {
  capture = |x: Int| Int { a };
  capture(b)
};
main = || Int { pickFirst(1u32, 2u32) };
"#;

        let ast_module = ast::Module::parse(source)?;

        let mut ir = Ir::new();
        ir.module(&ast_module);
        ir.check_types();

        test_util::render_graph(concat!(module_path!(), "::entity_assignments"), &ir)?;

        Ok(())
    }
}
