//! Intermediate representation variables for the compiler and interpreter.
use std::fmt;

use specs;

use crate::ast;
use crate::parser;

pub mod builder;
pub mod component;
pub mod error;
mod system;
#[cfg(test)]
mod tests;

/// A separate universe of the Tin intermediate representation.
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
    pub fn load(&mut self, module: &ast::Module<parser::Context>) -> Result<(), error::Error> {
        use specs::world::Builder;

        let entity = self.world.create_entity().build();
        builder::ModuleBuilder::new(&mut self.world).add_module(entity, module)?;

        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(
                system::apply_replacements::System,
                "apply_replacements",
                &[],
            )
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.maintain()?;

        Ok(())
    }

    /// Checks and infers types for all known variables.
    ///
    /// `resolve_references` should be called before this; types will not be inferred for unresolved
    /// references.
    pub fn check_types(&mut self) -> Result<(), error::Error> {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(system::infer_types::System, "infer_types", &[])
            .with(system::infer_constexpr::System, "infer_constexpr", &[])
            .with(system::infer_layouts::System::new(8), "infer_layouts", &[])
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.maintain()?;

        Ok(())
    }

    fn maintain(&mut self) -> Result<(), error::Error> {
        use specs::Join;

        self.world.maintain();

        let mut errors = Vec::new();

        let entities = self.world.entities();
        let locations = self.world.read_storage::<component::location::Location>();
        let type_errors = self
            .world
            .read_storage::<component::ty::error::Error<specs::Entity>>();
        let constexpr_errors = self
            .world
            .read_storage::<component::constexpr::error::Error>();

        for (entity, type_error) in (&entities, &type_errors).join() {
            errors.push(error::Error::Type(
                locations.get(entity).unwrap().0,
                component::ty::error::Error {
                    expected: type_error.expected.clone(),
                    actual: type_error.actual.clone(),
                    main_entity: locations.get(type_error.main_entity).unwrap().0,
                    aux_entities: type_error
                        .aux_entities
                        .iter()
                        .map(|aux_entity| component::ty::error::AuxEntity {
                            entity: locations.get(aux_entity.entity).unwrap().0,
                            label: aux_entity.label.clone(),
                        })
                        .collect(),
                },
            ))
        }

        for (entity, constexpr_error, _) in (&entities, &constexpr_errors, !&type_errors).join() {
            errors.push(error::Error::Constexpr(
                locations.get(entity).unwrap().0,
                constexpr_error.clone(),
            ));
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors
                .pop()
                .unwrap_or_else(|| error::Error::Multiple { errors }))
        }
    }
}

impl fmt::Debug for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Ir").finish()
    }
}
