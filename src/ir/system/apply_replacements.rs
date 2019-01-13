use std::collections;

use specs;
use specs_visitor;

use crate::ir::component::element;
use crate::ir::component::layout;
use crate::ir::component::replacement;
use crate::ir::component::symbol;
use crate::ir::component::ty;

pub struct System;

struct ReplacementEntityVisitor {
    replacements: collections::HashMap<specs::Entity, specs::Entity>,
}

impl<'a> specs::System<'a> for System {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, replacement::Replacement>,
        specs::WriteStorage<'a, element::Element>,
        specs::WriteStorage<'a, layout::Layout>,
        specs::WriteStorage<'a, symbol::Symbol>,
        specs::WriteStorage<'a, ty::Type>,
        specs::WriteStorage<'a, ty::error::Error<specs::Entity>>,
    );

    fn run(
        &mut self,
        (
            entities,
            replacements,
            mut elements,
            mut layouts,
            mut symbols,
            mut types,
            mut type_errors,
        ): Self::SystemData,
    ) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;
        use specs_visitor::VisitEntitiesMut;

        (&entities, &replacements)
            .par_join()
            .for_each(|(entity, _)| {
                entities.delete(entity).unwrap();
            });

        let replacements = (&entities, &replacements)
            .par_join()
            .map(|(entity, replacement)| (entity, replacement.to))
            .collect();

        let visitor = ReplacementEntityVisitor { replacements };

        (&mut elements).par_join().for_each(|element| {
            element.accept_mut(&visitor);
        });

        (&mut layouts).par_join().for_each(|layout| {
            layout.accept_mut(&visitor);
        });

        (&mut symbols).par_join().for_each(|symbol| {
            symbol.accept_mut(&visitor);
        });

        (&mut types).par_join().for_each(|ty| {
            ty.accept_mut(&visitor);
        });

        (&mut type_errors).par_join().for_each(|type_error| {
            type_error.accept_mut(&visitor);
        });
    }
}

impl specs_visitor::EntityVisitorMut for ReplacementEntityVisitor {
    fn visit_entity_mut(&self, from: &mut specs::Entity) {
        if let Some(to) = self.replacements.get(from) {
            trace!("applying replacement from {:?} to {:?}", from, to);
            *from = *to;
        }
    }
}
