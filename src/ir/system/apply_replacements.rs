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
    );

    fn run(
        &mut self,
        (entities, replacements, mut elements, mut layouts, mut symbols, mut types): Self::SystemData,
    ) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

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
            use specs_visitor::VisitEntities;
            element.accept_mut(&visitor);
        });

        (&mut layouts).par_join().for_each(|layout| {
            use specs_visitor::VisitEntities;
            layout.accept_mut(&visitor);
        });

        (&mut symbols).par_join().for_each(|symbol| {
            use specs_visitor::VisitEntities;
            symbol.accept_mut(&visitor);
        });

        (&mut types).par_join().for_each(|ty| {
            use specs_visitor::VisitEntities;
            ty.accept_mut(&visitor);
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
