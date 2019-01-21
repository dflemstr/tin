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
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::type_complexity))]
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
        use crate::best_iter::BestIterator;
        use crate::best_iter::BestIteratorCollect;
        use crate::best_iter::BestIteratorMap;
        use crate::best_iter::BestJoin;
        use specs_visitor::VisitEntitiesMut;

        (&entities, &replacements)
            .best_join()
            .best_for_each(|(entity, _)| {
                entities.delete(entity).unwrap();
            });

        let replacements = (&entities, &replacements)
            .best_join()
            .best_map(|(entity, replacement)| (entity, replacement.to))
            .best_collect();

        let visitor = ReplacementEntityVisitor { replacements };

        (&mut elements).best_join().best_for_each(|element| {
            element.accept_mut(&visitor);
        });

        (&mut layouts).best_join().best_for_each(|layout| {
            layout.accept_mut(&visitor);
        });

        (&mut symbols).best_join().best_for_each(|symbol| {
            symbol.accept_mut(&visitor);
        });

        (&mut types).best_join().best_for_each(|ty| {
            ty.accept_mut(&visitor);
        });

        (&mut type_errors).best_join().best_for_each(|type_error| {
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
