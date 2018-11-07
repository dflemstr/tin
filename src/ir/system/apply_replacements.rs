use std::collections;

use specs;
use specs_visitor;

use ir::component::element;
use ir::component::replacement;

pub struct ApplyReplacementsSystem;

struct ReplacementEntityVisitor {
    replacements: collections::HashMap<specs::Entity, specs::Entity>,
}

impl<'a> specs::System<'a> for ApplyReplacementsSystem {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, replacement::Replacement>,
        specs::WriteStorage<'a, element::Element>,
    );

    fn run(&mut self, (entities, replacements, mut elements): Self::SystemData) {
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
