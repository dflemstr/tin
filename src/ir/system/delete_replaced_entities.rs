use specs;

use ir::component::replacement;

pub struct DeleteReplacedEntitiesSystem;

impl<'a> specs::System<'a> for DeleteReplacedEntitiesSystem {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, replacement::Replacement>,
    );

    fn run(&mut self, (entities, replacements): Self::SystemData) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

        (&entities, &replacements).par_join().map(|(entity, _)| {
            entities.delete(entity).unwrap();
        });
    }
}
