use specs;

use std::ops;

use crate::interpreter;
use crate::ir::component::constexpr;
use crate::ir::component::element;

pub struct System;

impl<'a> specs::System<'a> for System {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::WriteStorage<'a, constexpr::Constexpr>,
    );

    fn run(&mut self, (entities, elements, mut constexprs): Self::SystemData) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

        loop {
            let new_constexprs = (&entities, &elements, !&constexprs)
                .par_join()
                .flat_map(|(entity, element, _)| {
                    System::infer_constexpr(element, &constexprs).map(|c| (entity, c))
                })
                .collect::<Vec<_>>();
            debug!("inferred new constexprs: {:?}", new_constexprs);
            if new_constexprs.is_empty() {
                break;
            }

            for (entity, constexpr) in new_constexprs {
                constexprs.insert(entity, constexpr).unwrap();
            }
        }
    }
}

impl System {
    fn infer_constexpr<D>(
        element: &element::Element,
        constexprs: &specs::Storage<constexpr::Constexpr, D>,
    ) -> Option<constexpr::Constexpr>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<constexpr::Constexpr>>,
    {
        interpreter::eval(element, |e| constexprs.get(e).map(|c| &c.value))
            .map(|value| constexpr::Constexpr { value })
    }
}
