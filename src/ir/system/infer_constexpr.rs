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
        specs::WriteStorage<'a, constexpr::error::Error>,
    );

    fn run(&mut self, (entities, elements, mut constexprs, mut errors): Self::SystemData) {
        use crate::best_iter::BestIteratorCollect;
        use crate::best_iter::BestIteratorFlatMap;
        use crate::best_iter::BestJoin;

        loop {
            let new_constexprs: Vec<_> = (&entities, &elements, !&constexprs, !&errors)
                .best_join()
                .best_flat_map(|(entity, element, _, _)| {
                    transpose(System::infer_constexpr(element, &constexprs)).map(|r| (entity, r))
                })
                .best_collect();

            debug!("inferred new constexprs: {:?}", new_constexprs);
            if new_constexprs.is_empty() {
                break;
            }

            for (entity, constexpr) in new_constexprs {
                match constexpr {
                    Ok(constexpr) => {
                        constexprs.insert(entity, constexpr).unwrap();
                    }
                    Err(error) => {
                        errors.insert(entity, error).unwrap();
                    }
                }
            }
        }
    }
}

impl System {
    fn infer_constexpr<D>(
        element: &element::Element,
        constexprs: &specs::Storage<constexpr::Constexpr, D>,
    ) -> Result<Option<constexpr::Constexpr>, constexpr::error::Error>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<constexpr::Constexpr>>,
    {
        Ok(
            interpreter::eval(element, |e| constexprs.get(e).map(|c| &c.value))?
                .map(|value| constexpr::Constexpr { value }),
        )
    }
}

impl From<interpreter::error::Error> for constexpr::error::Error {
    fn from(e: interpreter::error::Error) -> Self {
        constexpr::error::Error::Evaluation(e)
    }
}

// TODO: awaits https://github.com/rust-lang/rust/issues/47338
pub fn transpose<T, E>(result: Result<Option<T>, E>) -> Option<Result<T, E>> {
    match result {
        Ok(Some(x)) => Some(Ok(x)),
        Ok(None) => None,
        Err(e) => Some(Err(e)),
    }
}
