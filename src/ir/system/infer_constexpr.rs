use specs;

use crate::ir::component::constexpr;
use crate::ir::component::element;
use std::ops;

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
                .filter(|(_, element, _)| infer_constexpr(element, &constexprs))
                .map(|(entity, _, _)| entity)
                .collect::<Vec<_>>();
            debug!("inferred new constexprs: {:?}", new_constexprs);
            if new_constexprs.is_empty() {
                break;
            }

            for entity in new_constexprs {
                constexprs.insert(entity, constexpr::Constexpr).unwrap();
            }
        }
    }
}

fn infer_constexpr<D>(
    element: &element::Element,
    constexprs: &specs::Storage<constexpr::Constexpr, D>,
) -> bool
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<constexpr::Constexpr>>,
{
    match *element {
        element::Element::NumberValue(_) => true,
        element::Element::StringValue(_) => true,
        element::Element::Symbol(_) => true,
        element::Element::Tuple(element::Tuple { ref fields }) => {
            fields.iter().all(|f| constexprs.contains(*f))
        }
        element::Element::Record(element::Record { ref fields }) => {
            fields.values().all(|v| constexprs.contains(*v))
        }
        element::Element::UnOp(element::UnOp { operand, .. }) => constexprs.contains(operand),
        element::Element::BiOp(element::BiOp { lhs, rhs, .. }) => {
            constexprs.contains(lhs) && constexprs.contains(rhs)
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            constexprs.contains(initializer)
        }
        element::Element::Select(element::Select { record, .. }) => constexprs.contains(record),
        element::Element::Apply(element::Apply {
            function,
            ref parameters,
        }) => constexprs.contains(function) && parameters.iter().all(|p| constexprs.contains(*p)),
        element::Element::Parameter(element::Parameter { .. }) => false,
        element::Element::Capture(element::Capture { captured, .. }) => {
            constexprs.contains(captured)
        }
        element::Element::Closure(element::Closure {
            ref captures,
            ref statements,
            result,
            ..
        }) => {
            captures.iter().all(|c| constexprs.contains(*c))
                && statements.iter().all(|s| constexprs.contains(*s))
                && constexprs.contains(result)
        }
        element::Element::Module(element::Module { ref variables }) => {
            variables.values().all(|v| constexprs.contains(*v))
        }
    }
}
