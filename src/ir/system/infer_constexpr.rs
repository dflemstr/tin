use specs;

use ir::component::constexpr;
use ir::component::element;
use std::ops;

pub struct InferConstexprSystem;

impl<'a> specs::System<'a> for InferConstexprSystem {
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
        element::Element::Tuple(element::Tuple { ref fields }) => {
            fields.iter().all(|f| constexprs.contains(*f))
        }
        element::Element::Record(element::Record { ref fields }) => {
            fields.values().all(|v| constexprs.contains(*v))
        }
        element::Element::Reference(_) => false,
        element::Element::Variable(element::Variable {
            name: _,
            initializer,
        }) => constexprs.contains(initializer),
        element::Element::Select(element::Select { record, field: _ }) => {
            constexprs.contains(record)
        }
        element::Element::Apply(element::Apply {
            function,
            ref parameters,
        }) => constexprs.contains(function) && parameters.iter().all(|p| constexprs.contains(*p)),
        element::Element::Parameter(element::Parameter { .. }) => false,
        element::Element::Capture(element::Capture { name: _, captured }) => {
            constexprs.contains(captured)
        }
        element::Element::Closure(element::Closure {
            ref captures,
            parameters: _,
            ref statements,
            signature: _,
            result,
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
