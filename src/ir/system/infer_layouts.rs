#![allow(unused)]
use std::collections;
use std::usize;

use specs;

use ir::component::element;
use ir::component::layout;
use std::ops;

pub struct InferLayoutsSystem {
    ptr_size: usize,
}

impl<'a> specs::System<'a> for InferLayoutsSystem {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::WriteStorage<'a, layout::Layout>,
    );

    fn run(&mut self, (entities, elements, mut layouts): Self::SystemData) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

        loop {
            let new_layouts = (&entities, &elements, !&layouts)
                .par_join()
                .flat_map(|(entity, element, _)| {
                    self.infer_layout(element, &elements, &layouts)
                        .map(|layout| (entity, layout))
                }).collect::<Vec<_>>();
            debug!("inferred new layouts: {:?}", new_layouts);
            if new_layouts.is_empty() {
                break;
            }

            for (entity, layout) in new_layouts {
                layouts.insert(entity, layout).unwrap();
            }
        }
    }
}

impl InferLayoutsSystem {
    pub fn new(ptr_size: usize) -> InferLayoutsSystem {
        InferLayoutsSystem { ptr_size }
    }

    fn infer_layout<DE, DL>(
        &self,
        element: &element::Element,
        elements: &specs::Storage<element::Element, DE>,
        layouts: &specs::Storage<layout::Layout, DL>,
    ) -> Option<layout::Layout>
    where
        DE: ops::Deref<Target = specs::storage::MaskedStorage<element::Element>>,
        DL: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        match *element {
            element::Element::NumberValue(ref n) => self.infer_number_layout(n),
            element::Element::StringValue(_) => Some(layout::Layout::scalar(self.ptr_size)),
            element::Element::Tuple(element::Tuple { ref fields }) => {
                self.infer_tuple_layout(fields, layouts)
            }
            element::Element::Record(element::Record { ref fields }) => {
                self.infer_record_layout(fields, layouts)
            }
            element::Element::Reference(_) => None,
            element::Element::Variable(element::Variable {
                name: _,
                initializer,
            }) => self.infer_variable_layout(initializer, layouts),
            element::Element::Select(element::Select { record, ref field }) => {
                self.infer_select_layout(record, field, elements, layouts)
            }
            element::Element::Apply(element::Apply {
                function,
                ref parameters,
            }) => self.infer_apply_layout(function, parameters, layouts),
            element::Element::Parameter(element::Parameter { name: _, signature }) => {
                self.infer_parameter_layout(signature, layouts)
            }
            element::Element::Capture(element::Capture { name: _, captured }) => {
                self.infer_capture_layout(captured, layouts)
            }
            element::Element::Closure(element::Closure {
                captures: _,
                ref parameters,
                statements: _,
                signature,
                result,
            }) => self.infer_closure_layout(parameters, signature, result, layouts),
            element::Element::Module(element::Module { ref variables }) => {
                self.infer_module_layout(variables, layouts)
            }
        }
    }

    fn infer_number_layout(&self, number: &element::NumberValue) -> Option<layout::Layout> {
        match *number {
            element::NumberValue::U8(_) => Some(layout::Layout::scalar(1)),
            element::NumberValue::U16(_) => Some(layout::Layout::scalar(2)),
            element::NumberValue::U32(_) => Some(layout::Layout::scalar(4)),
            element::NumberValue::U64(_) => Some(layout::Layout::scalar(8)),
            element::NumberValue::I8(_) => Some(layout::Layout::scalar(1)),
            element::NumberValue::I16(_) => Some(layout::Layout::scalar(2)),
            element::NumberValue::I32(_) => Some(layout::Layout::scalar(4)),
            element::NumberValue::I64(_) => Some(layout::Layout::scalar(8)),
            element::NumberValue::F32(_) => Some(layout::Layout::scalar(4)),
            element::NumberValue::F64(_) => Some(layout::Layout::scalar(8)),
        }
    }

    fn infer_tuple_layout<D>(
        &self,
        fields: &[specs::Entity],
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        if let Some(mut layouts) = fields
            .iter()
            .enumerate()
            .map(|(i, f)| layouts.get(*f).map(|l| (i, l)))
            .collect::<Option<Vec<_>>>()
        {
            if layouts.is_empty() {
                Some(layout::Layout::zero())
            } else {
                layouts.sort_unstable_by_key(|(i, l)| (usize::MAX - l.size, *i));
                let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
                let mut size = 0;

                let mut unnamed_field_offsets = vec![0; layouts.len()];

                for (i, layout) in layouts {
                    let offset = align_up(size, layout.alignment);
                    size = offset + layout.size;
                    unnamed_field_offsets[i] = offset;
                }

                Some(layout::Layout::unnamed_fields(
                    size,
                    alignment,
                    unnamed_field_offsets,
                ))
            }
        } else {
            None
        }
    }

    fn infer_record_layout<D>(
        &self,
        fields: &collections::HashMap<String, specs::Entity>,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        if let Some(mut layouts) = fields
            .iter()
            .map(|(n, f)| layouts.get(*f).map(|l| (n, l)))
            .collect::<Option<Vec<_>>>()
        {
            if layouts.is_empty() {
                Some(layout::Layout::zero())
            } else {
                layouts.sort_unstable_by_key(|(n, l)| (usize::MAX - l.size, n.as_str()));
                let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
                let mut size = 0;

                let named_field_offsets = layouts
                    .into_iter()
                    .map(|(n, l)| {
                        let offset = align_up(size, l.alignment);
                        size = offset + l.size;
                        (n.clone(), offset)
                    }).collect::<collections::HashMap<_, _>>();

                Some(layout::Layout::named_fields(
                    size,
                    alignment,
                    named_field_offsets,
                ))
            }
        } else {
            None
        }
    }

    fn infer_variable_layout<D>(
        &self,
        initializer: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        layouts.get(initializer).cloned()
    }

    fn infer_select_layout<DE, DL>(
        &self,
        record: specs::Entity,
        field: &str,
        elements: &specs::Storage<element::Element, DE>,
        layouts: &specs::Storage<layout::Layout, DL>,
    ) -> Option<layout::Layout>
    where
        DE: ops::Deref<Target = specs::storage::MaskedStorage<element::Element>>,
        DL: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        match elements.get(record) {
            None => None,
            Some(t) => match t {
                element::Element::Record(element::Record { ref fields }) => {
                    if let Some(f) = fields.get(field) {
                        layouts.get(*f).cloned()
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }

    fn infer_apply_layout<D>(
        &self,
        function: specs::Entity,
        parameters: &[specs::Entity],
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        None
        /*
    match types.get(function) {
        None => {
            trace!("inference failure: missing function type for apply");
            None
        }
        Some(f) => match f {
            ty::Type::Function(ty::Function {
                parameters: ref formal_parameters,
                result,
            }) => {
                if let Some(parameters) = parameters
                    .iter()
                    .map(|p| types.get(*p).cloned())
                    .collect::<Option<Vec<_>>>()
                {
                    if parameters == *formal_parameters {
                        Some((**result).clone())
                    } else {
                        Some(ty::Type::Conflict(ty::Conflict {
                            expected: Box::new(ty::Type::Function(ty::Function {
                                parameters,
                                result: Box::new(ty::Type::Any),
                            })),
                            actual: Box::new(f.clone()),
                        }))
                    }
                } else {
                    None
                }
            }
            something => Some(ty::Type::Conflict(ty::Conflict {
                expected: Box::new(ty::Type::Function(ty::Function {
                    parameters: vec![ty::Type::Any],
                    result: Box::new(ty::Type::Any),
                })),
                actual: Box::new(something.clone()),
            })),
        },
    }
    */
    }

    fn infer_parameter_layout<D>(
        &self,
        signature: Option<specs::Entity>,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        None
        /*
    if let Some(signature) = signature {
        types.get(signature).cloned()
    } else {
        trace!("inference failure: no signature for parameter");
        // TODO: implement surjective type inference
        None
    }
    */
    }

    fn infer_capture_layout<D>(
        &self,
        capture: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        layouts.get(capture).cloned()
    }

    fn infer_closure_layout<D>(
        &self,
        parameters: &[specs::Entity],
        signature: Option<specs::Entity>,
        result: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        None
        /*
    if let Some(parameters) = parameters
        .iter()
        .map(|p| types.get(*p).cloned())
        .collect::<Option<Vec<_>>>()
    {
    if let Some(signature) = signature {
        if let Some(result) = types.get(signature).cloned() {
            let result = Box::new(result);
            Some(ty::Type::Function(ty::Function { parameters, result }))
        } else {
            trace!("inference failure: no signature for closure");
            None
        }
    } else if let Some(result) = types.get(result).cloned() {
        let result = Box::new(result);
        Some(ty::Type::Function(ty::Function { parameters, result }))
    } else {
        trace!("inference failure: missing signature type for closure, and no inferrable result type");
        None
    }
} else {
    trace!("inference failure: missing parameter type(s) for closure");
    // TODO: implement surjective type inference
    None
}
*/
    }

    fn infer_module_layout<D>(
        &self,
        variables: &collections::HashMap<String, specs::Entity>,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        None
        /*
variables
    .iter()
    .map(|(k, v)| types.get(*v).map(|t| (k.clone(), t.clone())))
    .collect::<Option<collections::HashMap<_, _>>>()
    .map(|fields| ty::Type::Record(ty::Record { fields }))
    */
    }
}

fn align_up(offset: usize, alignment: usize) -> usize {
    debug_assert!(alignment.is_power_of_two());
    offset + ((-(offset as isize)) & (alignment as isize - 1)) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn align_up_4() {
        assert_eq!(0, align_up(0, 4));
        assert_eq!(4, align_up(1, 4));
        assert_eq!(4, align_up(2, 4));
        assert_eq!(4, align_up(3, 4));
        assert_eq!(4, align_up(4, 4));
        assert_eq!(8, align_up(5, 4));
        assert_eq!(8, align_up(6, 4));
        assert_eq!(8, align_up(7, 4));
        assert_eq!(8, align_up(8, 4));
        assert_eq!(12, align_up(9, 4));
    }

    #[test]
    fn align_up_8() {
        assert_eq!(0, align_up(0, 8));
        assert_eq!(8, align_up(1, 8));
        assert_eq!(8, align_up(2, 8));
        assert_eq!(8, align_up(3, 8));
        assert_eq!(8, align_up(4, 8));
        assert_eq!(8, align_up(5, 8));
        assert_eq!(8, align_up(6, 8));
        assert_eq!(8, align_up(7, 8));
        assert_eq!(8, align_up(8, 8));
        assert_eq!(16, align_up(9, 8));
    }
}
