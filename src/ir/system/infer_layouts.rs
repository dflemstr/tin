use std::collections;
use std::usize;

use specs;

use crate::ir::component::element;
use crate::ir::component::layout;
use std::ops;

pub struct System {
    ptr_size: usize,
}

const BOOL_LAYOUT: layout::Layout = layout::Layout::scalar(1);

impl<'a> specs::System<'a> for System {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::WriteStorage<'a, layout::Layout>,
    );

    fn run(&mut self, (entities, elements, mut layouts): Self::SystemData) {
        use crate::best_iter::BestIteratorCollect;
        use crate::best_iter::BestIteratorFlatMap;
        use crate::best_iter::BestJoin;

        loop {
            let new_layouts: Vec<_> = (&entities, &elements, !&layouts)
                .best_join()
                .best_flat_map(|(entity, element, _)| {
                    self.infer_layout(element, &elements, &layouts)
                        .map(|layout| (entity, layout))
                })
                .best_collect();
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

impl System {
    pub fn new(ptr_size: usize) -> System {
        System { ptr_size }
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
            element::Element::Number(ref n) => self.infer_number_layout(n),
            element::Element::String(_) => Some(layout::Layout::scalar(self.ptr_size)),
            element::Element::Symbol(_) => Some(layout::Layout::zero()),
            element::Element::Tuple(element::Tuple { ref fields }) => {
                self.infer_tuple_layout(fields, layouts)
            }
            element::Element::Record(element::Record { ref fields }) => {
                self.infer_record_layout(fields, layouts)
            }
            element::Element::UnOp(element::UnOp { operator, operand }) => {
                self.infer_un_op_layout(operator, operand, layouts)
            }
            element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
                self.infer_bi_op_layout(lhs, operator, rhs, layouts)
            }
            element::Element::Variable(element::Variable { initializer, .. }) => {
                self.infer_variable_layout(initializer, layouts)
            }
            element::Element::Select(element::Select { record, ref field }) => {
                self.infer_select_layout(record, field, elements, layouts)
            }
            element::Element::Apply(element::Apply {
                function,
                ref parameters,
            }) => self.infer_apply_layout(function, parameters, layouts),
            element::Element::Parameter(element::Parameter { signature, .. }) => {
                self.infer_parameter_layout(signature, layouts)
            }
            element::Element::Capture(element::Capture { captured, .. }) => {
                self.infer_capture_layout(captured, layouts)
            }
            element::Element::Closure(element::Closure { ref captures, .. }) => {
                self.infer_closure_layout(captures, layouts)
            }
            element::Element::Module(element::Module { ref variables }) => {
                self.infer_module_layout(variables, layouts)
            }
        }
    }

    fn infer_number_layout(&self, number: &element::Number) -> Option<layout::Layout> {
        match *number {
            element::Number::U8(_) | element::Number::I8(_) => Some(layout::Layout::scalar(1)),
            element::Number::U16(_) | element::Number::I16(_) => Some(layout::Layout::scalar(2)),
            element::Number::U32(_) | element::Number::I32(_) | element::Number::F32(_) => {
                Some(layout::Layout::scalar(4))
            }
            element::Number::U64(_) | element::Number::I64(_) | element::Number::F64(_) => {
                Some(layout::Layout::scalar(8))
            }
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
                layouts.sort_unstable_by_key(|(i, l)| (usize::max_value() - l.size, *i));
                let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
                let mut size = 0;

                let mut unnamed_fields = vec![layout::Offset::zero(); layouts.len()];

                for (i, layout) in layouts {
                    let offset = align_up(size, layout.alignment);
                    size = offset + layout.size;
                    let layout = layout.clone();
                    unnamed_fields[i] = layout::Offset { offset, layout };
                }

                Some(layout::Layout::unnamed_fields(
                    size,
                    alignment,
                    unnamed_fields,
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
                layouts.sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, n.as_str()));
                let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
                let mut size = 0;

                let named_fields = layouts
                    .into_iter()
                    .map(|(field, layout)| {
                        let offset = align_up(size, layout.alignment);
                        size = offset + layout.size;
                        let field = field.clone();
                        let layout = layout.clone();
                        let offset_layout = layout::Offset { offset, layout };

                        layout::NamedField {
                            field,
                            offset_layout,
                        }
                    })
                    .collect::<Vec<_>>();

                Some(layout::Layout::named_fields(size, alignment, named_fields))
            }
        } else {
            None
        }
    }

    fn infer_un_op_layout<D>(
        &self,
        operator: element::UnOperator,
        operand: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        let operand = layouts.get(operand).cloned();

        match operator {
            element::UnOperator::Not => Some(BOOL_LAYOUT),
            element::UnOperator::BNot
            | element::UnOperator::Cl0
            | element::UnOperator::Cl1
            | element::UnOperator::Cls
            | element::UnOperator::Ct0
            | element::UnOperator::Ct1
            | element::UnOperator::C0
            | element::UnOperator::C1
            | element::UnOperator::Sqrt => operand,
        }
    }

    fn infer_bi_op_layout<D>(
        &self,
        lhs: specs::Entity,
        operator: element::BiOperator,
        rhs: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        let lhs = layouts.get(lhs).cloned();
        let rhs = layouts.get(rhs).cloned();

        if let (Some(lhs), Some(rhs)) = (lhs.as_ref(), rhs.as_ref()) {
            if lhs.size != rhs.size {
                return None;
            }
            if lhs.alignment != rhs.alignment {
                return None;
            }
        }

        match operator {
            element::BiOperator::Eq
            | element::BiOperator::Ne
            | element::BiOperator::Lt
            | element::BiOperator::Ge
            | element::BiOperator::Gt
            | element::BiOperator::Le
            | element::BiOperator::And
            | element::BiOperator::Or
            | element::BiOperator::Xor
            | element::BiOperator::AndNot
            | element::BiOperator::OrNot
            | element::BiOperator::XorNot => Some(BOOL_LAYOUT),
            element::BiOperator::Cmp => unimplemented!(),
            element::BiOperator::Add
            | element::BiOperator::Sub
            | element::BiOperator::Mul
            | element::BiOperator::Div
            | element::BiOperator::Rem
            | element::BiOperator::BAnd
            | element::BiOperator::BOr
            | element::BiOperator::BXor
            | element::BiOperator::BAndNot
            | element::BiOperator::BOrNot
            | element::BiOperator::BXorNot
            | element::BiOperator::RotL
            | element::BiOperator::RotR
            | element::BiOperator::ShL
            | element::BiOperator::ShR => lhs,
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
        _function: specs::Entity,
        _parameters: &[specs::Entity],
        _layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        // TODO
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
        signature: specs::Entity,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        layouts.get(signature).cloned()
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
        captures: &collections::HashMap<String, specs::Entity>,
        layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        if let Some(mut capture_layouts) = captures
            .iter()
            .map(|(n, f)| layouts.get(*f).map(|l| (n, l)))
            .collect::<Option<Vec<_>>>()
        {
            let unnamed_fields = vec![layout::Offset {
                offset: 0,
                layout: layout::Layout::scalar(self.ptr_size),
            }];

            capture_layouts
                .sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, n.as_str()));
            let alignment = capture_layouts
                .iter()
                .map(|(_, l)| l.alignment)
                .max()
                .unwrap_or(self.ptr_size);
            let mut size = self.ptr_size;

            let named_fields = capture_layouts
                .into_iter()
                .map(|(field, layout)| {
                    let offset = align_up(size, layout.alignment);
                    size = offset + layout.size;
                    let field = field.clone();
                    let layout = layout.clone();
                    let offset_layout = layout::Offset { offset, layout };

                    layout::NamedField {
                        field,
                        offset_layout,
                    }
                })
                .collect::<Vec<_>>();

            Some(layout::Layout {
                size,
                alignment,
                named_fields,
                unnamed_fields,
            })
        } else {
            None
        }
    }

    fn infer_module_layout<D>(
        &self,
        _variables: &collections::HashMap<String, specs::Entity>,
        _layouts: &specs::Storage<layout::Layout, D>,
    ) -> Option<layout::Layout>
    where
        D: ops::Deref<Target = specs::storage::MaskedStorage<layout::Layout>>,
    {
        // TODO
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

#[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
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
