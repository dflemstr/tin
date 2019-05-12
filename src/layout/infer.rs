use std::collections;
use std::sync;
use std::usize;

use crate::ir;
use crate::ir::element;
use crate::layout;

pub struct System {
    ptr_size: usize,
}

const BOOL_LAYOUT: layout::Layout = layout::Layout::scalar(1);

impl System {
    pub fn new(ptr_size: usize) -> System {
        System { ptr_size }
    }

    fn infer_layout(
        &self,
        element: &element::Element,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        match *element {
            element::Element::Reference(_) => unimplemented!(),
            element::Element::Number(ref n) => self.infer_number_layout(n),
            element::Element::String(_) => {
                Ok(sync::Arc::new(layout::Layout::scalar(self.ptr_size)))
            }
            element::Element::Symbol(_) => Ok(sync::Arc::new(layout::Layout::zero())),
            element::Element::Tuple(element::Tuple { ref fields }) => {
                self.infer_tuple_layout(fields, db)
            }
            element::Element::Record(element::Record { ref fields }) => {
                self.infer_record_layout(fields, db)
            }
            element::Element::UnOp(element::UnOp { operator, operand }) => {
                self.infer_un_op_layout(operator, operand, db)
            }
            element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
                self.infer_bi_op_layout(lhs, operator, rhs, db)
            }
            element::Element::Variable(element::Variable { initializer, .. }) => {
                self.infer_variable_layout(initializer, db)
            }
            element::Element::Select(element::Select { record, field }) => {
                self.infer_select_layout(record, field, db)
            }
            element::Element::Apply(element::Apply {
                function,
                ref parameters,
            }) => self.infer_apply_layout(function, parameters, db),
            element::Element::Parameter(element::Parameter { signature, .. }) => {
                self.infer_parameter_layout(signature, db)
            }
            element::Element::Capture(element::Capture { captured, .. }) => {
                self.infer_capture_layout(captured, db)
            }
            element::Element::Closure(element::Closure { ref captures, .. }) => {
                self.infer_closure_layout(captures, db)
            }
            element::Element::Module(element::Module { ref variables }) => {
                self.infer_module_layout(variables, db)
            }
        }
    }

    fn infer_number_layout(
        &self,
        number: &element::Number,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        match *number {
            element::Number::U8(_) | element::Number::I8(_) => {
                Ok(sync::Arc::new(layout::Layout::scalar(1)))
            }
            element::Number::U16(_) | element::Number::I16(_) => {
                Ok(sync::Arc::new(layout::Layout::scalar(2)))
            }
            element::Number::U32(_) | element::Number::I32(_) | element::Number::F32(_) => {
                Ok(sync::Arc::new(layout::Layout::scalar(4)))
            }
            element::Number::U64(_) | element::Number::I64(_) | element::Number::F64(_) => {
                Ok(sync::Arc::new(layout::Layout::scalar(8)))
            }
        }
    }

    fn infer_tuple_layout(
        &self,
        fields: &[ir::Entity],
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        let mut layouts = fields
            .iter()
            .enumerate()
            .map(|(i, f)| Ok((i, db.entity_layout(*f)?)))
            .collect::<layout::error::Result<Vec<_>>>()?;

        if layouts.is_empty() {
            Ok(sync::Arc::new(layout::Layout::zero()))
        } else {
            layouts.sort_unstable_by_key(|(i, l)| (usize::max_value() - l.size, *i));
            let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
            let mut size = 0;

            let mut unnamed_fields = vec![layout::Offset::zero(); layouts.len()];

            for (i, layout) in layouts {
                let offset = align_up(size, layout.alignment);
                size = offset + layout.size;
                let layout = (*layout).clone();
                unnamed_fields[i] = layout::Offset { offset, layout };
            }

            Ok(sync::Arc::new(layout::Layout::unnamed_fields(
                size,
                alignment,
                unnamed_fields,
            )))
        }
    }

    fn infer_record_layout(
        &self,
        fields: &collections::HashMap<ir::Ident, ir::Entity>,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        let mut layouts = fields
            .iter()
            .map(|(n, f)| Ok((*n, db.entity_layout(*f)?)))
            .collect::<layout::error::Result<Vec<_>>>()?;

        if layouts.is_empty() {
            Ok(sync::Arc::new(layout::Layout::zero()))
        } else {
            layouts
                .sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, db.lookup_ident(*n)));
            let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
            let mut size = 0;

            let named_fields = layouts
                .into_iter()
                .map(|(field, layout)| {
                    let offset = align_up(size, layout.alignment);
                    size = offset + layout.size;
                    let layout = (*layout).clone();
                    let offset_layout = layout::Offset { offset, layout };

                    layout::NamedField {
                        field,
                        offset_layout,
                    }
                })
                .collect::<Vec<_>>();

            Ok(sync::Arc::new(layout::Layout::named_fields(
                size,
                alignment,
                named_fields,
            )))
        }
    }

    fn infer_un_op_layout(
        &self,
        operator: element::UnOperator,
        operand: ir::Entity,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        match operator {
            element::UnOperator::Not => Ok(sync::Arc::new(BOOL_LAYOUT)),
            element::UnOperator::BNot
            | element::UnOperator::Cl0
            | element::UnOperator::Cl1
            | element::UnOperator::Cls
            | element::UnOperator::Ct0
            | element::UnOperator::Ct1
            | element::UnOperator::C0
            | element::UnOperator::C1
            | element::UnOperator::Sqrt => db.entity_layout(operand),
        }
    }

    fn infer_bi_op_layout(
        &self,
        lhs: ir::Entity,
        operator: element::BiOperator,
        rhs: ir::Entity,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        let lhs = db.entity_layout(lhs)?;
        let rhs = db.entity_layout(rhs)?;

        if lhs.size != rhs.size {
            unreachable!()
        }

        if lhs.alignment != rhs.alignment {
            unreachable!()
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
            | element::BiOperator::XorNot => Ok(sync::Arc::new(BOOL_LAYOUT)),
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
            | element::BiOperator::ShR => Ok(lhs),
        }
    }

    fn infer_variable_layout(
        &self,
        initializer: ir::Entity,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        db.entity_layout(initializer)
    }

    fn infer_select_layout(
        &self,
        record: ir::Entity,
        field: ir::Ident,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        match *db.element(record)? {
            element::Element::Record(element::Record { ref fields }) => {
                if let Some(f) = fields.get(&field) {
                    db.entity_layout(*f)
                } else {
                    panic!("field does not exist")
                }
            }
            _ => panic!("entity is not a record"),
        }
    }

    fn infer_apply_layout(
        &self,
        _function: ir::Entity,
        _parameters: &[ir::Entity],
        _db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        // TODO
        unimplemented!()
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

    fn infer_parameter_layout(
        &self,
        signature: ir::Entity,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        db.entity_layout(signature)
    }

    fn infer_capture_layout(
        &self,
        capture: ir::Entity,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        db.entity_layout(capture)
    }

    fn infer_closure_layout(
        &self,
        captures: &collections::HashMap<ir::Ident, ir::Entity>,
        db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        let mut capture_layouts = captures
            .iter()
            .map(|(n, f)| Ok((*n, db.entity_layout(*f)?)))
            .collect::<layout::error::Result<Vec<_>>>()?;
        let unnamed_fields = vec![layout::Offset {
            offset: 0,
            layout: layout::Layout::scalar(self.ptr_size),
        }];

        capture_layouts
            .sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, db.lookup_ident(*n)));
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
                let layout = (*layout).clone();
                let offset_layout = layout::Offset { offset, layout };

                layout::NamedField {
                    field,
                    offset_layout,
                }
            })
            .collect::<Vec<_>>();

        Ok(sync::Arc::new(layout::Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }))
    }

    fn infer_module_layout(
        &self,
        _variables: &collections::HashMap<ir::Ident, ir::Entity>,
        _db: impl layout::db::LayoutDb,
    ) -> layout::error::Result<sync::Arc<layout::Layout>> {
        // TODO
        unimplemented!()
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
