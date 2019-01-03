use std::collections;

use specs;

use ir::component::element;
use ir::component::ty;
use std::ops;

pub struct System;

impl<'a> specs::System<'a> for System {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::WriteStorage<'a, ty::Type>,
    );

    fn run(&mut self, (entities, elements, mut types): Self::SystemData) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

        loop {
            let new_types = (&entities, &elements, !&types)
                .par_join()
                .flat_map(|(entity, element, _)| infer_type(element, &types).map(|ty| (entity, ty)))
                .collect::<Vec<_>>();
            debug!("inferred new types: {:?}", new_types);
            if new_types.is_empty() {
                break;
            }

            for (entity, ty) in new_types {
                types.insert(entity, ty).unwrap();
            }
        }
    }
}

fn infer_type<D>(
    element: &element::Element,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    match *element {
        element::Element::NumberValue(ref n) => Some(ty::Type::Number(infer_number_type(n))),
        element::Element::StringValue(_) => Some(ty::Type::String),
        element::Element::Symbol(element::Symbol { ref label }) => {
            Some(ty::Type::Symbol(label.clone()))
        }
        element::Element::Tuple(element::Tuple { ref fields }) => infer_tuple_type(fields, types),
        element::Element::Record(element::Record { ref fields }) => {
            infer_record_type(fields, types)
        }
        element::Element::UnOp(element::UnOp { operand, operator }) => {
            infer_un_op_type(operand, operator, types)
        }
        element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
            infer_bi_op_type(lhs, operator, rhs, types)
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            infer_variable_type(initializer, types)
        }
        element::Element::Select(element::Select { record, ref field }) => {
            infer_select_type(record, field, types)
        }
        element::Element::Apply(element::Apply {
            function,
            ref parameters,
        }) => infer_apply_type(function, parameters, types),
        element::Element::Parameter(element::Parameter { signature, .. }) => {
            infer_parameter_type(signature, types)
        }
        element::Element::Capture(element::Capture { captured, .. }) => {
            infer_capture_type(captured, types)
        }
        element::Element::Closure(element::Closure {
            ref parameters,
            signature,
            result,
            ..
        }) => infer_closure_type(parameters, signature, result, types),
        element::Element::Module(element::Module { ref variables }) => {
            infer_module_type(variables, types)
        }
    }
}

fn infer_number_type(number: &element::NumberValue) -> ty::Number {
    match *number {
        element::NumberValue::U8(_) => ty::Number::U8,
        element::NumberValue::U16(_) => ty::Number::U16,
        element::NumberValue::U32(_) => ty::Number::U32,
        element::NumberValue::U64(_) => ty::Number::U64,
        element::NumberValue::I8(_) => ty::Number::I8,
        element::NumberValue::I16(_) => ty::Number::I16,
        element::NumberValue::I32(_) => ty::Number::I32,
        element::NumberValue::I64(_) => ty::Number::I64,
        element::NumberValue::F32(_) => ty::Number::F32,
        element::NumberValue::F64(_) => ty::Number::F64,
    }
}

fn infer_tuple_type<D>(
    fields: &[specs::Entity],
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    fields
        .iter()
        .map(|f| types.get(*f).cloned())
        .collect::<Option<Vec<_>>>()
        .map(|fields| ty::Type::Tuple(ty::Tuple { fields }))
}

fn infer_record_type<D>(
    fields: &collections::HashMap<String, specs::Entity>,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    fields
        .iter()
        .map(|(k, v)| types.get(*v).map(|t| (k.clone(), t.clone())))
        .collect::<Option<collections::HashMap<_, _>>>()
        .map(|fields| ty::Type::Record(ty::Record { fields }))
}

fn infer_un_op_type<D>(
    operand: specs::Entity,
    operator: element::UnOperator,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    if let Some(ty) = types.get(operand) {
        let result = match operator {
            element::UnOperator::Not => {
                if ty.scalar_class() == ty::ScalarClass::Boolean {
                    ty::Type::Boolean
                } else {
                    ty::Type::Conflict(ty::Conflict {
                        expected: ty::ExpectedType::Specific(Box::new(ty::Type::Boolean)),
                        actual: Box::new(ty.clone()),
                    })
                }
            }
            element::UnOperator::BNot => same_integral_type(ty),
            element::UnOperator::Cl0 => same_integral_type(ty),
            element::UnOperator::Cl1 => same_integral_type(ty),
            element::UnOperator::Cls => same_integral_type(ty),
            element::UnOperator::Ct0 => same_integral_type(ty),
            element::UnOperator::Ct1 => same_integral_type(ty),
            element::UnOperator::C0 => same_integral_type(ty),
            element::UnOperator::C1 => same_integral_type(ty),
            element::UnOperator::Sqrt => same_fractional_type(ty),
        };
        Some(result)
    } else {
        None
    }
}

fn infer_bi_op_type<D>(
    lhs: specs::Entity,
    operator: element::BiOperator,
    rhs: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    // TODO: check scalar type semantics so e.g. records can't be divided.
    match (types.get(lhs), types.get(rhs)) {
        (Some(lhs_ty), Some(rhs_ty)) => {
            let result = match operator {
                element::BiOperator::Eq => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Ne => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Lt => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Ge => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Gt => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Le => if_eq_then(lhs_ty, rhs_ty, &ty::Type::Boolean),
                element::BiOperator::Cmp => unimplemented!(),
                element::BiOperator::Add => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Sub => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Mul => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Div => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Rem => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::And => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BAnd => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Or => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BOr => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::Xor => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BXor => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::AndNot => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BAndNot => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::OrNot => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BOrNot => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::XorNot => bool_op(lhs_ty, rhs_ty),
                element::BiOperator::BXorNot => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::RotL => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::RotR => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::ShL => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
                element::BiOperator::ShR => if_eq_then(lhs_ty, rhs_ty, lhs_ty),
            };
            Some(result)
        }
        _ => None,
    }
}

fn infer_variable_type<D>(
    initializer: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(initializer).cloned()
}

fn infer_select_type<D>(
    record: specs::Entity,
    field: &str,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    match types.get(record) {
        None => None,
        Some(t) => match t {
            ty::Type::Record(ty::Record { ref fields }) => {
                if let Some(t) = fields.get(field) {
                    Some(t.clone())
                } else {
                    let mut expected_fields = collections::HashMap::new();
                    expected_fields.insert(field.to_owned(), ty::Type::Any);
                    Some(ty::Type::Conflict(ty::Conflict {
                        expected: ty::ExpectedType::Specific(Box::new(ty::Type::Record(
                            ty::Record {
                                fields: expected_fields,
                            },
                        ))),
                        actual: Box::new(t.clone()),
                    }))
                }
            }
            something => {
                let mut expected_fields = collections::HashMap::new();
                expected_fields.insert(field.to_owned(), ty::Type::Any);
                Some(ty::Type::Conflict(ty::Conflict {
                    expected: ty::ExpectedType::Specific(Box::new(ty::Type::Record(ty::Record {
                        fields: expected_fields,
                    }))),
                    actual: Box::new(something.clone()),
                }))
            }
        },
    }
}

fn infer_apply_type<D>(
    function: specs::Entity,
    parameters: &[specs::Entity],
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
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
                            expected: ty::ExpectedType::Specific(Box::new(ty::Type::Function(
                                ty::Function {
                                    parameters,
                                    result: Box::new(ty::Type::Any),
                                },
                            ))),
                            actual: Box::new(f.clone()),
                        }))
                    }
                } else {
                    None
                }
            }
            something => Some(ty::Type::Conflict(ty::Conflict {
                expected: ty::ExpectedType::Specific(Box::new(ty::Type::Function(ty::Function {
                    parameters: vec![ty::Type::Any],
                    result: Box::new(ty::Type::Any),
                }))),
                actual: Box::new(something.clone()),
            })),
        },
    }
}

fn infer_parameter_type<D>(
    signature: Option<specs::Entity>,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    if let Some(signature) = signature {
        types.get(signature).cloned()
    } else {
        trace!("inference failure: no signature for parameter");
        // TODO: implement surjective type inference
        None
    }
}

fn infer_capture_type<D>(
    capture: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(capture).cloned()
}

fn infer_closure_type<D>(
    parameters: &[specs::Entity],
    signature: Option<specs::Entity>,
    result: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
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
}

fn infer_module_type<D>(
    variables: &collections::HashMap<String, specs::Entity>,
    types: &specs::Storage<ty::Type, D>,
) -> Option<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    variables
        .iter()
        .map(|(k, v)| types.get(*v).map(|t| (k.clone(), t.clone())))
        .collect::<Option<collections::HashMap<_, _>>>()
        .map(|fields| ty::Type::Record(ty::Record { fields }))
}

fn if_eq_then(lhs: &ty::Type, rhs: &ty::Type, result: &ty::Type) -> ty::Type {
    if lhs == rhs {
        result.clone()
    } else {
        ty::Type::Conflict(ty::Conflict {
            expected: ty::ExpectedType::Specific(Box::new(lhs.clone())),
            actual: Box::new(rhs.clone()),
        })
    }
}

fn bool_op(lhs: &ty::Type, rhs: &ty::Type) -> ty::Type {
    match (lhs.scalar_class(), rhs.scalar_class()) {
        (ty::ScalarClass::Boolean, ty::ScalarClass::Boolean) => ty::Type::Boolean,
        (ty::ScalarClass::Boolean, _) => ty::Type::Conflict(ty::Conflict {
            expected: ty::ExpectedType::Specific(Box::new(ty::Type::Boolean)),
            actual: Box::new(rhs.clone()),
        }),
        _ => ty::Type::Conflict(ty::Conflict {
            expected: ty::ExpectedType::Specific(Box::new(ty::Type::Boolean)),
            actual: Box::new(lhs.clone()),
        }),
    }
}

fn same_integral_type(ty: &ty::Type) -> ty::Type {
    match ty.scalar_class() {
        ty::ScalarClass::Integral(_) => ty.clone(),
        _ => ty::Type::Conflict(ty::Conflict {
            expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Integral(
                ty::IntegralScalarClass::Any,
            )),
            actual: Box::new(ty.clone()),
        }),
    }
}

fn same_fractional_type(ty: &ty::Type) -> ty::Type {
    match ty.scalar_class() {
        ty::ScalarClass::Fractional => ty.clone(),
        _ => ty::Type::Conflict(ty::Conflict {
            expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Fractional),
            actual: Box::new(ty.clone()),
        }),
    }
}
