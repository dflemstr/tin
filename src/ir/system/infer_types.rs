use std::collections;

use specs;

use crate::ir::component::element;
use crate::ir::component::ty;
use std::ops;

lazy_static! {
    static ref BOOL_TYPE: ty::Type = {
        let alternatives = vec![
            ty::Symbol {
                label: "f".to_owned(),
            },
            ty::Symbol {
                label: "t".to_owned(),
            },
        ];
        ty::Type::Union(ty::Union { alternatives })
    };
}

pub struct System;

#[derive(Clone, Debug)]
enum Inference<T> {
    Type(T),
    Error(ty::TypeError<specs::Entity>),
}

type InferenceResult<T> = Option<Inference<T>>;

impl<'a> specs::System<'a> for System {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::WriteStorage<'a, ty::Type>,
        specs::WriteStorage<'a, ty::TypeError<specs::Entity>>,
    );

    fn run(&mut self, (entities, elements, mut types, mut errors): Self::SystemData) {
        use specs::prelude::ParallelIterator;
        use specs::ParJoin;

        loop {
            let new_types = (&entities, &elements, !&types, !&errors)
                .par_join()
                .flat_map(|(entity, element, _, _)| {
                    infer_type(element, &types).map(|ty| (entity, ty))
                })
                .collect::<Vec<_>>();
            debug!("inferred new types: {:?}", new_types);
            if new_types.is_empty() {
                break;
            }

            for (entity, inference) in new_types {
                match inference {
                    Inference::Type(ty) => {
                        types.insert(entity, ty).unwrap();
                    }
                    Inference::Error(error) => {
                        errors.insert(entity, error).unwrap();
                    }
                }
            }
        }
    }
}

fn infer_type<D>(
    element: &element::Element,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    match *element {
        element::Element::Number(ref n) => {
            Some(Inference::Type(ty::Type::Number(infer_number_type(n))))
        }
        element::Element::String(_) => Some(Inference::Type(ty::Type::String)),
        element::Element::Symbol(element::Symbol { ref label }) => {
            Some(Inference::Type(ty::Type::Symbol(ty::Symbol {
                label: label.clone(),
            })))
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

fn infer_number_type(number: &element::Number) -> ty::Number {
    match *number {
        element::Number::U8(_) => ty::Number::U8,
        element::Number::U16(_) => ty::Number::U16,
        element::Number::U32(_) => ty::Number::U32,
        element::Number::U64(_) => ty::Number::U64,
        element::Number::I8(_) => ty::Number::I8,
        element::Number::I16(_) => ty::Number::I16,
        element::Number::I32(_) => ty::Number::I32,
        element::Number::I64(_) => ty::Number::I64,
        element::Number::F32(_) => ty::Number::F32,
        element::Number::F64(_) => ty::Number::F64,
    }
}

fn infer_tuple_type<D>(
    fields: &[specs::Entity],
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    fields
        .iter()
        .map(|f| types.get(*f).cloned())
        .collect::<Option<Vec<_>>>()
        .map(|fields| Inference::Type(ty::Type::Tuple(ty::Tuple { fields })))
}

fn infer_record_type<D>(
    fields: &collections::HashMap<String, specs::Entity>,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    fields
        .iter()
        .map(|(k, v)| types.get(*v).map(|t| (k.clone(), t.clone())))
        .collect::<Option<collections::HashMap<_, _>>>()
        .map(|fields| Inference::Type(ty::Type::Record(ty::Record { fields })))
}

fn infer_un_op_type<D>(
    operand: specs::Entity,
    operator: element::UnOperator,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(operand).map(|ty| match operator {
        element::UnOperator::Not => {
            if *ty == *BOOL_TYPE {
                Inference::Type(BOOL_TYPE.clone())
            } else {
                Inference::Error(ty::TypeError {
                    expected: ty::ExpectedType::Specific(BOOL_TYPE.clone()),
                    actual: ty.clone(),
                    main_entity: operand,
                    aux_entities: vec![],
                })
            }
        }
        element::UnOperator::BNot => if_integral_then(operand, ty, ty),
        element::UnOperator::Cl0 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::Cl1 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::Cls => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::Ct0 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::Ct1 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::C0 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::C1 => {
            if_integral_then(operand, ty, &ty::Type::Number(ty::Number::U32))
        }
        element::UnOperator::Sqrt => if_fractional_then(operand, ty, ty),
    })
}

fn infer_bi_op_type<D>(
    lhs: specs::Entity,
    operator: element::BiOperator,
    rhs: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    // TODO: check scalar type semantics so e.g. records can't be divided.
    match (types.get(lhs), types.get(rhs)) {
        (Some(lhs_ty), Some(rhs_ty)) => {
            let result = match operator {
                element::BiOperator::Eq => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Ne => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Lt => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Ge => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Gt => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Le => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, &*BOOL_TYPE),
                element::BiOperator::Cmp => unimplemented!(),
                element::BiOperator::Add => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Sub => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Mul => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Div => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Rem => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::And => bool_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BAnd => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Or => or_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BOr => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::Xor => bool_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BXor => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::AndNot => bool_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BAndNot => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::OrNot => bool_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BOrNot => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::XorNot => bool_op(lhs, lhs_ty, rhs, rhs_ty),
                element::BiOperator::BXorNot => if_eq_then(lhs, lhs_ty, rhs, rhs_ty, lhs_ty),
                element::BiOperator::RotL => if_integral_and_eq_then(
                    lhs,
                    lhs_ty,
                    rhs,
                    rhs_ty,
                    &ty::Type::Number(ty::Number::U32),
                    lhs_ty,
                ),
                element::BiOperator::RotR => if_integral_and_eq_then(
                    lhs,
                    lhs_ty,
                    rhs,
                    rhs_ty,
                    &ty::Type::Number(ty::Number::U32),
                    lhs_ty,
                ),
                element::BiOperator::ShL => if_integral_and_eq_then(
                    lhs,
                    lhs_ty,
                    rhs,
                    rhs_ty,
                    &ty::Type::Number(ty::Number::U32),
                    lhs_ty,
                ),
                element::BiOperator::ShR => if_integral_and_eq_then(
                    lhs,
                    lhs_ty,
                    rhs,
                    rhs_ty,
                    &ty::Type::Number(ty::Number::U32),
                    lhs_ty,
                ),
            };
            Some(result)
        }
        _ => None,
    }
}

fn infer_variable_type<D>(
    initializer: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(initializer).cloned().map(Inference::Type)
}

fn infer_select_type<D>(
    record: specs::Entity,
    field: &str,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    match types.get(record) {
        None => None,
        Some(t) => match t {
            ty::Type::Record(ty::Record { ref fields }) => {
                if let Some(t) = fields.get(field) {
                    Some(Inference::Type(t.clone()))
                } else {
                    let mut expected_fields = collections::HashMap::new();
                    expected_fields.insert(
                        field.to_owned(),
                        ty::Type::Symbol(ty::Symbol {
                            label: "something".to_owned(),
                        }),
                    );
                    Some(Inference::Error(ty::TypeError {
                        expected: ty::ExpectedType::Specific(ty::Type::Record(ty::Record {
                            fields: expected_fields,
                        })),
                        actual: t.clone(),
                        main_entity: record,
                        aux_entities: vec![],
                    }))
                }
            }
            something => {
                let mut expected_fields = collections::HashMap::new();
                expected_fields.insert(
                    field.to_owned(),
                    ty::Type::Symbol(ty::Symbol {
                        label: "something".to_owned(),
                    }),
                );
                Some(Inference::Error(ty::TypeError {
                    expected: ty::ExpectedType::Specific(ty::Type::Record(ty::Record {
                        fields: expected_fields,
                    })),
                    actual: something.clone(),
                    main_entity: record,
                    aux_entities: vec![],
                }))
            }
        },
    }
}

fn infer_apply_type<D>(
    function: specs::Entity,
    parameters: &[specs::Entity],
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
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
                        Some(Inference::Type((**result).clone()))
                    } else {
                        // TODO: create a diff of expected and actual parameters
                        Some(Inference::Error(ty::TypeError {
                            expected: ty::ExpectedType::Specific(ty::Type::Function(
                                ty::Function {
                                    parameters,
                                    result: Box::new(ty::Type::Symbol(ty::Symbol {
                                        label: "something".to_owned(),
                                    })),
                                },
                            )),
                            actual: f.clone(),
                            main_entity: function,
                            aux_entities: vec![],
                        }))
                    }
                } else {
                    None
                }
            }
            something => Some(Inference::Error(ty::TypeError {
                expected: ty::ExpectedType::Specific(ty::Type::Function(ty::Function {
                    parameters: vec![ty::Type::Symbol(ty::Symbol {
                        label: "something".to_owned(),
                    })],
                    result: Box::new(ty::Type::Symbol(ty::Symbol {
                        label: "something".to_owned(),
                    })),
                })),
                actual: something.clone(),
                main_entity: function,
                aux_entities: vec![],
            })),
        },
    }
}

fn infer_parameter_type<D>(
    signature: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(signature).cloned().map(Inference::Type)
}

fn infer_capture_type<D>(
    capture: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    types.get(capture).cloned().map(Inference::Type)
}

fn infer_closure_type<D>(
    parameters: &[specs::Entity],
    signature: specs::Entity,
    result: specs::Entity,
    types: &specs::Storage<ty::Type, D>,
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    if let Some(parameters) = parameters
        .iter()
        .map(|p| types.get(*p).cloned())
        .collect::<Option<Vec<_>>>()
    {
        if let Some(result_ty) = types.get(result) {
                if let Some(signature_ty) = types.get(signature) {
                    if signature_ty == result_ty {
                        let result = Box::new(signature_ty.clone());
                        Some(Inference::Type(ty::Type::Function(ty::Function {
                            parameters,
                            result,
                        })))
                    } else {
                        Some(Inference::Error(ty::TypeError {
                            expected: ty::ExpectedType::Specific(signature_ty.clone()),
                            actual: result_ty.clone(),
                            main_entity: result,
                            aux_entities: vec![ty::AuxEntity {
                                entity: signature,
                                label: format!("declared return type is {}", signature_ty),
                            }],
                        }))
                    }
                } else {
                    trace!("inference failure: no signature for closure");
                    None
                }
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
) -> InferenceResult<ty::Type>
where
    D: ops::Deref<Target = specs::storage::MaskedStorage<ty::Type>>,
{
    variables
        .iter()
        .map(|(k, v)| types.get(*v).map(|t| (k.clone(), t.clone())))
        .collect::<Option<collections::HashMap<_, _>>>()
        .map(|fields| Inference::Type(ty::Type::Record(ty::Record { fields })))
}

fn if_eq_then(
    lhs_entity: specs::Entity,
    lhs: &ty::Type,
    rhs_entity: specs::Entity,
    rhs: &ty::Type,
    result: &ty::Type,
) -> Inference<ty::Type> {
    if lhs == rhs {
        Inference::Type(result.clone())
    } else {
        Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::Specific(lhs.clone()),
            actual: rhs.clone(),
            main_entity: rhs_entity,
            aux_entities: vec![ty::AuxEntity {
                entity: lhs_entity,
                label: format!("other operand has type {}", lhs),
            }],
        })
    }
}

fn bool_op(
    lhs_entity: specs::Entity,
    lhs: &ty::Type,
    rhs_entity: specs::Entity,
    rhs: &ty::Type,
) -> Inference<ty::Type> {
    if *lhs == *BOOL_TYPE {
        if *rhs == *BOOL_TYPE {
            Inference::Type(BOOL_TYPE.clone())
        } else {
            Inference::Error(ty::TypeError {
                expected: ty::ExpectedType::Specific(BOOL_TYPE.clone()),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type {}", lhs),
                }],
            })
        }
    } else {
        Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::Specific(BOOL_TYPE.clone()),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type {}", rhs),
            }],
        })
    }
}

fn or_op(
    lhs_entity: specs::Entity,
    lhs: &ty::Type,
    rhs_entity: specs::Entity,
    rhs: &ty::Type,
) -> Inference<ty::Type> {
    if let ty::Type::Union(u) = lhs {
        if let ty::Type::Symbol(ref symbol) = rhs {
            Inference::Type(ty::Type::Union(u.clone().with(symbol)))
        } else {
            Inference::Error(ty::TypeError {
                expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Symbol),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type {}", lhs),
                }],
            })
        }
    } else if *lhs == *BOOL_TYPE {
        if *rhs == *BOOL_TYPE {
            Inference::Type(BOOL_TYPE.clone())
        } else {
            Inference::Error(ty::TypeError {
                expected: ty::ExpectedType::Specific(BOOL_TYPE.clone()),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type {}", lhs),
                }],
            })
        }
    } else {
        Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::AnyOf(vec![
                ty::ExpectedType::Specific(BOOL_TYPE.clone()),
                ty::ExpectedType::Union,
            ]),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type {}", rhs),
            }],
        })
    }
}

fn if_integral_then(
    entity: specs::Entity,
    ty: &ty::Type,
    result: &ty::Type,
) -> Inference<ty::Type> {
    match ty.scalar_class() {
        ty::ScalarClass::Integral(_) => Inference::Type(result.clone()),
        _ => Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Integral(
                ty::IntegralScalarClass::Any,
            )),
            actual: ty.clone(),
            main_entity: entity,
            aux_entities: vec![],
        }),
    }
}

fn if_integral_and_eq_then(
    lhs_entity: specs::Entity,
    lhs: &ty::Type,
    rhs_entity: specs::Entity,
    rhs: &ty::Type,
    expected: &ty::Type,
    result: &ty::Type,
) -> Inference<ty::Type> {
    match lhs.scalar_class() {
        ty::ScalarClass::Integral(_) => {
            if rhs == expected {
                Inference::Type(result.clone())
            } else {
                Inference::Error(ty::TypeError {
                    expected: ty::ExpectedType::Specific(expected.clone()),
                    actual: rhs.clone(),
                    main_entity: rhs_entity,
                    aux_entities: vec![ty::AuxEntity {
                        entity: lhs_entity,
                        label: format!("other operand has type {}", lhs),
                    }],
                })
            }
        }
        _ => Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Integral(
                ty::IntegralScalarClass::Any,
            )),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type {}", rhs),
            }],
        }),
    }
}

fn if_fractional_then(
    entity: specs::Entity,
    ty: &ty::Type,
    result: &ty::Type,
) -> Inference<ty::Type> {
    match ty.scalar_class() {
        ty::ScalarClass::Fractional => Inference::Type(result.clone()),
        _ => Inference::Error(ty::TypeError {
            expected: ty::ExpectedType::ScalarClass(ty::ScalarClass::Fractional),
            actual: ty.clone(),
            main_entity: entity,
            aux_entities: vec![],
        }),
    }
}
