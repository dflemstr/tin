use std::collections;
use std::result;
use std::sync;

use crate::ir;
use crate::ir::element;
use crate::ty;

pub struct System;

type Result<T> = result::Result<sync::Arc<T>, ty::error::Error>;

fn infer_type(element: &element::Element, db: impl ty::db::TyDb) -> Result<ty::Type> {
    match *element {
        element::Element::Number(ref n) => {
            Ok(sync::Arc::new(ty::Type::Number(infer_number_type(n))))
        }
        element::Element::String(_) => Ok(sync::Arc::new(ty::Type::String)),
        element::Element::Symbol(element::Symbol { ref label }) => {
            Ok(sync::Arc::new(ty::Type::Symbol(ty::Symbol {
                label: *label,
            })))
        }
        element::Element::Tuple(element::Tuple { ref fields }) => infer_tuple_type(fields, db),
        element::Element::Record(element::Record { ref fields }) => infer_record_type(fields, db),
        element::Element::UnOp(element::UnOp { operand, operator }) => {
            infer_un_op_type(operand, operator, db)
        }
        element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
            infer_bi_op_type(lhs, operator, rhs, db)
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            infer_variable_type(initializer, db)
        }
        element::Element::Select(element::Select { record, field }) => {
            infer_select_type(record, field, db)
        }
        element::Element::Apply(element::Apply {
            function,
            ref parameters,
        }) => infer_apply_type(function, parameters, db),
        element::Element::Parameter(element::Parameter { signature, .. }) => {
            infer_parameter_type(signature, db)
        }
        element::Element::Capture(element::Capture { captured, .. }) => {
            infer_capture_type(captured, db)
        }
        element::Element::Closure(element::Closure {
            ref parameters,
            signature,
            result,
            ..
        }) => infer_closure_type(parameters, signature, result, db),
        element::Element::Module(element::Module { ref variables }) => {
            infer_module_type(variables, db)
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

fn infer_tuple_type(fields: &[ir::Entity], db: impl ty::db::TyDb) -> Result<ty::Type> {
    let fields = fields
        .iter()
        .map(|f| db.entity_type(*f))
        .collect::<result::Result<Vec<_>, ty::error::Error>>()?;
    Ok(sync::Arc::new(ty::Type::Tuple(ty::Tuple { fields })))
}

fn infer_record_type(
    fields: &collections::HashMap<ir::Ident, ir::Entity>,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let fields = fields
        .iter()
        .map(|(k, v)| Ok((*k, db.entity_type(*v)?)))
        .collect::<result::Result<collections::HashMap<_, _>, ty::error::Error>>()?;
    Ok(sync::Arc::new(ty::Type::Record(ty::Record { fields })))
}

fn infer_un_op_type(
    operand: ir::Entity,
    operator: element::UnOperator,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let ty = db.entity_type(operand)?;
    match operator {
        element::UnOperator::Not => {
            let bool_type = db.bool_type();
            if ty == bool_type {
                Ok(bool_type)
            } else {
                Err(ty::error::Error {
                    expected: ty::error::ExpectedType::Specific(bool_type),
                    actual: ty,
                    main_entity: operand,
                    aux_entities: vec![],
                })
            }
        }
        element::UnOperator::BNot => if_integral_then(operand, &ty, ty.clone()),
        element::UnOperator::Cl0
        | element::UnOperator::Cl1
        | element::UnOperator::Cls
        | element::UnOperator::Ct0
        | element::UnOperator::Ct1
        | element::UnOperator::C0
        | element::UnOperator::C1 => if_integral_then(
            operand,
            &ty,
            sync::Arc::new(ty::Type::Number(ty::Number::U32)),
        ),
        element::UnOperator::Sqrt => if_fractional_then(operand, &ty, ty.clone()),
    }
}

fn infer_bi_op_type(
    lhs: ir::Entity,
    operator: element::BiOperator,
    rhs: ir::Entity,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    // TODO: check scalar type semantics so e.g. records can't be divided.
    let lhs_ty = db.entity_type(lhs)?;
    let rhs_ty = db.entity_type(rhs)?;
    match operator {
        element::BiOperator::Eq
        | element::BiOperator::Ne
        | element::BiOperator::Lt
        | element::BiOperator::Ge
        | element::BiOperator::Gt
        | element::BiOperator::Le => if_eq_then(lhs, &lhs_ty, rhs, &rhs_ty, &db.bool_type(), db),
        element::BiOperator::Cmp => unimplemented!(),
        element::BiOperator::Add
        | element::BiOperator::Sub
        | element::BiOperator::Mul
        | element::BiOperator::Div
        | element::BiOperator::Rem
        // TODO: don't allow floats
        | element::BiOperator::BAnd
        | element::BiOperator::BOr
        | element::BiOperator::BXor
        | element::BiOperator::BAndNot
        | element::BiOperator::BOrNot
        | element::BiOperator::BXorNot => if_eq_then(lhs, &lhs_ty, rhs, &rhs_ty, &lhs_ty, db),
        element::BiOperator::Or => or_op(lhs, &lhs_ty, rhs, &rhs_ty, db),
        element::BiOperator::And | element::BiOperator::Xor | element::BiOperator::AndNot | element::BiOperator::OrNot | element::BiOperator::XorNot => bool_op(lhs, &lhs_ty, rhs, &rhs_ty, db),
        element::BiOperator::RotL | element::BiOperator::RotR | element::BiOperator::ShL | element::BiOperator::ShR => if_integral_and_eq_then(
            lhs,
            &lhs_ty,
            rhs,
            &rhs_ty,
            &sync::Arc::new(ty::Type::Number(ty::Number::U32)),
            lhs_ty.clone(),
        ),
    }
}

fn infer_variable_type(initializer: ir::Entity, db: impl ty::db::TyDb) -> Result<ty::Type> {
    db.entity_type(initializer)
}

fn infer_select_type(
    record: ir::Entity,
    field: ir::Ident,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let record_ty = db.entity_type(record)?;
    match *record_ty {
        ty::Type::Record(ty::Record { ref fields }) => {
            if let Some(t) = fields.get(&field) {
                Ok(t.clone())
            } else {
                let mut expected_fields = collections::HashMap::new();
                expected_fields.insert(field.to_owned(), sync::Arc::new(ty::Type::Placeholder));
                Err(ty::error::Error {
                    expected: ty::error::ExpectedType::Specific(sync::Arc::new(ty::Type::Record(
                        ty::Record {
                            fields: expected_fields,
                        },
                    ))),
                    actual: record_ty.clone(),
                    main_entity: record,
                    aux_entities: vec![],
                })
            }
        }
        _ => {
            let mut expected_fields = collections::HashMap::new();
            expected_fields.insert(field.to_owned(), sync::Arc::new(ty::Type::Placeholder));
            Err(ty::error::Error {
                expected: ty::error::ExpectedType::Specific(sync::Arc::new(ty::Type::Record(
                    ty::Record {
                        fields: expected_fields,
                    },
                ))),
                actual: record_ty,
                main_entity: record,
                aux_entities: vec![],
            })
        }
    }
}

fn infer_apply_type(
    function: ir::Entity,
    parameters: &[ir::Entity],
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let function_ty = db.entity_type(function)?;
    match *function_ty {
        ty::Type::Function(ty::Function {
            parameters: ref formal_parameters,
            ref result,
        }) => {
            let parameters = parameters
                .iter()
                .map(|p| db.entity_type(*p))
                .collect::<result::Result<Vec<_>, ty::error::Error>>()?;

            if parameters == *formal_parameters {
                Ok(result.clone())
            } else {
                // TODO: create a diff of expected and actual parameters
                Err(ty::error::Error {
                    expected: ty::error::ExpectedType::Specific(sync::Arc::new(
                        ty::Type::Function(ty::Function {
                            parameters,
                            result: sync::Arc::new(ty::Type::Placeholder),
                        }),
                    )),
                    actual: function_ty.clone(),
                    main_entity: function,
                    aux_entities: vec![],
                })
            }
        }
        _ => Err(ty::error::Error {
            expected: ty::error::ExpectedType::Specific(sync::Arc::new(ty::Type::Function(
                ty::Function {
                    parameters: vec![sync::Arc::new(ty::Type::Placeholder)],
                    result: sync::Arc::new(ty::Type::Placeholder),
                },
            ))),
            actual: function_ty,
            main_entity: function,
            aux_entities: vec![],
        }),
    }
}

fn infer_parameter_type(signature: ir::Entity, db: impl ty::db::TyDb) -> Result<ty::Type> {
    db.entity_type(signature)
}

fn infer_capture_type(capture: ir::Entity, db: impl ty::db::TyDb) -> Result<ty::Type> {
    db.entity_type(capture)
}

fn infer_closure_type(
    parameters: &[ir::Entity],
    signature: ir::Entity,
    result: ir::Entity,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let parameters = parameters
        .iter()
        .map(|p| db.entity_type(*p))
        .collect::<result::Result<Vec<_>, ty::error::Error>>()?;
    let result_ty = db.entity_type(result)?;
    let signature_ty = db.entity_type(signature)?;

    if signature_ty == result_ty {
        let result = signature_ty.clone();
        Ok(sync::Arc::new(ty::Type::Function(ty::Function {
            parameters,
            result,
        })))
    } else {
        Err(ty::error::Error {
            expected: ty::error::ExpectedType::Specific(signature_ty.clone()),
            actual: result_ty.clone(),
            main_entity: result,
            aux_entities: vec![ty::error::AuxEntity {
                entity: signature,
                label: format!("declared return type is `{}`", signature_ty),
            }],
        })
    }
}

fn infer_module_type(
    variables: &collections::HashMap<ir::Ident, ir::Entity>,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let fields = variables
        .iter()
        .map(|(k, v)| Ok((*k, db.entity_type(*v)?)))
        .collect::<result::Result<_, ty::error::Error>>()?;
    Ok(sync::Arc::new(ty::Type::Record(ty::Record { fields })))
}

fn if_eq_then(
    lhs_entity: ir::Entity,
    lhs: &sync::Arc<ty::Type>,
    rhs_entity: ir::Entity,
    rhs: &sync::Arc<ty::Type>,
    result: &sync::Arc<ty::Type>,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    if lhs == rhs {
        Ok(result.clone())
    } else {
        Err(ty::error::Error {
            expected: ty::error::ExpectedType::Specific(lhs.clone()),
            actual: rhs.clone(),
            main_entity: rhs_entity,
            aux_entities: vec![ty::error::AuxEntity {
                entity: lhs_entity,
                label: format!("other operand has type `{}`", lhs),
            }],
        })
    }
}

fn bool_op(
    lhs_entity: ir::Entity,
    lhs: &sync::Arc<ty::Type>,
    rhs_entity: ir::Entity,
    rhs: &sync::Arc<ty::Type>,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let bool_type = db.bool_type();
    if *lhs == bool_type {
        if *rhs == bool_type {
            Ok(bool_type)
        } else {
            Err(ty::error::Error {
                expected: ty::error::ExpectedType::Specific(bool_type),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::error::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type `{}`", lhs),
                }],
            })
        }
    } else {
        Err(ty::error::Error {
            expected: ty::error::ExpectedType::Specific(bool_type),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::error::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type `{}`", rhs),
            }],
        })
    }
}

fn or_op(
    lhs_entity: ir::Entity,
    lhs: &sync::Arc<ty::Type>,
    rhs_entity: ir::Entity,
    rhs: &sync::Arc<ty::Type>,
    db: impl ty::db::TyDb,
) -> Result<ty::Type> {
    let bool_type = db.bool_type();
    if let ty::Type::Union(ref u) = **lhs {
        if let ty::Type::Symbol(ref symbol) = **rhs {
            Ok(sync::Arc::new(ty::Type::Union(u.clone().with(symbol))))
        } else {
            Err(ty::error::Error {
                expected: ty::error::ExpectedType::ScalarClass(ty::class::Scalar::Symbol),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::error::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type `{}`", lhs),
                }],
            })
        }
    } else if *lhs == bool_type {
        if *rhs == bool_type {
            Ok(bool_type)
        } else {
            Err(ty::error::Error {
                expected: ty::error::ExpectedType::Specific(bool_type),
                actual: rhs.clone(),
                main_entity: rhs_entity,
                aux_entities: vec![ty::error::AuxEntity {
                    entity: lhs_entity,
                    label: format!("other operand has type `{}`", lhs),
                }],
            })
        }
    } else {
        Err(ty::error::Error {
            expected: ty::error::ExpectedType::AnyOf(vec![
                ty::error::ExpectedType::Specific(bool_type),
                ty::error::ExpectedType::Union,
            ]),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::error::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type `{}`", rhs),
            }],
        })
    }
}

fn if_integral_then(
    entity: ir::Entity,
    ty: &sync::Arc<ty::Type>,
    result: sync::Arc<ty::Type>,
) -> Result<ty::Type> {
    match ty.scalar_class() {
        ty::class::Scalar::Integral(_) => Ok(result),
        _ => Err(ty::error::Error {
            expected: ty::error::ExpectedType::ScalarClass(ty::class::Scalar::Integral(
                ty::class::IntegralScalar::Any,
            )),
            actual: ty.clone(),
            main_entity: entity,
            aux_entities: vec![],
        }),
    }
}

fn if_integral_and_eq_then(
    lhs_entity: ir::Entity,
    lhs: &sync::Arc<ty::Type>,
    rhs_entity: ir::Entity,
    rhs: &sync::Arc<ty::Type>,
    expected: &sync::Arc<ty::Type>,
    result: sync::Arc<ty::Type>,
) -> Result<ty::Type> {
    match lhs.scalar_class() {
        ty::class::Scalar::Integral(_) => {
            if rhs == expected {
                Ok(result)
            } else {
                Err(ty::error::Error {
                    expected: ty::error::ExpectedType::Specific(expected.clone()),
                    actual: rhs.clone(),
                    main_entity: rhs_entity,
                    aux_entities: vec![ty::error::AuxEntity {
                        entity: lhs_entity,
                        label: format!("other operand has type `{}`", lhs),
                    }],
                })
            }
        }
        _ => Err(ty::error::Error {
            expected: ty::error::ExpectedType::ScalarClass(ty::class::Scalar::Integral(
                ty::class::IntegralScalar::Any,
            )),
            actual: lhs.clone(),
            main_entity: lhs_entity,
            aux_entities: vec![ty::error::AuxEntity {
                entity: rhs_entity,
                label: format!("other operand has type `{}`", rhs),
            }],
        }),
    }
}

fn if_fractional_then(
    entity: ir::Entity,
    ty: &sync::Arc<ty::Type>,
    result: sync::Arc<ty::Type>,
) -> Result<ty::Type> {
    match ty.scalar_class() {
        ty::class::Scalar::Fractional => Ok(result),
        _ => Err(ty::error::Error {
            expected: ty::error::ExpectedType::ScalarClass(ty::class::Scalar::Fractional),
            actual: ty.clone(),
            main_entity: entity,
            aux_entities: vec![],
        }),
    }
}
