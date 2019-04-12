use std::cmp;
use std::collections;

use crate::interpreter;
use crate::ir::element;
use crate::module;
use crate::value;

pub mod db;
pub mod error;
#[macro_use]
mod macros;

pub fn eval(
    element: &element::Element,
    db: &impl interpreter::db::InterpreterDb,
) -> Result<Option<value::Value>, error::Error> {
    match element {
        element::Element::Number(v) => Ok(Some(value::Value::number(eval_number(v)))),
        element::Element::String(ref v) => Ok(Some(value::Value::string(v.as_str()))),
        element::Element::Symbol(element::Symbol { ref label }) => {
            Ok(Some(value::Value::symbol(label.as_str())))
        }
        element::Element::Tuple(element::Tuple { ref fields }) => Ok(fields
            .iter()
            .map(|f| db.entity_value(*f))
            .collect::<Result<Option<Vec<_>>, error::Error>>()?
            .map(|fields| value::Value::tuple(value::Tuple { fields }))),
        element::Element::Record(element::Record { ref fields }) => Ok(fields
            .iter()
            .map(|(k, f)| Ok(db.entity_value(*f)?.map(|v| (k.clone(), v))))
            .collect::<Result<Option<collections::HashMap<_, _>>, error::Error>>()?
            .map(|fields| value::Value::record(value::Record { fields }))),
        element::Element::UnOp(element::UnOp { operator, operand }) => transpose(
            db.entity_value(*operand)?
                .map(|operand| eval_un_op(*operator, &operand)),
        ),
        element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
            let lhs_value = db.entity_value(*lhs)?;
            let rhs_value = db.entity_value(*rhs)?;
            match (lhs_value, rhs_value) {
                (Some(ref lhs), Some(ref rhs)) => eval_bi_op(lhs, *operator, rhs).map(Some),
                _ => Ok(None),
            }
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            db.entity_value(*initializer)
        }
        element::Element::Select(element::Select { record, field }) => {
            transpose(db.entity_value(*record)?.map(|record| match record.case() {
                value::Case::Record(r) => Ok(r.fields[field].clone()),
                other => Err(error::Error::RuntimeTypeConflict(format!(
                    "not a record: {:?}",
                    other
                ))),
            }))
        }
        _ => Ok(None), // TODO
    }
}

fn eval_number(number: &element::Number) -> value::Number {
    match *number {
        element::Number::U8(n) => value::Number::U8(n),
        element::Number::U16(n) => value::Number::U16(n),
        element::Number::U32(n) => value::Number::U32(n),
        element::Number::U64(n) => value::Number::U64(n),
        element::Number::I8(n) => value::Number::I8(n),
        element::Number::I16(n) => value::Number::I16(n),
        element::Number::I32(n) => value::Number::I32(n),
        element::Number::I64(n) => value::Number::I64(n),
        element::Number::F32(n) => value::Number::F32(n),
        element::Number::F64(n) => value::Number::F64(n),
    }
}

fn eval_un_op(
    operator: element::UnOperator,
    operand: &value::Value,
) -> Result<value::Value, error::Error> {
    match operator {
        element::UnOperator::Not => Ok((!to_bool(operand)?).into()),
        element::UnOperator::BNot => match_integral_value!("~!", (operand), |v| Ok((!v).into())),
        element::UnOperator::Cl0 => {
            match_integral_value!("#^0", (operand), |v| Ok(v.leading_zeros().into()))
        }
        element::UnOperator::Cl1 => {
            match_integral_value!("#^1", (operand), |v| Ok((!v).leading_zeros().into()))
        }
        element::UnOperator::Cls => unimplemented!(),
        element::UnOperator::Ct0 => {
            match_integral_value!("#$0", (operand), |v| Ok(v.trailing_zeros().into()))
        }
        element::UnOperator::Ct1 => {
            match_integral_value!("#$1", (operand), |v| Ok((!v).trailing_zeros().into()))
        }
        element::UnOperator::C0 => {
            match_integral_value!("#0", (operand), |v| Ok(v.count_zeros().into()))
        }
        element::UnOperator::C1 => {
            match_integral_value!("#1", (operand), |v| Ok(v.count_ones().into()))
        }
        element::UnOperator::Sqrt => {
            match_fractional_value!("^/", (operand), |v| Ok(v.sqrt().into()))
        }
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
fn eval_bi_op(
    lhs: &value::Value,
    operator: element::BiOperator,
    rhs: &value::Value,
) -> Result<value::Value, error::Error> {
    match operator {
        element::BiOperator::Eq => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o == cmp::Ordering::Equal)).into())
        }
        element::BiOperator::Ne => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o != cmp::Ordering::Equal)).into())
        }
        element::BiOperator::Lt => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o == cmp::Ordering::Less)).into())
        }
        element::BiOperator::Ge => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o != cmp::Ordering::Less)).into())
        }
        element::BiOperator::Gt => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o == cmp::Ordering::Greater)).into())
        }
        element::BiOperator::Le => {
            Ok((cmp_value(lhs, rhs)?.map_or(false, |o| o != cmp::Ordering::Greater)).into())
        }
        element::BiOperator::Cmp => Ok(cmp_value(lhs, rhs)?.into()),
        element::BiOperator::Add => add(lhs, rhs),
        element::BiOperator::Sub => match_number_value!("-", (lhs, rhs), |l, r| int: Ok(
            (l.wrapping_sub(*r)).into()
        ), frac: Ok(
            (l.into_inner() - r.into_inner()).into()
        )),
        element::BiOperator::Mul => match_number_value!("*", (lhs, rhs), |l, r| int: Ok(
            (l.wrapping_mul(*r)).into()
        ), frac: Ok(
            (l.into_inner() * r.into_inner()).into()
        )),
        element::BiOperator::Div => match_number_value!("/", (lhs, rhs), |l, r| int: if *r == 0 {
            Err(error::Error::EvaluationError(module::Error::new(module::ErrorKind::IntegerDivisonByZero)))
        } else {
            Ok((l.wrapping_div(*r)).into())
        }, frac: Ok((l.into_inner() / r.into_inner()).into()
        )),
        element::BiOperator::Rem => match_number_value!("%", (lhs, rhs), |l, r| int: if *r == 0 {
            Err(error::Error::EvaluationError(module::Error::new(module::ErrorKind::IntegerDivisonByZero)))
        } else {
            Ok((l.wrapping_rem(*r)).into())
        }, frac: Ok(
            (l.into_inner() % r.into_inner()).into()
        )),
        element::BiOperator::And => bool_op("&", lhs, rhs, |l, r| l & r),
        element::BiOperator::BAnd => {
            match_integral_value!("~&", (lhs, rhs), |l, r| Ok((l & r).into()))
        }
        element::BiOperator::Or => bool_op("|", lhs, rhs, |l, r| l | r),
        element::BiOperator::BOr => {
            match_integral_value!("~|", (lhs, rhs), |l, r| Ok((l | r).into()))
        }
        element::BiOperator::Xor => bool_op("|", lhs, rhs, |l, r| l ^ r),
        element::BiOperator::BXor => {
            match_integral_value!("~^", (lhs, rhs), |l, r| Ok((l ^ r).into()))
        }
        element::BiOperator::AndNot => bool_op("&!", lhs, rhs, |l, r| l & !r),
        element::BiOperator::BAndNot => {
            match_integral_value!("~&!", (lhs, rhs), |l, r| Ok((l & !r).into()))
        }
        element::BiOperator::OrNot => bool_op("|!", lhs, rhs, |l, r| l | !r),
        element::BiOperator::BOrNot => {
            match_integral_value!("~|!", (lhs, rhs), |l, r| Ok((l | !r).into()))
        }
        element::BiOperator::XorNot => bool_op("^!", lhs, rhs, |l, r| l ^ !r),
        element::BiOperator::BXorNot => {
            match_integral_value!("~^!", (lhs, rhs), |l, r| Ok((l ^ !r).into()))
        }
        element::BiOperator::RotL => {
            match_integral_value!("<-<", (lhs), |l| Ok((l.rotate_left(to_u32(rhs)?)).into()))
        }
        element::BiOperator::RotR => {
            match_integral_value!(">->", (lhs), |l| Ok((l.rotate_right(to_u32(rhs)?)).into()))
        }
        element::BiOperator::ShL => {
            match_integral_value!("<<", (lhs), |l| Ok((l << to_u32(rhs)?).into()))
        }
        element::BiOperator::ShR => {
            match_integral_value!(">>", (lhs), |l| Ok((l >> to_u32(rhs)?).into()))
        }
    }
}

fn to_bool(value: &value::Value) -> Result<bool, error::Error> {
    if value == &*value::TRUE {
        Ok(true)
    } else if value == &*value::FALSE {
        Ok(false)
    } else {
        Err(error::Error::RuntimeTypeConflict(format!(
            "not a bool value: {:?}",
            value
        )))
    }
}

fn to_u32(value: &value::Value) -> Result<u32, error::Error> {
    match *value.case() {
        value::Case::Number(ref n) => match *n {
            value::Number::U32(n) => Ok(n),
            _ => Err(error::Error::RuntimeTypeConflict(format!(
                "not an u32: {:?}",
                value
            ))),
        },
        _ => Err(error::Error::RuntimeTypeConflict(format!(
            "not an u32: {:?}",
            value
        ))),
    }
}

fn cmp_value(
    lhs: &value::Value,
    rhs: &value::Value,
) -> Result<Option<cmp::Ordering>, error::Error> {
    match (lhs.case(), rhs.case()) {
        (value::Case::Number(lhs), value::Case::Number(rhs)) => cmp_number(lhs, rhs),
        (value::Case::String(lhs), value::Case::String(rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Case::Symbol(lhs), value::Case::Symbol(rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Case::Tuple(lhs), value::Case::Tuple(rhs)) => cmp_tuple(lhs, rhs),
        (value::Case::Record(lhs), value::Case::Record(rhs)) => cmp_record(lhs, rhs),
        _ => Err(error::Error::RuntimeTypeConflict(format!(
            "type mismatch; type of {:?} != type of {:?}",
            lhs, rhs
        ))),
    }
}

fn cmp_number(
    lhs: &value::Number,
    rhs: &value::Number,
) -> Result<Option<cmp::Ordering>, error::Error> {
    match (*lhs, *rhs) {
        (value::Number::U8(ref lhs), value::Number::U8(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::U16(ref lhs), value::Number::U16(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::U32(ref lhs), value::Number::U32(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::U64(ref lhs), value::Number::U64(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::I8(ref lhs), value::Number::I8(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::I16(ref lhs), value::Number::I16(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::I32(ref lhs), value::Number::I32(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::I64(ref lhs), value::Number::I64(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::F32(ref lhs), value::Number::F32(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        (value::Number::F64(ref lhs), value::Number::F64(ref rhs)) => Ok(lhs.partial_cmp(rhs)),
        _ => Err(error::Error::RuntimeTypeConflict(format!(
            "type mismatch; type of {:?} != type of {:?}",
            lhs, rhs
        ))),
    }
}

fn cmp_tuple(
    lhs: &value::Tuple,
    rhs: &value::Tuple,
) -> Result<Option<cmp::Ordering>, error::Error> {
    if lhs.fields.len() == rhs.fields.len() {
        for (lhs, rhs) in lhs.fields.iter().zip(rhs.fields.iter()) {
            let result = cmp_value(lhs, rhs)?;
            if result != Some(cmp::Ordering::Equal) {
                return Ok(result);
            }
        }
        Ok(Some(cmp::Ordering::Equal))
    } else {
        Err(error::Error::RuntimeTypeConflict(format!(
            "cannot compare tuples of different length: {} vs {}",
            lhs.fields.len(),
            rhs.fields.len()
        )))
    }
}

fn cmp_record(
    lhs: &value::Record,
    rhs: &value::Record,
) -> Result<Option<cmp::Ordering>, error::Error> {
    let rhs = &rhs.fields;
    let lhs = &lhs.fields;
    if lhs.len() == rhs.len() && lhs.keys().all(|k| rhs.contains_key(k)) {
        for k in lhs.keys() {
            let result = cmp_value(&lhs[k], &rhs[k])?;
            if result != Some(cmp::Ordering::Equal) {
                return Ok(result);
            }
        }
        Ok(Some(cmp::Ordering::Equal))
    } else {
        Err(error::Error::RuntimeTypeConflict(format!(
            "cannot compare records with different fields: {:?} vs {:?}",
            lhs.keys().collect::<Vec<_>>(),
            rhs.keys().collect::<Vec<_>>()
        )))
    }
}

fn add(lhs: &value::Value, rhs: &value::Value) -> Result<value::Value, error::Error> {
    match (lhs.case(), rhs.case()) {
        (value::Case::String(lhsv), value::Case::String(rhsv)) => {
            if lhsv.is_empty() {
                Ok(lhs.clone())
            } else if rhsv.is_empty() {
                Ok(rhs.clone())
            } else {
                Ok(value::Value::string(lhsv.clone() + rhsv))
            }
        }
        (value::Case::Number(lhs), value::Case::Number(rhs)) => {
            match_number!("+", (lhs, rhs), |l, r| int: Ok((l.wrapping_add(*r)).into()), frac: Ok((l.into_inner() + r.into_inner()).into()))
        }
        other => Err(error::Error::RuntimeTypeConflict(format!(
            "operation + not supported on values {:?}",
            other
        ))),
    }
}

fn bool_op<F>(
    _name: &str,
    lhs: &value::Value,
    rhs: &value::Value,
    op: F,
) -> Result<value::Value, error::Error>
where
    F: FnOnce(bool, bool) -> bool,
{
    let lhs = to_bool(lhs)?;
    let rhs = to_bool(rhs)?;

    Ok(op(lhs, rhs).into())
}

// TODO: awaits https://github.com/rust-lang/rust/issues/47338
fn transpose<A, E>(option: Option<Result<A, E>>) -> Result<Option<A>, E> {
    match option {
        Some(Ok(x)) => Ok(Some(x)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}
