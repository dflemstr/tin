use std::cmp;
use std::collections;
use std::sync;

use specs;

use crate::ir::component::element;
use crate::value;

#[macro_use]
mod macros;

pub fn eval<'a, F>(element: &element::Element, lookup: F) -> Option<sync::Arc<value::Value>>
where
    F: Fn(specs::Entity) -> Option<&'a sync::Arc<value::Value>>,
{
    match element {
        element::Element::Number(v) => Some(sync::Arc::new(value::Value::Number(eval_number(v)))),
        element::Element::String(ref v) => Some(sync::Arc::new(value::Value::String(
            sync::Arc::new(v.clone()),
        ))),
        element::Element::Symbol(element::Symbol { ref label }) => {
            Some(sync::Arc::new(value::Value::Symbol(value::Symbol {
                label: sync::Arc::new(label.clone()),
            })))
        }
        element::Element::Tuple(element::Tuple { ref fields }) => fields
            .iter()
            .map(|f| lookup(*f).cloned())
            .collect::<Option<Vec<_>>>()
            .map(|fields| sync::Arc::new(value::Value::Tuple(value::Tuple { fields }))),
        element::Element::Record(element::Record { ref fields }) => fields
            .iter()
            .map(|(k, f)| lookup(*f).map(|v| (k.clone(), v.clone())))
            .collect::<Option<collections::HashMap<_, _>>>()
            .map(|fields| sync::Arc::new(value::Value::Record(value::Record { fields }))),
        element::Element::UnOp(element::UnOp { operator, operand }) => {
            lookup(*operand).map(|operand| eval_un_op(*operator, &operand))
        }
        element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
            lookup(*lhs).and_then(|lhs| lookup(*rhs).map(|rhs| eval_bi_op(&lhs, *operator, &rhs)))
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            lookup(*initializer).cloned()
        }
        element::Element::Select(element::Select { record, field }) => {
            lookup(*record).map(|record| match &**record {
                value::Value::Record(r) => r.fields[field].clone(),
                other => panic!("not a record: {:?}", other),
            })
        }
        element::Element::Apply(element::Apply { .. }) => None, // TODO
        element::Element::Parameter(element::Parameter { .. }) => None, // TODO
        element::Element::Capture(element::Capture { .. }) => None, // TODO
        element::Element::Closure(element::Closure { .. }) => None, // TODO
        element::Element::Module(element::Module { .. }) => None, // TODO
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
    operand: &sync::Arc<value::Value>,
) -> sync::Arc<value::Value> {
    match operator {
        element::UnOperator::Not => sync::Arc::new((!to_bool(operand)).into()),
        element::UnOperator::BNot => {
            match_integral_value!("~!", (**operand), |v| sync::Arc::new((!v).into()))
        }
        element::UnOperator::Cl0 => match_integral_value!("#^0", (**operand), |v| sync::Arc::new(
            v.leading_zeros().into()
        )),
        element::UnOperator::Cl1 => match_integral_value!("#^1", (**operand), |v| sync::Arc::new(
            (!v).leading_zeros().into()
        )),
        element::UnOperator::Cls => unimplemented!(),
        element::UnOperator::Ct0 => match_integral_value!("#$0", (**operand), |v| sync::Arc::new(
            v.trailing_zeros().into()
        )),
        element::UnOperator::Ct1 => match_integral_value!("#$1", (**operand), |v| sync::Arc::new(
            (!v).trailing_zeros().into()
        )),
        element::UnOperator::C0 => match_integral_value!("#0", (**operand), |v| sync::Arc::new(
            v.count_zeros().into()
        )),
        element::UnOperator::C1 => {
            match_integral_value!("#1", (**operand), |v| sync::Arc::new(v.count_ones().into()))
        }
        element::UnOperator::Sqrt => {
            match_fractional_value!("^/", (**operand), |v| sync::Arc::new(v.sqrt().into()))
        }
    }
}

fn eval_bi_op(
    lhs: &sync::Arc<value::Value>,
    operator: element::BiOperator,
    rhs: &sync::Arc<value::Value>,
) -> sync::Arc<value::Value> {
    match operator {
        element::BiOperator::Eq => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o == cmp::Ordering::Equal)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Ne => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o != cmp::Ordering::Equal)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Lt => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o == cmp::Ordering::Less)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Ge => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o != cmp::Ordering::Less)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Gt => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o == cmp::Ordering::Greater)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Le => sync::Arc::new(
            (cmp_value(lhs, rhs)
                .map(|o| o != cmp::Ordering::Greater)
                .unwrap_or(false))
            .into(),
        ),
        element::BiOperator::Cmp => sync::Arc::new(cmp_value(lhs, rhs).into()),
        element::BiOperator::Add => add(lhs, rhs),
        element::BiOperator::Sub => {
            match_number_value!("-", (&**lhs, &**rhs), |l, r| sync::Arc::new((l - r).into()))
        }
        element::BiOperator::Mul => {
            match_number_value!("*", (&**lhs, &**rhs), |l, r| sync::Arc::new((l * r).into()))
        }
        element::BiOperator::Div => {
            match_number_value!("/", (&**lhs, &**rhs), |l, r| sync::Arc::new((l / r).into()))
        }
        element::BiOperator::Rem => {
            match_number_value!("/", (&**lhs, &**rhs), |l, r| sync::Arc::new((l % r).into()))
        }
        element::BiOperator::And => unimplemented!(),
        element::BiOperator::BAnd => {
            match_integral_value!("~&", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l & r).into()
            ))
        }
        element::BiOperator::Or => unimplemented!(),
        element::BiOperator::BOr => {
            match_integral_value!("~|", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l | r).into()
            ))
        }
        element::BiOperator::Xor => unimplemented!(),
        element::BiOperator::BXor => {
            match_integral_value!("~^", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l ^ r).into()
            ))
        }
        element::BiOperator::AndNot => unimplemented!(),
        element::BiOperator::BAndNot => {
            match_integral_value!("~&!", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l & !r).into()
            ))
        }
        element::BiOperator::OrNot => unimplemented!(),
        element::BiOperator::BOrNot => {
            match_integral_value!("~|!", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l | !r).into()
            ))
        }
        element::BiOperator::XorNot => unimplemented!(),
        element::BiOperator::BXorNot => {
            match_integral_value!("~^!", (&**lhs, &**rhs), |l, r| sync::Arc::new(
                (l ^ !r).into()
            ))
        }
        element::BiOperator::RotL => match_integral_value!("<-<", (&**lhs), |l| sync::Arc::new(
            (l.rotate_left(to_u32(rhs))).into()
        )),
        element::BiOperator::RotR => match_integral_value!(">->", (&**lhs), |l| sync::Arc::new(
            (l.rotate_right(to_u32(rhs))).into()
        )),
        element::BiOperator::ShL => match_integral_value!("<<", (&**lhs), |l| sync::Arc::new(
            (l << to_u32(rhs)).into()
        )),
        element::BiOperator::ShR => match_integral_value!(">>", (&**lhs), |l| sync::Arc::new(
            (l >> to_u32(rhs)).into()
        )),
    }
}

fn to_bool(value: &value::Value) -> bool {
    if value == &*value::TRUE {
        true
    } else if value == &*value::FALSE {
        false
    } else {
        panic!("not a bool value: {:?}", value)
    }
}

fn to_u32(value: &value::Value) -> u32 {
    match *value {
        value::Value::Number(n) => match n {
            value::Number::U32(n) => n,
            _ => panic!("not an u32: {:?}", value),
        },
        _ => panic!("not an u32: {:?}", value),
    }
}

fn cmp_value(lhs: &value::Value, rhs: &value::Value) -> Option<cmp::Ordering> {
    match (lhs, rhs) {
        (value::Value::Number(lhs), value::Value::Number(rhs)) => cmp_number(lhs, rhs),
        (value::Value::String(lhs), value::Value::String(rhs)) => lhs.partial_cmp(rhs),
        (value::Value::Symbol(lhs), value::Value::Symbol(rhs)) => lhs.partial_cmp(rhs),
        (value::Value::Tuple(lhs), value::Value::Tuple(rhs)) => cmp_tuple(lhs, rhs),
        (value::Value::Record(lhs), value::Value::Record(rhs)) => cmp_record(lhs, rhs),
        _ => panic!("type mismatch; type of {:?} != type of {:?}", lhs, rhs),
    }
}

fn cmp_number(lhs: &value::Number, rhs: &value::Number) -> Option<cmp::Ordering> {
    match (lhs, rhs) {
        (value::Number::U8(lhs), value::Number::U8(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::U16(lhs), value::Number::U16(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::U32(lhs), value::Number::U32(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::U64(lhs), value::Number::U64(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::I8(lhs), value::Number::I8(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::I16(lhs), value::Number::I16(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::I32(lhs), value::Number::I32(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::I64(lhs), value::Number::I64(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::F32(lhs), value::Number::F32(rhs)) => lhs.partial_cmp(rhs),
        (value::Number::F64(lhs), value::Number::F64(rhs)) => lhs.partial_cmp(rhs),
        _ => panic!("type mismatch; type of {:?} != type of {:?}", lhs, rhs),
    }
}

fn cmp_tuple(lhs: &value::Tuple, rhs: &value::Tuple) -> Option<cmp::Ordering> {
    if lhs.fields.len() == rhs.fields.len() {
        for (lhs, rhs) in lhs.fields.iter().zip(rhs.fields.iter()) {
            let result = cmp_value(&**lhs, &**rhs);
            if result != Some(cmp::Ordering::Equal) {
                return result;
            }
        }
        Some(cmp::Ordering::Equal)
    } else {
        panic!(
            "cannot compare tuples of different length: {} vs {}",
            lhs.fields.len(),
            rhs.fields.len()
        )
    }
}

fn cmp_record(lhs: &value::Record, rhs: &value::Record) -> Option<cmp::Ordering> {
    let rhs = &rhs.fields;
    let lhs = &lhs.fields;
    if lhs.len() == rhs.len() && lhs.keys().all(|k| rhs.contains_key(k)) {
        for k in lhs.keys() {
            let result = cmp_value(&*lhs[k], &*rhs[k]);
            if result != Some(cmp::Ordering::Equal) {
                return result;
            }
        }
        Some(cmp::Ordering::Equal)
    } else {
        panic!(
            "cannot compare records with different fields: {:?} vs {:?}",
            lhs.keys().collect::<Vec<_>>(),
            rhs.keys().collect::<Vec<_>>()
        )
    }
}

fn add(lhs: &sync::Arc<value::Value>, rhs: &sync::Arc<value::Value>) -> sync::Arc<value::Value> {
    match (&**lhs, &**rhs) {
        (value::Value::String(lhsv), value::Value::String(rhsv)) => {
            if lhsv.is_empty() {
                lhs.clone()
            } else if rhsv.is_empty() {
                rhs.clone()
            } else {
                sync::Arc::new(value::Value::String(sync::Arc::new(
                    (**lhsv).clone() + rhsv,
                )))
            }
        }
        (value::Value::Number(lhs), value::Value::Number(rhs)) => {
            match_number!("+", (lhs, rhs), |l, r| sync::Arc::new((l + r).into()))
        }
        other => panic!("operation + not supported on values {:?}", other),
    }
}
