use std::cmp;
use std::collections;
use std::sync;

lazy_static! {
    pub static ref NIL: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("nil".to_owned()),
        })
    };
    pub static ref FALSE: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("f".to_owned()),
        })
    };
    pub static ref TRUE: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("t".to_owned()),
        })
    };
    pub static ref LT: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("lt".to_owned()),
        })
    };
    pub static ref EQ: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("eq".to_owned()),
        })
    };
    pub static ref GT: Value = {
        Value::Symbol(Symbol {
            label: sync::Arc::new("gt".to_owned()),
        })
    };
}

#[derive(Clone, Debug, PartialEq, VisitEntities)]
pub enum Value {
    Number(Number),
    String(sync::Arc<String>),
    Symbol(Symbol),
    Tuple(Tuple),
    Record(Record),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, VisitEntities)]
pub enum Number {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, VisitEntities)]
pub struct Symbol {
    pub label: sync::Arc<String>,
}

#[derive(Clone, Debug, PartialEq, VisitEntities)]
pub struct Tuple {
    pub fields: Vec<sync::Arc<Value>>,
}

#[derive(Clone, Debug, PartialEq, VisitEntities)]
pub struct Record {
    pub fields: collections::HashMap<String, sync::Arc<Value>>,
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        if v {
            TRUE.clone()
        } else {
            FALSE.clone()
        }
    }
}

impl From<u8> for Value {
    fn from(v: u8) -> Self {
        Value::Number(Number::U8(v))
    }
}

impl From<u16> for Value {
    fn from(v: u16) -> Self {
        Value::Number(Number::U16(v))
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Value::Number(Number::U32(v))
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Value::Number(Number::U64(v))
    }
}

impl From<i8> for Value {
    fn from(v: i8) -> Self {
        Value::Number(Number::I8(v))
    }
}

impl From<i16> for Value {
    fn from(v: i16) -> Self {
        Value::Number(Number::I16(v))
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Value::Number(Number::I32(v))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Number(Number::I64(v))
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Value::Number(Number::F32(v))
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Number(Number::F64(v))
    }
}

impl From<cmp::Ordering> for Value {
    fn from(v: cmp::Ordering) -> Self {
        match v {
            cmp::Ordering::Less => LT.clone(),
            cmp::Ordering::Equal => EQ.clone(),
            cmp::Ordering::Greater => GT.clone(),
        }
    }
}

impl<A> From<Option<A>> for Value
where
    A: Into<Value>,
{
    fn from(v: Option<A>) -> Self {
        v.map(Into::into).unwrap_or_else(|| NIL.clone())
    }
}
