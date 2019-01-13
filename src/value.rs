use std::cmp;
use std::collections;
use std::sync;

lazy_static! {
    pub static ref NIL: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "nil".to_owned(),
        }))
    };
    pub static ref FALSE: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "f".to_owned(),
        }))
    };
    pub static ref TRUE: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "t".to_owned(),
        }))
    };
    pub static ref LT: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "lt".to_owned(),
        }))
    };
    pub static ref EQ: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "eq".to_owned(),
        }))
    };
    pub static ref GT: Value = {
        Value::new(ValueCase::Symbol(Symbol {
            label: "gt".to_owned(),
        }))
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value(sync::Arc<ValueCase>);

#[derive(Clone, Debug, PartialEq)]
pub enum ValueCase {
    Number(Number),
    String(String),
    Symbol(Symbol),
    Tuple(Tuple),
    Record(Record),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
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

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Symbol {
    pub label: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    pub fields: Vec<Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    pub fields: collections::HashMap<String, Value>,
}

impl Value {
    pub fn new(storage: ValueCase) -> Self {
        Value(sync::Arc::new(storage))
    }

    pub fn number(number: Number) -> Self {
        Value::new(ValueCase::Number(number))
    }

    pub fn string<S>(string: S) -> Self
    where
        S: ToString,
    {
        Value(sync::Arc::new(ValueCase::String(string.to_string())))
    }

    pub fn symbol<S>(label: S) -> Self
    where
        S: ToString,
    {
        let label = label.to_string();
        Value::new(ValueCase::Symbol(Symbol { label }))
    }

    pub fn tuple(tuple: Tuple) -> Self {
        Value::new(ValueCase::Tuple(tuple))
    }

    pub fn record(record: Record) -> Self {
        Value::new(ValueCase::Record(record))
    }

    pub fn case(&self) -> &ValueCase {
        &self.0
    }
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
        Value::new(ValueCase::Number(Number::U8(v)))
    }
}

impl From<u16> for Value {
    fn from(v: u16) -> Self {
        Value::new(ValueCase::Number(Number::U16(v)))
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Value::new(ValueCase::Number(Number::U32(v)))
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Value::new(ValueCase::Number(Number::U64(v)))
    }
}

impl From<i8> for Value {
    fn from(v: i8) -> Self {
        Value::new(ValueCase::Number(Number::I8(v)))
    }
}

impl From<i16> for Value {
    fn from(v: i16) -> Self {
        Value::new(ValueCase::Number(Number::I16(v)))
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Value::new(ValueCase::Number(Number::I32(v)))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::new(ValueCase::Number(Number::I64(v)))
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Value::new(ValueCase::Number(Number::F32(v)))
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::new(ValueCase::Number(Number::F64(v)))
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
