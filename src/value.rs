//! Norm value type definitions.
use std::collections;
use std::fmt;

use ast;

/// An identifier, referring to some value within a Norm scope.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

/// Any type of value.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// A number value.
    Number(f64),
    /// A string value.
    String(String),
    /// A tuple value.
    Tuple(Tuple),
    /// A record value.
    Record(Record),
    /// A function value.
    Function(Closure),
}

/// A tuple value.
#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    /// The fields of the tuple, in order.
    ///
    /// Note that each value might be of a different type, as is common with tuples.
    pub fields: Vec<Value>,
}

/// A record value.
#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    /// The fields of the record.
    ///
    /// The record fields are intentionally in a completely random order.  A different data
    /// structure should be used if order is to be preserved.
    pub fields: collections::HashMap<Identifier, Value>,
}

/// A callable closure.
#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub(crate) captured: collections::HashMap<Identifier, Value>,
    pub(crate) lambda: ast::Lambda,
}

impl Identifier {
    pub(crate) fn new(string: String) -> Self {
        Identifier(string)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<String> for Identifier {
    fn from(v: String) -> Self {
        Identifier(v)
    }
}

impl<'a> From<&'a str> for Identifier {
    fn from(v: &'a str) -> Self {
        Identifier(v.to_owned())
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Number(v)
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Value::String(v)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(v: &'a str) -> Self {
        Value::String(v.to_owned())
    }
}

macro_rules! tuple_impls {
  ($($len:tt => ($($n:tt $name:ident)+))+) => {
    $(
        impl<$($name,)+> From<($($name,)+)> for Value where $($name: Into<Value>,)+ {
            fn from(v: ($($name,)+)) -> Value {
                Value::Tuple(Tuple {
                    fields: vec![$(v.$n.into(),)+]
                })
            }
        }
    )+
  };
}

tuple_impls! {
    1  => (0 T0)
    2  => (0 T0 1 T1)
    3  => (0 T0 1 T1 2 T2)
    4  => (0 T0 1 T1 2 T2 3 T3)
    5  => (0 T0 1 T1 2 T2 3 T3 4 T4)
    6  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
    7  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
    8  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
    9  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
    10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
    11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
    12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
    13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
    14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
    15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
    16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}
