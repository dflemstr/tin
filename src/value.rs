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
