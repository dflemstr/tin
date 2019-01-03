use std::collections;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, Eq, PartialEq, VisitEntities)]
#[storage(VecStorage)]
pub enum Type {
    Boolean,
    Number(Number),
    String,
    Symbol(String),
    Tuple(Tuple),
    Record(Record),
    Function(Function),
    Conflict(Conflict),
    Any,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities)]
pub enum ScalarClass {
    Void,
    Boolean,
    Integral(IntegralScalarClass),
    Fractional,
    Complex,
    Undefined,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities)]
pub enum IntegralScalarClass {
    Unsigned,
    Signed,
    Any,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities)]
pub enum Number {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
pub struct Tuple {
    pub fields: Vec<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
pub struct Record {
    pub fields: collections::HashMap<String, Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
pub struct Function {
    pub parameters: Vec<Type>,
    pub result: Box<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
pub struct Conflict {
    pub expected: ExpectedType,
    pub actual: Box<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
pub enum ExpectedType {
    Specific(Box<Type>),
    ScalarClass(ScalarClass),
}

impl Type {
    pub fn scalar_class(&self) -> ScalarClass {
        match *self {
            Type::Boolean => ScalarClass::Boolean,
            Type::Number(ref n) => n.scalar_class(),
            Type::String => ScalarClass::Complex,
            Type::Symbol(_) => ScalarClass::Void,
            Type::Tuple(_) => ScalarClass::Complex,
            Type::Record(_) => ScalarClass::Complex,
            Type::Function(_) => ScalarClass::Undefined,
            Type::Conflict(_) => ScalarClass::Undefined,
            Type::Any => ScalarClass::Undefined,
        }
    }
}

impl Number {
    pub fn scalar_class(&self) -> ScalarClass {
        match *self {
            Number::U8 => ScalarClass::Integral(IntegralScalarClass::Unsigned),
            Number::U16 => ScalarClass::Integral(IntegralScalarClass::Unsigned),
            Number::U32 => ScalarClass::Integral(IntegralScalarClass::Unsigned),
            Number::U64 => ScalarClass::Integral(IntegralScalarClass::Unsigned),
            Number::I8 => ScalarClass::Integral(IntegralScalarClass::Signed),
            Number::I16 => ScalarClass::Integral(IntegralScalarClass::Signed),
            Number::I32 => ScalarClass::Integral(IntegralScalarClass::Signed),
            Number::I64 => ScalarClass::Integral(IntegralScalarClass::Signed),
            Number::F32 => ScalarClass::Fractional,
            Number::F64 => ScalarClass::Fractional,
        }
    }
}
