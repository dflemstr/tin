use std::collections;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, Eq, PartialEq, VisitEntities)]
#[storage(VecStorage)]
pub enum Type {
    Number(Number),
    String,
    Tuple(Tuple),
    Record(Record),
    Function(Function),
    Conflict(Conflict),
    Any,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities)]
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
    pub expected: Box<Type>,
    pub actual: Box<Type>,
}
