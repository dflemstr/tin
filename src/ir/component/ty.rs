use std::collections;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, Eq, PartialEq, VisitEntities)]
#[storage(VecStorage)]
pub enum Type {
    Number,
    String,
    Tuple(Tuple),
    Record(Record),
    Function(Function),
    Conflict(Conflict),
    Any,
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
