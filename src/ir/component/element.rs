use std::collections;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Debug, VisitEntities)]
#[storage(VecStorage)]
pub enum Element {
    NumberValue(NumberValue),
    StringValue(StringValue),
    Tuple(Tuple),
    Record(Record),
    Reference(Reference),
    Select(Select),
    Apply(Apply),
    Parameter(Parameter),
    #[allow(unused)]
    Capture(Capture),
    Closure(Closure),
    Module(Module),
}

#[derive(Debug, VisitEntities)]
pub struct NumberValue(pub f64);

#[derive(Debug, VisitEntities)]
pub struct StringValue(pub String);

#[derive(Debug, VisitEntities)]
pub struct Tuple {
    pub fields: Vec<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Record {
    pub fields: collections::HashMap<String, specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Reference(pub String);

#[derive(Debug, VisitEntities)]
pub struct Select {
    pub record: specs::Entity,
    pub field: String,
}

#[derive(Debug, VisitEntities)]
pub struct Apply {
    pub function: specs::Entity,
    pub parameters: Vec<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Parameter {
    pub name: String,
    pub signature: Option<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Capture {
    pub name: String,
    pub captured: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Closure {
    pub captures: collections::HashMap<String, specs::Entity>,
    pub parameters: Vec<specs::Entity>,
    pub statements: Vec<specs::Entity>,
    pub signature: Option<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Module {
    pub definitions: collections::HashMap<String, specs::Entity>,
}
