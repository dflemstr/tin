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
    Variable(Variable),
    Select(Select),
    Apply(Apply),
    Parameter(Parameter),
    #[allow(unused)]
    Capture(Capture),
    Closure(Closure),
    Module(Module),
}

#[derive(Clone, Debug, VisitEntities)]
#[allow(missing_copy_implementations)]
pub enum NumberValue {
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
pub struct Variable {
    pub name: String,
    pub initializer: specs::Entity,
}

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
    pub captures: Vec<specs::Entity>,
    pub parameters: Vec<specs::Entity>,
    pub statements: Vec<specs::Entity>,
    pub signature: Option<specs::Entity>,
    pub result: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Module {
    pub variables: collections::HashMap<String, specs::Entity>,
}
