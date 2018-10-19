use std::borrow;
use std::collections;

#[derive(Clone, Debug, PartialEq)]
pub struct Norm<'a> {
    pub definitions: collections::HashMap<Identifier<'a>, Expression<'a>>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier<'a>(pub borrow::Cow<'a, str>);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Number(f64),
    String(borrow::Cow<'a, str>),
    Tuple(Tuple<'a>),
    Record(Record<'a>),
    Identifier(Identifier<'a>),
    Lambda(Lambda<'a>),
    Select(Select<'a>),
    Apply(Apply<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple<'a> {
    pub fields: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Record<'a> {
    pub fields: collections::HashMap<Identifier<'a>, Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lambda<'a> {
    pub parameters: Vec<Parameter<'a>>,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Definition(Identifier<'a>, Expression<'a>),
    Expression(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Select<'a> {
    pub expression: Box<Expression<'a>>,
    pub field: Identifier<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Apply<'a> {
    pub function: Box<Expression<'a>>,
    pub parameters: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter<'a> {
    pub name: Identifier<'a>,
    pub signature: Option<Expression<'a>>,
}
