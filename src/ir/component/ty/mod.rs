use std::collections;
use std::fmt;

use specs::Component;
use specs::VecStorage;

pub mod class;
pub mod error;

#[derive(Component, Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
#[storage(VecStorage)]
pub enum Type {
    Number(Number),
    String,
    Symbol(Symbol),
    Tuple(Tuple),
    Union(Union),
    Record(Record),
    Function(Function),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
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

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, VisitEntities, VisitEntitiesMut)]
pub struct Symbol {
    pub label: String,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct Tuple {
    pub fields: Vec<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct Union {
    pub alternatives: Vec<Symbol>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct Record {
    pub fields: collections::HashMap<String, Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct Function {
    pub parameters: Vec<Type>,
    pub result: Box<Type>,
}

impl Type {
    pub fn scalar_class(&self) -> class::Scalar {
        match *self {
            Type::Number(ref n) => n.scalar_class(),
            Type::Symbol(_) => class::Scalar::Symbol,
            Type::Union(_) | Type::Function(_) => class::Scalar::Undefined,
            Type::String | Type::Tuple(_) | Type::Record(_) => class::Scalar::Complex,
        }
    }
}

impl Number {
    pub fn scalar_class(self) -> class::Scalar {
        match self {
            Number::U8 | Number::U16 | Number::U32 | Number::U64 => {
                class::Scalar::Integral(class::IntegralScalar::Unsigned)
            }
            Number::I8 | Number::I16 | Number::I32 | Number::I64 => {
                class::Scalar::Integral(class::IntegralScalar::Signed)
            }
            Number::F32 | Number::F64 => class::Scalar::Fractional,
        }
    }
}

impl Union {
    pub fn with(mut self, symbol: &Symbol) -> Union {
        match self.alternatives.binary_search(symbol) {
            Ok(_) => {}
            Err(n) => self.alternatives.insert(n, symbol.clone()),
        }

        self
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number(ref number) => number.fmt(f),
            Type::String => write!(f, "str"),
            Type::Symbol(ref label) => label.fmt(f),
            Type::Tuple(ref tuple) => tuple.fmt(f),
            Type::Union(ref union) => union.fmt(f),
            Type::Record(ref record) => record.fmt(f),
            Type::Function(ref function) => function.fmt(f),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Number::U8 => write!(f, "u8"),
            Number::U16 => write!(f, "u16"),
            Number::U32 => write!(f, "u32"),
            Number::U64 => write!(f, "u64"),
            Number::I8 => write!(f, "i8"),
            Number::I16 => write!(f, "i16"),
            Number::I32 => write!(f, "i32"),
            Number::I64 => write!(f, "i64"),
            Number::F32 => write!(f, "f32"),
            Number::F64 => write!(f, "f64"),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ":{}", self.label)
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut needs_sep = false;
        for ty in &self.fields {
            if needs_sep {
                write!(f, ",")?;
            }
            ty.fmt(f)?;
            needs_sep = true;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl fmt::Display for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut needs_sep = false;
        for ty in &self.alternatives {
            if needs_sep {
                write!(f, "|")?;
            }
            ty.fmt(f)?;
            needs_sep = true;
        }
        Ok(())
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let mut needs_sep = false;
        for (id, ty) in &self.fields {
            if needs_sep {
                write!(f, ",")?;
            }
            write!(f, "{}: ", id)?;
            ty.fmt(f)?;
            needs_sep = true;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "|")?;
        let mut needs_sep = false;
        for ty in &self.parameters {
            if needs_sep {
                write!(f, ",")?;
            }
            ty.fmt(f)?;
            needs_sep = true;
        }
        write!(f, "|")?;
        self.result.fmt(f)?;
        Ok(())
    }
}
