use std::collections;
use std::fmt;

use specs::Component;
use specs::VecStorage;

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
    Conflict(Conflict),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum ScalarClass {
    Symbol,
    Integral(IntegralScalarClass),
    Fractional,
    Complex,
    Undefined,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum IntegralScalarClass {
    Unsigned,
    Signed,
    Any,
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

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct Conflict {
    pub expected: ExpectedType,
    pub actual: Box<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum ExpectedType {
    Specific(Box<Type>),
    ScalarClass(ScalarClass),
    AnyOf(Vec<ExpectedType>),
    Union,
}

impl Type {
    pub fn scalar_class(&self) -> ScalarClass {
        match *self {
            Type::Number(ref n) => n.scalar_class(),
            Type::String => ScalarClass::Complex,
            Type::Symbol(_) => ScalarClass::Symbol,
            Type::Union(_) => ScalarClass::Undefined,
            Type::Tuple(_) => ScalarClass::Complex,
            Type::Record(_) => ScalarClass::Complex,
            Type::Function(_) => ScalarClass::Undefined,
            Type::Conflict(_) => ScalarClass::Undefined,
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
            Type::Conflict(ref conflict) => conflict.fmt(f),
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
        write!(f, "\\{{")?;
        let mut needs_sep = false;
        for (id, ty) in &self.fields {
            if needs_sep {
                write!(f, ",")?;
            }
            write!(f, "{}:", id)?;
            ty.fmt(f)?;
            needs_sep = true;
        }
        write!(f, "\\}}")?;
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\|")?;
        let mut needs_sep = false;
        for ty in &self.parameters {
            if needs_sep {
                write!(f, ",")?;
            }
            ty.fmt(f)?;
            needs_sep = true;
        }
        write!(f, "\\|")?;
        self.result.fmt(f)?;
        Ok(())
    }
}

impl fmt::Display for Conflict {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.actual.fmt(f)?;
        write!(f, "!=")?;
        self.expected.fmt(f)?;
        Ok(())
    }
}

impl fmt::Display for ExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpectedType::Specific(ref ty) => ty.fmt(f),
            ExpectedType::ScalarClass(ref class) => class.fmt(f),
            ExpectedType::AnyOf(ref options) => {
                let last = options.len() - 1;
                for (i, option) in options.iter().enumerate() {
                    if i == last {
                        write!(f, " or ")?;
                    } else if i > 0 {
                        write!(f, ", ")?;
                    }
                    option.fmt(f)?;
                }
                Ok(())
            }
            ExpectedType::Union => f.write_str("any union type"),
        }
    }
}

impl fmt::Display for ScalarClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ScalarClass::Symbol => f.write_str("any symbol"),
            ScalarClass::Integral(IntegralScalarClass::Unsigned) => {
                f.write_str("any unsigned integer type")
            }
            ScalarClass::Integral(IntegralScalarClass::Signed) => {
                f.write_str("any signed integer type")
            }
            ScalarClass::Integral(IntegralScalarClass::Any) => f.write_str("any integer type"),
            ScalarClass::Fractional => f.write_str("any floating point type"),
            ScalarClass::Complex => f.write_str("any complex type"),
            ScalarClass::Undefined => f.write_str("any non-scalar type"),
        }
    }
}
