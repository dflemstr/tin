use std::fmt;

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
