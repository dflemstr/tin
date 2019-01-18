use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum Scalar {
    Symbol,
    Integral(IntegralScalar),
    Fractional,
    Complex,
    Undefined,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum IntegralScalar {
    Unsigned,
    Signed,
    Any,
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Scalar::Symbol => f.write_str("any symbol"),
            Scalar::Integral(IntegralScalar::Unsigned) => f.write_str("any unsigned integer type"),
            Scalar::Integral(IntegralScalar::Signed) => f.write_str("any signed integer type"),
            Scalar::Integral(IntegralScalar::Any) => f.write_str("any integer type"),
            Scalar::Fractional => f.write_str("any floating point type"),
            Scalar::Complex => f.write_str("any complex type"),
            Scalar::Undefined => f.write_str("any non-scalar type"),
        }
    }
}
