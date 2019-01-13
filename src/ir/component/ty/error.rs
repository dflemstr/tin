use std::fmt;

use specs::Component;
use specs::VecStorage;

use crate::ir::component::ty;
use crate::ir::component::ty::class;

#[derive(Component, Clone, Debug, Eq, Fail, PartialEq, VisitEntities, VisitEntitiesMut)]
#[storage(VecStorage)]
pub struct Error<E>
where
    E: fmt::Debug + Send + Sync + 'static,
{
    pub expected: ExpectedType,
    pub actual: ty::Type,
    pub main_entity: E,
    pub aux_entities: Vec<AuxEntity<E>>,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub struct AuxEntity<E> {
    pub entity: E,
    pub label: String,
}

#[derive(Clone, Debug, Eq, PartialEq, VisitEntities, VisitEntitiesMut)]
pub enum ExpectedType {
    Specific(ty::Type),
    ScalarClass(class::ScalarClass),
    AnyOf(Vec<ExpectedType>),
    Union,
}

impl<E> fmt::Display for Error<E>
where
    E: fmt::Debug + Send + Sync,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expected ")?;
        self.expected.fmt(f)?;
        write!(f, " but got ")?;
        self.actual.fmt(f)?;
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
