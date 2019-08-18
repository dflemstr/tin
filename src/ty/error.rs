use std::fmt;
use std::result;
use std::sync;

use crate::diagnostic;
use crate::ir;
use crate::ty;
use crate::ty::class;

#[derive(Clone, Debug, Eq, failure::Fail, PartialEq)]
pub enum Error {
    #[fail(display = "type conflict: {}", _0)]
    Conflict(Conflict),
    #[fail(display = "semantic error")]
    Ir(#[cause] ir::error::Error),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Conflict {
    pub expected: ExpectedType,
    pub actual: sync::Arc<ty::Type>,
    pub main_entity: ir::location::Location,
    pub aux_entities: Vec<AuxEntity>,
}

pub type Result<A> = result::Result<A, Error>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AuxEntity {
    pub entity: ir::location::Location,
    pub label: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpectedType {
    Specific(sync::Arc<ty::Type>),
    ScalarClass(class::Scalar),
    AnyOf(Vec<ExpectedType>),
    Union,
}

impl From<ir::error::Error> for Error {
    fn from(error: ir::error::Error) -> Self {
        Error::Ir(error)
    }
}

impl fmt::Display for Conflict {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expected ")?;
        self.expected.fmt(f)?;
        write!(f, " but got `")?;
        self.actual.fmt(f)?;
        write!(f, "`")?;
        Ok(())
    }
}

impl fmt::Display for ExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpectedType::Specific(ref ty) => write!(f, "`{}`", ty),
            ExpectedType::ScalarClass(ref class) => class.fmt(f),
            ExpectedType::AnyOf(ref options) => {
                if options.is_empty() {
                    write!(f, "nothing")?;
                } else {
                    for (i, option) in options.iter().enumerate() {
                        if i + 1 == options.len() {
                            write!(f, " or ")?;
                        } else if i > 0 {
                            write!(f, ", ")?;
                        }
                        option.fmt(f)?;
                    }
                }
                Ok(())
            }
            ExpectedType::Union => f.write_str("any union type"),
        }
    }
}

impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        match self {
            Error::Conflict(conflict) => conflict.to_diagnostics(builder),
            Error::Ir(error) => error.to_diagnostics(builder),
        }
    }
}

impl diagnostic::Diagnostics for Conflict {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        builder.add_label(codespan_reporting::Label {
            span: self.main_entity.0,
            message: Some(self.to_string()),
            style: codespan_reporting::LabelStyle::Primary,
        });

        for aux_entity in &self.aux_entities {
            builder.add_label(codespan_reporting::Label {
                span: aux_entity.entity.0,
                message: Some(aux_entity.label.clone()),
                style: codespan_reporting::LabelStyle::Secondary,
            });
        }

        builder.add_diagnostic(codespan_reporting::Severity::Error, None, &self.to_string());
    }
}
