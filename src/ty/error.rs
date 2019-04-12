use std::fmt;
use std::sync;

use crate::ir;
use crate::ty;
use crate::ty::class;

#[derive(Clone, Debug, Eq, Fail, PartialEq)]
pub struct Error {
    pub expected: ExpectedType,
    pub actual: sync::Arc<ty::Type>,
    pub main_entity: ir::Entity,
    pub aux_entities: Vec<AuxEntity>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AuxEntity {
    pub entity: ir::Entity,
    pub label: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpectedType {
    Specific(sync::Arc<ty::Type>),
    ScalarClass(class::Scalar),
    AnyOf(Vec<ExpectedType>),
    Union,
}

impl fmt::Display for Error {
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

/*
impl diagnostic::Diagnostics for Error {
    fn to_diagnostics(&self, builder: &mut diagnostic::DiagnosticsBuilder) {
        builder.add_label(codespan_reporting::Label {
            span: entity,
            message: None,
            style: codespan_reporting::LabelStyle::Primary,
        });

        builder.add_label(codespan_reporting::Label {
            span: *main_entity,
            message: Some(type_error.to_string()),
            style: codespan_reporting::LabelStyle::Primary,
        });

        for aux_entity in aux_entities {
            builder.add_label(codespan_reporting::Label {
                span: aux_entity.entity,
                message: Some(aux_entity.label.clone()),
                style: codespan_reporting::LabelStyle::Secondary,
            });
        }

        builder.add_diagnostic(codespan_reporting::Severity::Error, None, &self.to_string());
    }
}
*/
