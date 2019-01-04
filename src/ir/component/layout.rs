use std::fmt;

use specs::Component;
use specs::VecStorage;

#[derive(Clone, Component, Debug, VisitEntities, VisitEntitiesMut)]
#[storage(VecStorage)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
    pub named_fields: Option<Vec<NamedField>>,
    pub unnamed_fields: Option<Vec<OffsetLayout>>,
}

#[derive(Clone, Debug, VisitEntities, VisitEntitiesMut)]
pub struct NamedField {
    pub field: String,
    pub offset_layout: OffsetLayout,
}

#[derive(Clone, Debug, VisitEntities, VisitEntitiesMut)]
pub struct OffsetLayout {
    pub offset: usize,
    pub layout: Layout,
}

impl Layout {
    pub const fn zero() -> Layout {
        Layout {
            size: 0,
            alignment: 1,
            named_fields: None,
            unnamed_fields: None,
        }
    }

    pub const fn scalar(size: usize) -> Layout {
        Layout {
            size,
            alignment: size,
            named_fields: None,
            unnamed_fields: None,
        }
    }

    pub fn named_fields(size: usize, alignment: usize, named_fields: Vec<NamedField>) -> Layout {
        let named_fields = Some(named_fields);
        let unnamed_fields = None;

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }

    pub fn unnamed_fields(
        size: usize,
        alignment: usize,
        unnamed_fields: Vec<OffsetLayout>,
    ) -> Layout {
        let named_fields = None;
        let unnamed_fields = Some(unnamed_fields);

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }
}

impl OffsetLayout {
    pub const fn zero() -> OffsetLayout {
        OffsetLayout {
            offset: 0,
            layout: Layout::zero(),
        }
    }
}

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.size, self.alignment)?;

        if self
            .named_fields
            .as_ref()
            .map(|v| !v.is_empty())
            .unwrap_or(false)
        {
            write!(f, "[")?;

            let mut needs_sep = false;
            for named_field in self.named_fields.as_ref().unwrap() {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{:?}:{}", named_field.field, named_field.offset_layout)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        if self
            .unnamed_fields
            .as_ref()
            .map(|v| !v.is_empty())
            .unwrap_or(false)
        {
            write!(f, "[")?;

            let mut needs_sep = false;
            for (idx, offset) in self.unnamed_fields.as_ref().unwrap().iter().enumerate() {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{}:{}", idx, offset)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        Ok(())
    }
}

impl fmt::Display for OffsetLayout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.offset, self.layout)
    }
}
