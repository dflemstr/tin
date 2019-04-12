use std::fmt;

pub mod db;
mod infer;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
    pub named_fields: Vec<NamedField>,
    pub unnamed_fields: Vec<Offset>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedField {
    pub field: String,
    pub offset_layout: Offset,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Offset {
    pub offset: usize,
    pub layout: Layout,
}

impl Layout {
    pub const fn zero() -> Layout {
        Layout {
            size: 0,
            alignment: 1,
            named_fields: Vec::new(),
            unnamed_fields: Vec::new(),
        }
    }

    pub const fn scalar(size: usize) -> Layout {
        Layout {
            size,
            alignment: size,
            named_fields: Vec::new(),
            unnamed_fields: Vec::new(),
        }
    }

    pub fn named_fields(size: usize, alignment: usize, named_fields: Vec<NamedField>) -> Layout {
        let unnamed_fields = Vec::new();

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }

    pub fn unnamed_fields(size: usize, alignment: usize, unnamed_fields: Vec<Offset>) -> Layout {
        let named_fields = Vec::new();

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }
}

impl Offset {
    pub const fn zero() -> Offset {
        Offset {
            offset: 0,
            layout: Layout::zero(),
        }
    }
}

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.size, self.alignment)?;

        if !self.named_fields.is_empty() {
            write!(f, "[")?;

            let mut needs_sep = false;
            for named_field in &self.named_fields {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{:?}:{}", named_field.field, named_field.offset_layout)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        if !self.unnamed_fields.is_empty() {
            write!(f, "{{")?;

            let mut needs_sep = false;
            for (idx, offset) in self.unnamed_fields.iter().enumerate() {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{}:{}", idx, offset)?;
                needs_sep = true;
            }

            write!(f, "}}")?;
        }

        Ok(())
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.offset, self.layout)
    }
}
