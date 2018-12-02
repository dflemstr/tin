use std::collections;
use std::fmt;

use specs::Component;
use specs::VecStorage;

#[derive(Clone, Component, Debug, VisitEntities)]
#[storage(VecStorage)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
    pub named_field_offsets: collections::HashMap<String, usize>,
    pub unnamed_field_offsets: Vec<usize>,
}

impl Layout {
    pub fn zero() -> Layout {
        let size = 0;
        let alignment = 1;
        let named_field_offsets = collections::HashMap::new();
        let unnamed_field_offsets = Vec::new();

        Layout {
            size,
            alignment,
            named_field_offsets,
            unnamed_field_offsets,
        }
    }

    pub fn scalar(size: usize) -> Layout {
        let alignment = size;
        let named_field_offsets = collections::HashMap::new();
        let unnamed_field_offsets = Vec::new();

        Layout {
            size,
            alignment,
            named_field_offsets,
            unnamed_field_offsets,
        }
    }

    pub fn named_fields(
        size: usize,
        alignment: usize,
        named_field_offsets: collections::HashMap<String, usize>,
    ) -> Layout {
        let unnamed_field_offsets = Vec::new();

        Layout {
            size,
            alignment,
            named_field_offsets,
            unnamed_field_offsets,
        }
    }

    pub fn unnamed_fields(
        size: usize,
        alignment: usize,
        unnamed_field_offsets: Vec<usize>,
    ) -> Layout {
        let named_field_offsets = collections::HashMap::new();

        Layout {
            size,
            alignment,
            named_field_offsets,
            unnamed_field_offsets,
        }
    }
}

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.size, self.alignment)?;

        if !self.named_field_offsets.is_empty() {
            write!(f, "[")?;

            let mut needs_sep = false;
            for (name, offset) in &self.named_field_offsets {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{:?}:{}", name, offset)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        if !self.unnamed_field_offsets.is_empty() {
            write!(f, "[")?;

            let mut needs_sep = false;
            for (idx, offset) in self.unnamed_field_offsets.iter().enumerate() {
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
