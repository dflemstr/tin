use std::collections;
use std::fmt;

use specs::Component;
use specs::VecStorage;

#[derive(Clone, Component, Debug, VisitEntities, VisitEntitiesMut)]
#[storage(VecStorage)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
    pub named_field_offsets: Option<collections::HashMap<String, usize>>,
    pub unnamed_field_offsets: Option<Vec<usize>>,
}

impl Layout {
    pub const fn zero() -> Layout {
        Layout {
            size: 0,
            alignment: 1,
            named_field_offsets: None,
            unnamed_field_offsets: None,
        }
    }

    pub const fn scalar(size: usize) -> Layout {
        Layout {
            size,
            alignment: size,
            named_field_offsets: None,
            unnamed_field_offsets: None,
        }
    }

    pub fn named_fields(
        size: usize,
        alignment: usize,
        named_field_offsets: collections::HashMap<String, usize>,
    ) -> Layout {
        let named_field_offsets = Some(named_field_offsets);
        let unnamed_field_offsets = None;

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
        let named_field_offsets = None;
        let unnamed_field_offsets = Some(unnamed_field_offsets);

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

        if self
            .named_field_offsets
            .as_ref()
            .map(|v| !v.is_empty())
            .unwrap_or(false)
        {
            write!(f, "[")?;

            let mut needs_sep = false;
            for (name, offset) in self.named_field_offsets.as_ref().unwrap() {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{:?}:{}", name, offset)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        if self
            .unnamed_field_offsets
            .as_ref()
            .map(|v| !v.is_empty())
            .unwrap_or(false)
        {
            write!(f, "[")?;

            let mut needs_sep = false;
            for (idx, offset) in self
                .unnamed_field_offsets
                .as_ref()
                .unwrap()
                .iter()
                .enumerate()
            {
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
