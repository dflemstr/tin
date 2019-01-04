use std::fmt;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Clone, Debug, VisitEntities, VisitEntitiesMut)]
#[storage(VecStorage)]
pub struct Symbol {
    parts: Vec<Part>,
}

#[derive(Clone, Debug, VisitEntities, VisitEntitiesMut)]
pub enum Part {
    Named(String),
    #[allow(unused)]
    Unnamed(u64),
}

impl Symbol {
    pub fn new(parts: Vec<Part>) -> Self {
        Symbol { parts }
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut needs_sep = false;

        for part in &self.parts {
            if needs_sep {
                f.write_str(".")?;
            }
            part.fmt(f)?;
            needs_sep = true;
        }

        Ok(())
    }
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Part::Named(ref name) => name.fmt(f),
            Part::Unnamed(ref id) => id.fmt(f),
        }
    }
}
