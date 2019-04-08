use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    public: bool,
    parts: Vec<Part>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Part {
    Named(String),
    #[allow(unused)]
    Unnamed(u64),
}

impl Symbol {
    pub fn new(parts: Vec<Part>) -> Self {
        let public = false;
        Symbol { public, parts }
    }

    pub fn into_public(self) -> Self {
        let public = true;
        let parts = self.parts;
        Symbol { public, parts }
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn is_public(&self) -> bool {
        self.public
    }

    pub fn is_top_level(&self) -> bool {
        self.parts.len() == 1
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.public {
            f.write_str("public:")?;
        }

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
