use crate::kind;

pub struct Token {
    pub kind: kind::Kind,
    pub contents: rowan::SmolStr
}
