use enum_primitive_derive::Primitive;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Primitive)]
pub enum Kind {
    // The discriminants in this enum are not stable and can change between releases without notice

    Error = 0,
    Whitespace = 1,

    // Simple tokens
    Semi = 2,
    Comma = 3,
    ParenOpen = 4,
    ParenClose = 5,
    CurlyOpen = 6,
    CurlyClose = 7,
    BracketOpen = 8,
    BracketClose = 9,
    AngleOpen = 10,
    AngleClose = 11,
    Arrow = 12,
    Dot = 13,
    Colon = 14,
    Is = 15,

    // Operator tokens
    Add = 16,
    Sub = 17,
    Mul = 18,
    Div = 19,

    // Complex tokens
    Comment = 20,
    Identifier = 21,
    SymbolLiteral = 22,
    StringLiteral = 23,
    UnsignedIntLiteral = 24,
    SignedIntLiteral = 25,
    FloatLiteral = 26,

    // Nodes
    Reference = 27,
    Module = 28,
    Variable = 29,
    Expression = 30,
    Statement = 31,
    Number = 32,
    String = 33,
    Symbol = 34,
    Tuple = 35,
    Record = 36,
    UnOp = 37,
    BiOp = 38,
    Lambda = 39,
    Parameter = 40,
    Apply = 41,
}

pub trait HasKind {
    fn kind(&self) -> Kind;
}

impl Kind {
    pub fn to_rowan_kind(self) -> rowan::SyntaxKind {
        use num_traits::cast::ToPrimitive;

        rowan::SyntaxKind(self.to_u16().unwrap())
    }

    pub fn from_rowan_kind(syntax_kind: rowan::SyntaxKind) -> Option<Self> {
        use num_traits::cast::FromPrimitive;

        Self::from_u16(syntax_kind.0)
    }
}
