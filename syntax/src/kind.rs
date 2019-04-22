use enum_primitive_derive::Primitive;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Primitive)]
pub enum Kind {
    // The discriminants in this enum are not stable and can change between releases without notice

    // Simple tokens
    Semi = 1,
    Comma = 2,
    ParenOpen = 3,
    ParenClose = 4,
    CurlyOpen = 5,
    CurlyClose = 6,
    BracketOpen = 7,
    BracketClose = 8,
    AngleOpen = 9,
    AngleClose = 10,
    Arrow = 11,
    Dot = 12,
    Colon = 13,
    Is = 14,

    // Operator tokens
    Add = 15,
    Sub = 16,
    Mul = 17,
    Div = 18,

    // Complex tokens
    Comment = 19,
    Identifier = 20,
    SymbolLiteral = 21,
    StringLiteral = 22,
    UnsignedIntLiteral = 23,
    SignedIntLiteral = 24,
    FloatLiteral = 25,

    // Nodes
    Reference = 26,
    Module = 27,
    Expression = 28,
    Statement = 29,
    Number = 30,
    String = 31,
    Symbol = 32,
    Tuple = 33,
    Record = 34,
    UnOp = 35,
    BiOp = 36,
    Lambda = 37,
    Parameter = 38,
    Apply = 39,
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
