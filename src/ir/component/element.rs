use std::collections;
use std::fmt;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Debug, VisitEntities)]
#[storage(VecStorage)]
pub enum Element {
    NumberValue(NumberValue),
    StringValue(StringValue),
    Symbol(Symbol),
    Tuple(Tuple),
    Record(Record),
    UnOp(UnOp),
    BiOp(BiOp),
    Variable(Variable),
    Select(Select),
    Apply(Apply),
    Parameter(Parameter),
    Capture(Capture),
    Closure(Closure),
    Module(Module),
}

#[derive(Clone, Copy, Debug, VisitEntities)]
pub enum NumberValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, VisitEntities)]
pub struct StringValue(pub String);

#[derive(Debug, VisitEntities)]
pub struct Symbol {
    pub label: String,
}

#[derive(Debug, VisitEntities)]
pub struct Tuple {
    pub fields: Vec<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Record {
    pub fields: collections::HashMap<String, specs::Entity>,
}

/// An unary operator.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, VisitEntities)]
pub enum UnOperator {
    /// Logical not.
    Not,
    /// Bit-wise not.
    BNot,

    /// Count leading zero bits.
    Cl0,
    /// Count leading one bits.
    Cl1,
    /// Count leading sign bits (number of consecutive bits equal to MSB after MSB).
    Cls,

    /// Count trailing zero bits.
    Ct0,
    /// Count trailing one bits.
    Ct1,

    /// Count number of zero bits.
    C0,
    /// Count number of one bits.
    C1,

    /// Square root.
    Sqrt,
}

/// An operator application with one operand.
#[derive(Clone, Copy, Debug, PartialEq, VisitEntities)]
pub struct UnOp {
    /// The unary operator that is being applied.
    pub operator: UnOperator,
    /// The operand to the operator.
    pub operand: specs::Entity,
}

/// A binary operator.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, VisitEntities)]
pub enum BiOperator {
    /// The equal-to operator.
    Eq,
    /// The not-equal-to operator.
    Ne,
    /// The less-than operator.
    Lt,
    /// The greater-than-or-equal-to operator.
    Ge,
    /// The greater-than operator.
    Gt,
    /// The less-than-or-equal-to operator.
    Le,
    /// The compare operator.
    Cmp,

    /// The addition operator.
    Add,
    /// The subtraction operator.
    Sub,
    /// The multiplication operator.
    Mul,
    /// The division operator.
    Div,
    /// The remainder operator.
    Rem,

    /// The logical and operator.
    And,
    /// The bit-wise and operator.
    BAnd,
    /// The logical or operator.
    Or,
    /// The bit-wise or operator.
    BOr,
    /// The logical xor operator.
    Xor,
    /// The bit-wise xor operator.
    BXor,
    /// The logical and-not operator.
    AndNot,
    /// The bit-wise and-not operator.
    BAndNot,
    /// The logical or-not operator.
    OrNot,
    /// The bit-wise or-not operator.
    BOrNot,
    /// The logical xor-not operator.
    XorNot,
    /// The bit-wise xor-not operator.
    BXorNot,

    /// The rotate-left operator.
    RotL,
    /// The rotate-right operator.
    RotR,
    /// The shift-left operator.
    ShL,
    /// The shift-right operator.
    ShR,
}

/// An operator application with two operands.
#[derive(Clone, Copy, Debug, PartialEq, VisitEntities)]
pub struct BiOp {
    /// The left-hand-side operand of the operator.
    pub lhs: specs::Entity,
    /// The binary operator that is being applied.
    pub operator: BiOperator,
    /// The right-hand-side operand of the operator.
    pub rhs: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Variable {
    pub name: String,
    pub initializer: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Select {
    pub record: specs::Entity,
    pub field: String,
}

#[derive(Debug, VisitEntities)]
pub struct Apply {
    pub function: specs::Entity,
    pub parameters: Vec<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Parameter {
    pub name: String,
    pub signature: Option<specs::Entity>,
}

#[derive(Debug, VisitEntities)]
pub struct Capture {
    pub name: String,
    pub captured: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Closure {
    pub captures: Vec<specs::Entity>,
    pub parameters: Vec<specs::Entity>,
    pub statements: Vec<specs::Entity>,
    pub signature: Option<specs::Entity>,
    pub result: specs::Entity,
}

#[derive(Debug, VisitEntities)]
pub struct Module {
    pub variables: collections::HashMap<String, specs::Entity>,
}

impl fmt::Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            UnOperator::Not => "!",
            UnOperator::BNot => "~!",
            UnOperator::Cl0 => "#^0",
            UnOperator::Cl1 => "#^1",
            UnOperator::Cls => "#^-",
            UnOperator::Ct0 => "#$0",
            UnOperator::Ct1 => "#$1",
            UnOperator::C0 => "#0",
            UnOperator::C1 => "#1",
            UnOperator::Sqrt => "^/",
        })
    }
}

impl fmt::Display for BiOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            BiOperator::Eq => "==",
            BiOperator::Ne => "!=",
            BiOperator::Lt => "<",
            BiOperator::Ge => ">=",
            BiOperator::Gt => ">",
            BiOperator::Le => "<=",
            BiOperator::Cmp => "<=>",
            BiOperator::Add => "+",
            BiOperator::Sub => "-",
            BiOperator::Mul => "*",
            BiOperator::Div => "/",
            BiOperator::Rem => "%",
            BiOperator::And => "&",
            BiOperator::BAnd => "~&",
            BiOperator::Or => "|",
            BiOperator::BOr => "~|",
            BiOperator::Xor => "^",
            BiOperator::BXor => "~^",
            BiOperator::AndNot => "&!",
            BiOperator::BAndNot => "~&!",
            BiOperator::OrNot => "|!",
            BiOperator::BOrNot => "~|!",
            BiOperator::XorNot => "^!",
            BiOperator::BXorNot => "~^!",
            BiOperator::RotL => ">->",
            BiOperator::RotR => "<-<",
            BiOperator::ShL => ">>",
            BiOperator::ShR => "<<",
        })
    }
}
