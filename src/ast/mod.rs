//! Abstract syntax tree definitions for Tin.
//!
//! Every AST node carries a context with a generic type named `C`.  This type can be used to attach
//! arbitrary metadata to every node; a common use is to store the source location for each node as
//! parsed by a parser (see [`parser::Context`] for an example).
//!
//! To generically interact with the context of a node, the [`AstNode`] trait can be used.  All of
//! the contexts of an entire AST can additionally be transformed using the [`MapContext`] trait.
mod ast_node;
mod map_context;

pub use self::ast_node::AstNode;
pub use self::map_context::MapContext;

/// Identifies the kind of AST node.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Kind {
    /// A [`Module`] AST node.
    Module,
    /// An [`Identifier`] AST node.
    Identifier,
    /// A [`NumberLiteral`] AST node.
    NumberLiteral,
    /// A [`StringLiteral`] AST node.
    StringLiteral,
    /// A [`Symbol`] AST node.
    Symbol,
    /// A [`Tuple`] AST node.
    Tuple,
    /// A [`Record`] AST node.
    Record,
    /// A [`UnOp`] AST node.
    UnOp,
    /// A [`BiOp`] AST node.
    BiOp,
    /// A [`Lambda`] AST node.
    Lambda,
    /// A [`Select`] AST node.
    Select,
    /// An [`Apply`] AST node.
    Apply,
    /// A [`Parameter`] AST node.
    Parameter,
    /// A [`Variable`] AST node.
    Variable,
}

/// A complete Tin module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module<C> {
    /// This node's AST context.
    pub context: C,
    /// Variable variables that are part of this module, in declaration order.
    pub variables: Vec<Variable<C>>,
}

/// An identifier.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier<C> {
    /// This node's AST context.
    pub context: C,
    /// The raw string name of the identifier.
    pub value: String,
}

/// Any valid expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<C> {
    /// A numeric literal.
    NumberLiteral(NumberLiteral<C>),
    /// A string literal.
    StringLiteral(StringLiteral<C>),
    /// A symbol literal.
    Symbol(Symbol<C>),
    /// A tuple literal.
    Tuple(Tuple<C>),
    /// A record literal.
    Record(Record<C>),
    /// A reference to an identifier.
    Identifier(Identifier<C>),
    /// A unary operator application.
    UnOp(UnOp<C>),
    /// A binary operator application.
    BiOp(BiOp<C>),
    /// A lambda literal.
    Lambda(Lambda<C>),
    /// A record field selection.
    Select(Select<C>),
    /// A function application.
    Apply(Apply<C>),

    /// An unknown expression.
    Unknown,
}

/// A numeric literal.
#[derive(Clone, Debug, PartialEq)]
pub struct NumberLiteral<C> {
    /// This node's AST context.
    pub context: C,
    /// The value of the literal.
    pub value: NumberValue,
}

/// All syntactic number literal value variants.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum NumberValue {
    /// Unsigned 8-bit integer value.
    U8(u8),
    /// Unsigned 16-bit integer value.
    U16(u16),
    /// Unsigned 32-bit integer value.
    U32(u32),
    /// Unsigned 64-bit integer value.
    U64(u64),
    /// Signed 8-bit integer value.
    I8(i8),
    /// Signed 16-bit integer value.
    I16(i16),
    /// Signed 32-bit integer value.
    I32(i32),
    /// Signed 64-bit integer value.
    I64(i64),
    /// Floating point 32-bit value.
    F32(f32),
    /// Floating point 64-bit value.
    F64(f64),
    /// The number value is invalid (e.g. out of bounds).
    Invalid,
}

/// A string literal.
#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral<C> {
    /// This node's AST context.
    pub context: C,
    /// The value of the literal.
    pub value: StringValue,
}

/// All syntactic string literal value variants.
#[derive(Clone, Debug, PartialEq)]
pub enum StringValue {
    /// A plain string.
    String(String),
    /// The string value is invalid (e.g. has an illegal character).
    Invalid,
}

/// A symbol.
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol<C> {
    /// This node's AST context.
    pub context: C,
    /// The label of the symbol.
    pub label: String,
}

/// A tuple expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Tuple<C> {
    /// This node's AST context.
    pub context: C,
    /// The fields of the tuple, in declaration order.
    pub fields: Vec<Expression<C>>,
}

/// A record expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Record<C> {
    /// This node's AST context.
    pub context: C,
    /// The fields of the record, in declaration order.
    pub fields: Vec<(Identifier<C>, Expression<C>)>,
}

/// An unary operator.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct UnOp<C> {
    /// This node's AST context.
    pub context: C,
    /// The unary operator that is being applied.
    pub operator: UnOperator,
    /// The operand to the operator.
    pub operand: Box<Expression<C>>,
}

/// A binary operator.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct BiOp<C> {
    /// This node's AST context.
    pub context: C,
    /// The left-hand-side operand of the operator.
    pub lhs: Box<Expression<C>>,
    /// The binary operator that is being applied.
    pub operator: BiOperator,
    /// The right-hand-side operand of the operator.
    pub rhs: Box<Expression<C>>,
}

/// A lambda expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Lambda<C> {
    /// This node's AST context.
    pub context: C,
    /// The parameters of the lambda.
    pub parameters: Vec<Parameter<C>>,
    /// The signature of the result of the lambda.
    pub signature: Option<Box<Expression<C>>>,
    /// The statements that constitute the lambda body.
    pub statements: Vec<Statement<C>>,
    /// The result ("return value") of the lambda.
    pub result: Box<Expression<C>>,
}

/// A valid statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<C> {
    /// A statement that defines a new identifier.
    Variable(Variable<C>),
    /// A statement that is an expression (for return values or side effects).
    Expression(Expression<C>),
}

/// A variable definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Variable<C> {
    /// This node's AST context.
    pub context: C,
    /// The name of the variable.
    pub name: Identifier<C>,
    /// The initializer expression of the variable.
    pub initializer: Expression<C>,
}

/// A field selection expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Select<C> {
    /// This node's AST context.
    pub context: C,
    /// The expression to select from; should evaluate to a record.
    pub record: Box<Expression<C>>,
    /// The field to select.
    pub field: Identifier<C>,
}

/// A function application expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Apply<C> {
    /// This node's AST context.
    pub context: C,
    /// The expression to apply; should evaluate to a function.
    pub function: Box<Expression<C>>,
    /// The parameters to pass in to the function; should match the function's accepted number of
    /// arguments.
    pub parameters: Vec<Expression<C>>,
}

/// A lambda parameter declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter<C> {
    /// This node's AST context.
    pub context: C,
    /// The name of the parameter.
    pub name: Identifier<C>,
    /// The signature of the parameter, if any.
    pub signature: Option<Expression<C>>,
}
