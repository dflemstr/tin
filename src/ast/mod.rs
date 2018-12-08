//! Abstract syntax tree definitions for Norm.
pub mod map_context;
pub mod node;

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
    /// A [`Tuple`] AST node.
    Tuple,
    /// A [`Record`] AST node.
    Record,
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

/// A complete Norm module.
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
    Number(NumberLiteral<C>),
    /// A string literal.
    String(StringLiteral<C>),
    /// A tuple literal.
    Tuple(Tuple<C>),
    /// A record literal.
    Record(Record<C>),
    /// A reference to an identifier.
    Identifier(Identifier<C>),
    /// A lambda literal.
    Lambda(Lambda<C>),
    /// A record field selection.
    Select(Select<C>),
    /// A function application.
    Apply(Apply<C>),
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
}

/// A string literal.
#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral<C> {
    /// This node's AST context.
    pub context: C,
    /// The value of the literal.
    pub value: String,
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
