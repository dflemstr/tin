//! Abstract syntax tree definitions for Norm.
use std::fmt;

/// A Norm AST node.
pub trait AstNode<C>: fmt::Debug + Sized {
    /// Returns a reference to the context of this node.
    fn context(&self) -> &C;

    /// Returns a mutable reference to the context of this node.
    fn context_mut(&mut self) -> &mut C;
}

/// A context mapping over some AST node.
pub trait MapContext<C1, C2>: AstNode<C1> {
    // This is really just a Functor in disguise, but I choose to specialize it for this specific
    // use-case.

    /// The output AST node of the mapping, with the new context.
    type Output: AstNode<C2>;

    /// Maps the context of this AST node to a new context of potentially a different type.
    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2;
}

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
}

/// A complete Norm module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module<C> {
    /// This node's AST context.
    pub context: C,
    /// Definitions that are part of this module, in declaration order.
    pub definitions: Vec<(Identifier<C>, Expression<C>)>,
}

/// An identifier.
#[derive(Clone, Debug, PartialEq)]
pub struct Identifier<C> {
    /// This node's AST context.
    pub context: C,
    /// The raw string backing the identifier.
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
    pub value: f64,
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
}

/// A valid statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<C> {
    /// A statement that defines a new identifier.
    Definition(Identifier<C>, Expression<C>),
    /// A statement that is an expression (for return values or side effects).
    Expression(Expression<C>),
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

impl<C> AstNode<C> for Module<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Module<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Module<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let definitions = self
            .definitions
            .into_iter()
            .map(|(i, e)| (i.map_context(mapping), e.map_context(mapping)))
            .collect();

        Module {
            context,
            definitions,
        }
    }
}

impl<C> AstNode<C> for Identifier<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Identifier<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Identifier<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;

        Identifier { context, value }
    }
}

impl<C> AstNode<C> for Expression<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        match *self {
            Expression::Number(ref v) => v.context(),
            Expression::String(ref v) => v.context(),
            Expression::Tuple(ref v) => v.context(),
            Expression::Record(ref v) => v.context(),
            Expression::Identifier(ref v) => v.context(),
            Expression::Lambda(ref v) => v.context(),
            Expression::Select(ref v) => v.context(),
            Expression::Apply(ref v) => v.context(),
        }
    }

    fn context_mut(&mut self) -> &mut C {
        match *self {
            Expression::Number(ref mut v) => v.context_mut(),
            Expression::String(ref mut v) => v.context_mut(),
            Expression::Tuple(ref mut v) => v.context_mut(),
            Expression::Record(ref mut v) => v.context_mut(),
            Expression::Identifier(ref mut v) => v.context_mut(),
            Expression::Lambda(ref mut v) => v.context_mut(),
            Expression::Select(ref mut v) => v.context_mut(),
            Expression::Apply(ref mut v) => v.context_mut(),
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for Expression<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Expression<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        match self {
            Expression::Number(e) => Expression::Number(e.map_context(mapping)),
            Expression::String(e) => Expression::String(e.map_context(mapping)),
            Expression::Tuple(e) => Expression::Tuple(e.map_context(mapping)),
            Expression::Record(e) => Expression::Record(e.map_context(mapping)),
            Expression::Identifier(e) => Expression::Identifier(e.map_context(mapping)),
            Expression::Lambda(e) => Expression::Lambda(e.map_context(mapping)),
            Expression::Select(e) => Expression::Select(e.map_context(mapping)),
            Expression::Apply(e) => Expression::Apply(e.map_context(mapping)),
        }
    }
}

impl<C> AstNode<C> for NumberLiteral<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for NumberLiteral<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = NumberLiteral<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        NumberLiteral { context, value }
    }
}

impl<C> AstNode<C> for StringLiteral<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for StringLiteral<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = StringLiteral<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        StringLiteral { context, value }
    }
}

impl<C> AstNode<C> for Tuple<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Tuple<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Tuple<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let fields = self
            .fields
            .into_iter()
            .map(|f| f.map_context(mapping))
            .collect();
        Tuple { context, fields }
    }
}

impl<C> AstNode<C> for Record<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Record<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Record<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let fields = self
            .fields
            .into_iter()
            .map(|(i, f)| (i.map_context(mapping), f.map_context(mapping)))
            .collect();
        Record { context, fields }
    }
}

impl<C> AstNode<C> for Lambda<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Lambda<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Lambda<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let parameters = self
            .parameters
            .into_iter()
            .map(|p| p.map_context(mapping))
            .collect();
        let signature = self.signature.map(|s| Box::new(s.map_context(mapping)));
        let statements = self
            .statements
            .into_iter()
            .map(|s| s.map_context(mapping))
            .collect();
        Lambda {
            context,
            parameters,
            signature,
            statements,
        }
    }
}

impl<C> AstNode<C> for Statement<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        match *self {
            Statement::Definition(_, ref v) => v.context(),
            Statement::Expression(ref v) => v.context(),
        }
    }

    fn context_mut(&mut self) -> &mut C {
        match *self {
            Statement::Definition(_, ref mut v) => v.context_mut(),
            Statement::Expression(ref mut v) => v.context_mut(),
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for Statement<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Statement<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        match self {
            Statement::Expression(e) => Statement::Expression(e.map_context(mapping)),
            Statement::Definition(i, e) => {
                Statement::Definition(i.map_context(mapping), e.map_context(mapping))
            }
        }
    }
}

impl<C> AstNode<C> for Select<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Select<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Select<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let record = Box::new(self.record.map_context(mapping));
        let field = self.field.map_context(mapping);
        Select {
            context,
            record,
            field,
        }
    }
}

impl<C> AstNode<C> for Apply<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Apply<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Apply<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let function = Box::new(self.function.map_context(mapping));
        let parameters = self
            .parameters
            .into_iter()
            .map(|e| e.map_context(mapping))
            .collect();
        Apply {
            context,
            function,
            parameters,
        }
    }
}

impl<C> AstNode<C> for Parameter<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        &self.context
    }

    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

impl<C1, C2> MapContext<C1, C2> for Parameter<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = Parameter<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let name = self.name.map_context(mapping);
        let signature = self.signature.map(|e| e.map_context(mapping));
        Parameter {
            context,
            name,
            signature,
        }
    }
}
