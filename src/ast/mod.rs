//! Abstract syntax tree definitions for Norm.
use std::fmt;

pub mod visitor;

/// A Norm AST node.
pub trait AstNode<C>: fmt::Debug + Sized {
    /// Returns a reference to the context of this node.
    fn context(&self) -> &C;

    /// Returns a mutable reference to the context of this node.
    fn context_mut(&mut self) -> &mut C;

    /// Traverses this node and its children with the specified visitor.
    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>;
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

impl<C> Module<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Module<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_module(self);
        visitor.visit_context(&self.context);
        for (ident, val) in &self.definitions {
            visitor.visit_definition(ident, &val.context());
        }

        for (ident, val) in &self.definitions {
            visitor.push_definition(ident);
            val.visit(visitor);
            visitor.pop_definition();
        }
        visitor.visit_after_module(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Identifier<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Identifier<C2>
    where
        F: FnMut(C) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;

        Identifier { context, value }
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_identifier(self);
        visitor.visit_context(&self.context);
        visitor.visit_after_identifier(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Expression<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Expression<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_expression(self);
        match *self {
            Expression::Number(ref number) => {
                number.visit(visitor);
            }
            Expression::String(ref string) => {
                string.visit(visitor);
            }
            Expression::Tuple(ref tuple) => {
                tuple.visit(visitor);
            }
            Expression::Record(ref record) => {
                record.visit(visitor);
            }
            Expression::Identifier(ref ident) => {
                ident.visit(visitor);
            }
            Expression::Lambda(ref lambda) => {
                lambda.visit(visitor);
            }
            Expression::Select(ref select) => {
                select.visit(visitor);
            }
            Expression::Apply(ref apply) => {
                apply.visit(visitor);
            }
        }
        visitor.visit_after_expression(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> NumberLiteral<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> NumberLiteral<C2>
    where
        F: FnMut(C) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        NumberLiteral { context, value }
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_number(self);
        visitor.visit_context(&self.context);
        visitor.visit_after_number(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> StringLiteral<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> StringLiteral<C2>
    where
        F: FnMut(C) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        StringLiteral { context, value }
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_string(self);
        visitor.visit_context(&self.context);
        visitor.visit_after_string(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Tuple<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Tuple<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_tuple(self);
        visitor.visit_context(&self.context);
        for field in &self.fields {
            field.visit(visitor);
        }
        visitor.visit_after_tuple(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Record<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Record<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_record(self);
        visitor.visit_context(&self.context);
        for (key, val) in &self.fields {
            key.visit(visitor);
            val.visit(visitor);
        }
        visitor.visit_after_record(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Lambda<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Lambda<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_lambda(self);
        visitor.visit_context(&self.context);
        if let Some(ref signature) = self.signature {
            signature.visit(visitor);
        }
        visitor.push_scope();
        for param in &self.parameters {
            param.visit(visitor);
        }
        for statement in &self.statements {
            statement.visit(visitor);
        }
        visitor.pop_scope();
        visitor.visit_after_lambda(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Statement<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Statement<C2>
    where
        F: FnMut(C) -> C2,
    {
        match self {
            Statement::Expression(e) => Statement::Expression(e.map_context(mapping)),
            Statement::Definition(i, e) => {
                Statement::Definition(i.map_context(mapping), e.map_context(mapping))
            }
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_statement(self);
        match *self {
            Statement::Definition(ref ident, ref expr) => {
                visitor.visit_definition(ident, expr.context());
                visitor.push_definition(ident);
                expr.visit(visitor);
                visitor.pop_definition();
            }
            Statement::Expression(ref expr) => {
                expr.visit(visitor);
            }
        }
        visitor.visit_after_statement(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Select<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Select<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_select(self);
        visitor.visit_context(&self.context);
        self.record.visit(visitor);
        self.field.visit(visitor);
        visitor.visit_after_select(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Apply<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Apply<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_apply(self);
        visitor.visit_context(&self.context);
        self.function.visit(visitor);

        for parameter in &self.parameters {
            parameter.visit(visitor);
        }
        visitor.visit_after_apply(self);
        trace!("end visiting {:?}", self);
    }
}

impl<C> Parameter<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Parameter<C2>
    where
        F: FnMut(C) -> C2,
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

    fn visit<V>(&self, visitor: &mut V)
    where
        V: visitor::Visitor<C>,
    {
        trace!("begin visiting {:?}", self);
        visitor.visit_before_parameter(self);
        visitor.visit_context(&self.context);
        visitor.visit_definition(&self.name, &self.context);
        if let Some(ref signature) = self.signature {
            signature.visit(visitor);
        }
        visitor.visit_after_parameter(self);
        trace!("end visiting {:?}", self);
    }
}
