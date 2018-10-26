//! Abstract syntax tree definitions for Norm.

/// A Norm AST node.
pub trait AstNode: Sized {
    /// Traverses this node and its children with the specified visitor.
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor;
}

/// A visitor for AST nodes.
pub trait Visitor {
    /// Called when an identifier is about to be defined.
    fn define_ident(&mut self, _ident: &Identifier) {}
    /// Called when an identifier was referenced.
    fn reference_ident(&mut self, _ident: &Identifier) {}
    /// Called when entering a lambda scope.
    fn push_lambda(&mut self) {}
    /// Called when entering the initialization of an identifier definition.
    fn push_definition(&mut self, _ident: &Identifier) {}
    /// Called when exiting a lambda scope.
    fn pop_lambda(&mut self) {}
    /// Called when exiting the initialization of an identifier definition.
    fn pop_definition(&mut self) {}
}

/// A complete Norm module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module<C> {
    /// This node's AST context.
    pub context: C,
    /// Definitions that are part of this module, in declaration order.
    pub definitions: Vec<(Identifier, Expression<C>)>,
}

/// An identifier.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(
    /// The raw string backing the identifier.
    pub String);

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
    Identifier(Identifier),
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
    pub fields: Vec<(Identifier, Expression<C>)>,
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
    Definition(Identifier, Expression<C>),
    /// A statement that is an expression (for return values or side effects).
    Expression(Expression<C>),
}

/// A field selection expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Select<C> {
    /// This node's AST context.
    pub context: C,
    /// The expression to select from; should evaluate to a record.
    pub expression: Box<Expression<C>>,
    /// The field to select.
    pub field: Identifier,
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
    pub name: Identifier,
    /// The signature of the parameter, if any.
    pub signature: Option<Expression<C>>,
}

impl<C> Module<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Module<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let definitions = self.definitions.into_iter().map(|(i, e)| (i, e.map_context(mapping))).collect();

        Module { context, definitions }
    }
}

impl<C> AstNode for Module<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        for (ident, _) in &self.definitions {
            visitor.define_ident(ident);
        }

        for (ident, val) in &self.definitions {
            ident.visit(visitor);
            visitor.push_definition(ident);
            val.visit(visitor);
            visitor.pop_definition();
        }
    }
}

impl AstNode for Identifier {
    fn visit<V>(&self, _visitor: &mut V)
        where
            V: Visitor,
    {
        // Nothing interesting to do here yet
    }
}

impl<C> Expression<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Expression<C2> where F: FnMut(C) -> C2 {
        match self {
            Expression::Number(e) => Expression::Number(e.map_context(mapping)),
            Expression::String(e) => Expression::String(e.map_context(mapping)),
            Expression::Tuple(e) => Expression::Tuple(e.map_context(mapping)),
            Expression::Record(e) => Expression::Record(e.map_context(mapping)),
            Expression::Identifier(e) => Expression::Identifier(e),
            Expression::Lambda(e) => Expression::Lambda(e.map_context(mapping)),
            Expression::Select(e) => Expression::Select(e.map_context(mapping)),
            Expression::Apply(e) => Expression::Apply(e.map_context(mapping)),
        }
    }
}

impl<C> AstNode for Expression<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
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
    }
}

impl<C> NumberLiteral<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> NumberLiteral<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let value = self.value;
        NumberLiteral { context, value }
    }
}

impl<C> AstNode for NumberLiteral<C> {
    fn visit<V>(&self, _visitor: &mut V)
        where
            V: Visitor,
    {
        // Nothing interesting to do here yet
    }
}

impl<C> StringLiteral<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> StringLiteral<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let value = self.value;
        StringLiteral { context, value }
    }
}

impl<C> AstNode for StringLiteral<C> {
    fn visit<V>(&self, _visitor: &mut V)
        where
            V: Visitor,
    {
        // Nothing interesting to do here yet
    }
}

impl<C> Tuple<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Tuple<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let fields = self.fields.into_iter().map(|f| f.map_context(mapping)).collect();
        Tuple { context, fields }
    }
}

impl<C> AstNode for Tuple<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        for field in &self.fields {
            field.visit(visitor);
        }
    }
}

impl<C> Record<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Record<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let fields = self.fields.into_iter().map(|(i, f)| (i, f.map_context(mapping))).collect();
        Record { context, fields }
    }
}

impl<C> AstNode for Record<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        for (key, val) in &self.fields {
            key.visit(visitor);
            val.visit(visitor);
        }
    }
}

impl<C> Lambda<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Lambda<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let parameters = self.parameters.into_iter().map(|p| p.map_context(mapping)).collect();
        let signature = self.signature.map(|s| Box::new(s.map_context(mapping)));
        let statements = self.statements.into_iter().map(|s| s.map_context(mapping)).collect();
        Lambda { context, parameters, signature, statements }
    }
}

impl<C> AstNode for Lambda<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        visitor.push_lambda();
        for param in &self.parameters {
            visitor.define_ident(&param.name);
        }
        for statement in &self.statements {
            statement.visit(visitor);
        }
        visitor.pop_lambda();
    }
}

impl<C> Statement<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Statement<C2> where F: FnMut(C) -> C2 {
        match self {
            Statement::Expression(e) => Statement::Expression(e.map_context(mapping)),
            Statement::Definition(i, e) => Statement::Definition(i, e.map_context(mapping)),
        }
    }
}

impl<C> AstNode for Statement<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        match *self {
            Statement::Definition(ref ident, ref expr) => {
                visitor.define_ident(ident);
                ident.visit(visitor);
                visitor.push_definition(ident);
                expr.visit(visitor);
                visitor.pop_definition();
            }
            Statement::Expression(ref expr) => {
                expr.visit(visitor);
            }
        }
    }
}

impl<C> Select<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Select<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let expression = Box::new(self.expression.map_context(mapping));
        let field = self.field;
        Select { context, expression, field }
    }
}

impl<C> AstNode for Select<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        self.expression.visit(visitor);
        self.field.visit(visitor);
    }
}

impl<C> Apply<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Apply<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let function = Box::new(self.function.map_context(mapping));
        let parameters = self.parameters.into_iter().map(|e| e.map_context(mapping)).collect();
        Apply { context, function, parameters }
    }
}

impl<C> AstNode for Apply<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        self.function.visit(visitor);

        for parameter in &self.parameters {
            parameter.visit(visitor);
        }
    }
}

impl<C> Parameter<C> {
    /// Transforms the AST context of this node and all its child nodes.
    pub fn map_context<F, C2>(self, mapping: &mut F) -> Parameter<C2> where F: FnMut(C) -> C2 {
        let context = mapping(self.context);
        let name = self.name;
        let signature = self.signature.map(|e| e.map_context(mapping));
        Parameter { context, name, signature }
    }
}

impl<C> AstNode for Parameter<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor,
    {
        self.name.visit(visitor);
        if let Some(ref signature) = self.signature {
            signature.visit(visitor);
        }
    }
}
