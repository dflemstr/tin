//! Abstract syntax tree definitions for Norm.
use parser;

/// A Norm AST node.
pub trait AstNode: parser::Parse + Sized {
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
pub struct Module {
    /// Definitions that are part of this module, in declaration order.
    pub definitions: Vec<(Identifier, Expression)>,
}

/// An identifier.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(
/// The raw string backing the identifier.
pub String);

/// Any valid expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// A numeric literal.
    Number(f64),
    /// A string literal.
    String(String),
    /// A tuple literal.
    Tuple(Tuple),
    /// A record literal.
    Record(Record),
    /// A reference to an identifier.
    Identifier(Identifier),
    /// A lambda literal.
    Lambda(Lambda),
    /// A record field selection.
    Select(Select),
    /// A function application.
    Apply(Apply),
}

/// A tuple expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    /// The fields of the tuple, in declaration order.
    pub fields: Vec<Expression>,
}

/// A record expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    /// The fields of the record, in declaration order.
    pub fields: Vec<(Identifier, Expression)>,
}

/// A lambda expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Lambda {
    /// The parameters of the lambda.
    pub parameters: Vec<Parameter>,
    /// The signature of the result of the lambda.
    pub signature: Option<Box<Expression>>,
    /// The statements that constitute the lambda body.
    pub statements: Vec<Statement>,
}

/// A valid statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    /// A statement that defines a new identifier.
    Definition(Identifier, Expression),
    /// A statement that is an expression (for return values or side effects).
    Expression(Expression),
}

/// A field selection expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Select {
    /// The expression to select from; should evaluate to a record.
    pub expression: Box<Expression>,
    /// The field to select.
    pub field: Identifier,
}

/// A function application expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Apply {
    /// The expression to apply; should evaluate to a function.
    pub function: Box<Expression>,
    /// The parameters to pass in to the function; should match the function's accepted number of
    /// arguments.
    pub parameters: Vec<Expression>,
}

/// A lambda parameter declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    /// The name of the parameter.
    pub name: Identifier,
    /// The signature of the parameter, if any.
    pub signature: Option<Expression>,
}

impl AstNode for Module {
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

impl AstNode for Expression {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor,
    {
        match *self {
            Expression::Number(_) => {
                // Nothing interesting to do here yet
            }
            Expression::String(_) => {
                // Nothing interesting to do here yet
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

impl AstNode for Tuple {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor,
    {
        for field in &self.fields {
            field.visit(visitor);
        }
    }
}

impl AstNode for Record {
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

impl AstNode for Lambda {
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

impl AstNode for Statement {
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

impl AstNode for Select {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor,
    {
        self.expression.visit(visitor);
        self.field.visit(visitor);
    }
}

impl AstNode for Apply {
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

impl AstNode for Parameter {
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
