use parser;

pub trait AstNode: parser::Parse + Sized {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor;
}

pub trait Visitor {
    fn define_ident(&mut self, ident: &Identifier);
    fn reference_ident(&mut self, ident: &Identifier);
    fn push_scope(&mut self);
    fn pop_scope(&mut self);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Norm {
    pub definitions: Vec<(Identifier, Expression)>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    String(String),
    Tuple(Tuple),
    Record(Record),
    Identifier(Identifier),
    Lambda(Lambda),
    Select(Select),
    Apply(Apply),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    pub fields: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    pub fields: Vec<(Identifier, Expression)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lambda {
    pub parameters: Vec<Parameter>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Definition(Identifier, Expression),
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Select {
    pub expression: Box<Expression>,
    pub field: Identifier,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Apply {
    pub function: Box<Expression>,
    pub parameters: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub signature: Option<Expression>,
}

impl AstNode for Norm {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor,
    {
        for (key, _) in &self.definitions {
            visitor.define_ident(key);
        }

        for (key, val) in &self.definitions {
            key.visit(visitor);
            val.visit(visitor);
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
            visitor.push_scope();
            field.visit(visitor);
            visitor.pop_scope();
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
            visitor.push_scope();
            val.visit(visitor);
            visitor.pop_scope();
        }
    }
}

impl AstNode for Lambda {
    fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor,
    {
        visitor.push_scope();
        for param in &self.parameters {
            visitor.define_ident(&param.name);
        }
        for statement in &self.statements {
            statement.visit(visitor);
        }
        visitor.pop_scope();
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
                expr.visit(visitor);
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
