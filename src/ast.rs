//! Abstract syntax tree definitions for Norm.

/// A Norm AST node.
pub trait AstNode<C>: Sized {
    /// Traverses this node and its children with the specified visitor.
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>;
}

/// A visitor for AST nodes.
pub trait Visitor<C> {
    /// Called before visiting a module.
    fn visit_before_module(&mut self, _module: &Module<C>) {}
    /// Called after visiting a module.
    fn visit_after_module(&mut self, _module: &Module<C>) {}

    /// Called when an identifier is about to be defined.
    fn define_ident(&mut self, _ident: &Identifier) {}
    /// Called when an identifier was referenced.
    fn reference_ident(&mut self, _ident: &Identifier) {}

    /// Called before visiting a module.
    fn visit_before_expression(&mut self, _expression: &Expression<C>) {}
    /// Called after visiting a module.
    fn visit_after_expression(&mut self, _expression: &Expression<C>) {}
    /// Called before visiting a module.
    fn visit_before_number(&mut self, _number: &NumberLiteral<C>) {}
    /// Called after visiting a module.
    fn visit_after_number(&mut self, _number: &NumberLiteral<C>) {}
    /// Called before visiting a module.
    fn visit_before_string(&mut self, _string: &StringLiteral<C>) {}
    /// Called after visiting a module.
    fn visit_after_string(&mut self, _string: &StringLiteral<C>) {}
    /// Called before visiting a module.
    fn visit_before_tuple(&mut self, _tuple: &Tuple<C>) {}
    /// Called after visiting a module.
    fn visit_after_tuple(&mut self, _tuple: &Tuple<C>) {}
    /// Called before visiting a module.
    fn visit_before_record(&mut self, _record: &Record<C>) {}
    /// Called after visiting a module.
    fn visit_after_record(&mut self, _record: &Record<C>) {}
    /// Called before visiting a module.
    fn visit_before_lambda(&mut self, _lambda: &Lambda<C>) {}
    /// Called after visiting a module.
    fn visit_after_lambda(&mut self, _lambda: &Lambda<C>) {}
    /// Called before visiting a module.
    fn visit_before_statement(&mut self, _statement: &Statement<C>) {}
    /// Called after visiting a module.
    fn visit_after_statement(&mut self, _statement: &Statement<C>) {}
    /// Called before visiting a module.
    fn visit_before_select(&mut self, _select: &Select<C>) {}
    /// Called after visiting a module.
    fn visit_after_select(&mut self, _select: &Select<C>) {}
    /// Called before visiting a module.
    fn visit_before_apply(&mut self, _apply: &Apply<C>) {}
    /// Called after visiting a module.
    fn visit_after_apply(&mut self, _apply: &Apply<C>) {}
    /// Called before visiting a module.
    fn visit_before_parameter(&mut self, _parameter: &Parameter<C>) {}
    /// Called after visiting a module.
    fn visit_after_parameter(&mut self, _parameter: &Parameter<C>) {}

    /// Called when entering a new lexical scope.
    fn push_scope(&mut self) {}
    /// Called when entering the initialization of an identifier definition.
    fn push_definition(&mut self, _ident: &Identifier) {}
    /// Called when exiting a lexical scope.
    fn pop_scope(&mut self) {}
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

impl<C> AstNode<C> for Module<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_module(self);
        for (ident, _) in &self.definitions {
            visitor.define_ident(ident);
        }

        for (ident, val) in &self.definitions {
            ident.visit(visitor);
            visitor.push_definition(ident);
            val.visit(visitor);
            visitor.pop_definition();
        }
        visitor.visit_after_module(self);
    }
}

impl<C> AstNode<C> for Identifier {
    fn visit<V>(&self, _visitor: &mut V)
        where
            V: Visitor<C>,
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

impl<C> AstNode<C> for Expression<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
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

impl<C> AstNode<C> for NumberLiteral<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_number(self);
        visitor.visit_after_number(self);
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

impl<C> AstNode<C> for StringLiteral<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_string(self);
        visitor.visit_after_string(self);
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

impl<C> AstNode<C> for Tuple<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_tuple(self);
        for field in &self.fields {
            field.visit(visitor);
        }
        visitor.visit_after_tuple(self);
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

impl<C> AstNode<C> for Record<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_record(self);
        for (key, val) in &self.fields {
            key.visit(visitor);
            val.visit(visitor);
        }
        visitor.visit_after_record(self);
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

impl<C> AstNode<C> for Lambda<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_lambda(self);
        visitor.push_scope();
        for param in &self.parameters {
            visitor.define_ident(&param.name);
        }
        for statement in &self.statements {
            statement.visit(visitor);
        }
        visitor.pop_scope();
        visitor.visit_after_lambda(self);
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

impl<C> AstNode<C> for Statement<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_statement(self);
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
        visitor.visit_after_statement(self);
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

impl<C> AstNode<C> for Select<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_select(self);
        self.expression.visit(visitor);
        self.field.visit(visitor);
        visitor.visit_after_select(self);
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

impl<C> AstNode<C> for Apply<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_apply(self);
        self.function.visit(visitor);

        for parameter in &self.parameters {
            parameter.visit(visitor);
        }
        visitor.visit_after_apply(self);
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

impl<C> AstNode<C> for Parameter<C> {
    fn visit<V>(&self, visitor: &mut V)
        where
            V: Visitor<C>,
    {
        visitor.visit_before_parameter(self);
        self.name.visit(visitor);
        if let Some(ref signature) = self.signature {
            signature.visit(visitor);
        }
        visitor.visit_after_parameter(self);
    }
}
