//! AST node context mapping helpers.
use std::fmt;

use crate::ast;
use crate::ast::ast_node;

/// A context mapping over some AST node.
pub trait MapContext<C1, C2>: ast_node::AstNode<C1> {
    // This is really just a Functor in disguise, but I choose to specialize it for this specific
    // use-case.

    /// The output AST node of the mapping, with the new context.
    type Output: ast_node::AstNode<C2>;

    /// Maps the context of this AST node to a new context of potentially a different type.
    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2;
}

impl<C1, C2> MapContext<C1, C2> for ast::Module<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Module<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let variables = self
            .variables
            .into_iter()
            .map(|v| v.map_context(mapping))
            .collect();

        ast::Module { context, variables }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Identifier<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Identifier<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;

        ast::Identifier { context, value }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Expression<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Expression<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        match self {
            ast::Expression::NumberLiteral(e) => {
                ast::Expression::NumberLiteral(e.map_context(mapping))
            }
            ast::Expression::StringLiteral(e) => {
                ast::Expression::StringLiteral(e.map_context(mapping))
            }
            ast::Expression::Symbol(e) => ast::Expression::Symbol(e.map_context(mapping)),
            ast::Expression::Tuple(e) => ast::Expression::Tuple(e.map_context(mapping)),
            ast::Expression::Record(e) => ast::Expression::Record(e.map_context(mapping)),
            ast::Expression::UnOp(e) => ast::Expression::UnOp(e.map_context(mapping)),
            ast::Expression::BiOp(e) => ast::Expression::BiOp(e.map_context(mapping)),
            ast::Expression::Identifier(e) => ast::Expression::Identifier(e.map_context(mapping)),
            ast::Expression::Lambda(e) => ast::Expression::Lambda(e.map_context(mapping)),
            ast::Expression::Select(e) => ast::Expression::Select(e.map_context(mapping)),
            ast::Expression::Apply(e) => ast::Expression::Apply(e.map_context(mapping)),
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::NumberLiteral<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::NumberLiteral<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        ast::NumberLiteral { context, value }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::StringLiteral<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::StringLiteral<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let value = self.value;
        ast::StringLiteral { context, value }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Symbol<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Symbol<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let label = self.label;
        ast::Symbol { context, label }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Tuple<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Tuple<C2>;

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
        ast::Tuple { context, fields }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Record<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Record<C2>;

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
        ast::Record { context, fields }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::UnOp<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::UnOp<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let operator = self.operator;
        let operand = Box::new(self.operand.map_context(mapping));
        ast::UnOp {
            context,
            operator,
            operand,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::BiOp<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::BiOp<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let lhs = Box::new(self.lhs.map_context(mapping));
        let operator = self.operator;
        let rhs = Box::new(self.rhs.map_context(mapping));
        ast::BiOp {
            context,
            lhs,
            operator,
            rhs,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Lambda<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Lambda<C2>;

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
        let result = Box::new(self.result.map_context(mapping));

        ast::Lambda {
            context,
            parameters,
            signature,
            statements,
            result,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Statement<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Statement<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        match self {
            ast::Statement::Variable(v) => ast::Statement::Variable(v.map_context(mapping)),
            ast::Statement::Expression(e) => ast::Statement::Expression(e.map_context(mapping)),
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Variable<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Variable<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let name = self.name.map_context(mapping);
        let initializer = self.initializer.map_context(mapping);

        ast::Variable {
            context,
            name,
            initializer,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Select<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Select<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let record = Box::new(self.record.map_context(mapping));
        let field = self.field.map_context(mapping);
        ast::Select {
            context,
            record,
            field,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Apply<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Apply<C2>;

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
        ast::Apply {
            context,
            function,
            parameters,
        }
    }
}

impl<C1, C2> MapContext<C1, C2> for ast::Parameter<C1>
where
    C1: fmt::Debug,
    C2: fmt::Debug,
{
    type Output = ast::Parameter<C2>;

    fn map_context<F>(self, mapping: &mut F) -> Self::Output
    where
        F: FnMut(C1) -> C2,
    {
        let context = mapping(self.context);
        let name = self.name.map_context(mapping);
        let signature = self.signature.map(|e| e.map_context(mapping));
        ast::Parameter {
            context,
            name,
            signature,
        }
    }
}
