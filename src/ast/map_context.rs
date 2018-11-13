//! AST node context mapping helpers.
use std::fmt;

use ast;
use ast::node;

/// A context mapping over some AST node.
pub trait MapContext<C1, C2>: node::AstNode<C1> {
    // This is really just a Functor in disguise, but I choose to specialize it for this specific
    // use-case.

    /// The output AST node of the mapping, with the new context.
    type Output: node::AstNode<C2>;

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
        let definitions = self
            .definitions
            .into_iter()
            .map(|v| v.map_context(mapping))
            .collect();

        ast::Module {
            context,
            definitions,
        }
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
            ast::Expression::Number(e) => ast::Expression::Number(e.map_context(mapping)),
            ast::Expression::String(e) => ast::Expression::String(e.map_context(mapping)),
            ast::Expression::Tuple(e) => ast::Expression::Tuple(e.map_context(mapping)),
            ast::Expression::Record(e) => ast::Expression::Record(e.map_context(mapping)),
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
            ast::Statement::Definition(v) => ast::Statement::Definition(v.map_context(mapping)),
            ast::Statement::Evaluation(e) => ast::Statement::Evaluation(e.map_context(mapping)),
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
