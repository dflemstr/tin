//! Generic node manipulation of AST nodes.
use std::fmt;

use ast;

/// A Norm AST node.
pub trait AstNode<C>: fmt::Debug + Sized {
    /// Returns a reference to the context of this node.
    fn context(&self) -> &C;

    /// Returns a mutable reference to the context of this node.
    fn context_mut(&mut self) -> &mut C;
}

impl<C> AstNode<C> for ast::Module<C>
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

impl<C> AstNode<C> for ast::Identifier<C>
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

impl<C> AstNode<C> for ast::Expression<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        match *self {
            ast::Expression::Number(ref v) => v.context(),
            ast::Expression::String(ref v) => v.context(),
            ast::Expression::Tuple(ref v) => v.context(),
            ast::Expression::Record(ref v) => v.context(),
            ast::Expression::Identifier(ref v) => v.context(),
            ast::Expression::Lambda(ref v) => v.context(),
            ast::Expression::Select(ref v) => v.context(),
            ast::Expression::Apply(ref v) => v.context(),
        }
    }

    fn context_mut(&mut self) -> &mut C {
        match *self {
            ast::Expression::Number(ref mut v) => v.context_mut(),
            ast::Expression::String(ref mut v) => v.context_mut(),
            ast::Expression::Tuple(ref mut v) => v.context_mut(),
            ast::Expression::Record(ref mut v) => v.context_mut(),
            ast::Expression::Identifier(ref mut v) => v.context_mut(),
            ast::Expression::Lambda(ref mut v) => v.context_mut(),
            ast::Expression::Select(ref mut v) => v.context_mut(),
            ast::Expression::Apply(ref mut v) => v.context_mut(),
        }
    }
}

impl<C> AstNode<C> for ast::NumberLiteral<C>
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

impl<C> AstNode<C> for ast::StringLiteral<C>
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

impl<C> AstNode<C> for ast::Tuple<C>
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

impl<C> AstNode<C> for ast::Record<C>
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

impl<C> AstNode<C> for ast::Lambda<C>
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

impl<C> AstNode<C> for ast::Statement<C>
where
    C: fmt::Debug,
{
    fn context(&self) -> &C {
        match *self {
            ast::Statement::Variable(ref v) => v.context(),
            ast::Statement::Expression(ref v) => v.context(),
        }
    }

    fn context_mut(&mut self) -> &mut C {
        match *self {
            ast::Statement::Variable(ref mut v) => v.context_mut(),
            ast::Statement::Expression(ref mut v) => v.context_mut(),
        }
    }
}

impl<C> AstNode<C> for ast::Variable<C>
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

impl<C> AstNode<C> for ast::Select<C>
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

impl<C> AstNode<C> for ast::Apply<C>
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

impl<C> AstNode<C> for ast::Parameter<C>
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
