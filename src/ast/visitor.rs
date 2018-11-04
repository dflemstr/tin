//! Tools for manipulating AST nodes using the visitor pattern.

/// A visitor for AST nodes.
pub trait Visitor<C> {
    /// Called for every AST context, after the corresponding `visit_before_*` method.
    fn visit_context(&mut self, _context: &C) {}

    /// Called before visiting a module.
    fn visit_before_module(&mut self, _module: &super::Module<C>) {}
    /// Called after visiting a module.
    fn visit_after_module(&mut self, _module: &super::Module<C>) {}

    /// Called when an identifier is about to be defined.
    fn define_ident(&mut self, _ident: &super::Identifier<C>, _value_context: &C) {}
    /// Called when an identifier was referenced.
    fn reference_ident(&mut self, _ident: &super::Identifier<C>) {}

    /// Called before visiting an identifier.
    fn visit_before_identifier(&mut self, _identifier: &super::Identifier<C>) {}
    /// Called after visiting an identifier.
    fn visit_after_identifier(&mut self, _identifier: &super::Identifier<C>) {}
    /// Called before visiting an expression.
    fn visit_before_expression(&mut self, _expression: &super::Expression<C>) {}
    /// Called after visiting an expression.
    fn visit_after_expression(&mut self, _expression: &super::Expression<C>) {}
    /// Called before visiting a number.
    fn visit_before_number(&mut self, _number: &super::NumberLiteral<C>) {}
    /// Called after visiting a number.
    fn visit_after_number(&mut self, _number: &super::NumberLiteral<C>) {}
    /// Called before visiting a string.
    fn visit_before_string(&mut self, _string: &super::StringLiteral<C>) {}
    /// Called after visiting a string.
    fn visit_after_string(&mut self, _string: &super::StringLiteral<C>) {}
    /// Called before visiting a tuple.
    fn visit_before_tuple(&mut self, _tuple: &super::Tuple<C>) {}
    /// Called after visiting a tuple.
    fn visit_after_tuple(&mut self, _tuple: &super::Tuple<C>) {}
    /// Called before visiting a record.
    fn visit_before_record(&mut self, _record: &super::Record<C>) {}
    /// Called after visiting a record.
    fn visit_after_record(&mut self, _record: &super::Record<C>) {}
    /// Called before visiting a lambda.
    fn visit_before_lambda(&mut self, _lambda: &super::Lambda<C>) {}
    /// Called after visiting a lambda.
    fn visit_after_lambda(&mut self, _lambda: &super::Lambda<C>) {}
    /// Called before visiting a statement.
    fn visit_before_statement(&mut self, _statement: &super::Statement<C>) {}
    /// Called after visiting a statement.
    fn visit_after_statement(&mut self, _statement: &super::Statement<C>) {}
    /// Called before visiting a select.
    fn visit_before_select(&mut self, _select: &super::Select<C>) {}
    /// Called after visiting a select.
    fn visit_after_select(&mut self, _select: &super::Select<C>) {}
    /// Called before visiting an apply.
    fn visit_before_apply(&mut self, _apply: &super::Apply<C>) {}
    /// Called after visiting an apply.
    fn visit_after_apply(&mut self, _apply: &super::Apply<C>) {}
    /// Called before visiting a parameter.
    fn visit_before_parameter(&mut self, _parameter: &super::Parameter<C>) {}
    /// Called after visiting a parameter.
    fn visit_after_parameter(&mut self, _parameter: &super::Parameter<C>) {}

    /// Called when entering a new lexical scope.
    fn push_scope(&mut self) {}
    /// Called when entering the initialization of an identifier definition.
    fn push_definition(&mut self, _ident: &super::Identifier<C>) {}
    /// Called when exiting a lexical scope.
    fn pop_scope(&mut self) {}
    /// Called when exiting the initialization of an identifier definition.
    fn pop_definition(&mut self) {}
}
