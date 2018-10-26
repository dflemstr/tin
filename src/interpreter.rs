//! An interpreter for Norm code.
use std::collections;
use std::result;

use ast;
use value;

/// An error that occurs during the interpretation process.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// The specified variable is not defined.
    #[fail(display = "undefined reference to {}", _0)]
    UndefinedReference(value::Identifier),
    /// The specified field on a record is not defined.
    #[fail(display = "undefined field {}", _0)]
    UndefinedField(value::Identifier),
    /// We tried to access a field on something that is not a record.
    #[fail(display = "not a record")]
    NotARecord,
    /// We tried to call something that is not a function.
    #[fail(display = "not a function")]
    NotAFunction,
    /// We tried to call something with the wrong number of arguments.
    #[fail(
        display = "wrong number of arguments; expected {} but got {}",
        _0,
        _1
    )]
    WrongNumberOfArguments(usize, usize),
    /// The last statement of a function was not an expression.
    #[fail(display = "function did not end with an expression")]
    FunctionDidNotEndWithAnExpression,
}

/// A convenience `Result` wrapper for [`Error`].
type Result<A> = result::Result<A, Error>;

/// An interpreter instance.
#[derive(Debug)]
pub struct Interpreter {
    scope: collections::HashMap<value::Identifier, value::Value>,
}

#[derive(Clone, Debug)]
struct FreeVariablesVisitor {
    scope_stack: Vec<collections::HashSet<ast::Identifier>>,
    free_variables: collections::HashSet<ast::Identifier>,
}

impl Interpreter {
    /// Creates a new empty interpreter instance.
    pub fn new() -> Interpreter {
        let scope = collections::HashMap::new();
        Interpreter { scope }
    }

    /// Runs the interpreter by defining all of the contents of the supplied module.
    pub fn run<C>(&mut self, ast: ast::Module<C>) -> Result<()> {
        for (key, value) in ast.definitions {
            let name = value::Identifier::new(key.0);
            let result = self.eval(value)?;
            self.scope.insert(name, result);
        }
        Ok(())
    }

    /// Runs the interpreter by evaluating the supplied expression.
    pub fn eval<C>(&mut self, ast: ast::Expression<C>) -> Result<value::Value> {
        match ast {
            ast::Expression::Tuple(tuple) => Ok(self.eval_tuple(tuple)?),
            ast::Expression::Record(record) => Ok(self.eval_record(record)?),
            ast::Expression::Identifier(identifier) => self.eval_identifier(identifier),
            ast::Expression::Number(number) => Ok(value::Value::Number(number.value)),
            ast::Expression::String(string) => Ok(value::Value::String(string.value)),
            ast::Expression::Lambda(lambda) => self.eval_lambda(lambda),
            ast::Expression::Select(select) => self.eval_select(select),
            ast::Expression::Apply(apply) => self.eval_apply(apply),
        }
    }

    fn eval_tuple<C>(&mut self, ast: ast::Tuple<C>) -> Result<value::Value> {
        let mut fields = Vec::with_capacity(ast.fields.len());
        for value in ast.fields {
            fields.push(self.eval(value)?);
        }
        Ok(value::Value::Tuple(value::Tuple { fields }))
    }

    fn eval_record<C>(&mut self, ast: ast::Record<C>) -> Result<value::Value> {
        let mut fields = collections::HashMap::with_capacity(ast.fields.len());
        for (key, value) in ast.fields {
            fields.insert(value::Identifier::new(key.0), self.eval(value)?);
        }
        Ok(value::Value::Record(value::Record { fields }))
    }

    fn eval_identifier(&mut self, ast: ast::Identifier) -> Result<value::Value> {
        let ident = value::Identifier::new(ast.0);
        self.scope
            .get(&ident)
            .map(|v| (*v).clone())
            .ok_or_else(|| Error::UndefinedReference(ident))
    }

    fn eval_lambda<C>(&mut self, ast: ast::Lambda<C>) -> Result<value::Value> {
        use ast::AstNode;
        let mut visitor = FreeVariablesVisitor::new();
        ast.visit(&mut visitor);

        let mut captured = collections::HashMap::with_capacity(visitor.free_variables.len());
        for free_variable in visitor.free_variables {
            let identifier = value::Identifier::new(free_variable.0);
            if let Some(value) = self.scope.get(&identifier) {
                captured.insert(identifier, value.clone());
            } else {
                return Err(Error::UndefinedReference(identifier));
            }
        }

        let function = value::Closure {
            captured,
            lambda: ast.map_context(&mut |_| ()),
        };

        Ok(value::Value::Function(function))
    }

    fn eval_select<C>(&mut self, ast: ast::Select<C>) -> Result<value::Value> {
        let value = self.eval(*ast.expression)?;
        let field_name = value::Identifier::new(ast.field.0);

        match value {
            value::Value::Record(record) => {
                let value = record
                    .fields
                    .get(&field_name)
                    .ok_or_else(|| Error::UndefinedField(field_name))?;
                Ok(value.clone())
            }
            _ => Err(Error::NotARecord),
        }
    }

    fn eval_apply<C>(&mut self, ast: ast::Apply<C>) -> Result<value::Value> {
        use itertools::Itertools;

        let value = self.eval(*ast.function)?;

        match value {
            value::Value::Function(function) => {
                let expected = function.lambda.parameters.len();
                let actual = ast.parameters.len();
                if expected != actual {
                    return Err(Error::WrongNumberOfArguments(expected, actual));
                }
                let parameter_values = ast
                    .parameters
                    .into_iter()
                    .map(|e| self.eval(e))
                    .collect::<Result<Vec<_>>>()?;

                let mut old_scope = self.scope.clone();

                for (key, val) in &function.captured {
                    self.scope.insert(key.clone(), val.clone());
                }

                for (key, val) in function
                    .lambda
                    .parameters
                    .iter()
                    .zip_eq(parameter_values.into_iter())
                {
                    self.scope
                        .insert(value::Identifier::new(key.name.0.clone()), val);
                }

                let mut last_result = None;
                for statement in &function.lambda.statements {
                    last_result = self.eval_statement(statement.clone())?;
                }

                self.scope = old_scope;

                last_result.ok_or(Error::FunctionDidNotEndWithAnExpression)
            }
            _ => Err(Error::NotAFunction),
        }
    }

    fn eval_statement<C>(&mut self, ast: ast::Statement<C>) -> Result<Option<value::Value>> {
        match ast {
            ast::Statement::Expression(expression) => Ok(Some(self.eval(expression)?)),
            ast::Statement::Definition(ident, expr) => {
                let result = self.eval(expr)?;
                self.scope.insert(value::Identifier::new(ident.0), result);
                Ok(None)
            }
        }
    }
}

impl FreeVariablesVisitor {
    fn new() -> Self {
        let scope_stack = vec![collections::HashSet::new()];
        let free_variables = collections::HashSet::new();
        FreeVariablesVisitor {
            scope_stack,
            free_variables,
        }
    }
}

impl<C> ast::Visitor<C> for FreeVariablesVisitor {
    fn define_ident(&mut self, ident: &ast::Identifier) {
        self.scope_stack
            .last_mut()
            .expect("scope_stack was empty")
            .insert(ident.clone());
    }

    fn reference_ident(&mut self, ident: &ast::Identifier) {
        if !self.scope_stack.iter().any(|scope| scope.contains(ident)) {
            self.free_variables.insert(ident.clone());
        }
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(collections::HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack
            .pop()
            .expect("more pop_scope() than push_scope() calls");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast;
    use value;

    #[test]
    fn e2e() {
        use parser::Parse;

        let program = ast::Module::parse(
            r#"
Int = 0;
String = "";
Person = { name: String, age: Int };

getName = |person: Person| {
  person.name
};

getAge = |person: Person| {
  person.age
};

getPersonName = || {
  getName({ name: "David", age: 27 })
};

getPersonAge = || {
  getAge({ name: "David", age: 27 })
};
        "#,
        ).unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.run(program).unwrap();
        let result = interpreter
            .eval(ast::Expression::parse("getPersonName()").unwrap());

        assert_eq!(Ok(value::Value::String("David".to_owned())), result);

        let result = interpreter
            .eval(ast::Expression::parse("getPersonAge()").unwrap());

        assert_eq!(Ok(value::Value::Number(27.0)), result);
    }

    #[test]
    fn scoped_lambda_var() {
        use parser::Parse;

        let program = ast::Module::parse(
            r#"
main = || {
  foo = |person| { 0 };
  person.name
};
        "#,
        ).unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.run(program).unwrap();
        let result = interpreter
            .eval(ast::Expression::parse("main()").unwrap());

        assert_eq!(Err(Error::UndefinedReference("person".into())), result);
    }

    #[test]
    fn unknown_reference() {
        use parser::Parse;

        let program = ast::Module::parse(
            r#"
main = || {
  person
};
        "#,
        ).unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.run(program).unwrap();
        let result = interpreter
            .eval(ast::Expression::parse("main()").unwrap());

        assert_eq!(Err(Error::UndefinedReference("person".into())), result);
    }

    #[test]
    fn use_before_defined() {
        use parser::Parse;

        let program = ast::Module::parse(
            r#"
main = || {
  person.name;
  person = 0;
  person
};
        "#,
        ).unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.run(program).unwrap();
        let result = interpreter
            .eval(ast::Expression::parse("main()").unwrap());

        assert_eq!(Err(Error::UndefinedReference("person".into())), result);
    }

    #[test]
    fn unknown_field() {
        use parser::Parse;

        let program = ast::Module::parse(
            r#"
main = || {
  person = { foo: 0 };
  person.name
};
        "#,
        ).unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.run(program).unwrap();
        let result = interpreter
            .eval(ast::Expression::parse("main()").unwrap());

        assert_eq!(Err(Error::UndefinedField("name".into())), result);
    }
}
