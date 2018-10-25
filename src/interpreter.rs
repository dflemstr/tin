use std::collections;
use std::result;

use ast;
use value;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "undefined reference to {}", _0)]
    UndefinedReference(value::Identifier),
    #[fail(display = "undefined field {}", _0)]
    UndefinedField(value::Identifier),
    #[fail(display = "not a record")]
    NotARecord,
    #[fail(display = "not a function")]
    NotAFunction,
    #[fail(
        display = "wrong number of arguments; expected {} but got {}",
        _0,
        _1
    )]
    WrongNumberOfArguments(usize, usize),
    #[fail(display = "function did not end with an expression")]
    FunctionDidNotEndWithAnExpression,
}

type Result<A> = result::Result<A, Error>;

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
    pub fn new() -> Interpreter {
        let scope = collections::HashMap::new();
        Interpreter { scope }
    }

    pub fn run(&mut self, ast: ast::Norm) -> Result<()> {
        for (key, value) in ast.definitions {
            let name = value::Identifier::new(key.0);
            let result = self.eval(value)?;
            self.scope.insert(name, result);
        }
        Ok(())
    }

    pub fn eval(&mut self, ast: ast::Expression) -> Result<value::Value> {
        match ast {
            ast::Expression::Tuple(tuple) => Ok(self.eval_tuple(tuple)?),
            ast::Expression::Record(record) => Ok(self.eval_record(record)?),
            ast::Expression::Identifier(identifier) => self.eval_identifier(identifier),
            ast::Expression::Number(number) => Ok(value::Value::Number(number)),
            ast::Expression::String(string) => Ok(value::Value::String(string)),
            ast::Expression::Lambda(lambda) => self.eval_lambda(lambda),
            ast::Expression::Select(select) => self.eval_select(select),
            ast::Expression::Apply(apply) => self.eval_apply(apply),
        }
    }

    fn eval_tuple(&mut self, ast: ast::Tuple) -> Result<value::Value> {
        let mut fields = Vec::with_capacity(ast.fields.len());
        for value in ast.fields {
            fields.push(self.eval(value)?);
        }
        Ok(value::Value::Tuple(value::Tuple { fields }))
    }

    fn eval_record(&mut self, ast: ast::Record) -> Result<value::Value> {
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

    fn eval_lambda(&mut self, ast: ast::Lambda) -> Result<value::Value> {
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
            lambda: ast,
        };

        Ok(value::Value::Function(function))
    }

    fn eval_select(&mut self, ast: ast::Select) -> Result<value::Value> {
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

    fn eval_apply(&mut self, ast: ast::Apply) -> Result<value::Value> {
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

    fn eval_statement(&mut self, ast: ast::Statement) -> Result<Option<value::Value>> {
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

impl ast::Visitor for FreeVariablesVisitor {
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

        let program = ast::Norm::parse(
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
            .eval(ast::Expression::parse("getPersonName()").unwrap())
            .unwrap();

        assert_eq!(value::Value::String("David".to_owned()), result);

        let result = interpreter
            .eval(ast::Expression::parse("getPersonAge()").unwrap())
            .unwrap();

        assert_eq!(value::Value::Number(27.0), result);
    }
}
