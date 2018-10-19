use std::borrow;
use std::collections;
use std::result;

use ast;

enum Error<'a> {
    Undefined(Identifier<'a>),
}

type Result<'a, A> = result::Result<A, Error<'a>>;

struct Norm<'a> {
    scope: collections::HashMap<Identifier<'a>, Value<'a>>
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Identifier<'a>(borrow::Cow<'a, str>);

#[derive(Clone, Debug, PartialEq)]
enum Value<'a> {
    Number(f64),
    String(borrow::Cow<'a, str>),
    Tuple(Tuple<'a>),
    Record(Record<'a>),
    Function(Function<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct Tuple<'a> {
    fields: Vec<Value<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct Record<'a> {
    fields: collections::HashMap<Identifier<'a>, Value<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct Function<'a> {
    captured: collections::HashMap<Identifier<'a>, Value<'a>>,
    lambda: ast::Lambda<'a>,
}

impl<'a> Norm<'a> {
    fn new() -> Norm<'a> {
        let scope = collections::HashMap::new();
        Norm { scope }
    }

    fn eval(&mut self, ast: ast::Expression<'a>) -> Result<'a, Value<'a>> {
        match ast {
            ast::Expression::Tuple(tuple) => Ok(self.eval_tuple(tuple)?),
            ast::Expression::Record(record) => Ok(self.eval_record(record)?),
            ast::Expression::Identifier(identifier) => self.eval_identifier(identifier),
            ast::Expression::Number(number) => Ok(Value::Number(number)),
            ast::Expression::String(string) => Ok(Value::String(string)),
            ast::Expression::Lambda(lambda) => self.eval_lambda(lambda),
            ast::Expression::Select(_) => unimplemented!(),
            ast::Expression::Apply(_) => unimplemented!(),
        }
    }

    fn eval_tuple(&mut self, ast: ast::Tuple<'a>) -> Result<'a, Value<'a>> {
        let mut fields = Vec::with_capacity(ast.fields.len());
        for value in ast.fields.into_iter() {
            fields.push(self.eval(value)?);
        }
        Ok(Value::Tuple(Tuple { fields }))
    }

    fn eval_record(&mut self, ast: ast::Record<'a>) -> Result<'a, Value<'a>> {
        let mut fields = collections::HashMap::with_capacity(ast.fields.len());
        for (key, value) in ast.fields.into_iter() {
            fields.insert(Identifier(key.0), self.eval(value)?);
        }
        Ok(Value::Record(Record { fields }))
    }

    fn eval_identifier(&mut self, ast: ast::Identifier<'a>) -> Result<'a, Value<'a>> {
        let ident = Identifier(ast.0.clone());
        self.scope.get(&ident).map(|v| (*v).clone()).ok_or(Error::Undefined(ident))
    }

    fn eval_lambda(&mut self, ast: ast::Lambda<'a>) -> Result<'a, Value<'a>> {
        unimplemented!()
    }
}
