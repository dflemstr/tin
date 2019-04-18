use std::collections;
use std::mem;

use crate::ir;
use crate::ir::element;
use crate::ir::error;
use crate::ir::location;
use crate::ir::symbol;
use crate::ir::world;
use crate::syntax::ast;
use crate::syntax::parser;

pub struct Builder<W>
where
    W: world::World,
{
    world: W,
    symbol: Vec<symbol::Part>,
    current_closure: Option<ir::Entity>,
    current_scope: collections::HashMap<ir::Ident, ir::Entity>,
    scopes: Vec<collections::HashMap<ir::Ident, ir::Entity>>,
    current_captures: collections::HashMap<ir::Ident, ir::Entity>,
    captures: Vec<collections::HashMap<ir::Ident, ir::Entity>>,
}

impl<W> Builder<W>
where
    W: world::World,
{
    pub fn new(world: W) -> Builder<W> {
        let symbol = Vec::new();
        let current_scope = collections::HashMap::new();
        let scopes = Vec::new();
        let current_captures = collections::HashMap::new();
        let captures = Vec::new();

        Builder {
            world,
            symbol,
            current_scope,
            scopes,
            current_captures,
            captures,
        }
    }

    pub fn add_module(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        ast: &ast::Module<parser::Context>,
    ) -> Result<(), error::Error> {
        let mut variables = collections::HashMap::new();

        for variable in &ast.variables {
            let ident = db.ident(variable.name.value.clone());
            let var_entity = db.entity(Some(entity), ident);

            self.current_scope
                .insert(ident, var_entity);

            variables.insert(ident, var_entity);
        }

        for variable in &ast.variables {
            let ident = db.ident(variable.name.value.clone());
            self.add_variable(db, variables[&ident], variable)?;
        }

        self.world.set_element(
            entity,
            element::Element::Module(element::Module { variables }),
        );
        self.world
            .set_symbol(entity, symbol::Symbol::new(self.symbol.clone()));
        self.world
            .set_location(entity, location::Location(ast.context.span));

        Ok(())
    }

    fn add_identifier(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        identifier: &ast::Identifier<parser::Context>,
    ) -> Result<(), error::Error> {
        let ident = db.ident(identifier.value.clone());

        let definition = self.current_scope.get(&ident).cloned().or_else(|| {
            self.scopes
                .iter()
                .rev()
                .flat_map(|scope| scope.get(&ident).cloned().into_iter())
                .next()
                .map(|e| {
                    let capture = db.child_entity(entity, ident);
                    self.world.set_element(
                        capture,
                        element::Element::Capture(element::Capture {
                            name: ident,
                            captured: e,
                        }),
                    );

                    self.world
                        .set_location(capture, location::Location(identifier.context.span));

                    self.current_captures.insert(name.clone(), capture);

                    capture
                })
        });

        let definition = definition.ok_or_else(|| error::Error::UndefinedReference {
            reference: name.clone(),
            location: identifier.context.span,
        })?;

        self.world.replace(entity, definition);

        self.world
            .set_location(entity, location::Location(identifier.context.span));

        Ok(())
    }

    fn add_expression(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        expression: &ast::Expression<parser::Context>,
    ) -> Result<(), error::Error> {
        match *expression {
            ast::Expression::NumberLiteral(ref v) => self.add_number(db, entity, v),
            ast::Expression::StringLiteral(ref v) => self.add_string(db, entity, v),
            ast::Expression::Symbol(ref v) => self.add_symbol(db, entity, v),
            ast::Expression::Tuple(ref v) => self.add_tuple(db, entity, v),
            ast::Expression::Record(ref v) => self.add_record(db, entity, v),
            ast::Expression::UnOp(ref v) => self.add_un_op(db, entity, v),
            ast::Expression::BiOp(ref v) => self.add_bi_op(db, entity, v),
            ast::Expression::Identifier(ref v) => self.add_identifier(db, entity, v),
            ast::Expression::Lambda(ref v) => self.add_lambda(db, entity, v),
            ast::Expression::Select(ref v) => self.add_select(db, entity, v),
            ast::Expression::Apply(ref v) => self.add_apply(db, entity, v),
            ast::Expression::Unknown => panic!("'unknown' AST nodes should not escape the parser"),
        }
    }

    fn add_number(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        number: &ast::NumberLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        self.world.set_element(
            entity,
            element::Element::Number(translate_number(number.value)),
        );

        self.world
            .set_location(entity, location::Location(number.context.span));

        Ok(())
    }

    fn add_string(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        string: &ast::StringLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        self.world.set_element(
            entity,
            element::Element::String(match string.value {
                ast::StringValue::String(ref s) => s.clone(),
                ast::StringValue::Invalid => {
                    panic!("'invalid' AST nodes should not escape the parser")
                }
            }),
        );

        self.world
            .set_location(entity, location::Location(string.context.span));

        Ok(())
    }

    fn add_tuple(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        tuple: &ast::Tuple<parser::Context>,
    ) -> Result<(), error::Error> {
        let fields = tuple
            .fields
            .iter()
            .map(|f| {
                let e = self.world.create_entity();
                self.add_expression(e, f)?;
                Ok(e)
            })
            .collect::<Result<_, error::Error>>()?;

        self.world
            .set_element(entity, element::Element::Tuple(element::Tuple { fields }));

        self.world
            .set_location(entity, location::Location(tuple.context.span));

        Ok(())
    }

    fn add_symbol(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        symbol: &ast::Symbol<parser::Context>,
    ) -> Result<(), error::Error> {
        let label = symbol.label.clone();

        self.world
            .set_element(entity, element::Element::Symbol(element::Symbol { label }));

        self.world
            .set_location(entity, location::Location(symbol.context.span));

        Ok(())
    }

    fn add_record(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        record: &ast::Record<parser::Context>,
    ) -> Result<(), error::Error> {
        let fields = record
            .fields
            .iter()
            .map(|(i, e)| {
                let en = self.world.create_entity();
                self.add_expression(en, e)?;
                Ok((i.value.clone(), en))
            })
            .collect::<Result<_, error::Error>>()?;

        self.world
            .set_element(entity, element::Element::Record(element::Record { fields }));

        self.world
            .set_location(entity, location::Location(record.context.span));

        Ok(())
    }

    fn add_un_op(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        un_op: &ast::UnOp<parser::Context>,
    ) -> Result<(), error::Error> {
        let operator = translate_un_operator(un_op.operator);

        let operand = self.world.create_entity();
        self.add_expression(operand, &*un_op.operand)?;

        self.world.set_element(
            entity,
            element::Element::UnOp(element::UnOp { operator, operand }),
        );

        self.world
            .set_location(entity, location::Location(un_op.context.span));

        Ok(())
    }

    fn add_bi_op(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        bi_op: &ast::BiOp<parser::Context>,
    ) -> Result<(), error::Error> {
        let lhs = self.world.create_entity();
        self.add_expression(lhs, &*bi_op.lhs)?;

        let operator = translate_bi_operator(bi_op.operator);

        let rhs = self.world.create_entity();
        self.add_expression(rhs, &*bi_op.rhs)?;

        self.world.set_element(
            entity,
            element::Element::BiOp(element::BiOp { lhs, operator, rhs }),
        );

        self.world
            .set_location(entity, location::Location(bi_op.context.span));

        Ok(())
    }

    fn add_lambda(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        lambda: &ast::Lambda<parser::Context>,
    ) -> Result<(), error::Error> {
        // TODO generate unique symbol for anonymous lambdas

        self.push_scope(Some(lambda.parameters.len()), None);

        let parameters = lambda
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity();
                self.add_parameter(e, p)?;
                Ok(e)
            })
            .collect::<Result<Vec<_>, error::Error>>()?;

        // Defer inserting parameters as variables until here to ensure that one parameter can't
        // depend on another one.
        for (entity, parameter) in parameters.iter().zip(lambda.parameters.iter()) {
            self.current_scope
                .insert(parameter.name.value.clone(), *entity);
        }

        let statements = lambda
            .statements
            .iter()
            .map(|s| {
                let e = self.world.create_entity();

                match s {
                    ast::Statement::Variable(ref variable) => {
                        self.current_scope.insert(variable.name.value.clone(), e);
                        self.add_variable(e, variable)?;
                    }
                    ast::Statement::Expression(ref expression) => {
                        self.add_expression(e, expression)?;
                    }
                }

                Ok(e)
            })
            .collect::<Result<Vec<_>, error::Error>>()?;

        let signature = self.world.create_entity();
        self.add_expression(signature, &*lambda.signature)?;

        let result = if let Some(ref result) = lambda.result {
            let e = self.world.create_entity();
            self.add_expression(e, &*result)?;
            e
        } else {
            signature
        };

        self.world.set_element(
            entity,
            element::Element::Closure(element::Closure {
                captures: self.current_captures.clone(),
                parameters,
                statements,
                signature,
                result,
            }),
        );

        self.world
            .set_symbol(entity, symbol::Symbol::new(self.symbol.clone()));

        self.world
            .set_location(entity, location::Location(lambda.context.span));

        self.pop_scope();

        Ok(())
    }

    fn add_variable(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        variable: &ast::Variable<parser::Context>,
    ) -> Result<(), error::Error> {
        self.symbol
            .push(symbol::Part::Named(variable.name.value.clone()));

        let name = variable.name.value.clone();
        let initializer = self.world.create_entity();

        self.add_expression(initializer, &variable.initializer)?;

        self.world.set_element(
            entity,
            element::Element::Variable(element::Variable { name, initializer }),
        );

        self.world
            .set_symbol(entity, symbol::Symbol::new(self.symbol.clone()));

        self.world
            .set_location(entity, location::Location(variable.context.span));

        self.symbol.pop();

        Ok(())
    }

    fn add_select(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        select: &ast::Select<parser::Context>,
    ) -> Result<(), error::Error> {
        let record = self.world.create_entity();
        self.add_expression(record, &*select.record)?;

        let field = select.field.value.clone();

        self.world.set_element(
            entity,
            element::Element::Select(element::Select { record, field }),
        );

        self.world
            .set_location(entity, location::Location(select.context.span));

        Ok(())
    }

    fn add_apply(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        apply: &ast::Apply<parser::Context>,
    ) -> Result<(), error::Error> {
        let function = self.world.create_entity();
        self.add_expression(function, &*apply.function)?;

        let parameters = apply
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity();
                self.add_expression(e, p)?;
                Ok(e)
            })
            .collect::<Result<_, error::Error>>()?;

        self.world.set_element(
            entity,
            element::Element::Apply(element::Apply {
                function,
                parameters,
            }),
        );

        self.world
            .set_location(entity, location::Location(apply.context.span));

        Ok(())
    }

    fn add_parameter(
        &mut self,
        db: impl ir::db::IrDb,
        entity: ir::Entity,
        parameter: &ast::Parameter<parser::Context>,
    ) -> Result<(), error::Error> {
        let name = parameter.name.value.clone();
        let signature = self.world.create_entity();
        self.add_expression(signature, &parameter.signature)?;

        self.world.set_element(
            entity,
            element::Element::Parameter(element::Parameter { name, signature }),
        );

        self.world
            .set_location(entity, location::Location(parameter.context.span));

        Ok(())
    }

    fn push_scope(&mut self, scope_size_hint: Option<usize>, captures_size_hint: Option<usize>) {
        self.scopes.push(mem::replace(
            &mut self.current_scope,
            scope_size_hint.map_or_else(
                collections::HashMap::new,
                collections::HashMap::with_capacity,
            ),
        ));
        self.captures.push(mem::replace(
            &mut self.current_captures,
            captures_size_hint.map_or_else(
                collections::HashMap::new,
                collections::HashMap::with_capacity,
            ),
        ));
    }

    fn pop_scope(&mut self) {
        self.current_scope = self.scopes.pop().unwrap();
        self.current_captures = self.captures.pop().unwrap();
    }
}

fn translate_number(number: ast::NumberValue) -> element::Number {
    match number {
        ast::NumberValue::U8(n) => element::Number::U8(n),
        ast::NumberValue::U16(n) => element::Number::U16(n),
        ast::NumberValue::U32(n) => element::Number::U32(n),
        ast::NumberValue::U64(n) => element::Number::U64(n),
        ast::NumberValue::I8(n) => element::Number::I8(n),
        ast::NumberValue::I16(n) => element::Number::I16(n),
        ast::NumberValue::I32(n) => element::Number::I32(n),
        ast::NumberValue::I64(n) => element::Number::I64(n),
        ast::NumberValue::F32(n) => element::Number::F32(n),
        ast::NumberValue::F64(n) => element::Number::F64(n),
        ast::NumberValue::Invalid => panic!("'invalid' AST nodes should not escape the parser"),
    }
}

fn translate_un_operator(un_operator: ast::UnOperator) -> element::UnOperator {
    match un_operator {
        ast::UnOperator::Not => element::UnOperator::Not,
        ast::UnOperator::BNot => element::UnOperator::BNot,
        ast::UnOperator::Cl0 => element::UnOperator::Cl0,
        ast::UnOperator::Cl1 => element::UnOperator::Cl1,
        ast::UnOperator::Cls => element::UnOperator::Cls,
        ast::UnOperator::Ct0 => element::UnOperator::Ct0,
        ast::UnOperator::Ct1 => element::UnOperator::Ct1,
        ast::UnOperator::C0 => element::UnOperator::C0,
        ast::UnOperator::C1 => element::UnOperator::C1,
        ast::UnOperator::Sqrt => element::UnOperator::Sqrt,
    }
}

fn translate_bi_operator(bi_operator: ast::BiOperator) -> element::BiOperator {
    match bi_operator {
        ast::BiOperator::Eq => element::BiOperator::Eq,
        ast::BiOperator::Ne => element::BiOperator::Ne,
        ast::BiOperator::Lt => element::BiOperator::Lt,
        ast::BiOperator::Ge => element::BiOperator::Ge,
        ast::BiOperator::Gt => element::BiOperator::Gt,
        ast::BiOperator::Le => element::BiOperator::Le,
        ast::BiOperator::Cmp => element::BiOperator::Cmp,
        ast::BiOperator::Add => element::BiOperator::Add,
        ast::BiOperator::Sub => element::BiOperator::Sub,
        ast::BiOperator::Mul => element::BiOperator::Mul,
        ast::BiOperator::Div => element::BiOperator::Div,
        ast::BiOperator::Rem => element::BiOperator::Rem,
        ast::BiOperator::And => element::BiOperator::And,
        ast::BiOperator::BAnd => element::BiOperator::BAnd,
        ast::BiOperator::Or => element::BiOperator::Or,
        ast::BiOperator::BOr => element::BiOperator::BOr,
        ast::BiOperator::Xor => element::BiOperator::Xor,
        ast::BiOperator::BXor => element::BiOperator::BXor,
        ast::BiOperator::AndNot => element::BiOperator::AndNot,
        ast::BiOperator::BAndNot => element::BiOperator::BAndNot,
        ast::BiOperator::OrNot => element::BiOperator::OrNot,
        ast::BiOperator::BOrNot => element::BiOperator::BOrNot,
        ast::BiOperator::XorNot => element::BiOperator::XorNot,
        ast::BiOperator::BXorNot => element::BiOperator::BXorNot,
        ast::BiOperator::RotL => element::BiOperator::RotL,
        ast::BiOperator::RotR => element::BiOperator::RotR,
        ast::BiOperator::ShL => element::BiOperator::ShL,
        ast::BiOperator::ShR => element::BiOperator::ShR,
    }
}
