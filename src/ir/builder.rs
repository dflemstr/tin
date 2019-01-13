use std::collections;
use std::mem;

use specs;

use crate::ast;
use crate::ir::component::element;
use crate::ir::component::location;
use crate::ir::component::replacement;
use crate::ir::component::symbol;
use crate::ir::error;
use crate::parser;

pub struct ModuleBuilder<'a> {
    world: &'a mut specs::World,
    symbol: Vec<symbol::Part>,
    current_scope: collections::HashMap<String, specs::Entity>,
    scopes: Vec<collections::HashMap<String, specs::Entity>>,
    current_captures: Vec<specs::Entity>,
    captures: Vec<Vec<specs::Entity>>,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(world: &'a mut specs::World) -> ModuleBuilder<'a> {
        let symbol = Vec::new();
        let current_scope = collections::HashMap::new();
        let scopes = Vec::new();
        let current_captures = Vec::new();
        let captures = Vec::new();

        ModuleBuilder {
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
        entity: specs::Entity,
        ast: &ast::Module<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let mut variables = collections::HashMap::new();
        for variable in &ast.variables {
            let var_entity = self.world.create_entity().build();

            self.current_scope
                .insert(variable.name.value.clone(), var_entity);

            variables.insert(variable.name.value.clone(), var_entity);
        }

        for variable in &ast.variables {
            let name = &variable.name.value;
            self.add_variable(variables[name], variable)?;
        }

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Module(element::Module { variables }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(ast.context.span))
            .unwrap();

        Ok(())
    }

    fn add_identifier(
        &mut self,
        entity: specs::Entity,
        identifier: &ast::Identifier<parser::Context>,
    ) -> Result<(), error::Error> {
        let name = &identifier.value;

        let definition = self.current_scope.get(name).cloned().or_else(|| {
            self.scopes
                .iter()
                .rev()
                .flat_map(|scope| scope.get(name).cloned().into_iter())
                .next()
                .map(|e| {
                    use specs::world::Builder;

                    let capture = self.world.create_entity().build();
                    self.world
                        .write_storage()
                        .insert(
                            capture,
                            element::Element::Capture(element::Capture {
                                name: name.clone(),
                                captured: e,
                            }),
                        )
                        .unwrap();

                    self.world
                        .write_storage()
                        .insert(capture, location::Location(identifier.context.span))
                        .unwrap();

                    self.current_captures.push(capture);

                    capture
                })
        });

        let definition = definition.ok_or_else(|| error::Error::UndefinedReference {
            reference: name.clone(),
            location: identifier.context.span,
        })?;

        self.world
            .write_storage()
            .insert(entity, replacement::Replacement { to: definition })
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(identifier.context.span))
            .unwrap();

        Ok(())
    }

    fn add_expression(
        &mut self,
        entity: specs::Entity,
        expression: &ast::Expression<parser::Context>,
    ) -> Result<(), error::Error> {
        match *expression {
            ast::Expression::NumberLiteral(ref v) => self.add_number(entity, v),
            ast::Expression::StringLiteral(ref v) => self.add_string(entity, v),
            ast::Expression::Symbol(ref v) => self.add_symbol(entity, v),
            ast::Expression::Tuple(ref v) => self.add_tuple(entity, v),
            ast::Expression::Record(ref v) => self.add_record(entity, v),
            ast::Expression::UnOp(ref v) => self.add_un_op(entity, v),
            ast::Expression::BiOp(ref v) => self.add_bi_op(entity, v),
            ast::Expression::Identifier(ref v) => self.add_identifier(entity, v),
            ast::Expression::Lambda(ref v) => self.add_lambda(entity, v),
            ast::Expression::Select(ref v) => self.add_select(entity, v),
            ast::Expression::Apply(ref v) => self.add_apply(entity, v),
            ast::Expression::Unknown => panic!("'unknown' AST nodes should not escape the parser"),
        }
    }

    fn add_number(
        &mut self,
        entity: specs::Entity,
        number: &ast::NumberLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Number(ModuleBuilder::from_ast_number(number.value)),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(number.context.span))
            .unwrap();

        Ok(())
    }

    fn from_ast_number(number: ast::NumberValue) -> element::Number {
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

    fn add_string(
        &mut self,
        entity: specs::Entity,
        string: &ast::StringLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::String(match string.value {
                    ast::StringValue::String(ref s) => s.clone(),
                    ast::StringValue::Invalid => {
                        panic!("'invalid' AST nodes should not escape the parser")
                    }
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(string.context.span))
            .unwrap();

        Ok(())
    }

    fn add_tuple(
        &mut self,
        entity: specs::Entity,
        tuple: &ast::Tuple<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let fields = tuple
            .fields
            .iter()
            .map(|f| {
                let e = self.world.create_entity().build();
                self.add_expression(e, f)?;
                Ok(e)
            })
            .collect::<Result<_, error::Error>>()?;

        self.world
            .write_storage()
            .insert(entity, element::Element::Tuple(element::Tuple { fields }))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(tuple.context.span))
            .unwrap();

        Ok(())
    }

    fn add_symbol(
        &mut self,
        entity: specs::Entity,
        symbol: &ast::Symbol<parser::Context>,
    ) -> Result<(), error::Error> {
        let label = symbol.label.clone();

        self.world
            .write_storage()
            .insert(entity, element::Element::Symbol(element::Symbol { label }))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(symbol.context.span))
            .unwrap();

        Ok(())
    }

    fn add_record(
        &mut self,
        entity: specs::Entity,
        record: &ast::Record<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let fields = record
            .fields
            .iter()
            .map(|(i, e)| {
                let en = self.world.create_entity().build();
                self.add_expression(en, e)?;
                Ok((i.value.clone(), en))
            })
            .collect::<Result<_, error::Error>>()?;

        self.world
            .write_storage()
            .insert(entity, element::Element::Record(element::Record { fields }))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(record.context.span))
            .unwrap();

        Ok(())
    }

    fn add_un_op(
        &mut self,
        entity: specs::Entity,
        un_op: &ast::UnOp<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let operator = ModuleBuilder::translate_un_operator(un_op.operator);

        let operand = self.world.create_entity().build();
        self.add_expression(operand, &*un_op.operand)?;

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::UnOp(element::UnOp { operator, operand }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(un_op.context.span))
            .unwrap();

        Ok(())
    }

    fn add_bi_op(
        &mut self,
        entity: specs::Entity,
        bi_op: &ast::BiOp<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let lhs = self.world.create_entity().build();
        self.add_expression(lhs, &*bi_op.lhs)?;

        let operator = ModuleBuilder::translate_bi_operator(bi_op.operator);

        let rhs = self.world.create_entity().build();
        self.add_expression(rhs, &*bi_op.rhs)?;

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::BiOp(element::BiOp { lhs, operator, rhs }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(bi_op.context.span))
            .unwrap();

        Ok(())
    }

    fn add_lambda(
        &mut self,
        entity: specs::Entity,
        lambda: &ast::Lambda<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        // TODO generate unique symbol for anonymous lambdas

        self.push_scope(Some(lambda.parameters.len()), None);

        let parameters = lambda
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity().build();
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
                let e = self.world.create_entity().build();

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

        let signature = self.world.create_entity().build();
        self.add_expression(signature, &*lambda.signature)?;

        let result = if let Some(ref result) = lambda.result {
            let e = self.world.create_entity().build();
            self.add_expression(e, &*result)?;
            e
        } else {
            signature
        };

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Closure(element::Closure {
                    captures: self.current_captures.clone(),
                    parameters,
                    statements,
                    signature,
                    result,
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(lambda.context.span))
            .unwrap();

        self.pop_scope();

        Ok(())
    }

    fn add_variable(
        &mut self,
        entity: specs::Entity,
        variable: &ast::Variable<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        self.symbol
            .push(symbol::Part::Named(variable.name.value.clone()));

        let name = variable.name.value.clone();
        let initializer = self.world.create_entity().build();

        self.add_expression(initializer, &variable.initializer)?;

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Variable(element::Variable { name, initializer }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(variable.context.span))
            .unwrap();

        self.symbol.pop();

        Ok(())
    }

    fn add_select(
        &mut self,
        entity: specs::Entity,
        select: &ast::Select<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let record = self.world.create_entity().build();
        self.add_expression(record, &*select.record)?;

        let field = select.field.value.clone();

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Select(element::Select { record, field }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(select.context.span))
            .unwrap();

        Ok(())
    }

    fn add_apply(
        &mut self,
        entity: specs::Entity,
        apply: &ast::Apply<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let function = self.world.create_entity().build();
        self.add_expression(function, &*apply.function)?;

        let parameters = apply
            .parameters
            .iter()
            .map(|p| {
                let e = self.world.create_entity().build();
                self.add_expression(e, p)?;
                Ok(e)
            })
            .collect::<Result<_, error::Error>>()?;

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Apply(element::Apply {
                    function,
                    parameters,
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(apply.context.span))
            .unwrap();

        Ok(())
    }

    fn add_parameter(
        &mut self,
        entity: specs::Entity,
        parameter: &ast::Parameter<parser::Context>,
    ) -> Result<(), error::Error> {
        use specs::world::Builder;

        let name = parameter.name.value.clone();
        let signature = self.world.create_entity().build();
        self.add_expression(signature, &parameter.signature)?;

        self.world
            .write_storage()
            .insert(
                entity,
                element::Element::Parameter(element::Parameter { name, signature }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, location::Location(parameter.context.span))
            .unwrap();

        Ok(())
    }

    fn push_scope(&mut self, scope_size_hint: Option<usize>, captures_size_hint: Option<usize>) {
        self.scopes.push(mem::replace(
            &mut self.current_scope,
            scope_size_hint
                .map(collections::HashMap::with_capacity)
                .unwrap_or_else(|| collections::HashMap::new()),
        ));
        self.captures.push(mem::replace(
            &mut self.current_captures,
            captures_size_hint
                .map(Vec::with_capacity)
                .unwrap_or_else(|| Vec::new()),
        ));
    }

    fn pop_scope(&mut self) {
        self.current_scope = self.scopes.pop().unwrap();
        self.current_captures = self.captures.pop().unwrap();
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
}
