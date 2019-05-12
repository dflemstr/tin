use std::collections;
use std::mem;
use std::sync;

use crate::ir;
use crate::ir::element;
use crate::ir::error;
use crate::ir::location;
use crate::syntax::ast;
use crate::syntax::parser;

pub struct Builder<'a, Db> {
    db: &'a Db,
    scope: Scope,
    infos: &'a mut collections::HashMap<ir::Entity, ir::EntityInfo>,
}

#[derive(Debug)]
struct Scope {
    parent: Option<Box<Scope>>,
    entity: ir::Entity,
    locals: collections::HashMap<ir::Ident, ir::Entity>,
    captures: collections::HashMap<ir::Entity, element::Capture>,
}

impl<'a, Db> Builder<'a, Db>
where
    Db: ir::Db,
{
    pub fn new(
        db: &'a Db,
        root: ir::Entity,
        infos: &'a mut collections::HashMap<ir::Entity, ir::EntityInfo>,
    ) -> Self {
        let scope = Scope::new(root);

        Builder { db, scope, infos }
    }

    pub fn build_module(mut self, ast: &ast::Module<parser::Context>) -> Result<(), error::Error> {
        self.add_module(self.scope.entity, ast)?;

        Ok(())
    }

    fn add_module(
        &mut self,
        entity: ir::Entity,
        ast: &ast::Module<parser::Context>,
    ) -> Result<(), error::Error> {
        let mut variables = collections::HashMap::new();

        for variable in &ast.variables {
            let ident = self.db.ident(variable.name.clone());
            let var_entity = self
                .db
                .entity(Some(entity), ir::EntityRole::VariableDefinition(ident));

            self.scope.locals.insert(ident, var_entity);

            variables.insert(ident, var_entity);
        }

        for variable in &ast.variables {
            let ident = self.db.ident(variable.name.clone());
            self.add_variable(variables[&ident], variable)?;
        }

        let element = element::Element::Module(element::Module { variables });
        let location = location::Location(ast.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_reference(
        &mut self,
        entity: ir::Entity,
        ast: &ast::Reference<parser::Context>,
    ) -> Result<(), error::Error> {
        let ident = self.db.ident(ast.value.clone());
        let entity = self
            .db
            .entity(Some(entity), ir::EntityRole::Reference(ident));
        let target = self
            .scope
            .resolve_capture(self.db, ident, ast.context.span)?;

        let element = element::Element::Reference(target);
        let location = location::Location(ast.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_expression(
        &mut self,
        entity: ir::Entity,
        ast: &ast::Expression<parser::Context>,
    ) -> Result<(), error::Error> {
        match *ast {
            ast::Expression::NumberLiteral(ref v) => self.add_number(entity, v),
            ast::Expression::StringLiteral(ref v) => self.add_string(entity, v),
            ast::Expression::Symbol(ref v) => self.add_symbol(entity, v),
            ast::Expression::Tuple(ref v) => self.add_tuple(entity, v),
            ast::Expression::Record(ref v) => self.add_record(entity, v),
            ast::Expression::UnOp(ref v) => self.add_un_op(entity, v),
            ast::Expression::BiOp(ref v) => self.add_bi_op(entity, v),
            ast::Expression::Reference(ref v) => self.add_reference(entity, v),
            ast::Expression::Lambda(ref v) => self.add_lambda(entity, v),
            ast::Expression::Select(ref v) => self.add_select(entity, v),
            ast::Expression::Apply(ref v) => self.add_apply(entity, v),
            ast::Expression::Unknown => panic!("'unknown' AST nodes should not escape the parser"),
        }
    }

    fn add_number(
        &mut self,
        entity: ir::Entity,
        ast: &ast::NumberLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        let element = element::Element::Number(translate_number(ast.value));
        let location = location::Location(ast.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_string(
        &mut self,
        entity: ir::Entity,
        ast: &ast::StringLiteral<parser::Context>,
    ) -> Result<(), error::Error> {
        let element = element::Element::String(match &ast.value {
            ast::StringValue::String(str) => str.clone(),
            ast::StringValue::Invalid => {
                panic!("'invalid' string values should not escape the parser")
            }
        });
        let location = location::Location(ast.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_tuple(
        &mut self,
        entity: ir::Entity,
        tuple: &ast::Tuple<parser::Context>,
    ) -> Result<(), error::Error> {
        let fields = tuple
            .fields
            .iter()
            .enumerate()
            .map(|(index, ast)| {
                let entity = self
                    .db
                    .entity(Some(entity), ir::EntityRole::TupleField(index));
                self.add_expression(entity, &*ast)?;
                Ok(entity)
            })
            .collect::<Result<_, error::Error>>()?;

        let element = element::Element::Tuple(element::Tuple { fields });
        let location = location::Location(tuple.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_symbol(
        &mut self,
        entity: ir::Entity,
        symbol: &ast::Symbol<parser::Context>,
    ) -> Result<(), error::Error> {
        let label = self.db.ident(symbol.label.clone());

        let element = element::Element::Symbol(element::Symbol { label });
        let location = location::Location(symbol.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_record(
        &mut self,
        entity: ir::Entity,
        record: &ast::Record<parser::Context>,
    ) -> Result<(), error::Error> {
        let fields = record
            .fields
            .iter()
            .map(|(field, value)| {
                let ident = self.db.ident(field.clone());
                let entity = self
                    .db
                    .entity(Some(entity), ir::EntityRole::RecordField(ident));
                self.add_expression(entity, value)?;
                Ok((ident, entity))
            })
            .collect::<Result<_, error::Error>>()?;

        let element = element::Element::Record(element::Record { fields });
        let location = location::Location(record.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_un_op(
        &mut self,
        entity: ir::Entity,
        un_op: &ast::UnOp<parser::Context>,
    ) -> Result<(), error::Error> {
        let operator = translate_un_operator(un_op.operator);

        let operand = self.db.entity(Some(entity), ir::EntityRole::UnOperand);
        self.add_expression(operand, &*un_op.operand)?;

        let element = element::Element::UnOp(element::UnOp { operator, operand });
        let location = location::Location(un_op.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_bi_op(
        &mut self,
        entity: ir::Entity,
        bi_op: &ast::BiOp<parser::Context>,
    ) -> Result<(), error::Error> {
        let lhs = self.db.entity(Some(entity), ir::EntityRole::BiLhs);
        self.add_expression(lhs, &*bi_op.lhs)?;

        let operator = translate_bi_operator(bi_op.operator);

        let rhs = self.db.entity(Some(entity), ir::EntityRole::BiRhs);
        self.add_expression(rhs, &*bi_op.rhs)?;

        let element = element::Element::BiOp(element::BiOp { lhs, operator, rhs });
        let location = location::Location(bi_op.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_lambda(
        &mut self,
        entity: ir::Entity,
        lambda: &ast::Lambda<parser::Context>,
    ) -> Result<(), error::Error> {
        // TODO generate unique symbol for anonymous lambdas

        self.push_scope(entity);

        let parameters = lambda
            .parameters
            .iter()
            .map(|parameter| {
                let ident = self.db.ident(parameter.name.clone());
                let entity = self
                    .db
                    .entity(Some(entity), ir::EntityRole::ClosureParameter(ident));
                self.add_parameter(entity, &*parameter)?;
                Ok(entity)
            })
            .collect::<Result<Vec<_>, error::Error>>()?;

        // Defer inserting parameters as variables until here to ensure that one parameter can't
        // depend on another one.
        for (entity, parameter) in parameters.iter().zip(lambda.parameters.iter()) {
            let ident = self.db.ident(parameter.name.clone());
            self.scope.locals.insert(ident, *entity);
        }

        let statements = lambda
            .statements
            .iter()
            .enumerate()
            .map(|(index, statement)| {
                let entity = self
                    .db
                    .entity(Some(entity), ir::EntityRole::ClosureStatement(index));

                match &**statement {
                    ast::Statement::Variable(ref variable) => {
                        let ident = self.db.ident(variable.name.clone());
                        self.scope.locals.insert(ident, entity);
                        self.add_variable(entity, variable)?;
                    }
                    ast::Statement::Expression(ref expression) => {
                        self.add_expression(entity, expression)?;
                    }
                }

                Ok(entity)
            })
            .collect::<Result<Vec<_>, error::Error>>()?;

        let signature = self
            .db
            .entity(Some(entity), ir::EntityRole::ClosureSignature);
        self.add_expression(signature, &*lambda.signature)?;

        let result = if let Some(ref result) = lambda.result {
            let e = self.db.entity(Some(entity), ir::EntityRole::ClosureResult);
            self.add_expression(e, &*result)?;
            e
        } else {
            signature
        };

        let location = location::Location(lambda.context.span);

        let scope = self.pop_scope();
        let captures = scope
            .captures
            .into_iter()
            .map(|(entity, capture)| {
                let ident = capture.name;
                self.infos.insert(
                    entity,
                    ir::EntityInfo::new(element::Element::Capture(capture), location),
                );
                (ident, entity)
            })
            .collect();

        let element = element::Element::Closure(element::Closure {
            captures,
            parameters,
            statements,
            signature,
            result,
        });

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_variable(
        &mut self,
        entity: ir::Entity,
        variable: &ast::Variable<parser::Context>,
    ) -> Result<(), error::Error> {
        let name = self.db.ident(variable.name.clone());

        let initializer = self
            .db
            .entity(Some(entity), ir::EntityRole::VariableInitializer);

        self.add_expression(initializer, &variable.initializer)?;

        let element = element::Element::Variable(element::Variable { name, initializer });
        let location = location::Location(variable.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_select(
        &mut self,
        entity: ir::Entity,
        select: &ast::Select<parser::Context>,
    ) -> Result<(), error::Error> {
        let field = self.db.ident(select.field.clone());
        let record = self
            .db
            .entity(Some(entity), ir::EntityRole::SelectField(field));
        self.add_expression(record, &*select.record)?;

        let element = element::Element::Select(element::Select { record, field });
        let location = location::Location(select.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_apply(
        &mut self,
        entity: ir::Entity,
        apply: &ast::Apply<parser::Context>,
    ) -> Result<(), error::Error> {
        let function = self
            .db
            .entity(Some(entity), ir::EntityRole::AppliedFunction);
        self.add_expression(function, &*apply.function)?;

        let parameters = apply
            .parameters
            .iter()
            .enumerate()
            .map(|(index, parameter)| {
                let entity = self
                    .db
                    .entity(Some(entity), ir::EntityRole::AppliedParameter(index));
                self.add_expression(entity, &*parameter)?;
                Ok(entity)
            })
            .collect::<Result<_, error::Error>>()?;

        let element = element::Element::Apply(element::Apply {
            function,
            parameters,
        });
        let location = location::Location(apply.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn add_parameter(
        &mut self,
        entity: ir::Entity,
        parameter: &ast::Parameter<parser::Context>,
    ) -> Result<(), error::Error> {
        let name = self.db.ident(parameter.name.clone());
        let signature = self
            .db
            .entity(Some(entity), ir::EntityRole::ParameterSignature);
        self.add_expression(signature, &parameter.signature)?;

        let element = element::Element::Parameter(element::Parameter { name, signature });
        let location = location::Location(parameter.context.span);

        self.infos
            .insert(entity, ir::EntityInfo::new(element, location));

        Ok(())
    }

    fn push_scope(&mut self, entity: ir::Entity) {
        let scope = Scope::new(entity);
        let parent_scope = mem::replace(&mut self.scope, scope);
        self.scope.parent = Some(Box::new(parent_scope));
    }

    fn pop_scope(&mut self) -> Scope {
        let parent = self
            .scope
            .parent
            .take()
            .expect("unbalanced push_scope/pop_scope calls");
        mem::replace(&mut self.scope, *parent)
    }
}

impl Scope {
    fn new(entity: ir::Entity) -> Self {
        let parent = None;
        let locals = collections::HashMap::new();
        let captures = collections::HashMap::new();

        Self {
            parent,
            entity,
            locals,
            captures,
        }
    }

    fn resolve_capture(
        &mut self,
        db: &impl ir::Db,
        ident: ir::Ident,
        location: codespan::ByteSpan,
    ) -> Result<ir::Entity, error::Error> {
        match self.locals.entry(ident) {
            collections::hash_map::Entry::Occupied(entry) => Ok(*entry.get()),
            collections::hash_map::Entry::Vacant(entry) => {
                if let Some(parent) = &mut self.parent {
                    let capture_entity = db.entity(
                        Some(self.entity),
                        ir::EntityRole::ClosureCaptureDefinition(ident),
                    );
                    let parent_entity = parent.resolve_capture(db, ident, location)?;
                    self.captures.insert(
                        capture_entity,
                        element::Capture {
                            name: ident,
                            captured: parent_entity,
                        },
                    );
                    entry.insert(capture_entity);
                    Ok(capture_entity)
                } else {
                    let reference = (*db.lookup_ident(ident)).clone();
                    Err(error::Error::UndefinedReference {
                        reference,
                        location,
                    })
                }
            }
        }
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
