//! Intermediate representation variables for the compiler and interpreter.

use std::collections;
use std::fmt;
use std::mem;

use specs;

use crate::ast;
use crate::diagnostic;
use crate::interpreter;
use crate::parser;

pub mod component;
mod system;

/// A separate universe of the Tin intermediate representation.
pub struct Ir {
    pub(crate) world: specs::World,
}

/// Errors that may occur while building and interacting with an [`Ir`].
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    /// The IR cannot be built because something refers to an identifier that is not defined.
    #[fail(display = "undefined reference to {:?}", reference)]
    UndefinedReference {
        /// The identifier of the undefined reference.
        reference: String,
        /// The location of the reference.
        location: codespan::ByteSpan,
    },

    /// The IR has a type error.
    #[fail(display = "type error")]
    Type(
        codespan::ByteSpan,
        #[cause] component::ty::TypeError<codespan::ByteSpan>,
    ),

    /// The IR has a constexpr that cannot be evaluated.
    #[fail(display = "constexpr error")]
    Constexpr(
        codespan::ByteSpan,
        #[cause] component::constexpr::ConstexprError,
    ),

    /// There were multiple IR errors.
    #[fail(display = "multiple IR errors")]
    Multiple {
        /// The errors, in the order they were encountered in the source code.
        errors: Vec<Error>,
    },
}

struct ModuleBuilder<'a> {
    world: &'a mut specs::World,
    symbol: Vec<component::symbol::Part>,
    current_scope: collections::HashMap<String, specs::Entity>,
    scopes: Vec<collections::HashMap<String, specs::Entity>>,
    current_captures: Vec<specs::Entity>,
    captures: Vec<Vec<specs::Entity>>,
}

impl Ir {
    /// Creates a new IR instance.
    pub fn new() -> Ir {
        let mut world = specs::World::new();
        component::register_all(&mut world);
        Ir { world }
    }

    /// Adds the specified AST module to the IR world.
    pub fn load(&mut self, module: &ast::Module<parser::Context>) -> Result<(), Error> {
        use specs::world::Builder;

        let entity = self.world.create_entity().build();
        ModuleBuilder::new(&mut self.world).add_module(entity, module)?;

        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(
                system::apply_replacements::System,
                "apply_replacements",
                &[],
            )
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.maintain()?;

        Ok(())
    }

    /// Checks and infers types for all known variables.
    ///
    /// `resolve_references` should be called before this; types will not be inferred for unresolved
    /// references.
    pub fn check_types(&mut self) -> Result<(), Error> {
        let mut dispatcher = specs::DispatcherBuilder::new()
            .with(system::infer_types::System, "infer_types", &[])
            .with(system::infer_constexpr::System, "infer_constexpr", &[])
            .with(system::infer_layouts::System::new(8), "infer_layouts", &[])
            .build();

        dispatcher.dispatch(&mut self.world.res);

        self.maintain()?;

        Ok(())
    }

    fn maintain(&mut self) -> Result<(), Error> {
        use specs::Join;

        self.world.maintain();

        let mut errors = Vec::new();

        let entities = self.world.entities();
        let locations = self.world.read_storage::<component::location::Location>();
        let type_errors = self
            .world
            .read_storage::<component::ty::TypeError<specs::Entity>>();
        let constexpr_errors = self
            .world
            .read_storage::<component::constexpr::ConstexprError>();

        for (entity, type_error) in (&entities, &type_errors).join() {
            errors.push(Error::Type(
                locations.get(entity).unwrap().0,
                component::ty::TypeError {
                    expected: type_error.expected.clone(),
                    actual: type_error.actual.clone(),
                    main_entity: locations.get(type_error.main_entity).unwrap().0,
                    aux_entities: type_error
                        .aux_entities
                        .iter()
                        .map(|aux_entity| component::ty::AuxEntity {
                            entity: locations.get(aux_entity.entity).unwrap().0,
                            label: aux_entity.label.clone(),
                        })
                        .collect(),
                },
            ))
        }

        for (entity, constexpr_error, _) in (&entities, &constexpr_errors, !&type_errors).join() {
            errors.push(Error::Constexpr(
                locations.get(entity).unwrap().0,
                constexpr_error.clone(),
            ));
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors.pop().unwrap_or_else(|| Error::Multiple { errors }))
        }
    }
}

impl<'a> ModuleBuilder<'a> {
    fn new(world: &'a mut specs::World) -> ModuleBuilder<'a> {
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

    fn add_module(
        &mut self,
        entity: specs::Entity,
        ast: &ast::Module<parser::Context>,
    ) -> Result<(), Error> {
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
                component::element::Element::Module(component::element::Module { variables }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(ast.context.span))
            .unwrap();

        Ok(())
    }

    fn add_identifier(
        &mut self,
        entity: specs::Entity,
        identifier: &ast::Identifier<parser::Context>,
    ) -> Result<(), Error> {
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
                            component::element::Element::Capture(component::element::Capture {
                                name: name.clone(),
                                captured: e,
                            }),
                        )
                        .unwrap();

                    self.world
                        .write_storage()
                        .insert(
                            capture,
                            component::location::Location(identifier.context.span),
                        )
                        .unwrap();

                    self.current_captures.push(capture);

                    capture
                })
        });

        let definition = definition.ok_or_else(|| Error::UndefinedReference {
            reference: name.clone(),
            location: identifier.context.span,
        })?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::replacement::Replacement { to: definition },
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(
                entity,
                component::location::Location(identifier.context.span),
            )
            .unwrap();

        Ok(())
    }

    fn add_expression(
        &mut self,
        entity: specs::Entity,
        expression: &ast::Expression<parser::Context>,
    ) -> Result<(), Error> {
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
    ) -> Result<(), Error> {
        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Number(ModuleBuilder::from_ast_number(number.value)),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(number.context.span))
            .unwrap();

        Ok(())
    }

    fn from_ast_number(number: ast::NumberValue) -> component::element::Number {
        match number {
            ast::NumberValue::U8(n) => component::element::Number::U8(n),
            ast::NumberValue::U16(n) => component::element::Number::U16(n),
            ast::NumberValue::U32(n) => component::element::Number::U32(n),
            ast::NumberValue::U64(n) => component::element::Number::U64(n),
            ast::NumberValue::I8(n) => component::element::Number::I8(n),
            ast::NumberValue::I16(n) => component::element::Number::I16(n),
            ast::NumberValue::I32(n) => component::element::Number::I32(n),
            ast::NumberValue::I64(n) => component::element::Number::I64(n),
            ast::NumberValue::F32(n) => component::element::Number::F32(n),
            ast::NumberValue::F64(n) => component::element::Number::F64(n),
            ast::NumberValue::Invalid => panic!("'invalid' AST nodes should not escape the parser"),
        }
    }

    fn add_string(
        &mut self,
        entity: specs::Entity,
        string: &ast::StringLiteral<parser::Context>,
    ) -> Result<(), Error> {
        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::String(match string.value {
                    ast::StringValue::String(ref s) => s.clone(),
                    ast::StringValue::Invalid => {
                        panic!("'invalid' AST nodes should not escape the parser")
                    }
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(string.context.span))
            .unwrap();

        Ok(())
    }

    fn add_tuple(
        &mut self,
        entity: specs::Entity,
        tuple: &ast::Tuple<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        let fields = tuple
            .fields
            .iter()
            .map(|f| {
                let e = self.world.create_entity().build();
                self.add_expression(e, f)?;
                Ok(e)
            })
            .collect::<Result<_, Error>>()?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Tuple(component::element::Tuple { fields }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(tuple.context.span))
            .unwrap();

        Ok(())
    }

    fn add_symbol(
        &mut self,
        entity: specs::Entity,
        symbol: &ast::Symbol<parser::Context>,
    ) -> Result<(), Error> {
        let label = symbol.label.clone();

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Symbol(component::element::Symbol { label }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(symbol.context.span))
            .unwrap();

        Ok(())
    }

    fn add_record(
        &mut self,
        entity: specs::Entity,
        record: &ast::Record<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        let fields = record
            .fields
            .iter()
            .map(|(i, e)| {
                let en = self.world.create_entity().build();
                self.add_expression(en, e)?;
                Ok((i.value.clone(), en))
            })
            .collect::<Result<_, Error>>()?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Record(component::element::Record { fields }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(record.context.span))
            .unwrap();

        Ok(())
    }

    fn add_un_op(
        &mut self,
        entity: specs::Entity,
        un_op: &ast::UnOp<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        let operator = ModuleBuilder::translate_un_operator(un_op.operator);

        let operand = self.world.create_entity().build();
        self.add_expression(operand, &*un_op.operand)?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::UnOp(component::element::UnOp { operator, operand }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(un_op.context.span))
            .unwrap();

        Ok(())
    }

    fn add_bi_op(
        &mut self,
        entity: specs::Entity,
        bi_op: &ast::BiOp<parser::Context>,
    ) -> Result<(), Error> {
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
                component::element::Element::BiOp(component::element::BiOp { lhs, operator, rhs }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(bi_op.context.span))
            .unwrap();

        Ok(())
    }

    fn add_lambda(
        &mut self,
        entity: specs::Entity,
        lambda: &ast::Lambda<parser::Context>,
    ) -> Result<(), Error> {
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
            .collect::<Result<Vec<_>, Error>>()?;

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
            .collect::<Result<Vec<_>, Error>>()?;

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
                component::element::Element::Closure(component::element::Closure {
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
            .insert(entity, component::symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(lambda.context.span))
            .unwrap();

        self.pop_scope();

        Ok(())
    }

    fn add_variable(
        &mut self,
        entity: specs::Entity,
        variable: &ast::Variable<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        self.symbol
            .push(component::symbol::Part::Named(variable.name.value.clone()));

        let name = variable.name.value.clone();
        let initializer = self.world.create_entity().build();

        self.add_expression(initializer, &variable.initializer)?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Variable(component::element::Variable {
                    name,
                    initializer,
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::symbol::Symbol::new(self.symbol.clone()))
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(variable.context.span))
            .unwrap();

        self.symbol.pop();

        Ok(())
    }

    fn add_select(
        &mut self,
        entity: specs::Entity,
        select: &ast::Select<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        let record = self.world.create_entity().build();
        self.add_expression(record, &*select.record)?;

        let field = select.field.value.clone();

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Select(component::element::Select { record, field }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(select.context.span))
            .unwrap();

        Ok(())
    }

    fn add_apply(
        &mut self,
        entity: specs::Entity,
        apply: &ast::Apply<parser::Context>,
    ) -> Result<(), Error> {
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
            .collect::<Result<_, Error>>()?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Apply(component::element::Apply {
                    function,
                    parameters,
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(entity, component::location::Location(apply.context.span))
            .unwrap();

        Ok(())
    }

    fn add_parameter(
        &mut self,
        entity: specs::Entity,
        parameter: &ast::Parameter<parser::Context>,
    ) -> Result<(), Error> {
        use specs::world::Builder;

        let name = parameter.name.value.clone();
        let signature = self.world.create_entity().build();
        self.add_expression(signature, &parameter.signature)?;

        self.world
            .write_storage()
            .insert(
                entity,
                component::element::Element::Parameter(component::element::Parameter {
                    name,
                    signature,
                }),
            )
            .unwrap();

        self.world
            .write_storage()
            .insert(
                entity,
                component::location::Location(parameter.context.span),
            )
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

    fn translate_un_operator(un_operator: ast::UnOperator) -> component::element::UnOperator {
        match un_operator {
            ast::UnOperator::Not => component::element::UnOperator::Not,
            ast::UnOperator::BNot => component::element::UnOperator::BNot,
            ast::UnOperator::Cl0 => component::element::UnOperator::Cl0,
            ast::UnOperator::Cl1 => component::element::UnOperator::Cl1,
            ast::UnOperator::Cls => component::element::UnOperator::Cls,
            ast::UnOperator::Ct0 => component::element::UnOperator::Ct0,
            ast::UnOperator::Ct1 => component::element::UnOperator::Ct1,
            ast::UnOperator::C0 => component::element::UnOperator::C0,
            ast::UnOperator::C1 => component::element::UnOperator::C1,
            ast::UnOperator::Sqrt => component::element::UnOperator::Sqrt,
        }
    }

    fn translate_bi_operator(bi_operator: ast::BiOperator) -> component::element::BiOperator {
        match bi_operator {
            ast::BiOperator::Eq => component::element::BiOperator::Eq,
            ast::BiOperator::Ne => component::element::BiOperator::Ne,
            ast::BiOperator::Lt => component::element::BiOperator::Lt,
            ast::BiOperator::Ge => component::element::BiOperator::Ge,
            ast::BiOperator::Gt => component::element::BiOperator::Gt,
            ast::BiOperator::Le => component::element::BiOperator::Le,
            ast::BiOperator::Cmp => component::element::BiOperator::Cmp,
            ast::BiOperator::Add => component::element::BiOperator::Add,
            ast::BiOperator::Sub => component::element::BiOperator::Sub,
            ast::BiOperator::Mul => component::element::BiOperator::Mul,
            ast::BiOperator::Div => component::element::BiOperator::Div,
            ast::BiOperator::Rem => component::element::BiOperator::Rem,
            ast::BiOperator::And => component::element::BiOperator::And,
            ast::BiOperator::BAnd => component::element::BiOperator::BAnd,
            ast::BiOperator::Or => component::element::BiOperator::Or,
            ast::BiOperator::BOr => component::element::BiOperator::BOr,
            ast::BiOperator::Xor => component::element::BiOperator::Xor,
            ast::BiOperator::BXor => component::element::BiOperator::BXor,
            ast::BiOperator::AndNot => component::element::BiOperator::AndNot,
            ast::BiOperator::BAndNot => component::element::BiOperator::BAndNot,
            ast::BiOperator::OrNot => component::element::BiOperator::OrNot,
            ast::BiOperator::BOrNot => component::element::BiOperator::BOrNot,
            ast::BiOperator::XorNot => component::element::BiOperator::XorNot,
            ast::BiOperator::BXorNot => component::element::BiOperator::BXorNot,
            ast::BiOperator::RotL => component::element::BiOperator::RotL,
            ast::BiOperator::RotR => component::element::BiOperator::RotR,
            ast::BiOperator::ShL => component::element::BiOperator::ShL,
            ast::BiOperator::ShR => component::element::BiOperator::ShR,
        }
    }
}

impl fmt::Debug for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Ir").finish()
    }
}

impl diagnostic::Diagnostic for Error {
    fn to_diagnostics(&self, result: &mut Vec<codespan_reporting::Diagnostic>) {
        let message = self.to_string();
        match *self {
            Error::UndefinedReference { location, .. } => {
                result.push(codespan_reporting::Diagnostic {
                    severity: codespan_reporting::Severity::Error,
                    code: None,
                    message,
                    labels: vec![codespan_reporting::Label {
                        span: location,
                        message: None,
                        style: codespan_reporting::LabelStyle::Primary,
                    }],
                });
            }
            Error::Type(entity, ref type_error) => match type_error {
                component::ty::TypeError {
                    ref main_entity,
                    ref aux_entities,
                    ..
                } => {
                    let mut labels = Vec::with_capacity(aux_entities.len() + 2);

                    labels.push(codespan_reporting::Label {
                        span: entity,
                        message: None,
                        style: codespan_reporting::LabelStyle::Primary,
                    });

                    labels.push(codespan_reporting::Label {
                        span: *main_entity,
                        message: Some(type_error.to_string()),
                        style: codespan_reporting::LabelStyle::Primary,
                    });

                    for aux_entity in aux_entities {
                        labels.push(codespan_reporting::Label {
                            span: aux_entity.entity,
                            message: Some(aux_entity.label.clone()),
                            style: codespan_reporting::LabelStyle::Secondary,
                        });
                    }

                    result.push(codespan_reporting::Diagnostic {
                        severity: codespan_reporting::Severity::Error,
                        code: None,
                        message,
                        labels,
                    });
                }
            },
            Error::Constexpr(entity, ref constexpr_error) => match constexpr_error {
                component::constexpr::ConstexprError::Evaluation(ref interpreter_error) => {
                    match interpreter_error {
                        interpreter::Error::RuntimeTypeConflict(cause) => {
                            let labels = vec![codespan_reporting::Label {
                                span: entity,
                                message: Some(cause.clone()),
                                style: codespan_reporting::LabelStyle::Primary,
                            }];
                            result.push(codespan_reporting::Diagnostic {
                                severity: codespan_reporting::Severity::Bug,
                                code: None,
                                message: "runtime type error during constexpr evaluation"
                                    .to_owned(),
                                labels,
                            });
                        }
                        interpreter::Error::EvaluationError(cause) => {
                            result.push(codespan_reporting::Diagnostic {
                                severity: codespan_reporting::Severity::Error,
                                code: None,
                                message: "error during constexpr evaluation".to_owned(),
                                labels: vec![codespan_reporting::Label {
                                    span: entity,
                                    message: Some(cause.to_string()),
                                    style: codespan_reporting::LabelStyle::Primary,
                                }],
                            });
                        }
                    }
                }
            },
            Error::Multiple { ref errors } => {
                for error in errors {
                    error.to_diagnostics(result);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger;
    use failure;

    use super::*;
    use crate::ast;
    use crate::test_util;

    #[test]
    fn entity_assignments() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
pickFirst = |a: u32, b: u32| -> u32 {
  capture = |x: u32| -> u32 { a };
  capture(b)
};
main = || -> u32 { pickFirst(1u32, 2u32) };
"#;
        let expected = Ok(());
        let actual = check_module("entity_assignments", source);

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn recursive_module_variables() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
a = || -> u32 {
  b()
};
b = || -> u32 {
  a()
};
"#;
        let expected = Ok(());
        let actual = check_module("recursive_module_variables", source);

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn lexically_scoped_closure_vars() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
a = || -> u32 {
  b = || -> u32 {
    c = 3u32;
    c
  };
  c
};
"#;
        let expected = Err(r#"error: undefined reference to "c"
- <lexically_scoped_closure_vars>:7:3
7 |   c
  |   ^
"#
        .to_owned());
        let actual = check_module("lexically_scoped_closure_vars", source);

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn ordered_local_vars() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
a = || -> u32 {
  b = c;
  c = 3u32;
  b
};
"#;
        let expected = Err(r#"error: undefined reference to "c"
- <ordered_local_vars>:3:7
3 |   b = c;
  |       ^
"#
        .to_owned());
        let actual = check_module("ordered_local_vars", source);

        assert_eq!(expected, actual);

        Ok(())
    }

    #[test]
    fn type_error() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
a = || -> u32 {
  1f32 + 2f64
};
"#;
        let expected = Err(r#"error: type error
- <type_error>:3:3
3 |   1f32 + 2f64
  |   ^^^^^^^^^^^
- <type_error>:3:10
3 |   1f32 + 2f64
  |          ^^^^ expected f32 but got f64
- <type_error>:3:3
3 |   1f32 + 2f64
  |   ---- other operand has type f32
"#
        .to_owned());
        let actual = check_module("type_error", source);

        assert_eq!(expected, actual);

        Ok(())
    }

    fn check_module(name: &'static str, source: &str) -> Result<(), String> {
        use crate::parser::Parse;

        let mut codemap = codespan::CodeMap::new();
        let span = codemap
            .add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned())
            .span();
        let ast_module =
            ast::Module::parse(span, source).map_err(|e| test_util::format_error(&codemap, e))?;

        let mut ir = Ir::new();
        ir.load(&ast_module)
            .map_err(|e| test_util::format_error(&codemap, e))?;
        ir.check_types()
            .map_err(|e| test_util::format_error(&codemap, e))?;

        test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &ir).unwrap();

        Ok(())
    }
}
