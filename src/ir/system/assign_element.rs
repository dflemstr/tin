use std::collections;

use specs;

use ast;
use ir;
use ir::component::element;

pub struct AssignElementSystem<'a> {
    ast: &'a ast::Module<ir::AstContext>,
}

struct AssignElementAstVisitor<'a> {
    storage: specs::WriteStorage<'a, element::Element>,
}

impl<'a> AssignElementSystem<'a> {
    pub fn new(ast: &'a ast::Module<ir::AstContext>) -> AssignElementSystem<'a> {
        AssignElementSystem { ast }
    }
}

impl<'a, 'c> specs::System<'c> for AssignElementSystem<'a> {
    type SystemData = specs::WriteStorage<'c, element::Element>;

    fn run(&mut self, data: Self::SystemData) {
        use ast::AstNode;
        self.ast.visit(&mut AssignElementAstVisitor::new(data));
    }
}

impl<'a> AssignElementAstVisitor<'a> {
    fn new(storage: specs::WriteStorage<'a, element::Element>) -> AssignElementAstVisitor<'a> {
        AssignElementAstVisitor { storage }
    }
}

impl<'a> ast::visitor::Visitor<ir::AstContext> for AssignElementAstVisitor<'a> {
    fn visit_after_module(&mut self, module: &ast::Module<ir::AstContext>) {
        use ast::AstNode;
        let definitions = module
            .definitions
            .iter()
            .map(|(i, v)| (i.value.clone(), v.context().entity))
            .collect();
        self.storage.insert(
            module.context.entity,
            element::Element::Module { definitions },
        );
    }

    fn visit_after_identifier(&mut self, identifier: &ast::Identifier<ir::AstContext>) {
        self.storage.insert(
            identifier.context.entity,
            element::Element::Reference(identifier.value.clone()),
        );
    }

    fn visit_after_number(&mut self, number: &ast::NumberLiteral<ir::AstContext>) {
        self.storage.insert(
            number.context.entity,
            element::Element::Number(number.value),
        );
    }

    fn visit_after_string(&mut self, string: &ast::StringLiteral<ir::AstContext>) {
        self.storage.insert(
            string.context.entity,
            element::Element::String(string.value.clone()),
        );
    }

    fn visit_after_tuple(&mut self, tuple: &ast::Tuple<ir::AstContext>) {
        use ast::AstNode;
        let fields = tuple.fields.iter().map(|f| f.context().entity).collect();
        self.storage
            .insert(tuple.context.entity, element::Element::Tuple { fields });
    }

    fn visit_after_record(&mut self, record: &ast::Record<ir::AstContext>) {
        use ast::AstNode;
        let fields = record
            .fields
            .iter()
            .map(|(i, e)| (i.value.clone(), e.context().entity))
            .collect();
        self.storage
            .insert(record.context.entity, element::Element::Record { fields });
    }

    fn visit_after_lambda(&mut self, lambda: &ast::Lambda<ir::AstContext>) {
        use ast::AstNode;

        let captures = collections::HashMap::new();
        let parameters = lambda.parameters.iter().map(|p| p.context.entity).collect();
        let statements = lambda
            .statements
            .iter()
            .map(|p| p.context().entity)
            .collect();
        let signature = lambda.signature.as_ref().map(|s| s.context().entity);

        self.storage.insert(
            lambda.context.entity,
            element::Element::Closure {
                captures,
                parameters,
                statements,
                signature,
            },
        );
    }

    fn visit_after_select(&mut self, select: &ast::Select<ir::AstContext>) {
        use ast::AstNode;

        let record = select.record.context().entity;
        let field = select.field.value.clone();

        self.storage.insert(
            select.context.entity,
            element::Element::Select { record, field },
        );
    }

    fn visit_after_apply(&mut self, apply: &ast::Apply<ir::AstContext>) {
        use ast::AstNode;

        let function = apply.function.context().entity;
        let parameters = apply
            .parameters
            .iter()
            .map(|p| p.context().entity)
            .collect();

        self.storage.insert(
            apply.context.entity,
            element::Element::Apply {
                function,
                parameters,
            },
        );
    }

    fn visit_after_parameter(&mut self, parameter: &ast::Parameter<ir::AstContext>) {
        use ast::AstNode;
        let name = parameter.name.value.clone();
        let signature = parameter.signature.as_ref().map(|s| s.context().entity);

        self.storage.insert(
            parameter.context.entity,
            element::Element::Parameter { name, signature },
        );
    }
}
