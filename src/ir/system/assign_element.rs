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
        let element = element::Element::Module { definitions };

        trace!("AST node {:?} is element {:?}", module, element);
        self.storage.insert(module.context.entity, element);
    }

    fn visit_after_identifier(&mut self, identifier: &ast::Identifier<ir::AstContext>) {
        let element = element::Element::Reference(identifier.value.clone());

        trace!("AST node {:?} is element {:?}", identifier, element);
        self.storage.insert(identifier.context.entity, element);
    }

    fn visit_after_number(&mut self, number: &ast::NumberLiteral<ir::AstContext>) {
        let element = element::Element::Number(number.value);

        trace!("AST node {:?} is element {:?}", number, element);
        self.storage.insert(number.context.entity, element);
    }

    fn visit_after_string(&mut self, string: &ast::StringLiteral<ir::AstContext>) {
        let element = element::Element::String(string.value.clone());

        trace!("AST node {:?} is element {:?}", string, element);
        self.storage.insert(string.context.entity, element);
    }

    fn visit_after_tuple(&mut self, tuple: &ast::Tuple<ir::AstContext>) {
        use ast::AstNode;
        let fields = tuple.fields.iter().map(|f| f.context().entity).collect();
        let element = element::Element::Tuple { fields };

        trace!("AST node {:?} is element {:?}", tuple, element);
        self.storage.insert(tuple.context.entity, element);
    }

    fn visit_after_record(&mut self, record: &ast::Record<ir::AstContext>) {
        use ast::AstNode;
        let fields = record
            .fields
            .iter()
            .map(|(i, e)| (i.value.clone(), e.context().entity))
            .collect();
        let element = element::Element::Record { fields };

        trace!("AST node {:?} is element {:?}", record, element);
        self.storage.insert(record.context.entity, element);
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
        let element = element::Element::Closure {
            captures,
            parameters,
            statements,
            signature,
        };

        trace!("AST node {:?} is element {:?}", lambda, element);
        self.storage.insert(lambda.context.entity, element);
    }

    fn visit_after_select(&mut self, select: &ast::Select<ir::AstContext>) {
        use ast::AstNode;

        let record = select.record.context().entity;
        let field = select.field.value.clone();
        let element = element::Element::Select { record, field };

        trace!("AST node {:?} is element {:?}", select, element);
        self.storage.insert(select.context.entity, element);
    }

    fn visit_after_apply(&mut self, apply: &ast::Apply<ir::AstContext>) {
        use ast::AstNode;

        let function = apply.function.context().entity;
        let parameters = apply
            .parameters
            .iter()
            .map(|p| p.context().entity)
            .collect();
        let element = element::Element::Apply {
            function,
            parameters,
        };

        trace!("AST node {:?} is element {:?}", apply, element);
        self.storage.insert(apply.context.entity, element);
    }

    fn visit_after_parameter(&mut self, parameter: &ast::Parameter<ir::AstContext>) {
        use ast::AstNode;
        let name = parameter.name.value.clone();
        let signature = parameter.signature.as_ref().map(|s| s.context().entity);
        let element = element::Element::Parameter { name, signature };

        trace!("AST node {:?} is element {:?}", parameter, element);
        self.storage.insert(parameter.context.entity, element);
    }
}
