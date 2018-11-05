use std::collections;

use specs;

use ast;
use ir;
use ir::component::replacement;

pub struct ResolveReferencesSystem<'a> {
    ast: &'a ast::Module<ir::AstContext>,
}

struct ResolveReferencesAstVisitor<'a> {
    storage: specs::WriteStorage<'a, replacement::Replacement>,
    stack: Vec<collections::HashMap<String, specs::Entity>>,
    scope: collections::HashMap<String, specs::Entity>,
}

impl<'a> ResolveReferencesSystem<'a> {
    pub fn new(ast: &'a ast::Module<ir::AstContext>) -> ResolveReferencesSystem<'a> {
        ResolveReferencesSystem { ast }
    }
}

impl<'a, 'c> specs::System<'c> for ResolveReferencesSystem<'a> {
    type SystemData = specs::WriteStorage<'c, replacement::Replacement>;

    fn run(&mut self, data: Self::SystemData) {
        use ast::AstNode;
        self.ast.visit(&mut ResolveReferencesAstVisitor::new(data));
    }
}

impl<'a> ResolveReferencesAstVisitor<'a> {
    fn new(
        storage: specs::WriteStorage<'a, replacement::Replacement>,
    ) -> ResolveReferencesAstVisitor<'a> {
        let stack = Vec::new();
        let scope = collections::HashMap::new();
        ResolveReferencesAstVisitor {
            storage,
            stack,
            scope,
        }
    }
}

impl<'a> ast::visitor::Visitor<ir::AstContext> for ResolveReferencesAstVisitor<'a> {
    fn visit_definition(
        &mut self,
        ident: &ast::Identifier<ir::AstContext>,
        value_context: &ir::AstContext,
    ) {
        trace!("defining {:?} as {:?}", ident.value, value_context.entity);
        self.scope.insert(ident.value.clone(), value_context.entity);
    }

    fn visit_after_identifier(&mut self, ident: &ast::Identifier<ir::AstContext>) {
        trace!("trying to resolve ident {:?}", ident.value);
        if let Some(entity) = self.scope.get(&ident.value) {
            let from = ident.context.entity;
            let to = *entity;
            trace!("replacing entity from {:?} to {:?}", from, to);
            self.storage.insert(from, replacement::Replacement { to });
        } else {
            panic!("unresolved reference: {}", ident.value);
        }
    }

    fn push_scope(&mut self) {
        let scope = self.scope.clone();
        self.stack.push(scope);
        trace!("push scope");
    }

    fn pop_scope(&mut self) {
        self.scope = self
            .stack
            .pop()
            .expect("more pop_scope() than push_scope() calls");
        trace!("pop scope");
    }
}
