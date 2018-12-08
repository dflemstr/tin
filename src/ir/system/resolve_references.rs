use specs;

use ir::component::element;
use ir::component::replacement;
use ir::component::scope;

pub struct ResolveReferencesSystem;

impl<'a> specs::System<'a> for ResolveReferencesSystem {
    type SystemData = (
        specs::Entities<'a>,
        specs::ReadStorage<'a, element::Element>,
        specs::ReadStorage<'a, scope::Scope>,
        specs::WriteStorage<'a, replacement::Replacement>,
    );

    fn run(&mut self, (entities, elements, scopes, mut replacements): Self::SystemData) {
        use specs::Join;

        for (entity, element, scope) in (&entities, &elements, &scopes).join() {
            trace!(
                "resolving references, entity {:?} element {:?} scope {:?}",
                entity,
                element,
                scope
            );
            if let element::Element::Reference(element::Reference(ref ident)) = element {
                if let Some(to) = scope.variables.get(ident) {
                    let to = *to;
                    debug!("resolved reference {:?} to {:?}", ident, to);
                    replacements
                        .insert(entity, replacement::Replacement { to })
                        .unwrap();
                } else {
                    warn!("undefined reference to {:?}", ident);
                }
            }
        }
    }
}
