use specs;

use super::visit_entities_derive;
use super::visit_entities_mut_derive;

#[test]
fn simple() {
    test_derive! {
        visit_entities_derive {
            struct Foo {
                entity: specs::Entity,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_specs_visitor_VisitEntities_FOR_Foo: () = {
                impl ::specs_visitor::VisitEntities for Foo {
                    fn accept<V>(&self, visitor: &V)
                    where
                        V: ::specs_visitor::EntityVisitor,
                    {
                        match *self {
                            Foo {
                                entity: ref __binding_0,
                            } => {{
                                ::specs_visitor::VisitEntities::accept(__binding_0, visitor);
                            }}
                        }
                    }
                }
            };
        }
    }
}

#[test]
fn simple_mut() {
    test_derive! {
        visit_entities_mut_derive {
            struct Foo {
                entity: specs::Entity,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_specs_visitor_VisitEntitiesMut_FOR_Foo: () = {
                impl ::specs_visitor::VisitEntitiesMut for Foo {
                    fn accept_mut<V>(&mut self, visitor: &V)
                    where
                        V: ::specs_visitor::EntityVisitorMut,
                    {
                        match *self {
                            Foo {
                                entity: ref mut __binding_0,
                            } => {{
                                ::specs_visitor::VisitEntitiesMut::accept_mut(__binding_0, visitor);
                            }}
                        }
                    }
                }
            };
        }
    }
}

#[test]
fn generic() {
    test_derive! {
        visit_entities_derive {
            struct Foo<T> {
                entity_stuff: T,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_specs_visitor_VisitEntities_FOR_Foo: () = {
                impl<T> ::specs_visitor::VisitEntities for Foo<T>
                    where
                        T: ::specs_visitor::VisitEntities
                {
                    fn accept<V>(&self, visitor: &V)
                    where
                        V: ::specs_visitor::EntityVisitor,
                    {
                        match *self {
                            Foo {
                                entity_stuff: ref __binding_0,
                            } => {{
                                ::specs_visitor::VisitEntities::accept(__binding_0, visitor);
                            }}
                        }
                    }
                }
            };
        }
    }
}

#[test]
fn generic_mut() {
    test_derive! {
        visit_entities_mut_derive {
            struct Foo<T> {
                entity_stuff: T,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_specs_visitor_VisitEntitiesMut_FOR_Foo: () = {
                impl<T> ::specs_visitor::VisitEntitiesMut for Foo<T>
                    where
                        T: ::specs_visitor::VisitEntitiesMut
                {
                    fn accept_mut<V>(&mut self, visitor: &V)
                    where
                        V: ::specs_visitor::EntityVisitorMut,
                    {
                        match *self {
                            Foo {
                                entity_stuff: ref mut __binding_0,
                            } => {{
                                ::specs_visitor::VisitEntitiesMut::accept_mut(__binding_0, visitor);
                            }}
                        }
                    }
                }
            };
        }
    }
}
