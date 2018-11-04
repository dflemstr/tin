#![recursion_limit = "128"]

#[cfg(test)]
extern crate specs;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;
extern crate proc_macro2;

fn visit_entities_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    let accept_body = s.each(|bi| {
        quote! {
            specs_visitor::VisitEntities::accept(#bi, visitor);
        }
    });
    s.bind_with(|_| synstructure::BindStyle::RefMut);
    let accept_mut_body = s.each(|bi| {
        quote! {
            specs_visitor::VisitEntities::accept_mut(#bi, visitor);
        }
    });

    s.add_bounds(synstructure::AddBounds::Generics);

    s.gen_impl(quote! {
        extern crate specs_visitor;

        gen impl specs_visitor::VisitEntities for @Self {
            fn accept<V>(&self, visitor: &V)
            where
                V: specs_visitor::EntityVisitor,
            {
                match *self {
                    #accept_body
                }
            }

            fn accept_mut<V>(&mut self, visitor: &V)
            where
                V: specs_visitor::EntityVisitorMut,
            {
                match *self {
                    #accept_mut_body
                }
            }
        }
    })
}

decl_derive!([VisitEntities] => visit_entities_derive);

#[cfg(test)]
mod test {
    use specs;

    use super::visit_entities_derive;

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
                    extern crate specs_visitor;
                    impl specs_visitor::VisitEntities for Foo {
                        fn accept<V>(&self, visitor: &V)
                        where
                            V: specs_visitor::EntityVisitor,
                        {
                            match *self {
                                Foo {
                                    entity: ref __binding_0,
                                } => {{
                                    specs_visitor::VisitEntities::accept(__binding_0, visitor);
                                }}
                            }
                        }
                        fn accept_mut<V>(&mut self, visitor: &V)
                        where
                            V: specs_visitor::EntityVisitorMut,
                        {
                            match *self {
                                Foo {
                                    entity: ref mut __binding_0,
                                } => {{
                                    specs_visitor::VisitEntities::accept_mut(__binding_0, visitor);
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
                    extern crate specs_visitor;
                    impl<T> specs_visitor::VisitEntities for Foo<T>
                        where
                            T: specs_visitor::VisitEntities
                    {
                        fn accept<V>(&self, visitor: &V)
                        where
                            V: specs_visitor::EntityVisitor,
                        {
                            match *self {
                                Foo {
                                    entity_stuff: ref __binding_0,
                                } => {{
                                    specs_visitor::VisitEntities::accept(__binding_0, visitor);
                                }}
                            }
                        }
                        fn accept_mut<V>(&mut self, visitor: &V)
                        where
                            V: specs_visitor::EntityVisitorMut,
                        {
                            match *self {
                                Foo {
                                    entity_stuff: ref mut __binding_0,
                                } => {{
                                    specs_visitor::VisitEntities::accept_mut(__binding_0, visitor);
                                }}
                            }
                        }
                    }
                };
            }
        }
    }
}
