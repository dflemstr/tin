//! Procedural macros for deriving specs entity visitors.
//!
//! These visitors can be used to implement generic transformations of ECS graphs that compile down
//! to very effective code.
//!
//! See the [`specs-visitor`](https://crates.io/crates/specs-visitor) crate for API documentation
//! for the actual types derived by this crate.
#![deny(nonstandard_style, warnings, unused)]
#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    unstable_features,
    unused_import_braces,
    unused_qualifications
)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy::all, clippy::pedantic))]
#![recursion_limit = "128"]

#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;

#[cfg(test)]
mod tests;

fn visit_entities_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    let accept_body = s.each(|bi| {
        quote! {
            ::specs_visitor::VisitEntities::accept(#bi, visitor);
        }
    });

    s.add_bounds(synstructure::AddBounds::Generics);

    s.gen_impl(quote! {
        gen impl ::specs_visitor::VisitEntities for @Self {
            fn accept<V>(&self, visitor: &V)
            where
                V: ::specs_visitor::EntityVisitor,
            {
                match *self {
                    #accept_body
                }
            }
        }
    })
}

fn visit_entities_mut_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    s.bind_with(|_| synstructure::BindStyle::RefMut);

    let accept_mut_body = s.each(|bi| {
        quote! {
            ::specs_visitor::VisitEntitiesMut::accept_mut(#bi, visitor);
        }
    });

    s.add_bounds(synstructure::AddBounds::Generics);

    s.gen_impl(quote! {
        gen impl ::specs_visitor::VisitEntitiesMut for @Self {
            fn accept_mut<V>(&mut self, visitor: &V)
            where
                V: ::specs_visitor::EntityVisitorMut,
            {
                match *self {
                    #accept_mut_body
                }
            }
        }
    })
}

decl_derive!([VisitEntities] => visit_entities_derive);
decl_derive!([VisitEntitiesMut] => visit_entities_mut_derive);
