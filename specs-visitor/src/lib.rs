//! Utilities for visiting specs components and resources very efficiently.
//!
//! This can be used to implement generic transformations of ECS graphs that compile down to very
//! effective code.
//!
//! Use the [`specs-visitor-derive`](https://crates.io/crates/specs-visitor-derive) crate to
//! automatically derive the traits in this crate.
#![deny(nonstandard_style, warnings, unused)]
#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    unstable_features,
    unused_import_braces,
    unused_qualifications
)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy::all, clippy::pedantic))]

use std::collections;
use std::hash;
use std::sync;

/// Support for generically visiting all `specs` entities of a type with a
/// visitor.
///
/// This trait can be derived automatically with `#[derive(VisitEntities)]` using the
/// [`specs-visitor-derive`](https://crates.io/crates/specs-visitor-derive) crate.
pub trait VisitEntities {
    /// Accepts the given visitor.
    ///
    /// The visitor's `visit_entity` method will be called for every entity contained within this
    /// type.  The visitor could potentially be called in parallel.
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor;
}

/// Support for generically mutably visiting all `specs` entities of a type with a
/// visitor.
///
/// This trait can be derived automatically with `#[derive(VisitEntitiesMut)]` using the
/// [`specs-visitor-derive`](https://crates.io/crates/specs-visitor-derive) crate.
pub trait VisitEntitiesMut {
    /// Accepts the given visitor.
    ///
    /// The visitor's `visit_entity_mut` method will be called for every entity contained within
    /// this type.  This gives the visitor mutable references to each entity, allowing them to be
    /// changed.  Note that the visitor itself has a shared borrow instead of a mutable borrow,
    /// because it might potentially be called in parallel.
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut;
}

/// A visitor for visiting entities in a read-only fashion.
///
/// Types implementing this trait are compatible with the `accept` method on the `VisitEntities`
/// trait.
pub trait EntityVisitor: Send + Sync {
    /// Allows the visitor to visit the specified entity.
    ///
    /// This method could potentially be called in parallel.
    fn visit_entity(&self, entity: specs::Entity);
}

/// A visitor for visiting entities mutably.
///
/// Types implementing this trait are compatible with the `accept_mut` method on the
/// `VisitEntitiesMut` trait.
pub trait EntityVisitorMut: Send + Sync {
    /// Allows the visitor to visit the specified entity mutably.
    ///
    /// This method could potentially be called in parallel.
    fn visit_entity_mut(&self, entity: &mut specs::Entity);
}

#[cfg(feature = "parallel")]
fn for_each<'a, A, I, F>(iter: &'a I, action: F)
where
    A: 'a + Send + Sync,
    I: rayon::iter::IntoParallelRefIterator<'a, Item = A>,
    F: Fn(A) + Send + Sync,
{
    use rayon::iter::ParallelIterator;
    iter.par_iter().for_each(action)
}

#[cfg(feature = "parallel")]
fn for_each_mut<'a, A, I, F>(iter: &'a mut I, action: F)
where
    A: 'a + Send + Sync,
    I: rayon::iter::IntoParallelRefMutIterator<'a, Item = A>,
    F: Fn(A) + Send + Sync,
{
    use rayon::iter::ParallelIterator;
    iter.par_iter_mut().for_each(action)
}

#[cfg(not(feature = "parallel"))]
fn for_each<A, I, F>(iter: I, action: F)
where
    I: IntoIterator<Item = A>,
    F: FnMut(A),
{
    iter.into_iter().for_each(action)
}

#[cfg(not(feature = "parallel"))]
fn for_each_mut<A, I, F>(iter: I, action: F)
where
    I: IntoIterator<Item = A>,
    F: FnMut(A),
{
    iter.into_iter().for_each(action)
}

impl VisitEntities for specs::Entity {
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        visitor.visit_entity(*self)
    }
}

impl VisitEntitiesMut for specs::Entity {
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        visitor.visit_entity_mut(self)
    }
}

impl<A> VisitEntities for Option<A>
where
    A: VisitEntities + Send + Sync,
{
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        for_each(self, |element| element.accept(visitor))
    }
}

impl<A> VisitEntitiesMut for Option<A>
where
    A: VisitEntitiesMut + Send + Sync,
{
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        for_each_mut(self, |element| element.accept_mut(visitor))
    }
}

impl<A> VisitEntities for Vec<A>
where
    A: VisitEntities + Send + Sync,
{
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        for_each(self, |element| element.accept(visitor))
    }
}

impl<A> VisitEntitiesMut for Vec<A>
where
    A: VisitEntitiesMut + Send + Sync,
{
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        for_each_mut(self, |element| element.accept_mut(visitor))
    }
}

impl<A> VisitEntities for Box<A>
where
    A: VisitEntities + Send + Sync,
{
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        (**self).accept(visitor)
    }
}

impl<A> VisitEntitiesMut for Box<A>
where
    A: VisitEntitiesMut + Send + Sync,
{
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        (**self).accept_mut(visitor)
    }
}

impl<A, B> VisitEntities for collections::HashMap<A, B>
where
    A: Eq + hash::Hash + Send + Sync,
    B: VisitEntities + Send + Sync,
{
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        for_each(self, |(_, element)| element.accept(visitor))
    }
}

impl<A, B> VisitEntitiesMut for collections::HashMap<A, B>
where
    A: Eq + hash::Hash + Send + Sync,
    B: VisitEntitiesMut + Send + Sync,
{
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        for_each_mut(self, |(_, element)| element.accept_mut(visitor))
    }
}

impl<A> VisitEntities for sync::Arc<A>
where
    A: VisitEntities,
{
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor,
    {
        (**self).accept(visitor)
    }
}

macro_rules! impl_visit_entities_empty {
    ($t:ty) => {
        impl VisitEntities for $t {
            #[inline(always)]
            fn accept<V>(&self, _visitor: &V)
            where
                V: EntityVisitor,
            {
            }
        }

        impl VisitEntitiesMut for $t {
            #[inline(always)]
            fn accept_mut<V>(&mut self, _visitor: &V)
            where
                V: EntityVisitorMut,
            {
            }
        }
    };
}

impl_visit_entities_empty!(i8);
impl_visit_entities_empty!(i16);
impl_visit_entities_empty!(i32);
impl_visit_entities_empty!(i64);
impl_visit_entities_empty!(isize);
impl_visit_entities_empty!(u8);
impl_visit_entities_empty!(u16);
impl_visit_entities_empty!(u32);
impl_visit_entities_empty!(u64);
impl_visit_entities_empty!(usize);
impl_visit_entities_empty!(f32);
impl_visit_entities_empty!(f64);
impl_visit_entities_empty!(bool);
impl_visit_entities_empty!(char);

impl_visit_entities_empty!(String);
