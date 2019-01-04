extern crate rayon;
extern crate specs;

use std::collections;
use std::hash;
use std::sync;

use rayon::prelude::*;

pub trait VisitEntities {
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor;
}

pub trait VisitEntitiesMut {
    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut;
}

pub trait EntityVisitor: Send + Sync {
    fn visit_entity(&self, entity: specs::Entity);
}

pub trait EntityVisitorMut: Send + Sync {
    fn visit_entity_mut(&self, entity: &mut specs::Entity);
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
        self.par_iter().for_each(|element| element.accept(visitor))
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
        self.par_iter_mut()
            .for_each(|element| element.accept_mut(visitor))
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
        self.par_iter().for_each(|element| element.accept(visitor))
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
        self.par_iter_mut()
            .for_each(|element| element.accept_mut(visitor))
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
        self.par_iter()
            .for_each(|(_, element)| element.accept(visitor))
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
        self.par_iter_mut()
            .for_each(|(_, element)| element.accept_mut(visitor))
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
