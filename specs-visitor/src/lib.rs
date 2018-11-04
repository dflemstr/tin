extern crate rayon;
extern crate specs;

use std::collections;
use std::hash;

use rayon::prelude::*;

pub trait VisitEntities {
    fn accept<V>(&self, visitor: &V)
    where
        V: EntityVisitor;

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
        self.par_iter()
            .map(|element| element.accept(visitor))
            .collect()
    }

    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        self.par_iter_mut()
            .map(|element| element.accept_mut(visitor))
            .collect()
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
            .map(|(_, element)| element.accept(visitor))
            .collect()
    }

    fn accept_mut<V>(&mut self, visitor: &V)
    where
        V: EntityVisitorMut,
    {
        self.par_iter_mut()
            .map(|(_, element)| element.accept_mut(visitor))
            .collect()
    }
}

macro_rules! impl_visit_entities_empty {
    ($t:ty) => {
        impl VisitEntities for $t {
            fn accept<V>(&self, _visitor: &V)
            where
                V: EntityVisitor,
            {
            }
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
