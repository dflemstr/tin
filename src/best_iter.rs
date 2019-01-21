pub trait BestJoin {
    type Iter: BestIterator;

    fn best_join(self) -> Self::Iter
    where
        Self: Sized;
}

pub trait BestIterator {
    type Item: Send;

    fn best_for_each<F>(self, op: F)
    where
        F: Fn(Self::Item) + Sync + Send;
}

pub trait BestIteratorMap<F, R>: BestIterator
where
    F: Fn(Self::Item) -> R + Sync + Send,
    R: Send,
{
    type Map;

    fn best_map(self, map_op: F) -> Self::Map;
}

pub trait BestIteratorFlatMap<F, II>: BestIterator
where
    F: Fn(Self::Item) -> II + Sync + Send,
{
    type FlatMap;

    fn best_flat_map(self, map_op: F) -> Self::FlatMap;
}

pub trait BestIteratorCollect<C>: BestIterator {
    fn best_collect(self) -> C;
}

#[cfg(feature = "parallel")]
impl<J> BestJoin for J
where
    J: specs::Join + specs::ParJoin + Send,
    J::Mask: Send + Sync,
    J::Type: Send,
    J::Value: Send,
{
    type Iter = specs::join::JoinParIter<Self>;

    fn best_join(self) -> Self::Iter
    where
        Self: Sized,
    {
        specs::join::ParJoin::par_join(self)
    }
}

#[cfg(not(feature = "parallel"))]
impl<J> BestJoin for J
where
    J: specs::Join,
    J::Type: Send,
{
    type Iter = specs::join::JoinIter<Self>;

    fn best_join(self) -> Self::Iter
    where
        Self: Sized,
    {
        specs::join::Join::join(self)
    }
}

#[cfg(feature = "parallel")]
impl<I> BestIterator for I
where
    I: rayon::iter::ParallelIterator,
{
    type Item = I::Item;

    fn best_for_each<F>(self, op: F)
    where
        F: Fn(Self::Item) + Sync + Send,
    {
        rayon::iter::ParallelIterator::for_each(self, op)
    }
}

#[cfg(not(feature = "parallel"))]
impl<I> BestIterator for I
where
    I: Iterator,
    I::Item: Send,
{
    type Item = I::Item;

    fn best_for_each<F>(self, op: F)
    where
        F: Fn(Self::Item) + Sync + Send,
    {
        Iterator::for_each(self, op)
    }
}

#[cfg(feature = "parallel")]
impl<I, F, R> BestIteratorMap<F, R> for I
where
    I: rayon::iter::ParallelIterator,
    F: Fn(Self::Item) -> R + Sync + Send,
    R: Send,
{
    type Map = rayon::iter::Map<Self, F>;

    fn best_map(self, map_op: F) -> Self::Map {
        rayon::iter::ParallelIterator::map(self, map_op)
    }
}

#[cfg(not(feature = "parallel"))]
impl<I, F, R> BestIteratorMap<F, R> for I
where
    I: Iterator,
    I::Item: Send,
    F: Fn(Self::Item) -> R + Sync + Send,
    R: Send,
{
    type Map = ::std::iter::Map<Self, F>;

    fn best_map(self, map_op: F) -> Self::Map {
        Iterator::map(self, map_op)
    }
}

#[cfg(feature = "parallel")]
impl<I, F, II> BestIteratorFlatMap<F, II> for I
where
    I: rayon::iter::ParallelIterator,
    II: rayon::iter::IntoParallelIterator,
    F: Fn(Self::Item) -> II + Sync + Send,
{
    type FlatMap = rayon::iter::FlatMap<Self, F>;

    fn best_flat_map(self, map_op: F) -> Self::FlatMap {
        rayon::iter::ParallelIterator::flat_map(self, map_op)
    }
}

#[cfg(not(feature = "parallel"))]
impl<I, F, II> BestIteratorFlatMap<F, II> for I
where
    I: Iterator,
    I::Item: Send,
    II: ::std::iter::IntoIterator,
    F: Fn(Self::Item) -> II + Sync + Send,
{
    type FlatMap = std::iter::FlatMap<Self, II, F>;

    fn best_flat_map(self, map_op: F) -> Self::FlatMap {
        Iterator::flat_map(self, map_op)
    }
}

#[cfg(feature = "parallel")]
impl<I, C> BestIteratorCollect<C> for I
where
    I: rayon::iter::ParallelIterator,
    C: rayon::iter::FromParallelIterator<Self::Item> + Send,
{
    fn best_collect(self) -> C {
        rayon::iter::ParallelIterator::collect(self)
    }
}

#[cfg(not(feature = "parallel"))]
impl<I, C> BestIteratorCollect<C> for I
where
    I: Iterator,
    I::Item: Send,
    C: ::std::iter::FromIterator<Self::Item> + Send,
{
    fn best_collect(self) -> C {
        Iterator::collect(self)
    }
}
