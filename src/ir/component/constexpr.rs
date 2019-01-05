use std::sync;

use specs::Component;
use specs::VecStorage;

use crate::interpreter;
use crate::value;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Constexpr {
    pub value: sync::Arc<value::Value>,
}

#[derive(Component, Clone, Debug, Fail, PartialEq)]
#[storage(VecStorage)]
pub enum ConstexprError {
    #[fail(display = "evaluation error")]
    Evaluation(#[cause] interpreter::Error),
}
