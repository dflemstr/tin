use specs::Component;
use specs::VecStorage;

use crate::interpreter;

#[derive(Component, Clone, Debug, Fail, PartialEq)]
#[storage(VecStorage)]
pub enum Error {
    #[fail(display = "evaluation error")]
    Evaluation(#[cause] interpreter::error::Error),
}
