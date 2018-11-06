use std::collections;

use specs;

use specs::Component;
use specs::VecStorage;

#[derive(Component, Debug, VisitEntities)]
#[storage(VecStorage)]
pub enum Element {
    Number(f64),
    String(String),
    Tuple {
        fields: Vec<specs::Entity>,
    },
    Record {
        fields: collections::HashMap<String, specs::Entity>,
    },
    Reference(String),
    Select {
        record: specs::Entity,
        field: String,
    },
    Apply {
        function: specs::Entity,
        parameters: Vec<specs::Entity>,
    },
    Parameter {
        name: String,
        signature: Option<specs::Entity>,
    },
    Capture {
        name: String,
        captured: specs::Entity,
    },
    Closure {
        captures: collections::HashMap<String, specs::Entity>,
        parameters: Vec<specs::Entity>,
        statements: Vec<specs::Entity>,
        signature: Option<specs::Entity>,
    },
    Module {
        definitions: collections::HashMap<String, specs::Entity>,
    },
}
