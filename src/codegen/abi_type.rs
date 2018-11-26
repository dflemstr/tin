//! Mappings to ABI-specific types.
use ir::component::ty;

use cranelift::prelude::*;

/// Create an ABI-specific type given an IR type.
pub fn from_type(ir_type: &ty::Type, ptr_type: Type) -> Type {
    match *ir_type {
        ty::Type::Number(ref n) => from_number_type(n),
        ty::Type::String => ptr_type,
        ty::Type::Tuple(_) => ptr_type,
        ty::Type::Record(_) => ptr_type,
        ty::Type::Function(_) => ptr_type,
        ty::Type::Conflict(_) => panic!("type conflict"),
        ty::Type::Any => panic!("can't map any type to concrete type"),
    }
}

fn from_number_type(ir_type: &ty::Number) -> Type {
    match *ir_type {
        ty::Number::U8 => types::I8,
        ty::Number::U16 => types::I16,
        ty::Number::U32 => types::I32,
        ty::Number::U64 => types::I64,
        ty::Number::I8 => types::I8,
        ty::Number::I16 => types::I16,
        ty::Number::I32 => types::I32,
        ty::Number::I64 => types::I64,
        ty::Number::F32 => types::F32,
        ty::Number::F64 => types::F64,
    }
}
