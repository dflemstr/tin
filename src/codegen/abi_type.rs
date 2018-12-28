//! Mappings to ABI-specific types.
use ir::component::ty;

use cranelift::prelude::*;

#[derive(Clone, Copy, Debug)]
pub enum AbiType {
    Scalar(Type),
    Ptr,
}

#[derive(Clone, Debug)]
pub struct AbiSignature<'a> {
    pub params: &'a [AbiType],
    pub returns: &'a [AbiType],
}

impl AbiType {
    /// Create an ABI-specific type given an IR type.
    pub fn from_ir_type(ir_type: &ty::Type) -> AbiType {
        match *ir_type {
            ty::Type::Boolean => AbiType::Scalar(types::B1),
            ty::Type::Number(ref n) => AbiType::from_ir_number_type(n),
            ty::Type::String => AbiType::Ptr,
            ty::Type::Tuple(_) => AbiType::Ptr,
            ty::Type::Record(_) => AbiType::Ptr,
            ty::Type::Function(_) => AbiType::Ptr,
            ty::Type::Conflict(_) => panic!("type conflict"),
            ty::Type::Any => panic!("can't map any type to concrete type"),
        }
    }

    pub fn into_specific(self, ptr_type: Type) -> Type {
        match self {
            AbiType::Scalar(ty) => ty,
            AbiType::Ptr => ptr_type,
        }
    }

    fn from_ir_number_type(ir_type: &ty::Number) -> AbiType {
        match *ir_type {
            ty::Number::U8 => AbiType::Scalar(types::I8),
            ty::Number::U16 => AbiType::Scalar(types::I16),
            ty::Number::U32 => AbiType::Scalar(types::I32),
            ty::Number::U64 => AbiType::Scalar(types::I64),
            ty::Number::I8 => AbiType::Scalar(types::I8),
            ty::Number::I16 => AbiType::Scalar(types::I16),
            ty::Number::I32 => AbiType::Scalar(types::I32),
            ty::Number::I64 => AbiType::Scalar(types::I64),
            ty::Number::F32 => AbiType::Scalar(types::F32),
            ty::Number::F64 => AbiType::Scalar(types::F64),
        }
    }
}
