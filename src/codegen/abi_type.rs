//! Mappings to ABI-specific types.
use std::u16;
use std::u32;
use std::u64;
use std::u8;

use crate::ir::component::ty;

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
            ty::Type::Number(ref n) => AbiType::from_ir_number_type(n),
            ty::Type::String => AbiType::Ptr,
            ty::Type::Symbol(_) => AbiType::Scalar(types::I8),
            ty::Type::Tuple(_) => AbiType::Ptr,
            ty::Type::Union(ty::Union { ref alternatives }) => {
                let n = alternatives.len();

                // Assumes unions only store symbols for now
                if n <= 2 {
                    AbiType::Scalar(types::B1)
                } else if n <= u8::MAX as usize {
                    AbiType::Scalar(types::I8)
                } else if n <= u16::MAX as usize {
                    AbiType::Scalar(types::I16)
                } else if n <= u32::MAX as usize {
                    AbiType::Scalar(types::I32)
                } else if n <= u64::MAX as usize {
                    AbiType::Scalar(types::I64)
                } else {
                    unimplemented!()
                }
            }
            ty::Type::Record(_) => AbiType::Ptr,
            ty::Type::Function(_) => AbiType::Ptr,
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
