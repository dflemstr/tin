//! Mappings to ABI-specific types.
use std::u16;
use std::u32;
use std::u64;
use std::u8;

use crate::ty;

#[derive(Clone, Copy, Debug)]
pub enum AbiType {
    Scalar(cranelift::codegen::ir::Type),
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
            ty::Type::Number(n) => AbiType::from_ir_number_type(n),
            ty::Type::String | ty::Type::Tuple(_) | ty::Type::Record(_) | ty::Type::Function(_) => {
                AbiType::Ptr
            }
            ty::Type::Symbol(_) => AbiType::Scalar(cranelift::codegen::ir::types::I8),
            ty::Type::Union(ty::Union { ref alternatives }) => {
                let n = alternatives.len();

                // Assumes unions only store symbols for now
                if n <= 2 {
                    AbiType::Scalar(cranelift::codegen::ir::types::B1)
                } else if n as u64 <= u64::from(u8::max_value()) {
                    AbiType::Scalar(cranelift::codegen::ir::types::I8)
                } else if n as u64 <= u64::from(u16::max_value()) {
                    AbiType::Scalar(cranelift::codegen::ir::types::I16)
                } else if n as u64 <= u64::from(u32::max_value()) {
                    AbiType::Scalar(cranelift::codegen::ir::types::I32)
                } else if n as u64 <= u64::max_value() {
                    AbiType::Scalar(cranelift::codegen::ir::types::I64)
                } else {
                    unimplemented!()
                }
            }
            ty::Type::Placeholder => panic!("cannot lower placeholder type"),
        }
    }

    pub fn into_specific(
        self,
        ptr_type: cranelift::codegen::ir::Type,
    ) -> cranelift::codegen::ir::Type {
        match self {
            AbiType::Scalar(ty) => ty,
            AbiType::Ptr => ptr_type,
        }
    }

    fn from_ir_number_type(ir_type: ty::Number) -> AbiType {
        match ir_type {
            ty::Number::U8 | ty::Number::I8 => AbiType::Scalar(cranelift::codegen::ir::types::I8),
            ty::Number::U16 | ty::Number::I16 => {
                AbiType::Scalar(cranelift::codegen::ir::types::I16)
            }
            ty::Number::U32 | ty::Number::I32 => {
                AbiType::Scalar(cranelift::codegen::ir::types::I32)
            }
            ty::Number::U64 | ty::Number::I64 => {
                AbiType::Scalar(cranelift::codegen::ir::types::I64)
            }
            ty::Number::F32 => AbiType::Scalar(cranelift::codegen::ir::types::F32),
            ty::Number::F64 => AbiType::Scalar(cranelift::codegen::ir::types::F64),
        }
    }
}
