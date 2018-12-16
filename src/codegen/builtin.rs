#![allow(unsafe_code)]

use std::alloc;
use std::num;

use codegen::abi_type;
use codegen::abi_type::AbiType::*;

#[derive(Debug)]
pub struct Builtin {
    pub symbol: &'static str,
    pub ptr: *const u8,
    pub signature: abi_type::AbiSignature<'static>,
}

macro_rules! builtins {
    ($(($name:ident, $builtin:ident, $params:expr, $returns:expr),)*) => {
        $(
            pub const $name: Builtin = builtin!($builtin, $params, $returns);
        )*

        pub const BUILTINS: &[Builtin] = {
            &[
                $($name),*
            ]
        };
    }
}

macro_rules! builtin {
    ($builtin:ident, $params:expr, $returns:expr) => {
        Builtin {
            symbol: concat!("@builtin:", stringify!($builtin)),
            ptr: $builtin as *const u8,
            signature: abi_type::AbiSignature {
                params: $params,
                returns: $returns,
            },
        }
    };
}

builtins! {
    (ALLOC, alloc, &[Ptr, Ptr], &[Ptr]),
    (DEALLOC, dealloc, &[Ptr, Ptr, Ptr], &[]),
}

unsafe extern "C" fn alloc(size: usize, align: num::NonZeroUsize) -> *mut u8 {
    let layout = alloc::Layout::from_size_align_unchecked(size, align.get());
    let ptr = alloc::alloc(layout);
    debug!("alloc size={:?} align={:?} ptr={:?}", size, align, ptr);
    ptr
}

unsafe extern "C" fn dealloc(ptr: *mut u8, size: usize, align: num::NonZeroUsize) {
    let layout = alloc::Layout::from_size_align_unchecked(size, align.get());
    debug!("dealloc ptr={:?} size={:?} align={:?}", ptr, size, align);
    alloc::dealloc(ptr, layout);
}
