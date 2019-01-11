use std::alloc;
use std::num;
use std::slice;
use std::str;

use cranelift::prelude::types;

use crate::codegen::abi_type;
use crate::codegen::abi_type::AbiType::*;
use crate::module;

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
    (ERROR, error, &[Scalar(types::I32)], &[Ptr]),
    (UNWIND_FRAME, unwind_frame, &[Ptr, Ptr, Ptr, Ptr, Ptr, Scalar(types::I32), Scalar(types::I32)], &[]),
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

unsafe extern "C" fn error(kind: u32) -> *mut module::Error {
    use num_traits::cast::FromPrimitive;

    let kind = module::ErrorKind::from_u32(kind).unwrap_or(module::ErrorKind::Unknown);

    debug!("error kind={:?}", kind);

    let error = module::Error::new(kind);

    Box::into_raw(Box::new(error))
}

unsafe extern "C" fn unwind_frame(
    error: *mut module::Error,
    name_data: *const u8,
    name_len: usize,
    path_data: *const u8,
    path_len: usize,
    line: u32,
    col: u32,
) {
    let error = error.as_mut().unwrap();

    let name = str::from_utf8_unchecked(slice::from_raw_parts(name_data, name_len));
    let path = str::from_utf8_unchecked(slice::from_raw_parts(path_data, path_len));
    let location = module::Point::new(path.to_owned(), line, col);

    debug!(
        "unwind_frame error={:?} name={:?} location={:?}",
        error, name, location
    );

    let frame = module::Frame::new(name.to_owned(), location);

    error.push_frame(frame);
}
