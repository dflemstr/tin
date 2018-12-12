#![allow(unsafe_code)]

use std::alloc;
use std::num;

pub const ALLOC_SYMBOL: &str = "@builtin:tin_alloc";
pub const DEALLOC_SYMBOL: &str = "@builtin:tin_dealloc";

pub const SYMBOLS: &[(&str, *const u8)] = &[
    (ALLOC_SYMBOL, tin_alloc as *const u8),
    (DEALLOC_SYMBOL, tin_dealloc as *const u8),
];

unsafe extern "C" fn tin_alloc(size: usize, align: num::NonZeroUsize) -> *mut u8 {
    let layout = alloc::Layout::from_size_align_unchecked(size, align.get());
    let ptr = alloc::alloc(layout);
    debug!("alloc size={:?} align={:?} ptr={:?}", size, align, ptr);
    ptr
}

unsafe extern "C" fn tin_dealloc(ptr: *mut u8, size: usize, align: num::NonZeroUsize) {
    let layout = alloc::Layout::from_size_align_unchecked(size, align.get());
    debug!("dealloc ptr={:?} size={:?} align={:?}", ptr, size, align);
    alloc::dealloc(ptr, layout);
}
