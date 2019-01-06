//! Definitions for compiled modules.
use std::collections;
use std::fmt;
use std::mem;
use std::ptr;

use cranelift_module;
use cranelift_simplejit;

/// A compiled module, the result of an invocation of `Tin::compile`.
pub struct Module {
    compiled: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    function_ids: collections::HashMap<String, cranelift_module::FuncId>,
}

/// An error that may happen at runtime.
#[derive(Clone, Copy, Debug, Fail, PartialEq)]
#[fail(display = "runtime error: {}", kind)]
pub struct Error {
    kind: ErrorKind,
}

/// The various kinds of allowed errors.
#[derive(Clone, Copy, Debug, PartialEq, Primitive)]
pub enum ErrorKind {
    /// An unknown error has occurred.
    ///
    /// If this happens, it is a bug.
    Unknown = 1,
    /// The stack has overflowed.
    StackOverflow = 2,
    /// There was an out-of-bounds array access.
    OutOfBounds = 3,
    /// An integer operation caused an overflow to happen.
    IntegerOverflow = 4,
    /// An integer operation caused a division-by-zero to happen.
    IntegerDivisonByZero = 5,
    /// A float-to-integer conversion failed.
    BadConversionToInteger = 6,
    /// User generated error.
    UserGenerated = 7,
}

/// A function that is exported from a [`Module`].
pub trait Function {
    /// Creates a new function from a raw pointer to generated machine code.
    ///
    /// # Unsafety
    ///
    /// This is probably as unsafe as it gets, since the passed-in pointer will be treated as a
    /// pointer to arbitrary machine code.  The pointer *must* come from a trusted source.
    unsafe fn from_ptr(ptr: *const u8) -> Self;
}

fn wrap_call<F, R>(call: F) -> Result<R, Error>
where
    F: FnOnce(*mut *mut Error) -> R,
{
    let mut error = ptr::null_mut();
    let result = call(&mut error);
    if !error.is_null() {
        Err(*unsafe { Box::from_raw(error) })
    } else {
        Ok(result)
    }
}

macro_rules! define_function {
    ($name:ident, $doc:expr, $ret:ident) => { define_function!($name, $doc, $ret,); };
    ($name:ident, $doc:expr, $ret:ident, $($argn:ident: $argt:ident),*) => {
        #[doc=$doc]
        pub struct $name<$ret, $($argt),*>(extern "C" fn($($argt,)* *mut *mut Error) -> $ret);

        impl<$ret, $($argt),*> $name<$ret, $($argt),*> {
            /// Call the underlying function in a safe manner.
            pub fn call(&self, $($argn: $argt),*) -> Result<$ret, Error> {
                wrap_call(|err| self.0($($argn,)* err))
            }
        }

        impl<$ret, $($argt),*> Function for $name<$ret, $($argt),*> {
            unsafe fn from_ptr(ptr: *const u8) -> Self {
                $name(mem::transmute(ptr))
            }
        }

        impl<$ret, $($argt),*> fmt::Debug for $name<$ret, $($argt),*> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_struct(stringify!($name)).finish()
            }
        }
    };
}

define_function!(Function0, "A function taking 0 arguments", R);
define_function!(Function1, "A function taking 1 arguments", R, a1: A1);
define_function!(
    Function2,
    "A function taking 2 arguments",
    R,
    a1: A1,
    a2: A2
);
define_function!(
    Function3,
    "A function taking 3 arguments",
    R,
    a1: A1,
    a2: A2,
    a3: A3
);
define_function!(
    Function4,
    "A function taking 4 arguments",
    R,
    a1: A1,
    a2: A2,
    a3: A3,
    a4: A4
);
define_function!(
    Function5,
    "A function taking 5 arguments",
    R,
    a1: A1,
    a2: A2,
    a3: A3,
    a4: A4,
    a5: A5
);
define_function!(
    Function6,
    "A function taking 6 arguments",
    R,
    a1: A1,
    a2: A2,
    a3: A3,
    a4: A4,
    a5: A5,
    a6: A6
);

impl Module {
    pub(crate) fn new(
        compiled: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
        function_ids: collections::HashMap<String, cranelift_module::FuncId>,
    ) -> Self {
        Module {
            compiled,
            function_ids,
        }
    }

    /// Fetches the specified function with the specified signature.
    ///
    /// Returns `None` if the signature does not match the compiled function.
    pub fn function<F>(&mut self, name: &str) -> Option<F>
    where
        F: Function,
    {
        if let Some(id) = self.function_ids.get(name) {
            // TODO: type check
            Some(unsafe { F::from_ptr(self.compiled.get_finalized_function(*id)) })
        } else {
            None
        }
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Module").finish()
    }
}

impl Error {
    /// Creates a new error of the specified kind.
    pub fn new(kind: ErrorKind) -> Self {
        Error { kind }
    }

    /// The kind of error.
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::Unknown => f.write_str("unknown"),
            ErrorKind::StackOverflow => f.write_str("stack overflow"),
            ErrorKind::OutOfBounds => f.write_str("out of bounds"),
            ErrorKind::IntegerOverflow => f.write_str("integer overflow"),
            ErrorKind::IntegerDivisonByZero => f.write_str("integer division by zero"),
            ErrorKind::BadConversionToInteger => f.write_str("bad conversion to integer"),
            ErrorKind::UserGenerated => f.write_str("user generated"),
        }
    }
}
