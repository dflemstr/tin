//! Definitions for compiled modules.
use std::collections;
use std::fmt;
use std::mem;
use std::ops;

use cranelift_module;
use cranelift_simplejit;

/// A compiled module, the result of an invocation of `Compiler::compile`.
pub struct Module {
    compiled: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    function_ids: collections::HashMap<String, cranelift_module::FuncId>,
}

/// A function that is exported from a [`Module`].
pub trait Function {
    /// Creates a new function from a raw pointer to generated machine code.
    ///
    /// # Unsafety
    ///
    /// This is probably as unsafe as it gets, since the passed-in pointer will be used to execute
    /// arbitrary machine code.  The pointer *must* come from a trusted source.
    #[allow(unsafe_code)]
    unsafe fn from_ptr(ptr: *const u8) -> Self;
}

macro_rules! define_function {
    ($name:ident, $doc:expr, $ret:ident) => { define_function!($name, $doc, $ret,); };
    ($name:ident, $doc:expr, $ret:ident, $($arg:ident),*) => {
        #[doc=$doc]
        pub struct $name<$ret, $($arg),*>(extern "sysv64" fn($($arg),*) -> $ret);

        impl<$ret, $($arg),*> ops::Deref for $name<$ret, $($arg),*> {
            type Target = extern "sysv64" fn($($arg),*) -> $ret;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<$ret, $($arg),*> Function for $name<$ret, $($arg),*> {
            #[allow(unsafe_code)]
            unsafe fn from_ptr(ptr: *const u8) -> Self {
                $name(mem::transmute(ptr))
            }
        }

        impl<$ret, $($arg),*> fmt::Debug for $name<$ret, $($arg),*> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_struct(stringify!($name)).finish()
            }
        }
    };
}

define_function!(Function0, "A function taking 0 arguments", R);
define_function!(Function1, "A function taking 1 arguments", R, A1);
define_function!(Function2, "A function taking 2 arguments", R, A1, A2);
define_function!(Function3, "A function taking 3 arguments", R, A1, A2, A3);
define_function!(
    Function4,
    "A function taking 4 arguments",
    R,
    A1,
    A2,
    A3,
    A4
);
define_function!(
    Function5,
    "A function taking 5 arguments",
    R,
    A1,
    A2,
    A3,
    A4,
    A5
);
define_function!(
    Function6,
    "A function taking 6 arguments",
    R,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6
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
    #[allow(unsafe_code)]
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
