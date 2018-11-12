//! Definitions for compiled modules.
use std::collections;
use std::fmt;
use std::mem;

use cranelift_module;
use cranelift_simplejit;

/// A compiled module, the result of an invocation of `Compiler::compile`.
pub struct Module {
    compiled: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    function_ids: collections::HashMap<String, cranelift_module::FuncId>,
}

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
    pub fn function(&mut self, name: &str) -> Option<fn() -> i64> {
        if let Some(id) = self.function_ids.get(name) {
            Some(unsafe { mem::transmute(self.compiled.get_finalized_function(*id)) })
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
