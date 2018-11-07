//! A JIT compiler implementation based on the IR.
use std::fmt;

use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;

/// A compiler instance, that can be used to compile IRs.
#[allow(unused)]
pub struct Compiler {
    ctx: codegen::Context,
    data_ctx: cranelift_module::DataContext,
    module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
}

impl Compiler {
    /// Creates a new compiler with an empty state.
    pub fn new() -> Compiler {
        let builder = cranelift_simplejit::SimpleJITBuilder::new();
        let module = cranelift_module::Module::new(builder);
        let ctx = module.make_context();
        let data_ctx = cranelift_module::DataContext::new();

        Compiler {
            ctx,
            data_ctx,
            module,
        }
    }
}

impl fmt::Debug for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Compiler").finish()
    }
}
