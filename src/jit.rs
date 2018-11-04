#![warn(warnings, unused)]

use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;

use ir;

pub struct Jit {
    ctx: codegen::Context,
    data_ctx: cranelift_module::DataContext,
    module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
}

impl Jit {
    pub fn new() -> Self {
        let builder = cranelift_simplejit::SimpleJITBuilder::new();
        let module = cranelift_module::Module::new(builder);
        let ctx = module.make_context();
        let data_ctx = cranelift_module::DataContext::new();

        Jit {
            ctx,
            data_ctx,
            module,
        }
    }

    pub fn compile(&self, _module: ir::Ir) {
        unimplemented!()
    }
}
