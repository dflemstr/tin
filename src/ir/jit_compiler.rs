use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;

struct Compiler {
    ctx: codegen::Context,
    data_ctx: cranelift_module::DataContext,
    module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
}

fn compile() {
    let builder = cranelift_simplejit::SimpleJITBuilder::new();
    let module = cranelift_module::Module::new(builder);
    let ctx = module.make_context();
    let data_ctx = cranelift_module::DataContext::new();

    let compiler = Compiler {
        ctx,
        data_ctx,
        module,
    };
}
