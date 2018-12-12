extern crate env_logger;
extern crate failure;
extern crate tin;

use std::io;

fn main() -> Result<(), failure::Error> {
    use tin::parser::Parse;
    use std::io::Read;

    env_logger::init();

    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut source = String::new();
    stdin.read_to_string(&mut source)?;

    let ast = tin::ast::Module::parse(&source)?;

    let mut ir = tin::ir::Ir::new();
    ir.module(&ast)?;
    ir.check_types();

    let codegen = tin::codegen::Codegen::new(&ir);
    let mut module = codegen.compile();

    let entrypoint = module
        .function::<tin::codegen::module::Function0<f64>>("main")
        .ok_or(failure::err_msg("missing a main function"))?;

    let result = entrypoint();

    eprintln!("{}", result);
    Ok(())
}
