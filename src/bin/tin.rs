extern crate env_logger;
extern crate failure;
extern crate tin_lang;

use std::io;

fn main() -> Result<(), failure::Error> {
    use std::io::Read;
    use tin_lang::parser::Parse;

    env_logger::init();

    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut source = String::new();
    stdin.read_to_string(&mut source)?;

    let ast = tin_lang::ast::Module::parse(&source)?;

    let mut ir = tin_lang::ir::Ir::new();
    ir.module(&ast)?;
    ir.check_types();

    let codegen = tin_lang::codegen::Codegen::new(&ir);
    let mut module = codegen.compile();

    let entrypoint = module
        .function::<tin_lang::codegen::module::Function0<u32>>("main")
        .ok_or(failure::err_msg("missing a main function"))?;

    let result = entrypoint();

    eprintln!("{}", result);
    Ok(())
}
