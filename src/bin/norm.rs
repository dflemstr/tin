extern crate failure;
extern crate norm;

use std::io;

fn main() -> Result<(), failure::Error> {
    use norm::parser::Parse;
    use std::io::Read;

    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut source = String::new();
    stdin.read_to_string(&mut source)?;

    let ast = norm::ast::Module::parse(&source)?;

    let mut ir = norm::ir::Ir::new();
    ir.add_module(&ast, &[]);
    ir.resolve_references();
    ir.check_types();

    let codegen = norm::codegen::Codegen::new(&ir);
    let mut module = codegen.compile();

    let entrypoint = module
        .function::<norm::codegen::module::Function0<f64>>("main")
        .ok_or(failure::err_msg("missing a main function"))?;

    let result = entrypoint();

    eprintln!("{}", result);
    Ok(())
}
