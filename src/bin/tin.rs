extern crate failure;
extern crate pretty_env_logger;
#[macro_use]
extern crate structopt;
extern crate tin_lang;

use std::fs;
use std::io;
use std::path;

#[derive(Debug, StructOpt)]
#[structopt(name = "tin")]
struct Options {
    /// Source file to compile (that contains a main function); will use stdin if omitted.
    #[structopt(name = "SOURCE", parse(from_os_str))]
    source: Option<path::PathBuf>,
}

fn main() -> Result<(), failure::Error> {
    use std::io::Read;
    use structopt::StructOpt;
    use tin_lang::parser::Parse;

    pretty_env_logger::init_timed();

    let options = Options::from_args();

    let mut source = String::new();
    if let Some(path) = options.source {
        let mut file = fs::File::open(path)?;
        file.read_to_string(&mut source)?;
    } else {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        stdin.read_to_string(&mut source)?;
    }

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
