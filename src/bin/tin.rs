extern crate failure;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate structopt;
extern crate tin_lang;

use std::fs;
use std::io;
use std::path;
use std::process;

#[derive(Debug, StructOpt)]
#[structopt(name = "tin")]
struct Options {
    /// Source file to compile (that contains a main function); will use stdin if omitted.
    #[structopt(name = "SOURCE", parse(from_os_str))]
    source: Option<path::PathBuf>,
}

fn main() {
    match run() {
        Ok(result) => process::exit(result),
        Err(error) => {
            for e in error.iter_chain() {
                error!("{:?}", e);
            }
            drop(error);
            process::exit(1)
        }
    }
}

fn run() -> Result<i32, failure::Error> {
    use std::io::Read;
    use structopt::StructOpt;

    pretty_env_logger::init_timed();

    let options = Options::from_args();

    let mut source = String::new();
    let file_name;
    if let Some(path) = options.source {
        file_name = path.to_string_lossy().into_owned();
        let mut file = fs::File::open(path)?;
        file.read_to_string(&mut source)?;
    } else {
        let stdin = io::stdin();
        file_name = "<stdin>".to_owned();
        let mut stdin = stdin.lock();
        stdin.read_to_string(&mut source)?;
    }

    let mut tin = tin_lang::Tin::new();
    tin.load(&file_name, &source)?;

    let mut module = tin.compile()?;

    let entrypoint = module
        .function::<tin_lang::module::Function0<i32>>("main")
        .ok_or(failure::err_msg("missing a main function"))?;

    let result = entrypoint();

    Ok(result)
}
