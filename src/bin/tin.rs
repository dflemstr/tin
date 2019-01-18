extern crate failure;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate structopt;
extern crate tin;

use std::fs;
use std::io;
use std::path;
use std::process;
use std::sync::atomic;

static REPORTED_DIAGNOSTICS: atomic::AtomicBool = atomic::AtomicBool::new(false);

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
            if !REPORTED_DIAGNOSTICS.load(atomic::Ordering::Relaxed) {
                for e in error.iter_chain() {
                    error!("{}", e);
                }
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
        let mut file = fs::File::open(&path)?;
        file_name = codespan::FileName::Real(path);
        file.read_to_string(&mut source)?;
    } else {
        let stdin = io::stdin();
        file_name = codespan::FileName::Virtual("stdin".into());
        let mut stdin = stdin.lock();
        stdin.read_to_string(&mut source)?;
    }

    let mut tin = tin::Tin::new();
    tin.load(file_name, &source)
        .map_err(|e| report_diagnostics(tin.codemap(), e))?;

    let mut module = tin
        .compile()
        .map_err(|e| report_diagnostics(tin.codemap(), e))?;

    let entrypoint = module
        .function::<tin::module::Function0<i32>>("main")
        .ok_or_else(|| failure::err_msg("missing a main function"))?;

    let result = entrypoint.call()?;

    Ok(result)
}

fn report_diagnostics(codemap: &codespan::CodeMap, error: tin::Error) -> tin::Error {
    use codespan_reporting::termcolor;
    use tin::diagnostic::Diagnostics;

    let mut builder = tin::diagnostic::DiagnosticsBuilder::new();
    error.to_diagnostics(&mut builder);
    let diagnostics = builder.build();

    if !diagnostics.is_empty() {
        let stream = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
        let mut output = stream.lock();

        for diagnostic in diagnostics {
            codespan_reporting::emit(&mut output, codemap, &diagnostic).unwrap();
        }

        REPORTED_DIAGNOSTICS.store(true, atomic::Ordering::Relaxed);
    }

    error
}
