#[macro_use]
extern crate pretty_assertions;

use std::fs;
use std::path;

// Set this to `true` to update the test fixtures
const RECORD: bool = false;

fn test_ok(path: &str) -> Result<(), failure::Error> {
    use std::io::Read;

    let path = path::Path::new(path);

    let mut source = String::new();
    fs::File::open(path)?.read_to_string(&mut source)?;

    let mut tin = tin_lang::Tin::new();
    let mut err = Vec::new();

    let _ = tin
        .load(path, &source)
        .map_err(|e| report_diagnostics(tin.codemap(), e, &mut err));

    assert_eq!("".to_owned(), String::from_utf8(err.clone()).unwrap());

    let module = tin
        .compile()
        .map_err(|e| report_diagnostics(tin.codemap(), e, &mut err));

    assert_eq!("".to_owned(), String::from_utf8(err.clone()).unwrap());

    let main = module
        .unwrap()
        .function::<tin_lang::module::Function0<i32>>("main")
        .unwrap();

    assert_eq!(0, main());

    Ok(())
}

fn test_err(path: &str, err: &str) -> Result<(), failure::Error> {
    use std::io::Read;
    use std::io::Write;

    let path = path::Path::new(path);
    let err = path::Path::new(err);

    let mut source = String::new();
    fs::File::open(path)?.read_to_string(&mut source)?;

    let mut tin = tin_lang::Tin::new();
    let mut err_actual = Vec::new();

    if tin
        .load(path, &source)
        .map_err(|e| report_diagnostics(tin.codemap(), e, &mut err_actual))
        .is_ok()
    {
        let _ = tin
            .compile()
            .map_err(|e| report_diagnostics(tin.codemap(), e, &mut err_actual));
    }

    let err_actual = String::from_utf8(err_actual).unwrap();

    if RECORD {
        fs::File::create(err)?.write_all(err_actual.as_bytes())?;
    } else {
        let mut err_expected = String::new();
        fs::File::open(err)?.read_to_string(&mut err_expected)?;
        assert_eq!(err_expected, err_actual);
    }

    Ok(())
}

fn report_diagnostics(codemap: &codespan::CodeMap, error: tin_lang::Error, mut out: &mut Vec<u8>) {
    use tin_lang::diagnostic::Diagnostic;

    let mut diagnostics = Vec::new();
    error.to_diagnostics(&mut diagnostics);

    for diagnostic in diagnostics {
        codespan_reporting::emit(
            codespan_reporting::termcolor::NoColor::new(&mut out),
            codemap,
            &diagnostic,
        )
        .unwrap();
    }
}

macro_rules! integration_test_ok {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() -> Result<(), failure::Error> {
            test_ok($path)
        }
    };
}

macro_rules! integration_test_err {
    ($name:ident, $path:expr, $err:expr) => {
        #[test]
        fn $name() -> Result<(), failure::Error> {
            test_err($path, $err)
        }
    };
}

include!(concat!(env!("OUT_DIR"), "/integration_test_ok.rs"));
include!(concat!(env!("OUT_DIR"), "/integration_test_err.rs"));