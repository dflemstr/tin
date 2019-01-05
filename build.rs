extern crate lalrpop;

use std::env;
use std::fs;
use std::path;

fn main() -> Result<(), failure::Error> {
    use std::io::Write;

    println!("cargo:rerun-if-changed=src/parser/tin.lalrpop");
    lalrpop::process_root().unwrap();

    let out_dir_str = env::var_os("OUT_DIR").unwrap();
    let out_dir_path = path::Path::new(&out_dir_str);

    let integration_test_ok_path = out_dir_path.join("integration_test_ok.rs");
    let mut integration_test_ok_file = fs::File::create(integration_test_ok_path)?;
    println!("cargo:rerun-if-changed=testdata/ok");
    for dir_entry in fs::read_dir("testdata/ok")? {
        let path = dir_entry?.path();
        println!("cargo:rerun-if-changed={}", path.to_string_lossy());
        let name = path.file_name().unwrap().to_string_lossy();

        if name.ends_with(".tn") {
            let name = &name[..name.len() - ".tn".len()];
            writeln!(
                integration_test_ok_file,
                "integration_test_ok!(ok_{}, {:?});",
                name, path
            )?;
        }
    }

    let integration_test_err_path = out_dir_path.join("integration_test_err.rs");
    let mut integration_test_err_file = fs::File::create(integration_test_err_path)?;
    println!("cargo:rerun-if-changed=testdata/err");
    for dir_entry in fs::read_dir("testdata/err")? {
        let path = dir_entry?.path();
        println!("cargo:rerun-if-changed={}", path.to_string_lossy());
        let name = path.file_name().unwrap().to_string_lossy();

        if name.ends_with(".tn") {
            let name = &name[..name.len() - ".tn".len()];
            let err_path = path.with_extension("err.txt");
            writeln!(
                integration_test_err_file,
                "integration_test_err!(err_{}, {:?}, {:?});",
                name, path, err_path
            )?;
        }
    }

    Ok(())
}
