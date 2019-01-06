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
    find_test_files("testdata/ok", |path, name| {
        writeln!(
            integration_test_ok_file,
            "integration_test_ok!(ok_{}, {:?});",
            name, path
        )?;
        Ok(())
    })?;

    let integration_test_err_path = out_dir_path.join("integration_test_err.rs");
    let mut integration_test_err_file = fs::File::create(integration_test_err_path)?;
    find_test_files("testdata/err", |path, name| {
        let err_path = path.with_extension("err.txt");
        writeln!(
            integration_test_err_file,
            "integration_test_err!(err_{}, {:?}, {:?});",
            name, path, err_path
        )?;
        Ok(())
    })?;

    Ok(())
}

fn find_test_files<P, F>(base_path: P, mut callback: F) -> Result<(), failure::Error>
where
    P: AsRef<path::Path>,
    F: FnMut(&path::Path, &str) -> Result<(), failure::Error>,
{
    let name_replace_pattern = regex::Regex::new(r"[^a-zA-Z0-9_]+").unwrap();
    let base_path = base_path.as_ref();
    println!("cargo:rerun-if-changed={}", base_path.to_string_lossy());

    for dir_entry in walkdir::WalkDir::new(base_path) {
        let dir_entry = dir_entry?;
        let path = dir_entry.path();
        println!("cargo:rerun-if-changed={}", path.to_string_lossy());
        let name = path.strip_prefix(base_path)?.to_string_lossy();
        if name.ends_with(".tn") {
            let name = name_replace_pattern.replace_all(&name[..name.len() - ".tn".len()], "_");
            callback(path, &*name)?;
        }
    }
    Ok(())
}
