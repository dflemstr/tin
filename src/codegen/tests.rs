use env_logger;
use failure;

use super::*;
use crate::ir;
use crate::source;
use crate::syntax::ast;
use crate::test_util;

#[test]
fn immediate() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { 42u32 };
"#;

    let mut module = compile_module("immediate", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("immediate", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(42), result);
    Ok(())
}

#[test]
fn variable() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { a = 43u32; a };
"#;

    let mut module = compile_module("variable", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("variable", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter1() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32| -> u32 { a };
"#;

    let mut module = compile_module("parameter1", source)?;

    let main = module
        .function::<module::Function1<u32, u32>, _>("parameter1", "main")
        .unwrap();

    let result = main.call(43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter2() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32, b: u32| -> u32 { b };
"#;

    let mut module = compile_module("parameter2", source)?;

    let main = module
        .function::<module::Function2<u32, u32, u32>, _>("parameter2", "main")
        .unwrap();

    let result = main.call(1, 43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter3() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32, b: u32, c: u32| -> u32 { c };
"#;

    let mut module = compile_module("parameter3", source)?;

    let main = module
        .function::<module::Function3<u32, u32, u32, u32>, _>("parameter3", "main")
        .unwrap();

    let result = main.call(1, 2, 43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter4() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32, b: u32, c: u32, d: u32| -> u32 { d };
"#;

    let mut module = compile_module("parameter4", source)?;

    let main = module
        .function::<module::Function4<u32, u32, u32, u32, u32>, _>("parameter4", "main")
        .unwrap();

    let result = main.call(1, 2, 3, 43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter5() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32, b: u32, c: u32, d: u32, e: u32| -> u32 { e };
"#;

    let mut module = compile_module("parameter5", source)?;

    let main = module
        .function::<module::Function5<u32, u32, u32, u32, u32, u32>, _>("parameter5", "main")
        .unwrap();

    let result = main.call(1, 2, 3, 4, 43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn parameter6() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = |a: u32, b: u32, c: u32, d: u32, e: u32, f: u32| -> u32 { f };
"#;

    let mut module = compile_module("parameter6", source)?;

    let main = module
        .function::<module::Function6<u32, u32, u32, u32, u32, u32, u32>, _>("parameter6", "main")
        .unwrap();

    let result = main.call(1, 2, 3, 4, 5, 43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn apply() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
other = |x: u32| -> u32 { x };
main = |y: u32| -> u32 { a = other(y); other(other(a)) };
"#;

    let mut module = compile_module("apply", source)?;

    let main = module
        .function::<module::Function1<u32, u32>, _>("apply", "main")
        .unwrap();

    let result = main.call(43);
    assert_eq!(Ok(43), result);
    Ok(())
}

#[test]
fn record() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { a = { x: 1u32, y: 2u32, z: 3u32}; a.y };
"#;

    let mut module = compile_module("record", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("record", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(2), result);
    Ok(())
}

#[test]
fn operators_u32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { a = 1u32; b = 2u32; (a * 24u32 + b * 3u32) / 10u32 };
"#;

    let mut module = compile_module("operators_u32", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("operators_u32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3), result);
    Ok(())
}

#[test]
fn operators_f32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> f32 { a = 1f32; b = 2f32; (a * 24f32 + b * 3f32) / 10f32 };
"#;

    let mut module = compile_module("operators_f32", source)?;

    let main = module
        .function::<module::Function0<f32>, _>("operators_f32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3.0), result);
    Ok(())
}

#[test]
fn operators_f64() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> f64 { a = 1f64; b = 2f64; (a * 24f64 + b * 3f64) / 10f64 };
"#;

    let mut module = compile_module("operators_f64", source)?;

    let main = module
        .function::<module::Function0<f64>, _>("operators_f64", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3.0), result);
    Ok(())
}

#[test]
fn add_u32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { a = 1u32; b = 2u32; a + b };
"#;

    let mut module = compile_module("add_u32", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("add_u32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3), result);
    Ok(())
}

#[test]
fn add_i32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> i32 { a = 1i32; b = 2i32; a + b };
"#;

    let mut module = compile_module("add_i32", source)?;

    let main = module
        .function::<module::Function0<i32>, _>("add_i32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3), result);
    Ok(())
}

#[test]
fn add_f32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> f32 { a = 1f32; b = 2f32; a + b };
"#;

    let mut module = compile_module("add_f32", source)?;

    let main = module
        .function::<module::Function0<f32>, _>("add_f32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(3.0), result);
    Ok(())
}

#[test]
fn sub_u32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { a = 2u32; b = 1u32; a - b };
"#;

    let mut module = compile_module("sub_u32", source)?;

    let main = module
        .function::<module::Function0<u32>, _>("sub_u32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(1), result);
    Ok(())
}

#[test]
fn sub_i32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> i32 { a = 1i32; b = 2i32; a - b };
"#;

    let mut module = compile_module("sub_i32", source)?;

    let main = module
        .function::<module::Function0<i32>, _>("sub_i32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(-1), result);
    Ok(())
}

#[test]
fn sub_f32() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> f32 { a = 2f32; b = 1f32; a - b };
"#;

    let mut module = compile_module("sub_f32", source)?;

    let main = module
        .function::<module::Function0<f32>, _>("sub_f32", "main")
        .unwrap();

    let result = main.call();
    assert_eq!(Ok(1.0), result);
    Ok(())
}

fn compile_module(name: &'static str, source: &str) -> Result<module::Module, failure::Error> {
    use crate::codegen::Db as _;
    use crate::codegen::Db as _;
    use crate::layout::Db as _;
    use crate::source::Db as _;
    use crate::ty::Db as _;

    let mut codemap = codespan::CodeMap::new();
    codemap.add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned());

    let root_id = source::RootId(1);
    let file_id = source::FileId(1);
    let path = relative_path::RelativePath::new(name).to_owned();
    let mut files = collections::HashMap::new();
    files.insert(path.clone(), file_id);
    let root = source::Root { files };

    let mut db = db::Db::new();

    db.set_file_text(file_id, sync::Arc::new(source.to_owned()));
    db.set_file_relative_path(file_id, path);
    db.set_file_source_root(file_id, root_id);
    db.set_source_root(root_id, sync::Arc::new(root));
    db.set_all_source_roots(sync::Arc::new(vec![root_id]));
    db.set_ptr_size(layout::PtrSize::Size64);

    let module = (*db.codegen()?).clone();

    test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &db).unwrap();

    Ok(module)
}
