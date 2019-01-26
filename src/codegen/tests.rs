use env_logger;
use failure;

use super::*;
use crate::ir;
use crate::syntax::ast;
use crate::test_util;

#[test]
fn immediate() -> Result<(), failure::Error> {
    let _ = env_logger::try_init();

    let source = r#"
main = || -> u32 { 42u32 };
"#;

    let mut module = compile_module("immediate", source)?;

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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
        .function::<module::Function1<u32, u32>>("main")
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
        .function::<module::Function2<u32, u32, u32>>("main")
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
        .function::<module::Function3<u32, u32, u32, u32>>("main")
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
        .function::<module::Function4<u32, u32, u32, u32, u32>>("main")
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
        .function::<module::Function5<u32, u32, u32, u32, u32, u32>>("main")
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
        .function::<module::Function6<u32, u32, u32, u32, u32, u32, u32>>("main")
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
        .function::<module::Function1<u32, u32>>("main")
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

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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

    let main = module.function::<module::Function0<f32>>("main").unwrap();

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

    let main = module.function::<module::Function0<f64>>("main").unwrap();

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

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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

    let main = module.function::<module::Function0<i32>>("main").unwrap();

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

    let main = module.function::<module::Function0<f32>>("main").unwrap();

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

    let main = module.function::<module::Function0<u32>>("main").unwrap();

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

    let main = module.function::<module::Function0<i32>>("main").unwrap();

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

    let main = module.function::<module::Function0<f32>>("main").unwrap();

    let result = main.call();
    assert_eq!(Ok(1.0), result);
    Ok(())
}

fn compile_module(name: &'static str, source: &str) -> Result<module::Module, failure::Error> {
    use crate::syntax::parser::Parse;

    let mut codemap = codespan::CodeMap::new();
    let span = codemap
        .add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned())
        .span();
    let ast_module = ast::Module::parse(span, source)?;
    let mut ir = ir::Ir::new();
    ir.load(&ast_module)?;
    ir.check_types()?;
    test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &ir)?;
    let compiler = Codegen::new(&ir, &codemap);
    let module = compiler.compile();

    Ok(module)
}
