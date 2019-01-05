//! A JIT compiler implementation based on the IR.
use std::collections;
use std::fmt;

use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;
use specs;

use crate::ir;
use crate::ir::component::constexpr;
use crate::ir::component::element;
use crate::ir::component::layout;
use crate::ir::component::symbol;
use crate::ir::component::ty;
use crate::module;

use cranelift::prelude::*;

mod abi_type;
mod builtin;
mod translation;

/// A codegen system, that can be used for JIT compilation.
pub struct Codegen<'a> {
    entities: specs::Entities<'a>,
    constexprs: specs::ReadStorage<'a, constexpr::Constexpr>,
    elements: specs::ReadStorage<'a, element::Element>,
    layouts: specs::ReadStorage<'a, layout::Layout>,
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
}

impl<'a> Codegen<'a> {
    /// Creates a new codegen instance around the specified IR.
    pub fn new(ir: &'a ir::Ir) -> Self {
        let entities = ir.world.entities();
        let constexprs = ir.world.read_storage();
        let elements = ir.world.read_storage();
        let layouts = ir.world.read_storage();
        let symbols = ir.world.read_storage();
        let types = ir.world.read_storage();

        Codegen {
            entities,
            constexprs,
            elements,
            layouts,
            symbols,
            types,
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        use rayon::iter::ParallelIterator;
        use specs::Join;
        use specs::ParJoin;

        let Codegen {
            ref entities,
            ref constexprs,
            ref elements,
            ref layouts,
            ref symbols,
            ref types,
        } = *self;

        let mut builder = cranelift_simplejit::SimpleJITBuilder::new();

        builder.symbols(builtin::BUILTINS.iter().map(|b| (b.symbol, b.ptr)));

        let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
            cranelift_module::Module::new(builder);
        let ptr_type = module.target_config().pointer_type();

        let data_ctxs = (entities, layouts, constexprs)
            .par_join()
            .map(|(entity, layout, constexpr)| {
                let mut ctx = cranelift_module::DataContext::new();
                let mut data = Vec::new();
                translation::DataTranslator::new(&mut data, ptr_type)
                    .store_value(layout, &constexpr.value);
                ctx.define(data.into_boxed_slice());
                (entity, ctx)
            })
            .collect::<Vec<_>>();

        let function_ctxs = (elements, symbols, types)
            .join()
            .filter_map(|(el, sy, ty)| as_closure(el, ty).map(|(el, ty)| (el, sy, ty)))
            .map(|(closure, sy, ty)| {
                let mut ctx: codegen::Context = module.make_context();
                let mut builder_context = FunctionBuilderContext::new();

                for parameter in &ty.parameters {
                    ctx.func.signature.params.push(AbiParam::new(
                        abi_type::AbiType::from_ir_type(parameter).into_specific(ptr_type),
                    ));
                }
                let ret_type = abi_type::AbiType::from_ir_type(&ty.result).into_specific(ptr_type);
                ctx.func.signature.returns.push(AbiParam::new(ret_type));

                {
                    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
                    let entry_ebb = builder.create_ebb();
                    builder.append_ebb_params_for_function_params(entry_ebb);
                    builder.switch_to_block(entry_ebb);
                    builder.seal_block(entry_ebb);

                    let variables = declare_variables(
                        elements,
                        types,
                        ptr_type,
                        &mut builder,
                        &closure.parameters,
                        &closure.statements,
                        entry_ebb,
                    );

                    let result = {
                        let mut translation_ctx = translation::FunctionTranslator::new(
                            &mut module,
                            &mut builder,
                            constexprs,
                            elements,
                            layouts,
                            symbols,
                            types,
                            ptr_type,
                            variables,
                        );

                        for stmt in &closure.statements {
                            translation_ctx.exec_element(*stmt, self.elements.get(*stmt).unwrap());
                        }

                        translation_ctx.eval_element(
                            closure.result,
                            self.elements.get(closure.result).unwrap(),
                        )
                    };

                    builder.ins().return_(&[result]);
                    builder.finalize();

                    debug!("generated function: {}", builder.display(None));
                }

                (sy, ctx)
            })
            .collect::<Vec<_>>();

        let mut declared_functions = Vec::new();
        let mut declared_data = Vec::new();
        let mut function_ids = collections::HashMap::new();

        for (entity, ctx) in data_ctxs {
            let data_name = entity.id().to_string();
            let data_id = module
                .declare_data(&data_name, cranelift_module::Linkage::Local, false)
                .unwrap();
            declared_data.push((data_id, ctx));
        }

        for (sy, ctx) in function_ctxs {
            let fn_name = sy.to_string();
            let fn_id = module
                .declare_function(
                    &fn_name,
                    cranelift_module::Linkage::Export,
                    &ctx.func.signature,
                )
                .unwrap();
            declared_functions.push((fn_id, ctx));
            function_ids.insert(fn_name, fn_id);
        }

        for (id, data_ctx) in declared_data {
            module.define_data(id, &data_ctx).unwrap();
        }

        for (id, mut function_ctx) in declared_functions {
            module.define_function(id, &mut function_ctx).unwrap();
            module.clear_context(&mut function_ctx);
        }

        module.finalize_definitions();

        module::Module::new(module, function_ids)
    }
}

impl<'a> fmt::Debug for Codegen<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Compiler").finish()
    }
}

fn declare_variables(
    elements: &specs::ReadStorage<element::Element>,
    types: &specs::ReadStorage<ty::Type>,
    ptr_type: Type,
    builder: &mut FunctionBuilder,
    params: &[specs::Entity],
    statements: &[specs::Entity],
    entry_ebb: Ebb,
) -> collections::HashMap<specs::Entity, Variable> {
    let mut next_var = 0;
    let mut variables = collections::HashMap::new();

    for (i, param) in params.iter().enumerate() {
        let param_initializer = builder.ebb_params(entry_ebb)[i];
        let var = declare_variable(
            types,
            ptr_type,
            builder,
            &mut variables,
            &mut next_var,
            *param,
        );
        builder.def_var(var, param_initializer);
    }

    for statement in statements {
        declare_variables_in_element(
            elements,
            types,
            ptr_type,
            builder,
            &mut variables,
            &mut next_var,
            *statement,
        );
    }

    variables
}

fn declare_variables_in_element(
    elements: &specs::ReadStorage<element::Element>,
    types: &specs::ReadStorage<ty::Type>,
    ptr_type: Type,
    builder: &mut FunctionBuilder,
    variables: &mut collections::HashMap<specs::Entity, Variable>,
    next_var: &mut usize,
    entity: specs::Entity,
) {
    match *elements.get(entity).unwrap() {
        element::Element::Variable(_) => {
            declare_variable(types, ptr_type, builder, variables, next_var, entity);
        }
        _ => {}
    }
}

fn declare_variable(
    types: &specs::ReadStorage<ty::Type>,
    ptr_type: Type,
    builder: &mut FunctionBuilder,
    variables: &mut collections::HashMap<specs::Entity, Variable>,
    next_var: &mut usize,
    entity: specs::Entity,
) -> Variable {
    let var = Variable::new(*next_var);
    if !variables.contains_key(&entity) {
        variables.insert(entity, var);
        builder.declare_var(
            var,
            abi_type::AbiType::from_ir_type(types.get(entity).unwrap()).into_specific(ptr_type),
        );
        *next_var += 1;
    }
    var
}

fn as_closure<'a, 'b>(
    element: &'a element::Element,
    ty: &'b ty::Type,
) -> Option<(&'a element::Closure, &'b ty::Function)> {
    match (element, ty) {
        (element::Element::Closure(c), ty::Type::Function(f)) => Some((c, f)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use env_logger;
    use failure;

    use super::*;
    use crate::ast;
    use crate::ir;
    use crate::test_util;

    #[test]
    fn immediate() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { 42u32 };
"#;

        let mut module = compile_module("immediate", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(42, result);
        Ok(())
    }

    #[test]
    fn variable() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { a = 43u32; a };
"#;

        let mut module = compile_module("variable", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter1() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32| u32 { a };
"#;

        let mut module = compile_module("parameter1", source)?;

        let main = module
            .function::<module::Function1<u32, u32>>("main")
            .unwrap();

        let result = main(43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter2() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32, b: u32| u32 { b };
"#;

        let mut module = compile_module("parameter2", source)?;

        let main = module
            .function::<module::Function2<u32, u32, u32>>("main")
            .unwrap();

        let result = main(1, 43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter3() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32, b: u32, c: u32| u32 { c };
"#;

        let mut module = compile_module("parameter3", source)?;

        let main = module
            .function::<module::Function3<u32, u32, u32, u32>>("main")
            .unwrap();

        let result = main(1, 2, 43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter4() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32, b: u32, c: u32, d: u32| u32 { d };
"#;

        let mut module = compile_module("parameter4", source)?;

        let main = module
            .function::<module::Function4<u32, u32, u32, u32, u32>>("main")
            .unwrap();

        let result = main(1, 2, 3, 43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter5() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32, b: u32, c: u32, d: u32, e: u32| u32 { e };
"#;

        let mut module = compile_module("parameter5", source)?;

        let main = module
            .function::<module::Function5<u32, u32, u32, u32, u32, u32>>("main")
            .unwrap();

        let result = main(1, 2, 3, 4, 43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn parameter6() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = |a: u32, b: u32, c: u32, d: u32, e: u32, f: u32| u32 { f };
"#;

        let mut module = compile_module("parameter6", source)?;

        let main = module
            .function::<module::Function6<u32, u32, u32, u32, u32, u32, u32>>("main")
            .unwrap();

        let result = main(1, 2, 3, 4, 5, 43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn apply() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
other = |x: u32| u32 { x };
main = |y: u32| u32 { a = other(y); other(other(a)) };
"#;

        let mut module = compile_module("apply", source)?;

        let main = module
            .function::<module::Function1<u32, u32>>("main")
            .unwrap();

        let result = main(43);
        assert_eq!(43, result);
        Ok(())
    }

    #[test]
    fn record() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { a = { x: 1u32, y: 2u32, z: 3u32}; a.y };
"#;

        let mut module = compile_module("record", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(2, result);
        Ok(())
    }

    #[test]
    fn operators_u32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { a = 1u32; b = 2u32; (a * 24u32 + b * 3u32) / 10u32 };
"#;

        let mut module = compile_module("operators_u32", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(3, result);
        Ok(())
    }

    #[test]
    fn operators_f32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || f32 { a = 1f32; b = 2f32; (a * 24f32 + b * 3f32) / 10f32 };
"#;

        let mut module = compile_module("operators_f32", source)?;

        let main = module.function::<module::Function0<f32>>("main").unwrap();

        let result = main();
        assert_eq!(3.0, result);
        Ok(())
    }

    #[test]
    fn operators_f64() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || f64 { a = 1f64; b = 2f64; (a * 24f64 + b * 3f64) / 10f64 };
"#;

        let mut module = compile_module("operators_f64", source)?;

        let main = module.function::<module::Function0<f64>>("main").unwrap();

        let result = main();
        assert_eq!(3.0, result);
        Ok(())
    }

    #[test]
    fn add_u32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { a = 1u32; b = 2u32; a + b };
"#;

        let mut module = compile_module("add_u32", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(3, result);
        Ok(())
    }

    #[test]
    fn add_i32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || i32 { a = 1i32; b = 2i32; a + b };
"#;

        let mut module = compile_module("add_i32", source)?;

        let main = module.function::<module::Function0<i32>>("main").unwrap();

        let result = main();
        assert_eq!(3, result);
        Ok(())
    }

    #[test]
    fn add_f32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || f32 { a = 1f32; b = 2f32; a + b };
"#;

        let mut module = compile_module("add_f32", source)?;

        let main = module.function::<module::Function0<f32>>("main").unwrap();

        let result = main();
        assert_eq!(3.0, result);
        Ok(())
    }

    #[test]
    fn sub_u32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || u32 { a = 2u32; b = 1u32; a - b };
"#;

        let mut module = compile_module("sub_u32", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(1, result);
        Ok(())
    }

    #[test]
    fn sub_i32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || i32 { a = 1i32; b = 2i32; a - b };
"#;

        let mut module = compile_module("sub_i32", source)?;

        let main = module.function::<module::Function0<i32>>("main").unwrap();

        let result = main();
        assert_eq!(-1, result);
        Ok(())
    }

    #[test]
    fn sub_f32() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
main = || f32 { a = 2f32; b = 1f32; a - b };
"#;

        let mut module = compile_module("sub_f32", source)?;

        let main = module.function::<module::Function0<f32>>("main").unwrap();

        let result = main();
        assert_eq!(1.0, result);
        Ok(())
    }

    fn compile_module(name: &'static str, source: &str) -> Result<module::Module, failure::Error> {
        use crate::parser::Parse;

        let mut codemap = codespan::CodeMap::new();
        let span = codemap
            .add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned())
            .span();
        let ast_module = ast::Module::parse(span, source)?;
        let mut ir = ir::Ir::new();
        ir.load(&ast_module)?;
        ir.check_types()?;
        test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &ir)?;
        let compiler = Codegen::new(&ir);
        let module = compiler.compile();

        Ok(module)
    }
}
