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
use crate::ir::component::location;
use crate::ir::component::symbol;
use crate::ir::component::ty;
use crate::module;

use cranelift::prelude::*;

mod abi_type;
mod builtin;
mod translation;
mod util;

/// A codegen system, that can be used for JIT compilation.
pub struct Codegen<'a> {
    entities: specs::Entities<'a>,
    constexprs: specs::ReadStorage<'a, constexpr::Constexpr>,
    elements: specs::ReadStorage<'a, element::Element>,
    layouts: specs::ReadStorage<'a, layout::Layout>,
    locations: specs::ReadStorage<'a, location::Location>,
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
    codemap: &'a codespan::CodeMap,
}

impl<'a> Codegen<'a> {
    /// Creates a new codegen instance around the specified IR.
    pub fn new(ir: &'a ir::Ir, codemap: &'a codespan::CodeMap) -> Self {
        let entities = ir.world.entities();
        let constexprs = ir.world.read_storage();
        let elements = ir.world.read_storage();
        let layouts = ir.world.read_storage();
        let locations = ir.world.read_storage();
        let symbols = ir.world.read_storage();
        let types = ir.world.read_storage();

        Codegen {
            entities,
            constexprs,
            elements,
            layouts,
            locations,
            symbols,
            types,
            codemap,
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        #[cfg(feature = "parallel")]
        use rayon::iter::ParallelIterator;
        use specs::Join;
        #[cfg(feature = "parallel")]
        use specs::ParJoin;

        let Codegen {
            ref entities,
            ref constexprs,
            ref elements,
            ref layouts,
            ref locations,
            ref symbols,
            ref types,
            ref codemap,
        } = *self;

        let mut builder = cranelift_simplejit::SimpleJITBuilder::new();

        builder.symbols(builtin::BUILTINS.iter().map(|b| (b.symbol, b.ptr)));

        let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
            cranelift_module::Module::new(builder);
        let ptr_type = module.target_config().pointer_type();

        #[cfg(feature = "parallel")]
        let data_tuples = (entities, layouts, constexprs).par_join();
        #[cfg(not(feature = "parallel"))]
        let data_tuples = (entities, layouts, constexprs).join();

        let data_ctxs = data_tuples
            .map(|(entity, layout, constexpr)| {
                let mut ctx = cranelift_module::DataContext::new();
                let mut data = Vec::new();
                translation::DataTranslator::new(&mut data, ptr_type)
                    .store_value(layout, &constexpr.value);
                ctx.define(data.into_boxed_slice());
                (entity, ctx)
            })
            .collect::<Vec<_>>();

        let mut defined_strings = collections::HashMap::new();

        let function_ctxs = (elements, symbols, types)
            .join()
            .filter_map(|(el, sy, ty)| as_closure(el, ty).map(|(el, ty)| (el, sy, ty)))
            .flat_map(|(closure, sy, ty)| {
                let mut ctx: codegen::Context = module.make_context();
                let mut builder_context = FunctionBuilderContext::new();

                for parameter in &ty.parameters {
                    ctx.func.signature.params.push(AbiParam::new(
                        abi_type::AbiType::from_ir_type(parameter).into_specific(ptr_type),
                    ));
                }
                let ret_type = abi_type::AbiType::from_ir_type(&ty.result).into_specific(ptr_type);
                // Result
                ctx.func.signature.returns.push(AbiParam::new(ret_type));
                // Error
                ctx.func.signature.returns.push(AbiParam::new(ptr_type));

                {
                    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

                    let name = sy.to_string();
                    let name_len = name.len();
                    let name_data_id = util::define_string(
                        &mut module,
                        &mut defined_strings,
                        &format!("funcname:{}", sy),
                        name,
                    );
                    let name_global_value = module.declare_data_in_func(name_data_id, builder.func);

                    let entry_ebb = builder.create_ebb();
                    builder.append_ebb_params_for_function_params(entry_ebb);

                    let error_throw_ebb = builder.create_ebb();
                    let error_unwind_ebb = builder.create_ebb();

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
                            locations,
                            symbols,
                            types,
                            ptr_type,
                            error_throw_ebb,
                            error_unwind_ebb,
                            &variables,
                            &mut defined_strings,
                            codemap,
                        );

                        for stmt in &closure.statements {
                            translation_ctx.exec_element(*stmt, self.elements.get(*stmt).unwrap());
                        }

                        translation_ctx.eval_element(
                            closure.result,
                            self.elements.get(closure.result).unwrap(),
                        )
                    };

                    let null_error = builder.ins().iconst(ptr_type, 0);
                    builder.ins().return_(&[result, null_error]);

                    let error_kind = builder.append_ebb_param(error_throw_ebb, types::I32);
                    let error_filename = builder.append_ebb_param(error_throw_ebb, ptr_type);
                    let error_filename_len = builder.append_ebb_param(error_throw_ebb, ptr_type);
                    let error_line = builder.append_ebb_param(error_throw_ebb, types::I32);
                    let error_col = builder.append_ebb_param(error_throw_ebb, types::I32);
                    builder.switch_to_block(error_throw_ebb);
                    builder.seal_block(error_throw_ebb);

                    let error = {
                        let mut translation_ctx = translation::FunctionTranslator::new(
                            &mut module,
                            &mut builder,
                            constexprs,
                            elements,
                            layouts,
                            locations,
                            symbols,
                            types,
                            ptr_type,
                            error_throw_ebb,
                            error_unwind_ebb,
                            &variables,
                            &mut defined_strings,
                            codemap,
                        );
                        translation_ctx.builtin_error(error_kind)
                    };
                    builder.ins().jump(
                        error_unwind_ebb,
                        &[
                            error,
                            error_filename,
                            error_filename_len,
                            error_line,
                            error_col,
                        ],
                    );

                    let error = builder.append_ebb_param(error_unwind_ebb, ptr_type);
                    let error_filename = builder.append_ebb_param(error_unwind_ebb, ptr_type);
                    let error_filename_len = builder.append_ebb_param(error_unwind_ebb, ptr_type);
                    let error_line = builder.append_ebb_param(error_unwind_ebb, types::I32);
                    let error_col = builder.append_ebb_param(error_unwind_ebb, types::I32);
                    builder.switch_to_block(error_unwind_ebb);
                    builder.seal_block(error_unwind_ebb);

                    let name = builder.ins().global_value(ptr_type, name_global_value);
                    let name_len = builder.ins().iconst(ptr_type, name_len as i64);

                    {
                        let mut translation_ctx = translation::FunctionTranslator::new(
                            &mut module,
                            &mut builder,
                            constexprs,
                            elements,
                            layouts,
                            locations,
                            symbols,
                            types,
                            ptr_type,
                            error_throw_ebb,
                            error_unwind_ebb,
                            &variables,
                            &mut defined_strings,
                            codemap,
                        );
                        translation_ctx.builtin_unwind_frame(
                            error,
                            name,
                            name_len,
                            error_filename,
                            error_filename_len,
                            error_line,
                            error_col,
                        );
                    };

                    let null_result = if ret_type.is_int() {
                        builder.ins().iconst(ret_type, 0)
                    } else if ret_type == types::F32 {
                        builder.ins().f32const(Ieee32::with_float(0.0))
                    } else if ret_type == types::F64 {
                        builder.ins().f64const(Ieee64::with_float(0.0))
                    } else {
                        unimplemented!()
                    };
                    builder.ins().return_(&[null_result, error]);

                    builder.finalize();

                    debug!("generated function: {}", builder.display(None));
                }

                let mut result = Vec::new();

                if sy.is_top_level() {
                    let mut public_ctx: codegen::Context = module.make_context();
                    let mut public_builder_context = FunctionBuilderContext::new();

                    for i in 0..ty.parameters.len() {
                        public_ctx
                            .func
                            .signature
                            .params
                            .push(ctx.func.signature.params[i]);
                    }
                    // Error pointer
                    public_ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(ptr_type));

                    // Result
                    public_ctx
                        .func
                        .signature
                        .returns
                        .push(AbiParam::new(ret_type));

                    {
                        let mut builder =
                            FunctionBuilder::new(&mut public_ctx.func, &mut public_builder_context);
                        let entry_ebb = builder.create_ebb();
                        builder.append_ebb_params_for_function_params(entry_ebb);

                        let error_ebb = builder.create_ebb();

                        builder.switch_to_block(entry_ebb);
                        builder.seal_block(entry_ebb);

                        let fn_name = sy.to_string();
                        let callee = module
                            .declare_function(
                                &fn_name,
                                cranelift_module::Linkage::Local,
                                &ctx.func.signature,
                            )
                            .unwrap();
                        let local_callee = module.declare_func_in_func(callee, &mut builder.func);

                        let (error_out_ptr, parameter_values) =
                            builder.ebb_params(entry_ebb).split_last().unwrap();
                        let error_out_ptr = *error_out_ptr;
                        let parameter_values = parameter_values.to_vec();

                        let call = builder.ins().call(local_callee, &parameter_values);

                        let results = builder.inst_results(call);
                        let result = results[0];
                        let error = results[1];

                        builder
                            .ins()
                            .brnz(error, error_ebb, &[error, error_out_ptr]);
                        builder.ins().return_(&[result]);

                        let error = builder.append_ebb_param(error_ebb, ptr_type);
                        let error_out_ptr = builder.append_ebb_param(error_ebb, ptr_type);
                        builder.switch_to_block(error_ebb);
                        builder.seal_block(error_ebb);

                        let null_result = if ret_type.is_int() {
                            builder.ins().iconst(ret_type, 0)
                        } else if ret_type == types::F32 {
                            builder.ins().f32const(Ieee32::with_float(0.0))
                        } else if ret_type == types::F64 {
                            builder.ins().f64const(Ieee64::with_float(0.0))
                        } else {
                            unimplemented!()
                        };

                        let mut mem_flags = MemFlags::new();
                        mem_flags.set_notrap();
                        mem_flags.set_aligned();
                        builder.ins().store(mem_flags, error, error_out_ptr, 0i32);
                        builder.ins().return_(&[null_result]);

                        builder.finalize();
                    }

                    result.push((sy.clone().into_public(), public_ctx));
                }
                result.push((sy.clone(), ctx));

                result
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
                    if sy.is_public() {
                        cranelift_module::Linkage::Export
                    } else {
                        cranelift_module::Linkage::Local
                    },
                    &ctx.func.signature,
                )
                .unwrap();
            declared_functions.push((fn_id, ctx));
            if sy.is_public() {
                function_ids.insert(fn_name["public:".len()..].to_owned(), fn_id);
            }
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
        let compiler = Codegen::new(&ir, &codemap);
        let module = compiler.compile();

        Ok(module)
    }
}
