//! A JIT compiler implementation based on the IR.
use std::collections;
use std::fmt;

use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;
use specs;

use builtin;
use ir;
use ir::component::element;
use ir::component::layout;
use ir::component::symbol;
use ir::component::ty;

use cranelift::prelude::*;

pub mod abi_type;
pub mod module;
pub mod translation;

/// A codegen system, that can be used for JIT compilation.
#[allow(unused)]
pub struct Codegen<'a> {
    elements: specs::ReadStorage<'a, element::Element>,
    layouts: specs::ReadStorage<'a, layout::Layout>,
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
}

impl<'a> Codegen<'a> {
    /// Creates a new codegen instance around the specified IR.
    pub fn new(ir: &'a ir::Ir) -> Self {
        let elements = ir.world.read_storage();
        let layouts = ir.world.read_storage();
        let symbols = ir.world.read_storage();
        let types = ir.world.read_storage();

        Codegen {
            elements,
            layouts,
            symbols,
            types,
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        use specs::Join;

        let Codegen {
            ref elements,
            ref layouts,
            ref symbols,
            ref types,
        } = *self;

        let mut builder = cranelift_simplejit::SimpleJITBuilder::new();

        builder.symbols(builtin::SYMBOLS.to_vec());

        let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
            cranelift_module::Module::new(builder);
        let ptr_type = module.target_config().pointer_type();
        // let data_ctx = cranelift_module::DataContext::new();

        let function_ctxs = (elements, symbols, types)
            .join()
            .filter_map(|(el, sy, ty)| as_closure(el, ty).map(|(el, ty)| (el, sy, ty)))
            .map(|(closure, sy, ty)| {
                let mut ctx: codegen::Context = module.make_context();
                let mut builder_context = FunctionBuilderContext::new();

                for parameter in &ty.parameters {
                    ctx.func
                        .signature
                        .params
                        .push(AbiParam::new(abi_type::from_type(parameter, ptr_type)));
                }
                let ret_type = abi_type::from_type(&ty.result, ptr_type);
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
        let mut function_ids = collections::HashMap::new();

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
        let val = builder.ebb_params(entry_ebb)[i];
        let var = declare_variable(
            types,
            ptr_type,
            builder,
            &mut variables,
            &mut next_var,
            *param,
        );
        builder.def_var(var, val);
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
            abi_type::from_type(types.get(entity).unwrap(), ptr_type),
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
    use ast;
    use ir;
    use test_util;

    #[test]
    fn immediate() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
Int = 0u32;
main = || Int { 42u32 };
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
Int = 0u32;
main = || Int { a = 43u32; a };
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
Int = 0u32;
main = |a: Int| Int { a };
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
Int = 0u32;
main = |a: Int, b: Int| Int { b };
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
Int = 0u32;
main = |a: Int, b: Int, c: Int| Int { c };
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
Int = 0u32;
main = |a: Int, b: Int, c: Int, d: Int| Int { d };
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
Int = 0u32;
main = |a: Int, b: Int, c: Int, d: Int, e: Int| Int { e };
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
Int = 0u32;
main = |a: Int, b: Int, c: Int, d: Int, e: Int, f: Int| Int { f };
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
Int = 0u32;
other = |x: Int| Int { x };
main = |y: Int| Int { a = other(y); other(other(a)) };
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
Int = 0u32;
main = || Int { a = { x: 1u32, y: 2u32, z: 3u32}; a.y };
"#;

        let mut module = compile_module("record", source)?;

        let main = module.function::<module::Function0<u32>>("main").unwrap();

        let result = main();
        assert_eq!(2, result);
        Ok(())
    }

    fn compile_module(name: &str, source: &str) -> Result<module::Module, failure::Error> {
        use parser::Parse;

        let ast_module = ast::Module::parse(source)?;
        let mut ir = ir::Ir::new();
        ir.module(&ast_module)?;
        ir.check_types();
        test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &ir)?;
        let compiler = Codegen::new(&ir);
        let module = compiler.compile();

        Ok(module)
    }
}
