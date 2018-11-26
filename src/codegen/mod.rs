//! A JIT compiler implementation based on the IR.
use std::collections;
use std::fmt;

use cranelift::codegen;
use cranelift_module;
use cranelift_simplejit;
use specs;

use ir;
use ir::component::element;
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
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
}

impl<'a> Codegen<'a> {
    /// Creates a new codegen instance around the specified IR.
    pub fn new(ir: &'a ir::Ir) -> Self {
        let elements = ir.world.read_storage();
        let symbols = ir.world.read_storage();
        let types = ir.world.read_storage();

        Codegen {
            elements,
            symbols,
            types,
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        use specs::Join;

        let Codegen {
            ref elements,
            ref symbols,
            ref types,
        } = *self;

        let builder = cranelift_simplejit::SimpleJITBuilder::new();
        let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
            cranelift_module::Module::new(builder);
        // let data_ctx = cranelift_module::DataContext::new();

        let function_ctxs = (elements, symbols, types)
            .join()
            .filter_map(|(el, sy, ty)| as_closure(el, ty).map(|(el, ty)| (el, sy, ty)))
            .map(|(closure, sy, ty)| {
                let ptr_type = module.pointer_type();
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

                    let result = {
                        let mut translation_ctx =
                            translation::Context::new(self, &mut builder, &self.elements, ptr_type);
                        for stmt in &closure.statements {
                            translation_ctx.declare_element(
                                *stmt,
                                self.elements.get(*stmt).unwrap(),
                                self.types.get(*stmt).unwrap(),
                            );
                        }
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
            }).collect::<Vec<_>>();

        let mut declared_functions = Vec::new();
        let mut function_ids = collections::HashMap::new();

        for (sy, ctx) in function_ctxs {
            let fn_name = format!("{}", sy);
            let fn_id = module
                .declare_function(
                    &fn_name,
                    cranelift_module::Linkage::Export,
                    &ctx.func.signature,
                ).unwrap();
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
Int = 0;
main = || Int { 42 };
"#;

        let mut module = compile_module("immediate", source)?;

        let main = module.function::<f64>("main").unwrap();

        let result = main();
        assert_eq!(42.0, result);
        Ok(())
    }

    #[test]
    fn variable() -> Result<(), failure::Error> {
        let _ = env_logger::try_init();

        let source = r#"
Int = 0;
main = || Int { a = 43; a };
"#;

        let mut module = compile_module("variable", source)?;

        let main = module.function::<f64>("main").unwrap();

        let result = main();
        assert_eq!(43.0, result);
        Ok(())
    }

    fn compile_module(name: &str, source: &str) -> Result<module::Module, failure::Error> {
        use parser::Parse;

        let ast_module = ast::Module::parse(source)?;
        let mut ir = ir::Ir::new();
        ir.add_module(&ast_module, &[]);
        ir.resolve_references();
        ir.check_types();
        test_util::render_graph(&format!(concat!(module_path!(), "::{}"), name), &ir)?;
        let compiler = Codegen::new(&ir);
        let module = compiler.compile();

        Ok(module)
    }
}
