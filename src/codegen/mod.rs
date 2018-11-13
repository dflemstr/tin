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

pub mod module;

/// A compiler system, that can be used for JIT compilation.
#[allow(unused)]
pub struct Compiler<'a> {
    elements: specs::ReadStorage<'a, element::Element>,
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
}

impl<'a> Compiler<'a> {
    /// Creates a new compiler instance around the specified IR.
    pub fn new(ir: &'a ir::Ir) -> Self {
        let elements = ir.world.read_storage();
        let symbols = ir.world.read_storage();
        let types = ir.world.read_storage();

        Compiler {
            elements,
            symbols,
            types,
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        use specs::Join;

        let Compiler {
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
            .map(|(_el, sy, ty)| {
                let ptr_type = module.pointer_type();
                let mut ctx: codegen::Context = module.make_context();
                let mut builder_context = FunctionBuilderContext::new();

                for parameter in &ty.parameters {
                    ctx.func
                        .signature
                        .params
                        .push(AbiParam::new(to_type(parameter, ptr_type)));
                }
                let ret_type = types::I64; //to_type(&ty.result, ptr_type);
                ctx.func.signature.returns.push(AbiParam::new(ret_type));

                {
                    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
                    let entry_ebb = builder.create_ebb();
                    builder.append_ebb_params_for_function_params(entry_ebb);
                    builder.switch_to_block(entry_ebb);
                    builder.seal_block(entry_ebb);
                    let arg = builder.ins().iconst(ret_type, 42);
                    builder.ins().return_(&[arg]);
                    builder.finalize();
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

impl<'a> fmt::Debug for Compiler<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Compiler").finish()
    }
}

fn to_type(ir_type: &ty::Type, ptr_type: Type) -> Type {
    match *ir_type {
        ty::Type::Number(ref n) => to_number_type(n),
        ty::Type::String => ptr_type,
        ty::Type::Tuple(_) => ptr_type,
        ty::Type::Record(_) => ptr_type,
        ty::Type::Function(_) => ptr_type,
        ty::Type::Conflict(_) => ptr_type,
        ty::Type::Any => panic!("can't map any type to concrete type"),
    }
}

fn to_number_type(ir_type: &ty::Number) -> Type {
    match *ir_type {
        ty::Number::U8 => types::I8,
        ty::Number::U16 => types::I16,
        ty::Number::U32 => types::I32,
        ty::Number::U64 => types::I64,
        ty::Number::I8 => types::I8,
        ty::Number::I16 => types::I16,
        ty::Number::I32 => types::I32,
        ty::Number::I64 => types::I64,
        ty::Number::F32 => types::F32,
        ty::Number::F64 => types::F64,
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
    use dot;
    use env_logger;
    use failure;

    use super::*;
    use ast;
    use ir;

    #[test]
    fn compile_simple() -> Result<(), failure::Error> {
        use parser::Parse;

        let _ = env_logger::try_init();

        let source = r#"
Int = 0;
pickFirst = |a: Int, b: Int| Int {
  a
};
main = || Int { pickFirst(42, 65313) };
"#;

        let ast_module = ast::Module::parse(source)?;

        let mut ir = ir::Ir::new();
        ir.add_module(&ast_module, &[]);
        ir.resolve_references();
        ir.check_types();

        let graph = ir::graph::Graph::new(&ir);

        let mut file = ::std::fs::File::create("/tmp/compiler.dot")?;
        dot::render(&graph, &mut file)?;
        drop(file);
        ::std::process::Command::new("dot")
            .args(&["-Tpng", "-o/tmp/compiler.png", "/tmp/compiler.dot"])
            .spawn()?
            .wait()?;

        let compiler = Compiler::new(&ir);
        let mut module = compiler.compile();
        let main = module.function("main").unwrap();
        let result = main();
        assert_eq!(42, result);
        Ok(())
    }
}
