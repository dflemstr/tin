//! A JIT compiler implementation based on the IR.
use log::debug;
use std::collections;
use std::fmt;
use std::ops;
use std::sync;

use cranelift::codegen;
use cranelift::codegen::ir::immediates;
use cranelift::codegen::ir::types;
use cranelift::frontend;
use cranelift_module;
use cranelift_simplejit;

use crate::db;
use crate::interpreter;
use crate::ir;
use crate::ir::{element, Entity};
use crate::layout;
use crate::module;
use crate::ty;

mod abi_type;
mod builtin;
mod data;
pub mod error;
mod function;
// #[cfg(test)]
// mod tests;
mod util;

/// A codegen system, that can be used for JIT compilation.
#[salsa::query_group(CodegenStorage)]
pub trait Db: salsa::Database + interpreter::Db + ir::Db + layout::Db + ty::Db {
    #[salsa::dependencies]
    fn codegen_function(&self, entity: ir::Entity) -> error::Result<Option<sync::Arc<Function>>>;

    #[salsa::dependencies]
    fn codegen_data(&self, entity: ir::Entity) -> error::Result<Option<sync::Arc<Data>>>;

    fn codegen_ptr_type(&self) -> codegen::ir::Type;

    #[salsa::dependencies]
    fn codegen_isa(&self) -> Isa;
}

pub struct Function {
    ctx: codegen::Context,
}

pub struct Data {
    ctx: cranelift_module::DataContext,
}

#[derive(Clone)]
pub struct Isa {
    raw: sync::Arc<codegen::isa::TargetIsa>,
}

struct Symbol<'a, D>(&'a D, ir::Entity);

fn codegen_function(
    db: &impl Db,
    entity: ir::Entity,
) -> error::Result<Option<sync::Arc<Function>>> {
    if let (element::Element::Closure(closure), ty::Type::Function(ty)) =
        (&*db.element(entity)?, &*db.ty(entity)?)
    {
        let ptr_type = db.codegen_ptr_type();

        let mut ctx = codegen::Context::new();
        ctx.func.signature.call_conv = db.codegen_isa().default_call_conv();
        populate_function_signature(ty, ptr_type, &mut ctx);

        let mut builder_context = frontend::FunctionBuilderContext::new();
        let mut builder =
            cranelift::frontend::FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let name = Symbol(db, entity).to_string();
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
            let mut translation_ctx = function::Translator::new(
                &mut module,
                &mut builder,
                db,
                ptr_type,
                error_throw_ebb,
                error_unwind_ebb,
                &variables,
                &mut defined_strings,
                codemap,
            );

            for stmt in &closure.statements {
                translation_ctx.exec_element(*stmt, &*db.element(*stmt)?);
            }

            translation_ctx.eval_element(closure.result, db.element(&*closure.result)?)
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
            let mut translation_ctx = function::Translator::new(
                &mut module,
                &mut builder,
                db,
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
            let mut translation_ctx = function::Translator::new(
                &mut module,
                &mut builder,
                db,
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
            builder.ins().f32const(immediates::Ieee32::with_float(0.0))
        } else if ret_type == types::F64 {
            builder.ins().f64const(immediates::Ieee64::with_float(0.0))
        } else {
            unimplemented!()
        };
        builder.ins().return_(&[null_result, error]);

        builder.finalize();

        debug!("generated function: {}", builder.display(None));
        Ok(Some(sync::Arc::new(Function { ctx })))
    } else {
        Ok(None)
    }
}

fn populate_function_signature(
    ty: &ty::Function,
    ptr_type: codegen::ir::Type,
    ctx: &mut codegen::Context,
) {
    for parameter in &ty.parameters {
        ctx.func.signature.params.push(codegen::ir::AbiParam::new(
            abi_type::AbiType::from_ir_type(parameter).into_specific(ptr_type),
        ));
    }

    let ret_type = abi_type::AbiType::from_ir_type(&ty.result).into_specific(ptr_type);

    // Result, error
    ctx.func.signature.returns.extend(&[
        codegen::ir::AbiParam::new(ret_type),
        codegen::ir::AbiParam::new(ptr_type),
    ]);
}

fn codegen_data(db: &impl Db, entity: ir::Entity) -> error::Result<Option<sync::Arc<Data>>> {
    if let Some(value) = db.value(entity)? {
        let layout = db.layout(entity)?;
        let ptr_type = db.codegen_ptr_type();
        let mut ctx = cranelift_module::DataContext::new();
        let mut data = Vec::new();
        data::Translator::new(&mut data, ptr_type).store_value(&*layout, &value);
        ctx.define(data.into_boxed_slice());
        Ok(Some(sync::Arc::new(Data { ctx })))
    } else {
        Ok(None)
    }
}

fn codegen_ptr_type(db: &impl Db) -> codegen::ir::Type {
    match db.ptr_size() {
        layout::PtrSize::Size8 => codegen::ir::types::I8,
        layout::PtrSize::Size16 => codegen::ir::types::I16,
        layout::PtrSize::Size32 => codegen::ir::types::I32,
        layout::PtrSize::Size64 => codegen::ir::types::I64,
    }
}

fn codegen_isa(db: &impl Db) -> Isa {
    let flag_builder = codegen::settings::builder();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    let raw = isa_builder
        .finish(codegen::settings::Flags::new(flag_builder))
        .into();
    Isa { raw }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Function").finish()
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Data").finish()
    }
}

impl fmt::Debug for Isa {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Isa").finish()
    }
}

impl ops::Deref for Isa {
    type Target = codegen::isa::TargetIsa;

    fn deref(&self) -> &Self::Target {
        &*self.raw
    }
}

/*
impl<'a> Codegen<'a> {
    /// Creates a new codegen instance around the specified IR.
    pub fn new(db: &'a db::Db) -> Self {
        Self {
            db
        }
    }

    /// Compiles the captured IR into a module.
    pub fn compile(&self) -> module::Module {
        let entities = db.entities();

        let mut builder = cranelift_simplejit::SimpleJITBuilder::new();

        builder.symbols(builtin::BUILTINS.iter().map(|b| (b.symbol, b.ptr)));

        let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
            cranelift_module::Module::new(builder);
        let ptr_type = module.target_config().pointer_type();

        let data_ctxs: Vec<_> = entities.all()
            .map(|(entity, layout, constexpr)| {
                let layout = self.db.layout(entity);
                let value = self.db.value(entity);

                let mut ctx = cranelift_module::DataContext::new();
                let mut data = Vec::new();
                data::Translator::new(&mut data, ptr_type).store_value(layout, &constexpr.value);
                ctx.define(data.into_boxed_slice());
                (entity, ctx)
            })
            .best_collect();

        let mut defined_strings = collections::HashMap::new();

        let function_ctxs = entities.all()
            .flat_map(|(el, sy, ty)| {
                if let Some((closure, ty)) = as_closure(el, ty) {
                    let mut ctx: codegen::Context = module.make_context();
                    let mut builder_context = FunctionBuilderContext::new();

                    for parameter in &ty.parameters {
                        ctx.func.signature.params.push(AbiParam::new(
                            abi_type::AbiType::from_ir_type(parameter).into_specific(ptr_type),
                        ));
                    }
                    let ret_type =
                        abi_type::AbiType::from_ir_type(&ty.result).into_specific(ptr_type);
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
                        let name_global_value =
                            module.declare_data_in_func(name_data_id, builder.func);

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
                            let mut translation_ctx = function::Translator::new(
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
                                translation_ctx
                                    .exec_element(*stmt, self.elements.get(*stmt).unwrap());
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
                        let error_filename_len =
                            builder.append_ebb_param(error_throw_ebb, ptr_type);
                        let error_line = builder.append_ebb_param(error_throw_ebb, types::I32);
                        let error_col = builder.append_ebb_param(error_throw_ebb, types::I32);
                        builder.switch_to_block(error_throw_ebb);
                        builder.seal_block(error_throw_ebb);

                        let error = {
                            let mut translation_ctx = function::Translator::new(
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
                        let error_filename_len =
                            builder.append_ebb_param(error_unwind_ebb, ptr_type);
                        let error_line = builder.append_ebb_param(error_unwind_ebb, types::I32);
                        let error_col = builder.append_ebb_param(error_unwind_ebb, types::I32);
                        builder.switch_to_block(error_unwind_ebb);
                        builder.seal_block(error_unwind_ebb);

                        let name = builder.ins().global_value(ptr_type, name_global_value);
                        let name_len = builder.ins().iconst(ptr_type, name_len as i64);

                        {
                            let mut translation_ctx = function::Translator::new(
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
                            let mut builder = FunctionBuilder::new(
                                &mut public_ctx.func,
                                &mut public_builder_context,
                            );
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
                            let local_callee =
                                module.declare_func_in_func(callee, &mut builder.func);

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
                            builder.ins().store(mem_flags, error, error_out_ptr, 0_i32);
                            builder.ins().return_(&[null_result]);

                            builder.finalize();
                        }

                        result.push((sy.clone().into_public(), public_ctx));
                    }
                    result.push((sy.clone(), ctx));

                    result
                } else {
                    Vec::new()
                }
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
        f.debug_struct("Codegen").finish()
    }
}

fn declare_variables(
    elements: &specs::ReadStorage<element::Element>,
    types: &specs::ReadStorage<ty::Type>,
    ptr_type: Type,
    builder: &mut FunctionBuilder,
    params: &[ir::Entity],
    statements: &[ir::Entity],
    entry_ebb: Ebb,
) -> collections::HashMap<ir::Entity, Variable> {
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
    variables: &mut collections::HashMap<ir::Entity, Variable>,
    next_var: &mut usize,
    entity: ir::Entity,
) {
    if let element::Element::Variable(_) = *elements.get(entity).unwrap() {
        declare_variable(types, ptr_type, builder, variables, next_var, entity);
    }
}

fn declare_variable(
    types: &specs::ReadStorage<ty::Type>,
    ptr_type: Type,
    builder: &mut FunctionBuilder,
    variables: &mut collections::HashMap<ir::Entity, Variable>,
    next_var: &mut usize,
    entity: ir::Entity,
) -> Variable {
    *variables.entry(entity).or_insert_with(|| {
        let var = Variable::new(*next_var);
        *next_var += 1;
        builder.declare_var(
            var,
            abi_type::AbiType::from_ir_type(types.get(entity).unwrap()).into_specific(ptr_type),
        );
        var
    })
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
*/

impl<'a, D> fmt::Display for Symbol<'a, D>
where
    D: Db,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        symbol_fmt_impl(self.1, f, self.0)
    }
}

fn symbol_fmt_impl(entity: Entity, f: &mut fmt::Formatter, db: &impl Db) -> fmt::Result {
    let (parent, role) = db.lookup_entity(entity);

    if let Some(parent) = parent {
        symbol_fmt_impl(parent, f, db)?;
        write!(f, ".")?;
    }

    match role {
        ir::EntityRole::File(file_id) => write!(f, "{:?}", db.file_relative_path(file_id)),
        ir::EntityRole::RecordField(ident) => write!(f, "{}", db.lookup_ident(file_id)),
        ir::EntityRole::TupleField(idx) => write!(f, "{}", idx),
        ir::EntityRole::VariableDefinition(ident) => write!(f, "{}", db.lookup_ident(file_id)),
        ir::EntityRole::VariableInitializer => write!(f, "(initializer)"),
        ir::EntityRole::SelectField(ident) => write!(f, "{}", db.lookup_ident(ident)),
        ir::EntityRole::AppliedFunction => write!(f, "(applied function)"),
        ir::EntityRole::AppliedParameter(idx) => write!(f, "(parameter {})", idx),
        ir::EntityRole::ParameterSignature => write!(f, "(signature)"),
        ir::EntityRole::ClosureCaptureDefinition(ident) => {
            write!(f, "(capture definition {})", db.lookup_ident(ident))
        }
        ir::EntityRole::ClosureParameter(ident) => {
            write!(f, "(parameter definition {})", db.lookup_ident(ident))
        }
        ir::EntityRole::ClosureStatement(idx) => write!(f, "(statement {})", idx),
        ir::EntityRole::ClosureSignature => write!(f, "(signature)"),
        ir::EntityRole::ClosureResult => write!(f, "(result expression)"),
        ir::EntityRole::ModuleDefinition(ident) => {
            write!(f, "(parameter definition {})", db.lookup_ident(ident))
        }
        ir::EntityRole::UnOperand => write!(f, "(operand)"),
        ir::EntityRole::BiLhs => write!(f, "(left operand)"),
        ir::EntityRole::BiRhs => write!(f, "(right operand)"),
    }
}
