//! A JIT compiler implementation based on the IR.
use log::debug;
use log::trace;
use std::collections;
use std::fmt;
use std::mem;
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
use crate::ir::element;
use crate::layout;
use crate::module;
use crate::ty;

mod abi_type;
mod builtin;
mod data;
pub mod error;
mod function;
#[cfg(test)]
mod tests;
mod util;

/// A codegen system, that can be used for JIT compilation.
#[salsa::query_group(CodegenStorage)]
pub trait Db: salsa::Database + interpreter::Db + ir::Db + layout::Db + ty::Db {
    #[salsa::dependencies]
    fn codegen(&self) -> error::Result<sync::Arc<module::Module>>;
}

pub struct Data {
    ctx: cranelift_module::DataContext,
}

struct Symbol<'d, D>(&'d D, ir::Entity);

struct Function {
    ctx: codegen::Context,
    name: String,
    entity: ir::Entity,
    is_public: bool,
}

struct Codegen<'m, 'd, B, D>
where
    B: cranelift_module::Backend,
{
    ptr_type: codegen::ir::Type,
    module: &'m mut cranelift_module::Module<B>,
    db: &'d D,
    functions: Vec<Function>,
    defined_strings: collections::HashMap<String, cranelift_module::DataId>,
    exposed_functions:
        collections::HashMap<(relative_path::RelativePathBuf, String), cranelift_module::FuncId>,
}

fn codegen(db: &impl Db) -> error::Result<sync::Arc<module::Module>> {
    let ptr_type = codegen_ptr_type(db);
    let mut builder =
        cranelift_simplejit::SimpleJITBuilder::new(cranelift_module::default_libcall_names());
    builder.symbols(builtin::BUILTINS.iter().map(|b| (b.symbol, b.ptr)));
    let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
        cranelift_module::Module::new(builder);
    let mut codegen = Codegen::new(ptr_type, &mut module, db);
    codegen.codegen_all()?;
    let function_ids = codegen.exposed_functions;

    Ok(sync::Arc::new(module::Module::new(module, function_ids)))
}

fn codegen_ptr_type(db: &impl Db) -> codegen::ir::Type {
    match db.ptr_size() {
        layout::PtrSize::Size8 => codegen::ir::types::I8,
        layout::PtrSize::Size16 => codegen::ir::types::I16,
        layout::PtrSize::Size32 => codegen::ir::types::I32,
        layout::PtrSize::Size64 => codegen::ir::types::I64,
    }
}

impl<'m, 'd, B, D> Codegen<'m, 'd, B, D>
where
    B: cranelift_module::Backend,
    D: Db,
{
    fn new(
        ptr_type: codegen::ir::Type,
        module: &'m mut cranelift_module::Module<B>,
        db: &'d D,
    ) -> Self {
        let defined_strings = collections::HashMap::new();
        let exposed_functions = collections::HashMap::new();
        let functions = Vec::new();
        Self {
            ptr_type,
            module,
            db,
            defined_strings,
            exposed_functions,
            functions,
        }
    }

    fn codegen_all(&mut self) -> error::Result<()> {
        for entity in self.db.entities()?.all() {
            self.codegen_entity(entity)?;
        }

        for Function {
            name,
            entity,
            ctx,
            is_public,
        } in &mut self.functions
        {
            let linkage = if *is_public {
                cranelift_module::Linkage::Export
            } else {
                cranelift_module::Linkage::Local
            };
            let func_id = self
                .module
                .declare_function(name, linkage, &ctx.func.signature)
                .unwrap();

            self.module.define_function(func_id, ctx).unwrap();
            self.module.clear_context(ctx);

            if *is_public {
                if let (Some(parent_id), ir::EntityRole::VariableDefinition(func_ident)) =
                    self.db.lookup_entity(*entity)
                {
                    let (_, parent_role) = self.db.lookup_entity(parent_id);
                    if let ir::EntityRole::File(file_id) = parent_role {
                        let func_ident = (*self.db.lookup_ident(func_ident)).clone();
                        let file_path = (*self.db.file_relative_path(file_id)).to_owned();
                        self.exposed_functions
                            .insert((file_path, func_ident), func_id);
                    } else {
                        debug!(
                                "not exposing function `{}` because it is not at module level (parent role is {:?})",
                                name,
                                parent_role,
                            );
                    }
                } else {
                    debug!("not exposing function `{}` because it is not directly a variable definition", name);
                }
            }
        }
        self.module.finalize_definitions();

        Ok(())
    }

    fn codegen_entity(&mut self, entity: ir::Entity) -> error::Result<()> {
        match &*self.db.element(entity)? {
            ir::element::Element::Module(ir::element::Module { variables }) => {
                for value in variables.values() {
                    self.codegen_definition(*value, true)?;
                }
            }
            ir::element::Element::Closure(ir::element::Closure { statements, .. }) => {
                for statement in statements {
                    self.codegen_definition(*statement, false)?;
                }
            }
            _ => (),
        }
        Ok(())
    }

    fn codegen_definition(&mut self, entity: ir::Entity, top_level: bool) -> error::Result<()> {
        match &*self.db.element(entity)? {
            element::Element::Variable(ref variable) => {
                let initializer = self.db.element(variable.initializer)?;
                match (&*initializer, &*self.db.ty(entity)?) {
                    (element::Element::Closure(ref closure), ty::Type::Function(ref ty)) => {
                        let (name, ctx) = self.codegen_function(entity, closure, ty)?;

                        if top_level {
                            let (name, ctx) = self.codegen_public_wrapper_function(
                                entity, closure, ty, &name, &ctx,
                            )?;
                            let is_public = true;
                            self.functions.push(Function {
                                name,
                                entity,
                                ctx,
                                is_public,
                            });
                        }

                        let is_public = false;
                        self.functions.push(Function {
                            name,
                            entity,
                            ctx,
                            is_public,
                        });
                    }
                    _ => (),
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn codegen_function(
        &mut self,
        entity: ir::Entity,
        closure: &element::Closure,
        ty: &ty::Function,
    ) -> error::Result<(String, codegen::Context)> {
        use cranelift::codegen::ir::InstBuilder;

        let mut ctx = self.module.make_context();
        let ret_type = self.populate_function_signature(ty, &mut ctx, false);

        let mut builder_context = frontend::FunctionBuilderContext::new();
        let mut builder =
            cranelift::frontend::FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let name = Symbol(self.db, entity).to_string();
        let name_len = name.len();
        let name_data_id = util::define_string(
            self.module,
            &mut self.defined_strings,
            &format!("funcname:{}", name),
            name.clone(),
        );
        let name_global_value = self.module.declare_data_in_func(name_data_id, builder.func);

        let entry_ebb = builder.create_ebb();
        builder.append_ebb_params_for_function_params(entry_ebb);

        let error_throw_ebb = builder.create_ebb();
        let error_unwind_ebb = builder.create_ebb();

        builder.switch_to_block(entry_ebb);
        builder.seal_block(entry_ebb);

        let variables = self.declare_variables(
            &mut builder,
            &closure.parameters,
            &closure.statements,
            entry_ebb,
        )?;

        let result = {
            let mut translation_ctx = function::Translator::new(
                self.module,
                &mut builder,
                self.db,
                self.ptr_type,
                error_throw_ebb,
                error_unwind_ebb,
                &variables,
                &mut self.defined_strings,
            );

            for stmt in &closure.statements {
                translation_ctx.exec_element(*stmt, &*self.db.element(*stmt)?)?;
            }

            translation_ctx.eval_element(closure.result, &*self.db.element(closure.result)?)?
        };

        let null_error = builder.ins().iconst(self.ptr_type, 0);
        builder.ins().return_(&[result, null_error]);

        let error_kind = builder.append_ebb_param(error_throw_ebb, types::I32);
        let error_filename = builder.append_ebb_param(error_throw_ebb, self.ptr_type);
        let error_filename_len = builder.append_ebb_param(error_throw_ebb, self.ptr_type);
        let error_line = builder.append_ebb_param(error_throw_ebb, types::I32);
        let error_col = builder.append_ebb_param(error_throw_ebb, types::I32);
        builder.switch_to_block(error_throw_ebb);
        builder.seal_block(error_throw_ebb);

        let error = {
            let mut translation_ctx = function::Translator::new(
                self.module,
                &mut builder,
                self.db,
                self.ptr_type,
                error_throw_ebb,
                error_unwind_ebb,
                &variables,
                &mut self.defined_strings,
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

        let error = builder.append_ebb_param(error_unwind_ebb, self.ptr_type);
        let error_filename = builder.append_ebb_param(error_unwind_ebb, self.ptr_type);
        let error_filename_len = builder.append_ebb_param(error_unwind_ebb, self.ptr_type);
        let error_line = builder.append_ebb_param(error_unwind_ebb, types::I32);
        let error_col = builder.append_ebb_param(error_unwind_ebb, types::I32);
        builder.switch_to_block(error_unwind_ebb);
        builder.seal_block(error_unwind_ebb);

        let name_value = builder.ins().global_value(self.ptr_type, name_global_value);
        let name_len_value = builder.ins().iconst(self.ptr_type, name_len as i64);

        {
            let mut translation_ctx = function::Translator::new(
                self.module,
                &mut builder,
                self.db,
                self.ptr_type,
                error_throw_ebb,
                error_unwind_ebb,
                &variables,
                &mut self.defined_strings,
            );
            translation_ctx.builtin_unwind_frame(
                error,
                name_value,
                name_len_value,
                error_filename,
                error_filename_len,
                error_line,
                error_col,
            );
        };

        let null_result = <Codegen<'m, 'd, B, D>>::default_value(ret_type, &mut builder);
        builder.ins().return_(&[null_result, error]);

        builder.finalize();

        debug!(
            "generated function `{}`:\n\n{}",
            name,
            builder.display(None)
        );
        Ok((name, ctx))
    }

    fn codegen_public_wrapper_function(
        &mut self,
        entity: ir::Entity,
        closure: &element::Closure,
        ty: &ty::Function,
        wrapped_name: &str,
        wrapped: &codegen::Context,
    ) -> error::Result<(String, codegen::Context)> {
        use cranelift::codegen::ir::InstBuilder;

        let mut ctx: codegen::Context = self.module.make_context();
        let mut builder_context = frontend::FunctionBuilderContext::new();

        let name = format!("public:{}", Symbol(self.db, entity));
        let ret_type = self.populate_function_signature(ty, &mut ctx, true);

        let mut builder =
            cranelift::frontend::FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        let entry_ebb = builder.create_ebb();
        builder.append_ebb_params_for_function_params(entry_ebb);

        let error_ebb = builder.create_ebb();

        builder.switch_to_block(entry_ebb);
        builder.seal_block(entry_ebb);

        let callee = self
            .module
            .declare_function(
                wrapped_name,
                cranelift_module::Linkage::Local,
                &wrapped.func.signature,
            )
            .unwrap();
        let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

        let (error_out_ptr, parameter_values) = builder.ebb_params(entry_ebb).split_last().unwrap();
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

        let error = builder.append_ebb_param(error_ebb, self.ptr_type);
        let error_out_ptr = builder.append_ebb_param(error_ebb, self.ptr_type);
        builder.switch_to_block(error_ebb);
        builder.seal_block(error_ebb);

        let null_result = <Codegen<'m, 'd, B, D>>::default_value(ret_type, &mut builder);

        let mut mem_flags = codegen::ir::MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();
        builder.ins().store(mem_flags, error, error_out_ptr, 0_i32);
        builder.ins().return_(&[null_result]);

        builder.finalize();

        debug!(
            "generated public wrapper function `{}`:\n\n{}",
            name,
            builder.display(None)
        );

        Ok((name, ctx))
    }

    fn default_value(
        ret_type: codegen::ir::Type,
        builder: &mut frontend::FunctionBuilder,
    ) -> codegen::ir::Value {
        use cranelift::codegen::ir::InstBuilder;

        if ret_type.is_int() {
            builder.ins().iconst(ret_type, 0)
        } else if ret_type == types::F32 {
            builder.ins().f32const(immediates::Ieee32::with_float(0.0))
        } else if ret_type == types::F64 {
            builder.ins().f64const(immediates::Ieee64::with_float(0.0))
        } else {
            unimplemented!()
        }
    }

    fn populate_function_signature(
        &mut self,
        ty: &ty::Function,
        ctx: &mut codegen::Context,
        is_public: bool,
    ) -> codegen::ir::Type {
        for parameter in &ty.parameters {
            ctx.func.signature.params.push(codegen::ir::AbiParam::new(
                abi_type::AbiType::from_ir_type(parameter).into_specific(self.ptr_type),
            ));
        }

        let ret_type = abi_type::AbiType::from_ir_type(&ty.result).into_specific(self.ptr_type);
        // Result
        ctx.func
            .signature
            .returns
            .push(codegen::ir::AbiParam::new(ret_type));

        // Error: if the function is public, pass in error pointer; else, use multiple return values
        if is_public {
            ctx.func
                .signature
                .params
                .push(codegen::ir::AbiParam::new(self.ptr_type));
        } else {
            ctx.func
                .signature
                .returns
                .push(codegen::ir::AbiParam::new(self.ptr_type))
        }

        ret_type
    }

    fn codegen_data(&mut self, entity: ir::Entity) -> error::Result<Option<sync::Arc<Data>>> {
        if let Some(value) = self.db.value(entity)? {
            let layout = self.db.layout(entity)?;
            let mut ctx = cranelift_module::DataContext::new();
            let mut data = Vec::new();
            data::Translator::new(&mut data, self.ptr_type).store_value(&*layout, &value);
            ctx.define(data.into_boxed_slice());
            Ok(Some(sync::Arc::new(Data { ctx })))
        } else {
            Ok(None)
        }
    }

    fn declare_variables(
        &mut self,
        builder: &mut cranelift::frontend::FunctionBuilder,
        params: &[ir::Entity],
        statements: &[ir::Entity],
        entry_ebb: codegen::ir::Ebb,
    ) -> error::Result<collections::HashMap<ir::Entity, cranelift::frontend::Variable>> {
        let mut next_var = 0;
        let mut variables = collections::HashMap::new();

        for (i, param) in params.iter().enumerate() {
            let param_initializer = builder.ebb_params(entry_ebb)[i];
            let var = self.declare_variable(builder, &mut variables, &mut next_var, *param)?;
            builder.def_var(var, param_initializer);
        }

        for statement in statements {
            self.declare_variables_in_element(builder, &mut variables, &mut next_var, *statement)?;
        }

        Ok(variables)
    }

    fn declare_variables_in_element(
        &mut self,
        builder: &mut cranelift::frontend::FunctionBuilder,
        variables: &mut collections::HashMap<ir::Entity, cranelift::frontend::Variable>,
        next_var: &mut u32,
        entity: ir::Entity,
    ) -> error::Result<()> {
        if let element::Element::Variable(_) = *self.db.element(entity)? {
            self.declare_variable(builder, variables, next_var, entity)?;
        }
        Ok(())
    }

    fn declare_variable(
        &mut self,
        builder: &mut cranelift::frontend::FunctionBuilder,
        variables: &mut collections::HashMap<ir::Entity, cranelift::frontend::Variable>,
        next_var: &mut u32,
        entity: ir::Entity,
    ) -> error::Result<cranelift::frontend::Variable> {
        let ty = self.db.ty(entity)?;
        Ok(*variables.entry(entity).or_insert_with(|| {
            let var = cranelift::frontend::Variable::with_u32(*next_var);
            *next_var += 1;
            builder.declare_var(
                var,
                abi_type::AbiType::from_ir_type(&*ty).into_specific(self.ptr_type),
            );
            var
        }))
    }
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

impl<'d, D> fmt::Display for Symbol<'d, D>
where
    D: Db,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        symbol_fmt_impl(self.1, f, self.0)
    }
}

fn symbol_fmt_impl(entity: ir::Entity, f: &mut fmt::Formatter, db: &impl Db) -> fmt::Result {
    let (parent, role) = db.lookup_entity(entity);

    if let Some(parent) = parent {
        symbol_fmt_impl(parent, f, db)?;
        write!(f, ".")?;
    }

    match role {
        ir::EntityRole::File(file_id) => write!(f, "{:?}", db.file_relative_path(file_id)),
        ir::EntityRole::RecordField(ident) => write!(f, "{}", db.lookup_ident(ident)),
        ir::EntityRole::TupleField(idx) => write!(f, "{}", idx),
        ir::EntityRole::VariableDefinition(ident) => write!(f, "{}", db.lookup_ident(ident)),
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
        ir::EntityRole::UnOperand => write!(f, "(operand)"),
        ir::EntityRole::BiLhs => write!(f, "(left operand)"),
        ir::EntityRole::BiRhs => write!(f, "(right operand)"),
    }
}
