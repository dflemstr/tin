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
use crate::ir::element;
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
    fn codegen(&self) -> error::Result<sync::Arc<module::Module>>;

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
    raw: sync::Arc<dyn codegen::isa::TargetIsa>,
}

struct Symbol<'d, D>(&'d D, ir::Entity);

struct Codegen<'m, 'd, B, D>
where
    B: cranelift_module::Backend,
{
    ptr_type: codegen::ir::Type,
    module: &'m mut cranelift_module::Module<B>,
    db: &'d D,
    defined_strings: collections::HashMap<String, cranelift_module::DataId>,
}

fn codegen(db: &impl Db) -> error::Result<sync::Arc<module::Module>> {
    let ptr_type = db.codegen_ptr_type();
    let mut builder =
        cranelift_simplejit::SimpleJITBuilder::new(cranelift_module::default_libcall_names());
    let mut module: cranelift_module::Module<cranelift_simplejit::SimpleJITBackend> =
        cranelift_module::Module::new(builder);
    let mut codegen = Codegen::new(ptr_type, &mut module, db);
    codegen.codegen_all()?;

    Ok(sync::Arc::new(module::Module::new(
        module,
        collections::HashMap::new(),
    )))
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
        Self {
            ptr_type,
            module,
            db,
            defined_strings,
        }
    }

    fn codegen_all(&mut self) -> error::Result<()> {
        for module_entity in self.db.entities()?.modules() {
            match &*self.db.element(module_entity)? {
                ir::element::Element::Module(ir::element::Module { variables }) => {
                    for value in variables.values() {
                        self.codegen_definition(*value)?;
                    }
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn codegen_definition(&mut self, entity: ir::Entity) -> error::Result<()> {
        match (
            &*self.db.element(entity)?,
            &*self.db.ty(entity)?,
            self.db.value(entity)?,
        ) {
            (element::Element::Closure(ref closure), ty::Type::Function(ref ty), _) => {
                self.codegen_function(entity, closure, ty)?;
                Ok(())
            }
            _ => Ok(()), // TODO logic error
        }
    }

    fn codegen_function(
        &mut self,
        entity: ir::Entity,
        closure: &element::Closure,
        ty: &ty::Function,
    ) -> error::Result<sync::Arc<Function>> {
        use cranelift::codegen::ir::InstBuilder;

        let mut ctx = codegen::Context::new();
        ctx.func.signature.call_conv = self.db.codegen_isa().default_call_conv();
        let ret_type = self.populate_function_signature(ty, &mut ctx);

        let mut builder_context = frontend::FunctionBuilderContext::new();
        let mut builder =
            cranelift::frontend::FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let name = Symbol(self.db, entity).to_string();
        let name_len = name.len();
        let name_data_id = util::define_string(
            self.module,
            &mut self.defined_strings,
            &format!("funcname:{}", name),
            name,
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

        let name = builder.ins().global_value(self.ptr_type, name_global_value);
        let name_len = builder.ins().iconst(self.ptr_type, name_len as i64);

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
        Ok(sync::Arc::new(Function { ctx }))
    }

    fn populate_function_signature(
        &mut self,
        ty: &ty::Function,
        ctx: &mut codegen::Context,
    ) -> codegen::ir::Type {
        for parameter in &ty.parameters {
            ctx.func.signature.params.push(codegen::ir::AbiParam::new(
                abi_type::AbiType::from_ir_type(parameter).into_specific(self.ptr_type),
            ));
        }

        let ret_type = abi_type::AbiType::from_ir_type(&ty.result).into_specific(self.ptr_type);

        // Result, error
        ctx.func.signature.returns.extend(&[
            codegen::ir::AbiParam::new(ret_type),
            codegen::ir::AbiParam::new(self.ptr_type),
        ]);

        ret_type
    }

    fn codegen_data(&mut self, entity: ir::Entity) -> error::Result<Option<sync::Arc<Data>>> {
        if let Some(value) = self.db.value(entity)? {
            let layout = self.db.layout(entity)?;
            let ptr_type = self.db.codegen_ptr_type();
            let mut ctx = cranelift_module::DataContext::new();
            let mut data = Vec::new();
            data::Translator::new(&mut data, ptr_type).store_value(&*layout, &value);
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

impl fmt::Debug for Isa {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Isa").finish()
    }
}

impl ops::Deref for Isa {
    type Target = dyn codegen::isa::TargetIsa;

    fn deref(&self) -> &Self::Target {
        &*self.raw
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
        ir::EntityRole::ModuleDefinition(ident) => {
            write!(f, "(parameter definition {})", db.lookup_ident(ident))
        }
        ir::EntityRole::UnOperand => write!(f, "(operand)"),
        ir::EntityRole::BiLhs => write!(f, "(left operand)"),
        ir::EntityRole::BiRhs => write!(f, "(right operand)"),
    }
}
