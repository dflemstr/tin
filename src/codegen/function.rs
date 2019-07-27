use std::collections;
use std::fmt;

use cranelift::prelude::*;
use cranelift_module;
use cranelift_simplejit;

use crate::codegen::abi_type;
use crate::codegen::builtin;
use crate::codegen::error;
use crate::codegen::util;
use crate::db;
use crate::ir;
use crate::ir::element;
use crate::ir::location;
use crate::layout;
use crate::module;
use crate::ty;
use crate::value;

pub struct Translator<'a, 'f, B, D>
where
    'f: 'a,
    B: cranelift_module::Backend,
{
    module: &'a mut cranelift_module::Module<B>,
    builder: &'a mut FunctionBuilder<'f>,
    db: &'a D,
    ptr_type: Type,
    error_throw_ebb: Ebb,
    error_unwind_ebb: Ebb,
    variables: &'a collections::HashMap<ir::Entity, Variable>,
    defined_strings: &'a mut collections::HashMap<String, cranelift_module::DataId>,
}

impl<'a, 'f, B, D> Translator<'a, 'f, B, D>
where
    B: cranelift_module::Backend,
    D: super::Db,
{
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::too_many_arguments))]
    pub fn new(
        module: &'a mut cranelift_module::Module<B>,
        builder: &'a mut FunctionBuilder<'f>,
        db: &'a D,
        ptr_type: Type,
        error_throw_ebb: Ebb,
        error_unwind_ebb: Ebb,
        variables: &'a collections::HashMap<ir::Entity, Variable>,
        defined_strings: &'a mut collections::HashMap<String, cranelift_module::DataId>,
    ) -> Self {
        Translator {
            module,
            builder,
            db,
            ptr_type,
            error_throw_ebb,
            error_unwind_ebb,
            variables,
            defined_strings,
        }
    }

    pub fn exec_element(
        &mut self,
        entity: ir::Entity,
        element: &element::Element,
    ) -> error::Result<()> {
        if let element::Element::Variable(ref v) = element {
            self.exec_variable(entity, v)?;
        } else {
            self.eval_element(entity, element)?;
        }
        Ok(())
    }

    pub fn exec_variable(
        &mut self,
        entity: ir::Entity,
        variable: &element::Variable,
    ) -> error::Result<()> {
        use crate::ir::Db as _;

        let initializer_element = self.db.element(variable.initializer)?;
        let value = self.eval_element(variable.initializer, &*initializer_element)?;
        self.builder.def_var(self.variables[&entity], value);
        Ok(())
    }

    pub fn eval_element(
        &mut self,
        entity: ir::Entity,
        element: &element::Element,
    ) -> error::Result<Value> {
        use crate::interpreter::Db as _;

        if let Some(value) = self.db.value(entity)? {
            self.eval_value(entity, &value)
        } else {
            match *element {
                element::Element::Number(ref v) => self.eval_number_value(entity, v),
                element::Element::String(ref v) => self.eval_string_value(entity, v),
                element::Element::Symbol(_) => {
                    // TODO type dependent
                    unimplemented!()
                }
                element::Element::Tuple(ref v) => self.eval_tuple(entity, v),
                element::Element::Record(ref v) => self.eval_record(entity, v),
                element::Element::UnOp(ref v) => self.eval_un_op(entity, v),
                element::Element::BiOp(ref v) => self.eval_bi_op(entity, v),
                element::Element::Variable(ref v) => self.eval_variable(entity, v),
                element::Element::Select(ref v) => self.eval_select(entity, v),
                element::Element::Apply(ref v) => self.eval_apply(entity, v),
                element::Element::Parameter(ref v) => self.eval_parameter(entity, v),
                element::Element::Capture(ref v) => self.eval_capture(entity, v),
                element::Element::Closure(ref v) => self.eval_closure(entity, v),
                element::Element::Module(ref v) => self.eval_module(entity, v),
                element::Element::Reference(entity) => {
                    self.eval_element(entity, &*self.db.element(entity)?)
                }
            }
        }
    }

    fn eval_value(&mut self, entity: ir::Entity, value: &value::Value) -> error::Result<Value> {
        use crate::layout::Db as _;
        use crate::ty::Db as _;

        if let value::Case::Number(ref n) = *value.case() {
            match *n {
                value::Number::U8(ref v) => Ok(self.builder.ins().iconst(types::I8, i64::from(*v))),
                value::Number::U16(ref v) => {
                    Ok(self.builder.ins().iconst(types::I16, i64::from(*v)))
                }
                value::Number::U32(ref v) => {
                    Ok(self.builder.ins().iconst(types::I32, i64::from(*v)))
                }
                value::Number::U64(ref v) => {
                    #[cfg_attr(
                        feature = "cargo-clippy",
                        allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
                    )]
                    let value = *v as i64;
                    Ok(self.builder.ins().iconst(types::I64, value))
                }
                value::Number::I8(ref v) => Ok(self.builder.ins().iconst(types::I8, i64::from(*v))),
                value::Number::I16(ref v) => {
                    Ok(self.builder.ins().iconst(types::I16, i64::from(*v)))
                }
                value::Number::I32(ref v) => {
                    Ok(self.builder.ins().iconst(types::I32, i64::from(*v)))
                }
                value::Number::I64(ref v) => Ok(self.builder.ins().iconst(types::I64, *v)),
                value::Number::F32(ref v) => Ok(self
                    .builder
                    .ins()
                    .f32const(Ieee32::with_float(v.into_inner()))),
                value::Number::F64(ref v) => Ok(self
                    .builder
                    .ins()
                    .f64const(Ieee64::with_float(v.into_inner()))),
            }
        } else {
            let ty = self.db.ty(entity)?;
            let layout = self.db.layout(entity)?;
            let abi_type = abi_type::AbiType::from_ir_type(&*ty).into_specific(self.ptr_type);

            assert_eq!(self.ptr_type, abi_type);

            let data_id = self
                .module
                .declare_data(
                    &entity.id().to_string(),
                    cranelift_module::Linkage::Local,
                    false,
                    Some(layout.alignment as u8),
                )
                .unwrap();
            let global_value = self
                .module
                .declare_data_in_func(data_id, &mut self.builder.func);
            Ok(self.builder.ins().global_value(self.ptr_type, global_value))
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_possible_wrap))]
    pub fn eval_number_value(
        &mut self,
        _entity: ir::Entity,
        number_value: &element::Number,
    ) -> error::Result<Value> {
        match *number_value {
            element::Number::U8(v) => Ok(self.builder.ins().iconst(types::I8, i64::from(v))),
            element::Number::U16(v) => Ok(self.builder.ins().iconst(types::I16, i64::from(v))),
            element::Number::U32(v) => Ok(self.builder.ins().iconst(types::I32, i64::from(v))),
            element::Number::U64(v) => Ok(self.builder.ins().iconst(types::I64, v as i64)),
            element::Number::I8(v) => Ok(self.builder.ins().iconst(types::I8, i64::from(v))),
            element::Number::I16(v) => Ok(self.builder.ins().iconst(types::I16, i64::from(v))),
            element::Number::I32(v) => Ok(self.builder.ins().iconst(types::I32, i64::from(v))),
            element::Number::I64(v) => Ok(self.builder.ins().iconst(types::I64, v)),
            element::Number::F32(v) => Ok(self
                .builder
                .ins()
                .f32const(Ieee32::with_float(v.into_inner()))),
            element::Number::F64(v) => Ok(self
                .builder
                .ins()
                .f64const(Ieee64::with_float(v.into_inner()))),
        }
    }

    pub fn eval_string_value(
        &mut self,
        entity: ir::Entity,
        _string_value: &str,
    ) -> error::Result<Value> {
        use crate::layout::Db as _;

        // TODO: create data symbol
        let layout = self.db.layout(entity)?;
        let symbol = self
            .module
            .declare_data(
                &format!("__data_{}", entity.id()),
                cranelift_module::Linkage::Local,
                false,
                Some(layout.alignment as u8),
            )
            .unwrap();
        let local_id = self
            .module
            .declare_data_in_func(symbol, &mut self.builder.func);

        Ok(self.builder.ins().symbol_value(self.ptr_type, local_id))
    }

    #[cfg_attr(
        feature = "cargo-clippy",
        allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
    )]
    pub fn eval_tuple(
        &mut self,
        entity: ir::Entity,
        tuple: &element::Tuple,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;
        use crate::layout::Db as _;

        let layout = self.db.layout(entity)?;
        let alloc_size = self.builder.ins().iconst(self.ptr_type, layout.size as i64);
        let alloc_align = self
            .builder
            .ins()
            .iconst(self.ptr_type, layout.alignment as i64);
        let result = self.builtin_alloc(alloc_size, alloc_align);

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();

        for (idx, offset_layout) in layout.unnamed_fields.iter().enumerate() {
            let value = tuple.fields[idx];
            let value = self.eval_element(value, &*self.db.element(value)?)?;
            let offset = offset_layout.offset as i32;
            self.builder.ins().store(mem_flags, value, result, offset);
        }

        Ok(result)
    }

    pub fn eval_record(
        &mut self,
        entity: ir::Entity,
        record: &element::Record,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;
        use crate::layout::Db as _;

        let layout = self.db.layout(entity)?;

        #[cfg_attr(
            feature = "cargo-clippy",
            allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
        )]
        let alloc_size = self.builder.ins().iconst(self.ptr_type, layout.size as i64);

        #[cfg_attr(
            feature = "cargo-clippy",
            allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
        )]
        let alloc_align = self
            .builder
            .ins()
            .iconst(self.ptr_type, layout.alignment as i64);
        let result = self.builtin_alloc(alloc_size, alloc_align);

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();

        for named_field in &layout.named_fields {
            let value = record.fields[&named_field.field];
            let value = self.eval_element(value, &*self.db.element(value)?)?;

            #[cfg_attr(
                feature = "cargo-clippy",
                allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
            )]
            let offset = named_field.offset_layout.offset as i32;
            self.builder.ins().store(mem_flags, value, result, offset);
        }

        Ok(result)
    }

    pub fn eval_un_op(
        &mut self,
        _entity: ir::Entity,
        un_op: &element::UnOp,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;

        let element::UnOp { operator, operand } = un_op;

        let operand_value = self.eval_element(*operand, &*self.db.element(*operand)?)?;

        match operator {
            element::UnOperator::Not | element::UnOperator::BNot => {
                Ok(self.builder.ins().bnot(operand_value))
            }
            element::UnOperator::Cl0 => Ok(self.builder.ins().clz(operand_value)),
            element::UnOperator::Cl1 => {
                let inverted = self.builder.ins().bnot(operand_value);
                Ok(self.builder.ins().clz(inverted))
            }
            element::UnOperator::Cls => Ok(self.builder.ins().cls(operand_value)),
            element::UnOperator::Ct0 => Ok(self.builder.ins().ctz(operand_value)),
            element::UnOperator::Ct1 => {
                let inverted = self.builder.ins().bnot(operand_value);
                Ok(self.builder.ins().ctz(inverted))
            }
            element::UnOperator::C0 => {
                let inverted = self.builder.ins().bnot(operand_value);
                Ok(self.builder.ins().popcnt(inverted))
            }
            element::UnOperator::C1 => Ok(self.builder.ins().popcnt(operand_value)),
            element::UnOperator::Sqrt => Ok(self.builder.ins().sqrt(operand_value)),
        }
    }

    pub fn eval_bi_op(
        &mut self,
        entity: ir::Entity,
        bi_op: &element::BiOp,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;
        use crate::ty::Db as _;

        let element::BiOp { lhs, operator, rhs } = bi_op;
        let lhs = *lhs;
        let rhs = *rhs;

        // TODO: support lazy evaluation
        let lhs_value = self.eval_element(lhs, &*self.db.element(lhs)?)?;
        let rhs_value = self.eval_element(rhs, &*self.db.element(rhs)?)?;

        match operator {
            element::BiOperator::Eq => unimplemented!(),
            element::BiOperator::Ne => unimplemented!(),
            element::BiOperator::Lt => unimplemented!(),
            element::BiOperator::Ge => unimplemented!(),
            element::BiOperator::Gt => unimplemented!(),
            element::BiOperator::Le => unimplemented!(),
            element::BiOperator::Cmp => unimplemented!(),
            element::BiOperator::Add => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(_) => Ok(self.builder.ins().iadd(lhs_value, rhs_value)),
                ty::class::Scalar::Fractional => Ok(self.builder.ins().fadd(lhs_value, rhs_value)),
                _ => unreachable!(),
            },
            element::BiOperator::Sub => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(_) => Ok(self.builder.ins().isub(lhs_value, rhs_value)),
                ty::class::Scalar::Fractional => Ok(self.builder.ins().fsub(lhs_value, rhs_value)),
                _ => unreachable!(),
            },
            element::BiOperator::Mul => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(_) => Ok(self.builder.ins().imul(lhs_value, rhs_value)),
                ty::class::Scalar::Fractional => Ok(self.builder.ins().fmul(lhs_value, rhs_value)),
                _ => unreachable!(),
            },
            element::BiOperator::Div => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Unsigned) => {
                    self.error_if_zero(entity, rhs_value, module::ErrorKind::IntegerDivisonByZero)?;
                    Ok(self.builder.ins().udiv(lhs_value, rhs_value))
                }
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Signed) => {
                    self.error_if_zero(entity, rhs_value, module::ErrorKind::IntegerDivisonByZero)?;
                    Ok(self.builder.ins().sdiv(lhs_value, rhs_value))
                }
                ty::class::Scalar::Fractional => Ok(self.builder.ins().fdiv(lhs_value, rhs_value)),
                _ => unreachable!(),
            },
            element::BiOperator::Rem => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Unsigned) => {
                    self.error_if_zero(entity, rhs_value, module::ErrorKind::IntegerDivisonByZero)?;
                    Ok(self.builder.ins().urem(lhs_value, rhs_value))
                }
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Signed) => {
                    self.error_if_zero(entity, rhs_value, module::ErrorKind::IntegerDivisonByZero)?;
                    Ok(self.builder.ins().srem(lhs_value, rhs_value))
                }
                _ => unreachable!(),
            },
            element::BiOperator::And | element::BiOperator::BAnd => {
                Ok(self.builder.ins().band(lhs_value, rhs_value))
            }
            element::BiOperator::Or | element::BiOperator::BOr => {
                Ok(self.builder.ins().bor(lhs_value, rhs_value))
            }
            element::BiOperator::Xor | element::BiOperator::BXor => {
                Ok(self.builder.ins().bxor(lhs_value, rhs_value))
            }
            element::BiOperator::AndNot | element::BiOperator::BAndNot => {
                Ok(self.builder.ins().band_not(lhs_value, rhs_value))
            }
            element::BiOperator::OrNot | element::BiOperator::BOrNot => {
                Ok(self.builder.ins().bor_not(lhs_value, rhs_value))
            }
            element::BiOperator::XorNot | element::BiOperator::BXorNot => {
                Ok(self.builder.ins().bxor_not(lhs_value, rhs_value))
            }
            element::BiOperator::RotL => Ok(self.builder.ins().rotl(lhs_value, rhs_value)),
            element::BiOperator::RotR => Ok(self.builder.ins().rotr(lhs_value, rhs_value)),
            element::BiOperator::ShL => Ok(self.builder.ins().ishl(lhs_value, rhs_value)),
            element::BiOperator::ShR => match self.db.ty(lhs)?.scalar_class() {
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Unsigned) => {
                    Ok(self.builder.ins().ushr(lhs_value, rhs_value))
                }
                ty::class::Scalar::Integral(ty::class::IntegralScalar::Signed) => {
                    Ok(self.builder.ins().sshr(lhs_value, rhs_value))
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn eval_variable(
        &mut self,
        entity: ir::Entity,
        _variable: &element::Variable,
    ) -> error::Result<Value> {
        Ok(self.builder.use_var(self.variables[&entity]))
    }

    pub fn eval_select(
        &mut self,
        _entity: ir::Entity,
        select: &element::Select,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;
        use crate::layout::Db as _;
        use crate::ty::Db as _;

        let record_layout = self.db.layout(select.record)?;
        let ty = self.db.ty(select.record)?;
        let record_type = match &*ty {
            ty::Type::Record(r) => r,
            _ => unreachable!(),
        };

        let field_type = &record_type.fields[&select.field];
        let field_abi_type =
            abi_type::AbiType::from_ir_type(field_type).into_specific(self.ptr_type);

        #[cfg_attr(
            feature = "cargo-clippy",
            allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)
        )]
        let field_offset = record_layout
            .named_fields
            .iter()
            .find(|f| f.field == select.field)
            .unwrap()
            .offset_layout
            .offset as i32;

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();
        mem_flags.set_readonly();

        let record = self.eval_element(select.record, &*self.db.element(select.record)?)?;

        Ok(self
            .builder
            .ins()
            .load(field_abi_type, mem_flags, record, field_offset))
    }

    pub fn eval_parameter(
        &mut self,
        entity: ir::Entity,
        _parameter: &element::Parameter,
    ) -> error::Result<Value> {
        Ok(self.builder.use_var(self.variables[&entity]))
    }

    pub fn eval_apply(
        &mut self,
        entity: ir::Entity,
        apply: &element::Apply,
    ) -> error::Result<Value> {
        use crate::ir::Db as _;
        use crate::ty::Db as _;

        let mut sig = self.module.make_signature();

        for parameter in &apply.parameters {
            sig.params.push(AbiParam::new(
                abi_type::AbiType::from_ir_type(&*self.db.ty(*parameter)?)
                    .into_specific(self.ptr_type),
            ));
        }
        // Result
        sig.returns.push(AbiParam::new(
            abi_type::AbiType::from_ir_type(&*self.db.ty(entity)?).into_specific(self.ptr_type),
        ));
        // Error
        sig.returns.push(AbiParam::new(self.ptr_type));

        let name = super::Symbol(self.db, apply.function).to_string();
        let callee = self
            .module
            .declare_function(&name, cranelift_module::Linkage::Import, &sig)
            .unwrap();
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let parameter_values = apply
            .parameters
            .iter()
            .map(|p| Ok(self.eval_element(*p, &*self.db.element(*p)?)?))
            .collect::<error::Result<Vec<_>>>()?;

        let call = self.builder.ins().call(local_callee, &parameter_values);

        let results = self.builder.inst_results(call);
        let result = results[0];
        let error = results[1];

        let location = self.db.location(entity)?;
        let (filename, filename_len, line, col) = self.immediate_location(location);

        self.builder.ins().brnz(
            error,
            self.error_unwind_ebb,
            &[error, filename, filename_len, line, col],
        );

        Ok(result)
    }

    pub fn eval_capture(
        &mut self,
        entity: ir::Entity,
        _capture: &element::Capture,
    ) -> error::Result<Value> {
        Ok(self.builder.use_var(self.variables[&entity]))
    }

    pub fn eval_closure(
        &mut self,
        _entity: ir::Entity,
        _closure: &element::Closure,
    ) -> error::Result<Value> {
        unimplemented!()
    }

    pub fn eval_module(
        &mut self,
        _entity: ir::Entity,
        _module: &element::Module,
    ) -> error::Result<Value> {
        unimplemented!()
    }

    pub fn error_if_zero(
        &mut self,
        entity: ir::Entity,
        value: Value,
        kind: module::ErrorKind,
    ) -> error::Result<()> {
        use crate::ir::Db as _;
        use num_traits::cast::ToPrimitive;

        let kind = self
            .builder
            .ins()
            .iconst(types::I32, i64::from(kind.to_u32().unwrap()));

        let location = self.db.location(entity)?;

        let (filename, filename_len, line, col) = self.immediate_location(location);

        self.builder.ins().brz(
            value,
            self.error_throw_ebb,
            &[kind, filename, filename_len, line, col],
        );

        Ok(())
    }

    pub fn immediate_location(
        &mut self,
        location: location::Location,
    ) -> (Value, Value, Value, Value) {
        let location = location.0;

        let filemap = self
            .db
            .code_map()
            .raw
            .find_file(location.start())
            .unwrap()
            .clone();
        let (line, col) = filemap.location(location.start()).unwrap();
        let filename = filemap.name().to_string();
        let filename_len = filename.len();
        let filename_data_id = util::define_string(
            self.module,
            self.defined_strings,
            &format!("filename:{}", filename),
            filename,
        );
        let filename_global_value = self
            .module
            .declare_data_in_func(filename_data_id, self.builder.func);
        let filename = self
            .builder
            .ins()
            .global_value(self.ptr_type, filename_global_value);
        let filename_len = self
            .builder
            .ins()
            .iconst(self.ptr_type, filename_len as i64);
        let line = self.builder.ins().iconst(types::I32, i64::from(line.0));
        let col = self.builder.ins().iconst(types::I32, i64::from(col.0));

        (filename, filename_len, line, col)
    }

    pub fn builtin_alloc(&mut self, size: Value, align: Value) -> Value {
        let local_callee = self.declare_builtin(&builtin::ALLOC);

        let call = self.builder.ins().call(local_callee, &[size, align]);

        self.builder.inst_results(call)[0]
    }

    pub fn builtin_error(&mut self, kind: Value) -> Value {
        let local_callee = self.declare_builtin(&builtin::ERROR);

        let call = self.builder.ins().call(local_callee, &[kind]);

        self.builder.inst_results(call)[0]
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::too_many_arguments))]
    pub fn builtin_unwind_frame(
        &mut self,
        error: Value,
        name: Value,
        name_len: Value,
        filename: Value,
        filename_len: Value,
        line: Value,
        col: Value,
    ) {
        let local_callee = self.declare_builtin(&builtin::UNWIND_FRAME);

        self.builder.ins().call(
            local_callee,
            &[error, name, name_len, filename, filename_len, line, col],
        );
    }

    fn declare_builtin(&mut self, builtin: &builtin::Builtin) -> cranelift::codegen::ir::FuncRef {
        let mut signature = self.module.make_signature();

        for param in builtin.signature.params {
            signature
                .params
                .push(AbiParam::new(param.into_specific(self.ptr_type)));
        }

        for ret in builtin.signature.returns {
            signature
                .returns
                .push(AbiParam::new(ret.into_specific(self.ptr_type)));
        }

        let callee = self
            .module
            .declare_function(
                builtin.symbol,
                cranelift_module::Linkage::Import,
                &signature,
            )
            .unwrap();

        self.module
            .declare_func_in_func(callee, &mut self.builder.func)
    }
}

impl<'a, 'f, B, D> fmt::Debug for Translator<'a, 'f, B, D>
where
    B: cranelift_module::Backend,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Translator").finish()
    }
}
