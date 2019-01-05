use std::collections;
use std::fmt;

use specs;

use cranelift::prelude::*;
use cranelift_module;
use cranelift_simplejit;

use crate::codegen::abi_type;
use crate::codegen::builtin;
use crate::ir::component::constexpr;
use crate::ir::component::element;
use crate::ir::component::layout;
use crate::ir::component::symbol;
use crate::ir::component::ty;
use crate::value;

pub struct DataTranslator<'a> {
    storage: &'a mut Vec<u8>,
    ptr_type: Type,
}

pub struct FunctionTranslator<'a, 'f>
where
    'f: 'a,
{
    module: &'a mut cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    builder: &'a mut FunctionBuilder<'f>,
    constexprs: &'a specs::ReadStorage<'a, constexpr::Constexpr>,
    elements: &'a specs::ReadStorage<'a, element::Element>,
    layouts: &'a specs::ReadStorage<'a, layout::Layout>,
    symbols: &'a specs::ReadStorage<'a, symbol::Symbol>,
    types: &'a specs::ReadStorage<'a, ty::Type>,
    ptr_type: Type,
    variables: collections::HashMap<specs::Entity, Variable>,
}

impl<'a> DataTranslator<'a> {
    pub fn new(storage: &'a mut Vec<u8>, ptr_type: Type) -> Self {
        DataTranslator { storage, ptr_type }
    }

    pub fn store_value(&mut self, layout: &layout::Layout, value: &value::Value) {
        self.storage.reserve(layout.size);
        match value {
            value::Value::Number(v) => self.store_number(layout, *v),
            value::Value::String(v) => self.store_string(layout, v),
            value::Value::Symbol(_) => {} // TODO type dependent
            value::Value::Tuple(v) => self.store_tuple(layout, v),
            value::Value::Record(v) => self.store_record(layout, v),
        }
    }

    pub fn store_number(&mut self, _layout: &layout::Layout, number: value::Number) {
        use byteorder::WriteBytesExt;

        match number {
            value::Number::U8(v) => self.storage.write_u8(v).unwrap(),
            value::Number::U16(v) => self
                .storage
                .write_u16::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::U32(v) => self
                .storage
                .write_u32::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::U64(v) => self
                .storage
                .write_u64::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::I8(v) => self.storage.write_i8(v).unwrap(),
            value::Number::I16(v) => self
                .storage
                .write_i16::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::I32(v) => self
                .storage
                .write_i32::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::I64(v) => self
                .storage
                .write_i64::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::F32(v) => self
                .storage
                .write_f32::<byteorder::NativeEndian>(v)
                .unwrap(),
            value::Number::F64(v) => self
                .storage
                .write_f64::<byteorder::NativeEndian>(v)
                .unwrap(),
        }
    }

    pub fn store_string(&mut self, _layout: &layout::Layout, string: &str) {
        use byteorder::WriteBytesExt;
        use std::io::Write;

        let len = string.len();
        match self.ptr_type.bits() {
            8 => self.storage.write_u8(len as u8).unwrap(),
            16 => self
                .storage
                .write_u16::<byteorder::NativeEndian>(len as u16)
                .unwrap(),
            32 => self
                .storage
                .write_u32::<byteorder::NativeEndian>(len as u32)
                .unwrap(),
            64 => self
                .storage
                .write_u64::<byteorder::NativeEndian>(len as u64)
                .unwrap(),
            _ => unimplemented!(),
        }

        self.storage.write_all(string.as_bytes()).unwrap();
    }

    pub fn store_tuple(&mut self, layout: &layout::Layout, tuple: &value::Tuple) {
        use std::io::Write;

        let unnamed_fields = layout.unnamed_fields.as_ref().unwrap();
        let mut pos = 0;
        assert_eq!(tuple.fields.len(), unnamed_fields.len());

        for (value, offset_layout) in tuple.fields.iter().zip(unnamed_fields.iter()) {
            assert!(offset_layout.offset >= pos);
            while pos < offset_layout.offset {
                self.storage.write_all(&[0u8]).unwrap();
                pos += 1;
            }
            self.store_value(&offset_layout.layout, &**value);
            pos += offset_layout.layout.size;
        }
    }

    pub fn store_record(&mut self, layout: &layout::Layout, record: &value::Record) {
        use std::io::Write;

        let named_fields = layout.named_fields.as_ref().unwrap();
        let mut pos = 0;
        assert_eq!(record.fields.len(), named_fields.len());

        for ((_, value), named_field) in record.fields.iter().zip(named_fields.iter()) {
            let offset_layout = &named_field.offset_layout;
            assert!(offset_layout.offset >= pos);
            while pos < offset_layout.offset {
                self.storage.write_all(&[0u8]).unwrap();
                pos += 1;
            }
            self.store_value(&offset_layout.layout, &**value);
            pos += offset_layout.layout.size;
        }
    }
}

impl<'a, 'f> FunctionTranslator<'a, 'f> {
    pub fn new(
        module: &'a mut cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
        builder: &'a mut FunctionBuilder<'f>,
        constexprs: &'a specs::ReadStorage<'a, constexpr::Constexpr>,
        elements: &'a specs::ReadStorage<'a, element::Element>,
        layouts: &'a specs::ReadStorage<'a, layout::Layout>,
        symbols: &'a specs::ReadStorage<'a, symbol::Symbol>,
        types: &'a specs::ReadStorage<'a, ty::Type>,
        ptr_type: Type,
        variables: collections::HashMap<specs::Entity, Variable>,
    ) -> Self {
        FunctionTranslator {
            module,
            builder,
            constexprs,
            elements,
            layouts,
            symbols,
            types,
            ptr_type,
            variables,
        }
    }

    pub fn exec_element(&mut self, entity: specs::Entity, element: &element::Element) {
        match *element {
            element::Element::Variable(ref v) => self.exec_variable(entity, v),
            _ => {
                self.eval_element(entity, element);
            }
        }
    }

    pub fn exec_variable(&mut self, entity: specs::Entity, variable: &element::Variable) {
        let initializer_element = self.elements.get(variable.initializer).unwrap();
        let value = self.eval_element(variable.initializer, initializer_element);
        self.builder.def_var(self.variables[&entity], value);
    }

    pub fn eval_element(&mut self, entity: specs::Entity, element: &element::Element) -> Value {
        if let Some(constexpr) = self.constexprs.get(entity) {
            self.eval_constexpr(&entity, constexpr)
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
            }
        }
    }

    fn eval_constexpr(
        &mut self,
        entity: &specs::Entity,
        constexpr: &constexpr::Constexpr,
    ) -> Value {
        match *constexpr.value {
            value::Value::Number(n) => match n {
                value::Number::U8(v) => self.builder.ins().iconst(types::I8, v as i64),
                value::Number::U16(v) => self.builder.ins().iconst(types::I16, v as i64),
                value::Number::U32(v) => self.builder.ins().iconst(types::I32, v as i64),
                value::Number::U64(v) => self.builder.ins().iconst(types::I64, v as i64),
                value::Number::I8(v) => self.builder.ins().iconst(types::I8, v as i64),
                value::Number::I16(v) => self.builder.ins().iconst(types::I16, v as i64),
                value::Number::I32(v) => self.builder.ins().iconst(types::I32, v as i64),
                value::Number::I64(v) => self.builder.ins().iconst(types::I64, v),
                value::Number::F32(v) => self.builder.ins().f32const(Ieee32::with_float(v)),
                value::Number::F64(v) => self.builder.ins().f64const(Ieee64::with_float(v)),
            },
            _ => {
                let ty = self.types.get(*entity).unwrap();
                let abi_type = abi_type::AbiType::from_ir_type(ty).into_specific(self.ptr_type);

                assert_eq!(self.ptr_type, abi_type);

                let data_id = self
                    .module
                    .declare_data(
                        &entity.id().to_string(),
                        cranelift_module::Linkage::Local,
                        false,
                    )
                    .unwrap();
                let global_value = self
                    .module
                    .declare_data_in_func(data_id, &mut self.builder.func);
                self.builder.ins().global_value(self.ptr_type, global_value)
            }
        }
    }

    pub fn eval_number_value(
        &mut self,
        _entity: specs::Entity,
        number_value: &element::Number,
    ) -> Value {
        match *number_value {
            element::Number::U8(v) => self.builder.ins().iconst(types::I8, v as i64),
            element::Number::U16(v) => self.builder.ins().iconst(types::I16, v as i64),
            element::Number::U32(v) => self.builder.ins().iconst(types::I32, v as i64),
            element::Number::U64(v) => self.builder.ins().iconst(types::I64, v as i64),
            element::Number::I8(v) => self.builder.ins().iconst(types::I8, v as i64),
            element::Number::I16(v) => self.builder.ins().iconst(types::I16, v as i64),
            element::Number::I32(v) => self.builder.ins().iconst(types::I32, v as i64),
            element::Number::I64(v) => self.builder.ins().iconst(types::I64, v),
            element::Number::F32(v) => self.builder.ins().f32const(Ieee32::with_float(v)),
            element::Number::F64(v) => self.builder.ins().f64const(Ieee64::with_float(v)),
        }
    }

    pub fn eval_string_value(&mut self, entity: specs::Entity, _string_value: &str) -> Value {
        // TODO: create data symbol
        let symbol = self
            .module
            .declare_data(
                &format!("__data_{}", entity.id()),
                cranelift_module::Linkage::Local,
                false,
            )
            .unwrap();
        let local_id = self
            .module
            .declare_data_in_func(symbol, &mut self.builder.func);

        self.builder.ins().symbol_value(self.ptr_type, local_id)
    }

    pub fn eval_tuple(&mut self, entity: specs::Entity, tuple: &element::Tuple) -> Value {
        let layout = self.layouts.get(entity).unwrap();
        let alloc_size = self.builder.ins().iconst(self.ptr_type, layout.size as i64);
        let alloc_align = self
            .builder
            .ins()
            .iconst(self.ptr_type, layout.alignment as i64);
        let result = self.builtin_alloc(alloc_size, alloc_align);

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();

        for (idx, offset_layout) in layout.unnamed_fields.as_ref().unwrap().iter().enumerate() {
            let value = tuple.fields[idx];
            let value = self.eval_element(value, self.elements.get(value).unwrap());
            let offset = offset_layout.offset as i32;
            self.builder.ins().store(mem_flags, value, result, offset);
        }

        result
    }

    pub fn eval_record(&mut self, entity: specs::Entity, record: &element::Record) -> Value {
        let layout = self.layouts.get(entity).unwrap();
        let alloc_size = self.builder.ins().iconst(self.ptr_type, layout.size as i64);
        let alloc_align = self
            .builder
            .ins()
            .iconst(self.ptr_type, layout.alignment as i64);
        let result = self.builtin_alloc(alloc_size, alloc_align);

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();

        for named_field in layout.named_fields.as_ref().unwrap() {
            let value = record.fields[&named_field.field];
            let value = self.eval_element(value, self.elements.get(value).unwrap());
            let offset = named_field.offset_layout.offset as i32;
            self.builder.ins().store(mem_flags, value, result, offset);
        }

        result
    }

    pub fn eval_un_op(&mut self, _entity: specs::Entity, un_op: &element::UnOp) -> Value {
        let element::UnOp { operator, operand } = un_op;
        let operand = *operand;

        let operand_value = self.eval_element(operand, self.elements.get(operand).unwrap());

        match operator {
            element::UnOperator::Not => self.builder.ins().bnot(operand_value),
            element::UnOperator::BNot => self.builder.ins().bnot(operand_value),
            element::UnOperator::Cl0 => self.builder.ins().clz(operand_value),
            element::UnOperator::Cl1 => {
                let inverted = self.builder.ins().bnot(operand_value);
                self.builder.ins().clz(inverted)
            }
            element::UnOperator::Cls => self.builder.ins().cls(operand_value),
            element::UnOperator::Ct0 => self.builder.ins().ctz(operand_value),
            element::UnOperator::Ct1 => {
                let inverted = self.builder.ins().bnot(operand_value);
                self.builder.ins().ctz(inverted)
            }
            element::UnOperator::C0 => {
                let inverted = self.builder.ins().bnot(operand_value);
                self.builder.ins().popcnt(inverted)
            }
            element::UnOperator::C1 => self.builder.ins().popcnt(operand_value),
            element::UnOperator::Sqrt => self.builder.ins().sqrt(operand_value),
        }
    }

    pub fn eval_bi_op(&mut self, _entity: specs::Entity, bi_op: &element::BiOp) -> Value {
        let element::BiOp { lhs, operator, rhs } = bi_op;
        let lhs = *lhs;
        let rhs = *rhs;

        // TODO: support lazy evaluation
        let lhs_value = self.eval_element(lhs, self.elements.get(lhs).unwrap());
        let rhs_value = self.eval_element(rhs, self.elements.get(rhs).unwrap());

        match operator {
            element::BiOperator::Eq => unimplemented!(),
            element::BiOperator::Ne => unimplemented!(),
            element::BiOperator::Lt => unimplemented!(),
            element::BiOperator::Ge => unimplemented!(),
            element::BiOperator::Gt => unimplemented!(),
            element::BiOperator::Le => unimplemented!(),
            element::BiOperator::Cmp => unimplemented!(),
            element::BiOperator::Add => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(_) => self.builder.ins().iadd(lhs_value, rhs_value),
                ty::ScalarClass::Fractional => self.builder.ins().fadd(lhs_value, rhs_value),
                _ => unreachable!(),
            },
            element::BiOperator::Sub => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(_) => self.builder.ins().isub(lhs_value, rhs_value),
                ty::ScalarClass::Fractional => self.builder.ins().fsub(lhs_value, rhs_value),
                _ => unreachable!(),
            },
            element::BiOperator::Mul => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(_) => self.builder.ins().imul(lhs_value, rhs_value),
                ty::ScalarClass::Fractional => self.builder.ins().fmul(lhs_value, rhs_value),
                _ => unreachable!(),
            },
            element::BiOperator::Div => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Unsigned) => {
                    self.builder.ins().udiv(lhs_value, rhs_value)
                }
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Signed) => {
                    self.builder.ins().sdiv(lhs_value, rhs_value)
                }
                ty::ScalarClass::Fractional => self.builder.ins().fdiv(lhs_value, rhs_value),
                _ => unreachable!(),
            },
            element::BiOperator::Rem => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Unsigned) => {
                    self.builder.ins().urem(lhs_value, rhs_value)
                }
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Signed) => {
                    self.builder.ins().srem(lhs_value, rhs_value)
                }
                _ => unreachable!(),
            },
            element::BiOperator::And => self.builder.ins().band(lhs_value, rhs_value),
            element::BiOperator::BAnd => self.builder.ins().band(lhs_value, rhs_value),
            element::BiOperator::Or => self.builder.ins().bor(lhs_value, rhs_value),
            element::BiOperator::BOr => self.builder.ins().bor(lhs_value, rhs_value),
            element::BiOperator::Xor => self.builder.ins().bxor(lhs_value, rhs_value),
            element::BiOperator::BXor => self.builder.ins().bxor(lhs_value, rhs_value),
            element::BiOperator::AndNot => self.builder.ins().band_not(lhs_value, rhs_value),
            element::BiOperator::BAndNot => self.builder.ins().band_not(lhs_value, rhs_value),
            element::BiOperator::OrNot => self.builder.ins().bor_not(lhs_value, rhs_value),
            element::BiOperator::BOrNot => self.builder.ins().bor_not(lhs_value, rhs_value),
            element::BiOperator::XorNot => self.builder.ins().bxor_not(lhs_value, rhs_value),
            element::BiOperator::BXorNot => self.builder.ins().bxor_not(lhs_value, rhs_value),
            element::BiOperator::RotL => self.builder.ins().rotl(lhs_value, rhs_value),
            element::BiOperator::RotR => self.builder.ins().rotr(lhs_value, rhs_value),
            element::BiOperator::ShL => self.builder.ins().ishl(lhs_value, rhs_value),
            element::BiOperator::ShR => match self.types.get(lhs).unwrap().scalar_class() {
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Unsigned) => {
                    self.builder.ins().ushr(lhs_value, rhs_value)
                }
                ty::ScalarClass::Integral(ty::IntegralScalarClass::Signed) => {
                    self.builder.ins().sshr(lhs_value, rhs_value)
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn eval_variable(&mut self, entity: specs::Entity, _variable: &element::Variable) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_select(&mut self, _entity: specs::Entity, select: &element::Select) -> Value {
        let record_layout = self.layouts.get(select.record).unwrap();
        let record_type = match self.types.get(select.record).unwrap() {
            ty::Type::Record(r) => r,
            _ => unreachable!(),
        };

        let field_type = &record_type.fields[&select.field];
        let field_abi_type =
            abi_type::AbiType::from_ir_type(field_type).into_specific(self.ptr_type);
        let field_offset = record_layout
            .named_fields
            .as_ref()
            .unwrap()
            .iter()
            .find(|f| *f.field == select.field)
            .unwrap()
            .offset_layout
            .offset as i32;

        let mut mem_flags = MemFlags::new();
        mem_flags.set_notrap();
        mem_flags.set_aligned();
        mem_flags.set_readonly();

        let record = self.eval_element(select.record, self.elements.get(select.record).unwrap());

        self.builder
            .ins()
            .load(field_abi_type, mem_flags, record, field_offset)
    }

    pub fn eval_parameter(
        &mut self,
        entity: specs::Entity,
        _parameter: &element::Parameter,
    ) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_apply(&mut self, entity: specs::Entity, apply: &element::Apply) -> Value {
        let mut sig = self.module.make_signature();

        for parameter in &apply.parameters {
            sig.params.push(AbiParam::new(
                abi_type::AbiType::from_ir_type(self.types.get(*parameter).unwrap())
                    .into_specific(self.ptr_type),
            ));
        }
        sig.returns.push(AbiParam::new(
            abi_type::AbiType::from_ir_type(self.types.get(entity).unwrap())
                .into_specific(self.ptr_type),
        ));

        let name = self.get_symbol(apply.function).unwrap().to_string();
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
            .map(|p| self.eval_element(*p, self.elements.get(*p).unwrap()))
            .collect::<Vec<_>>();

        let call = self.builder.ins().call(local_callee, &parameter_values);

        self.builder.inst_results(call)[0]
    }

    pub fn eval_capture(&mut self, entity: specs::Entity, _capture: &element::Capture) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_closure(&mut self, _entity: specs::Entity, _closure: &element::Closure) -> Value {
        unimplemented!()
    }

    pub fn eval_module(&mut self, _entity: specs::Entity, _module: &element::Module) -> Value {
        unimplemented!()
    }

    fn get_symbol(&self, entity: specs::Entity) -> Option<&symbol::Symbol> {
        if let Some(symbol) = self.symbols.get(entity) {
            Some(symbol)
        } else if let Some(element) = self.elements.get(entity) {
            match element {
                element::Element::Capture(element::Capture { captured, .. }) => {
                    self.get_symbol(*captured)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn builtin_alloc(&mut self, size: Value, align: Value) -> Value {
        let local_callee = self.declare_builtin(&builtin::ALLOC);

        let call = self.builder.ins().call(local_callee, &[size, align]);

        self.builder.inst_results(call)[0]
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
                builtin::ALLOC.symbol,
                cranelift_module::Linkage::Import,
                &signature,
            )
            .unwrap();

        self.module
            .declare_func_in_func(callee, &mut self.builder.func)
    }
}

impl<'a> fmt::Debug for DataTranslator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("DataTranslator").finish()
    }
}

impl<'a, 'f> fmt::Debug for FunctionTranslator<'a, 'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FunctionTranslator").finish()
    }
}
