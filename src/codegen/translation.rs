#![allow(missing_docs)]

use std::collections;
use std::fmt;

use specs;

use cranelift::prelude::*;
use cranelift_module;
use cranelift_simplejit;

use codegen::abi_type;
use ir::component::element;
use ir::component::symbol;
use ir::component::ty;

pub struct FunctionTranslator<'a, 'f>
where
    'f: 'a,
{
    module: &'a mut cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    builder: &'a mut FunctionBuilder<'f>,
    elements: &'a specs::ReadStorage<'a, element::Element>,
    symbols: &'a specs::ReadStorage<'a, symbol::Symbol>,
    types: &'a specs::ReadStorage<'a, ty::Type>,
    ptr_type: Type,
    variables: collections::HashMap<specs::Entity, Variable>,
}

#[allow(unused)]
impl<'a, 'f> FunctionTranslator<'a, 'f> {
    pub fn new(
        module: &'a mut cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
        builder: &'a mut FunctionBuilder<'f>,
        elements: &'a specs::ReadStorage<'a, element::Element>,
        symbols: &'a specs::ReadStorage<'a, symbol::Symbol>,
        types: &'a specs::ReadStorage<'a, ty::Type>,
        ptr_type: Type,
        variables: collections::HashMap<specs::Entity, Variable>,
    ) -> Self {
        FunctionTranslator {
            module,
            builder,
            elements,
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
        match *element {
            element::Element::NumberValue(ref v) => self.eval_number_value(entity, v),
            element::Element::StringValue(ref v) => self.eval_string_value(entity, v),
            element::Element::Tuple(ref v) => self.eval_tuple(entity, v),
            element::Element::Record(ref v) => self.eval_record(entity, v),
            element::Element::Reference(ref v) => self.eval_reference(entity, v),
            element::Element::Variable(ref v) => self.eval_variable(entity, v),
            element::Element::Select(ref v) => self.eval_select(entity, v),
            element::Element::Apply(ref v) => self.eval_apply(entity, v),
            element::Element::Parameter(ref v) => self.eval_parameter(entity, v),
            element::Element::Capture(ref v) => self.eval_capture(entity, v),
            element::Element::Closure(ref v) => self.eval_closure(entity, v),
            element::Element::Module(ref v) => self.eval_module(entity, v),
        }
    }

    pub fn eval_number_value(
        &mut self,
        entity: specs::Entity,
        number_value: &element::NumberValue,
    ) -> Value {
        match *number_value {
            element::NumberValue::U8(v) => self.builder.ins().iconst(types::I8, v as i64),
            element::NumberValue::U16(v) => self.builder.ins().iconst(types::I16, v as i64),
            element::NumberValue::U32(v) => self.builder.ins().iconst(types::I32, v as i64),
            element::NumberValue::U64(v) => self.builder.ins().iconst(types::I64, v as i64),
            element::NumberValue::I8(v) => self.builder.ins().iconst(types::I8, v as i64),
            element::NumberValue::I16(v) => self.builder.ins().iconst(types::I16, v as i64),
            element::NumberValue::I32(v) => self.builder.ins().iconst(types::I32, v as i64),
            element::NumberValue::I64(v) => self.builder.ins().iconst(types::I64, v),
            element::NumberValue::F32(v) => self.builder.ins().f32const(Ieee32::with_float(v)),
            element::NumberValue::F64(v) => self.builder.ins().f64const(Ieee64::with_float(v)),
        }
    }

    pub fn eval_string_value(
        &mut self,
        entity: specs::Entity,
        string_value: &element::StringValue,
    ) -> Value {
        unimplemented!()
    }

    pub fn eval_tuple(&mut self, entity: specs::Entity, tuple: &element::Tuple) -> Value {
        unimplemented!()
    }

    pub fn eval_record(&mut self, entity: specs::Entity, record: &element::Record) -> Value {
        unimplemented!()
    }

    pub fn eval_reference(
        &mut self,
        entity: specs::Entity,
        reference: &element::Reference,
    ) -> Value {
        unimplemented!()
    }

    pub fn eval_variable(&mut self, entity: specs::Entity, variable: &element::Variable) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_select(&mut self, entity: specs::Entity, select: &element::Select) -> Value {
        unimplemented!()
    }

    pub fn eval_parameter(
        &mut self,
        entity: specs::Entity,
        parameter: &element::Parameter,
    ) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_apply(&mut self, entity: specs::Entity, apply: &element::Apply) -> Value {
        let mut sig = self.module.make_signature();

        for parameter in &apply.parameters {
            sig.params.push(AbiParam::new(abi_type::from_type(
                self.types.get(*parameter).unwrap(),
                self.ptr_type,
            )));
        }
        sig.returns.push(AbiParam::new(abi_type::from_type(
            self.types.get(entity).unwrap(),
            self.ptr_type,
        )));

        let callee = self
            .module
            .declare_function(
                &self.symbols.get(apply.function).unwrap().to_string(),
                cranelift_module::Linkage::Import,
                &sig,
            ).unwrap();
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut parameter_values = apply
            .parameters
            .iter()
            .map(|p| self.eval_element(*p, self.elements.get(*p).unwrap()))
            .collect::<Vec<_>>();

        let call = self.builder.ins().call(local_callee, &parameter_values);

        self.builder.inst_results(call)[0]
    }

    pub fn eval_capture(&mut self, entity: specs::Entity, capture: &element::Capture) -> Value {
        self.builder.use_var(self.variables[&entity])
    }

    pub fn eval_closure(&mut self, entity: specs::Entity, closure: &element::Closure) -> Value {
        unimplemented!()
    }

    pub fn eval_module(&mut self, entity: specs::Entity, module: &element::Module) -> Value {
        unimplemented!()
    }
}

impl<'a, 'f> fmt::Debug for FunctionTranslator<'a, 'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FunctionTranslator").finish()
    }
}
