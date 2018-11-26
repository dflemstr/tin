#![allow(missing_docs)]

use std::collections;
use std::fmt;

use specs;

use codegen;
use ir::component::element;

use cranelift::prelude::*;

pub struct Context<'a, 'f>
where
    'f: 'a,
{
    #[allow(unused)]
    codegen: &'a codegen::Codegen<'a>,
    builder: &'a mut FunctionBuilder<'f>,
    #[allow(unused)]
    ptr_type: Type,
    #[allow(unused)]
    variables: collections::HashMap<specs::Entity, String>,
}

#[allow(unused)]
impl<'a, 'f> Context<'a, 'f> {
    pub fn new(
        codegen: &'a codegen::Codegen<'a>,
        builder: &'a mut FunctionBuilder<'f>,
        ptr_type: Type,
    ) -> Self {
        let variables = collections::HashMap::new();

        Context {
            codegen,
            builder,
            ptr_type,
            variables,
        }
    }

    pub fn translate_element(&mut self, element: &element::Element) -> Value {
        match *element {
            element::Element::NumberValue(ref v) => self.translate_number_value(v),
            element::Element::StringValue(ref v) => self.translate_string_value(v),
            element::Element::Tuple(ref v) => self.translate_tuple(v),
            element::Element::Record(ref v) => self.translate_record(v),
            element::Element::Reference(ref v) => self.translate_reference(v),
            element::Element::Variable(ref v) => self.translate_variable(v),
            element::Element::Select(ref v) => self.translate_select(v),
            element::Element::Apply(ref v) => self.translate_apply(v),
            element::Element::Parameter(ref v) => self.translate_parameter(v),
            element::Element::Capture(ref v) => self.translate_capture(v),
            element::Element::Closure(ref v) => self.translate_closure(v),
            element::Element::Module(ref v) => self.translate_module(v),
        }
    }

    pub fn translate_number_value(&mut self, number_value: &element::NumberValue) -> Value {
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

    pub fn translate_string_value(&mut self, string_value: &element::StringValue) -> Value {
        unimplemented!()
    }

    pub fn translate_tuple(&mut self, tuple: &element::Tuple) -> Value {
        unimplemented!()
    }

    pub fn translate_record(&mut self, record: &element::Record) -> Value {
        unimplemented!()
    }

    pub fn translate_reference(&mut self, reference: &element::Reference) -> Value {
        unimplemented!()
    }

    pub fn translate_variable(&mut self, variable: &element::Variable) -> Value {
        unimplemented!()
    }

    pub fn translate_select(&mut self, select: &element::Select) -> Value {
        unimplemented!()
    }

    pub fn translate_parameter(&mut self, parameter: &element::Parameter) -> Value {
        unimplemented!()
    }

    pub fn translate_apply(&mut self, apply: &element::Apply) -> Value {
        unimplemented!()
    }

    pub fn translate_capture(&mut self, capture: &element::Capture) -> Value {
        unimplemented!()
    }

    pub fn translate_closure(&mut self, closure: &element::Closure) -> Value {
        unimplemented!()
    }

    pub fn translate_module(&mut self, module: &element::Module) -> Value {
        unimplemented!()
    }
}

impl<'a, 'f> fmt::Debug for Context<'a, 'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Context").finish()
    }
}
