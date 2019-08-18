use std::fmt;

use cranelift::prelude::*;

use crate::layout;
use crate::value;

pub struct Translator<'a> {
    storage: &'a mut Vec<u8>,
    ptr_type: Type,
}

impl<'a> Translator<'a> {
    pub fn new(storage: &'a mut Vec<u8>, ptr_type: Type) -> Self {
        Translator { storage, ptr_type }
    }

    pub fn store_value(&mut self, layout: &layout::Layout, value: &value::Value) {
        self.storage.reserve(layout.size);
        match value.case() {
            value::Case::Number(v) => self.store_number(layout, *v),
            value::Case::String(v) => self.store_string(layout, v),
            value::Case::Symbol(_) => {} // TODO type dependent
            value::Case::Tuple(v) => self.store_tuple(layout, v),
            value::Case::Record(v) => self.store_record(layout, v),
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
                .write_f32::<byteorder::NativeEndian>(v.into_inner())
                .unwrap(),
            value::Number::F64(v) => self
                .storage
                .write_f64::<byteorder::NativeEndian>(v.into_inner())
                .unwrap(),
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cast_possible_truncation))]
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

        let unnamed_fields = &layout.unnamed_fields;
        let mut pos = 0;
        assert_eq!(tuple.fields.len(), unnamed_fields.len());

        for (value, offset_layout) in tuple.fields.iter().zip(unnamed_fields.iter()) {
            assert!(offset_layout.offset >= pos);
            while pos < offset_layout.offset {
                self.storage.write_all(&[0_u8]).unwrap();
                pos += 1;
            }
            self.store_value(&offset_layout.layout, value);
            pos += offset_layout.layout.size;
        }
    }

    pub fn store_record(&mut self, layout: &layout::Layout, record: &value::Record) {
        use std::io::Write;

        let named_fields = &layout.named_fields;
        let mut pos = 0;
        assert_eq!(record.fields.len(), named_fields.len());

        for ((_, value), named_field) in record.fields.iter().zip(named_fields.iter()) {
            let offset_layout = &named_field.offset_layout;
            assert!(offset_layout.offset >= pos);
            while pos < offset_layout.offset {
                self.storage.write_all(&[0_u8]).unwrap();
                pos += 1;
            }
            self.store_value(&offset_layout.layout, value);
            pos += offset_layout.layout.size;
        }
    }
}

impl<'a> fmt::Debug for Translator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Translator").finish()
    }
}
