use crate::ir;
use crate::ir::element;
use crate::ty;
use std::collections;
use std::fmt;
use std::sync;
use std::usize;

pub mod error;

#[salsa::query_group(LayoutStorage)]
pub trait Db: salsa::Database + ir::Db {
    #[salsa::input]
    fn ptr_size(&self) -> PtrSize;

    fn layout(&self, entity: ir::Entity) -> error::Result<sync::Arc<Layout>>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
    pub named_fields: Vec<NamedField>,
    pub unnamed_fields: Vec<Offset>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedField {
    pub field: ir::Ident,
    pub offset_layout: Offset,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Offset {
    pub offset: usize,
    pub layout: Layout,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PtrSize {
    Size8 = 8,
    Size16 = 16,
    Size32 = 32,
    Size64 = 64,
}

const BOOL_LAYOUT: Layout = Layout::scalar(1);

impl Layout {
    pub const fn zero() -> Layout {
        Layout {
            size: 0,
            alignment: 1,
            named_fields: Vec::new(),
            unnamed_fields: Vec::new(),
        }
    }

    pub const fn pointer(size: PtrSize) -> Layout {
        let size = size as usize;
        Layout {
            size,
            alignment: size,
            named_fields: Vec::new(),
            unnamed_fields: Vec::new(),
        }
    }

    pub const fn scalar(size: usize) -> Layout {
        Layout {
            size,
            alignment: size,
            named_fields: Vec::new(),
            unnamed_fields: Vec::new(),
        }
    }

    pub fn named_fields(size: usize, alignment: usize, named_fields: Vec<NamedField>) -> Layout {
        let unnamed_fields = Vec::new();

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }

    pub fn unnamed_fields(size: usize, alignment: usize, unnamed_fields: Vec<Offset>) -> Layout {
        let named_fields = Vec::new();

        Layout {
            size,
            alignment,
            named_fields,
            unnamed_fields,
        }
    }
}

impl Offset {
    pub const fn zero() -> Offset {
        Offset {
            offset: 0,
            layout: Layout::zero(),
        }
    }
}

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.size, self.alignment)?;

        if !self.named_fields.is_empty() {
            write!(f, "[")?;

            let mut needs_sep = false;
            for named_field in &self.named_fields {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{:?}:{}", named_field.field, named_field.offset_layout)?;
                needs_sep = true;
            }

            write!(f, "]")?;
        }

        if !self.unnamed_fields.is_empty() {
            write!(f, "{{")?;

            let mut needs_sep = false;
            for (idx, offset) in self.unnamed_fields.iter().enumerate() {
                if needs_sep {
                    write!(f, ",")?;
                }
                write!(f, "{}:{}", idx, offset)?;
                needs_sep = true;
            }

            write!(f, "}}")?;
        }

        Ok(())
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.offset, self.layout)
    }
}

fn layout(db: &impl Db, entity: ir::Entity) -> error::Result<sync::Arc<Layout>> {
    let element = db.element(entity)?;
    let layout = element_layout(db, &*element)?;
    Ok(layout)
}

fn element_layout(db: &impl Db, element: &element::Element) -> error::Result<sync::Arc<Layout>> {
    match *element {
        element::Element::Reference(entity) => element_layout(db, &*db.element(entity)?),
        element::Element::Number(ref n) => number_layout(db, n),
        element::Element::String(_) => Ok(sync::Arc::new(Layout::pointer(db.ptr_size()))),
        element::Element::Symbol(_) => Ok(sync::Arc::new(Layout::zero())),
        element::Element::Tuple(element::Tuple { ref fields }) => tuple_layout(db, fields),
        element::Element::Record(element::Record { ref fields }) => record_layout(db, fields),
        element::Element::UnOp(element::UnOp { operator, operand }) => {
            un_op_layout(db, operator, operand)
        }
        element::Element::BiOp(element::BiOp { lhs, operator, rhs }) => {
            bi_op_layout(db, lhs, operator, rhs)
        }
        element::Element::Variable(element::Variable { initializer, .. }) => {
            variable_layout(db, initializer)
        }
        element::Element::Select(element::Select { record, field }) => {
            select_layout(db, record, field)
        }
        element::Element::Apply(element::Apply {
            function,
            ref parameters,
        }) => apply_layout(db, function, parameters),
        element::Element::Parameter(element::Parameter { signature, .. }) => {
            parameter_layout(db, signature)
        }
        element::Element::Capture(element::Capture { captured, .. }) => {
            capture_layout(db, captured)
        }
        element::Element::Closure(element::Closure { ref captures, .. }) => {
            closure_layout(db, captures)
        }
        element::Element::Module(element::Module { ref variables }) => module_layout(db, variables),
    }
}

fn number_layout(db: &impl Db, number: &element::Number) -> error::Result<sync::Arc<Layout>> {
    match *number {
        element::Number::U8(_) | element::Number::I8(_) => Ok(sync::Arc::new(Layout::scalar(1))),
        element::Number::U16(_) | element::Number::I16(_) => Ok(sync::Arc::new(Layout::scalar(2))),
        element::Number::U32(_) | element::Number::I32(_) | element::Number::F32(_) => {
            Ok(sync::Arc::new(Layout::scalar(4)))
        }
        element::Number::U64(_) | element::Number::I64(_) | element::Number::F64(_) => {
            Ok(sync::Arc::new(Layout::scalar(8)))
        }
    }
}

fn tuple_layout(db: &impl Db, fields: &[ir::Entity]) -> error::Result<sync::Arc<Layout>> {
    let mut layouts = fields
        .iter()
        .enumerate()
        .map(|(i, f)| Ok((i, db.layout(*f)?)))
        .collect::<error::Result<Vec<_>>>()?;

    if layouts.is_empty() {
        Ok(sync::Arc::new(Layout::zero()))
    } else {
        layouts.sort_unstable_by_key(|(i, l)| (usize::max_value() - l.size, *i));
        let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
        let mut size = 0;

        let mut unnamed_fields = vec![Offset::zero(); layouts.len()];

        for (i, layout) in layouts {
            let offset = align_up(size, layout.alignment);
            size = offset + layout.size;
            let layout = (*layout).clone();
            unnamed_fields[i] = Offset { offset, layout };
        }

        Ok(sync::Arc::new(Layout::unnamed_fields(
            size,
            alignment,
            unnamed_fields,
        )))
    }
}

fn record_layout(
    db: &impl Db,
    fields: &collections::HashMap<ir::Ident, ir::Entity>,
) -> error::Result<sync::Arc<Layout>> {
    let mut layouts = fields
        .iter()
        .map(|(n, f)| Ok((*n, db.layout(*f)?)))
        .collect::<error::Result<Vec<_>>>()?;

    if layouts.is_empty() {
        Ok(sync::Arc::new(Layout::zero()))
    } else {
        layouts.sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, db.lookup_ident(*n)));
        let alignment = layouts.iter().map(|(_, l)| l.alignment).max().unwrap();
        let mut size = 0;

        let named_fields = layouts
            .into_iter()
            .map(|(field, layout)| {
                let offset = align_up(size, layout.alignment);
                size = offset + layout.size;
                let layout = (*layout).clone();
                let offset_layout = Offset { offset, layout };

                NamedField {
                    field,
                    offset_layout,
                }
            })
            .collect::<Vec<_>>();

        Ok(sync::Arc::new(Layout::named_fields(
            size,
            alignment,
            named_fields,
        )))
    }
}

fn un_op_layout(
    db: &impl Db,
    operator: element::UnOperator,
    operand: ir::Entity,
) -> error::Result<sync::Arc<Layout>> {
    match operator {
        element::UnOperator::Not => Ok(sync::Arc::new(BOOL_LAYOUT)),
        element::UnOperator::BNot
        | element::UnOperator::Cl0
        | element::UnOperator::Cl1
        | element::UnOperator::Cls
        | element::UnOperator::Ct0
        | element::UnOperator::Ct1
        | element::UnOperator::C0
        | element::UnOperator::C1
        | element::UnOperator::Sqrt => db.layout(operand),
    }
}

fn bi_op_layout(
    db: &impl Db,
    lhs: ir::Entity,
    operator: element::BiOperator,
    rhs: ir::Entity,
) -> error::Result<sync::Arc<Layout>> {
    let lhs = db.layout(lhs)?;
    let rhs = db.layout(rhs)?;

    if lhs.size != rhs.size || lhs.alignment != rhs.alignment {
        return Err(error::Error::UnknownSize);
    }

    match operator {
        element::BiOperator::Eq
        | element::BiOperator::Ne
        | element::BiOperator::Lt
        | element::BiOperator::Ge
        | element::BiOperator::Gt
        | element::BiOperator::Le
        | element::BiOperator::And
        | element::BiOperator::Or
        | element::BiOperator::Xor
        | element::BiOperator::AndNot
        | element::BiOperator::OrNot
        | element::BiOperator::XorNot => Ok(sync::Arc::new(BOOL_LAYOUT)),
        element::BiOperator::Cmp => unimplemented!(),
        element::BiOperator::Add
        | element::BiOperator::Sub
        | element::BiOperator::Mul
        | element::BiOperator::Div
        | element::BiOperator::Rem
        | element::BiOperator::BAnd
        | element::BiOperator::BOr
        | element::BiOperator::BXor
        | element::BiOperator::BAndNot
        | element::BiOperator::BOrNot
        | element::BiOperator::BXorNot
        | element::BiOperator::RotL
        | element::BiOperator::RotR
        | element::BiOperator::ShL
        | element::BiOperator::ShR => Ok(lhs),
    }
}

fn variable_layout(db: &impl Db, initializer: ir::Entity) -> error::Result<sync::Arc<Layout>> {
    db.layout(initializer)
}

fn select_layout(
    db: &impl Db,
    record: ir::Entity,
    field: ir::Ident,
) -> error::Result<sync::Arc<Layout>> {
    match *db.element(record)? {
        element::Element::Record(element::Record { ref fields }) => {
            if let Some(f) = fields.get(&field) {
                db.layout(*f)
            } else {
                Err(error::Error::UnknownSize)
            }
        }
        _ => Err(error::Error::UnknownSize),
    }
}

fn apply_layout(
    db: &impl Db,
    function: ir::Entity,
    _parameters: &[ir::Entity],
) -> error::Result<sync::Arc<Layout>> {
    match *db.element(function)? {
        element::Element::Closure(element::Closure { signature, .. }) => {
            element_layout(db, &*db.element(signature)?)
        }
        _ => Err(error::Error::UnknownSize),
    }
}

fn parameter_layout(db: &impl Db, signature: ir::Entity) -> error::Result<sync::Arc<Layout>> {
    db.layout(signature)
}

fn capture_layout(db: &impl Db, capture: ir::Entity) -> error::Result<sync::Arc<Layout>> {
    db.layout(capture)
}

fn closure_layout(
    db: &impl Db,
    captures: &collections::HashMap<ir::Ident, ir::Entity>,
) -> error::Result<sync::Arc<Layout>> {
    let mut capture_layouts = captures
        .iter()
        .map(|(n, f)| Ok((*n, db.layout(*f)?)))
        .collect::<error::Result<Vec<_>>>()?;
    let unnamed_fields = vec![Offset {
        offset: 0,
        layout: Layout::pointer(db.ptr_size()),
    }];

    capture_layouts
        .sort_unstable_by_key(|(n, l)| (usize::max_value() - l.size, db.lookup_ident(*n)));
    let alignment = capture_layouts
        .iter()
        .map(|(_, l)| l.alignment)
        .max()
        .unwrap_or(db.ptr_size() as usize);
    let mut size = db.ptr_size() as usize;

    let named_fields = capture_layouts
        .into_iter()
        .map(|(field, layout)| {
            let offset = align_up(size, layout.alignment);
            size = offset + layout.size;
            let layout = (*layout).clone();
            let offset_layout = Offset { offset, layout };

            NamedField {
                field,
                offset_layout,
            }
        })
        .collect::<Vec<_>>();

    Ok(sync::Arc::new(Layout {
        size,
        alignment,
        named_fields,
        unnamed_fields,
    }))
}

fn module_layout(
    db: &impl Db,
    variables: &collections::HashMap<ir::Ident, ir::Entity>,
) -> error::Result<sync::Arc<Layout>> {
    record_layout(db, variables)
}

#[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
fn align_up(offset: usize, alignment: usize) -> usize {
    debug_assert!(alignment.is_power_of_two());
    offset + ((-(offset as isize)) & (alignment as isize - 1)) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn align_up_4() {
        assert_eq!(0, align_up(0, 4));
        assert_eq!(4, align_up(1, 4));
        assert_eq!(4, align_up(2, 4));
        assert_eq!(4, align_up(3, 4));
        assert_eq!(4, align_up(4, 4));
        assert_eq!(8, align_up(5, 4));
        assert_eq!(8, align_up(6, 4));
        assert_eq!(8, align_up(7, 4));
        assert_eq!(8, align_up(8, 4));
        assert_eq!(12, align_up(9, 4));
    }

    #[test]
    fn align_up_8() {
        assert_eq!(0, align_up(0, 8));
        assert_eq!(8, align_up(1, 8));
        assert_eq!(8, align_up(2, 8));
        assert_eq!(8, align_up(3, 8));
        assert_eq!(8, align_up(4, 8));
        assert_eq!(8, align_up(5, 8));
        assert_eq!(8, align_up(6, 8));
        assert_eq!(8, align_up(7, 8));
        assert_eq!(8, align_up(8, 8));
        assert_eq!(16, align_up(9, 8));
    }
}
