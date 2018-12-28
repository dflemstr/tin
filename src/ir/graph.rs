//! Graph manipulation and rendering tools for the intermediate representation.
use std::borrow;
use std::fmt;

use dot;
use specs;

use ir;
use ir::component::element;
use ir::component::layout;
use ir::component::symbol;
use ir::component::ty;

/// A graph representation of IR.
pub struct Graph<'a> {
    entities: specs::Entities<'a>,
    elements: specs::ReadStorage<'a, element::Element>,
    layouts: specs::ReadStorage<'a, layout::Layout>,
    symbols: specs::ReadStorage<'a, symbol::Symbol>,
    types: specs::ReadStorage<'a, ty::Type>,
}

/// A node in the IR graph.
///
/// This is a simple mapping to an ECS entity for now.
pub type Node = specs::Entity;

/// An edge in the IR graph.
///
/// This is a labeled directed relationship between two ECS entities.
#[derive(Clone, Debug)]
pub struct Edge<'a> {
    source: Node,
    target: Node,
    label: Label<'a>,
}

#[derive(Clone, Debug)]
enum Label<'a> {
    RecordField(&'a str),
    TupleField(usize),
    VariableInitializer,
    SelectField(&'a str),
    AppliedFunction,
    AppliedParameter(usize),
    ParameterSignature,
    ClosureCaptureDefinition(&'a str),
    ClosureCaptureUsage(usize),
    ClosureParameter(usize),
    ClosureStatement(usize),
    ClosureSignature,
    ClosureResult,
    ModuleDefinition(&'a str),
    UnOperand,
    BiLhs,
    BiRhs,
}

struct PrettyTy<T>(T);

impl<'a> Graph<'a> {
    /// Creates a new IR graph based on the supplied intermediate representation.
    pub fn new(ir: &'a ir::Ir) -> Graph<'a> {
        let world = &ir.world;
        let entities = world.entities();
        let elements = world.read_storage();
        let layouts = world.read_storage();
        let symbols = world.read_storage();
        let types = world.read_storage();

        Graph {
            entities,
            elements,
            layouts,
            symbols,
            types,
        }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge<'a>> for Graph<'a> {
    fn nodes(&'a self) -> borrow::Cow<'a, [Node]> {
        use specs::Join;

        borrow::Cow::Owned(
            self.entities
                .join()
                .filter(|e| self.elements.contains(*e))
                .collect::<Vec<_>>(),
        )
    }

    fn edges(&'a self) -> borrow::Cow<'a, [Edge<'a>]> {
        use specs::Join;

        let mut edges = Vec::new();

        for entity in self.entities.join() {
            if let Some(element) = self.elements.get(entity) {
                match element {
                    element::Element::NumberValue(_) => {}
                    element::Element::StringValue(_) => {}
                    element::Element::Tuple(element::Tuple { fields }) => {
                        for (idx, field) in fields.iter().enumerate() {
                            edges.push(Edge {
                                source: entity,
                                target: *field,
                                label: Label::TupleField(idx),
                            });
                        }
                    }
                    element::Element::Record(element::Record { fields }) => {
                        for (name, field) in fields {
                            edges.push(Edge {
                                source: entity,
                                target: *field,
                                label: Label::RecordField(name),
                            });
                        }
                    }
                    element::Element::UnOp(element::UnOp {
                        operator: _,
                        operand,
                    }) => {
                        edges.push(Edge {
                            source: entity,
                            target: *operand,
                            label: Label::UnOperand,
                        });
                    }
                    element::Element::BiOp(element::BiOp {
                        lhs,
                        operator: _,
                        rhs,
                    }) => {
                        edges.push(Edge {
                            source: entity,
                            target: *lhs,
                            label: Label::BiLhs,
                        });
                        edges.push(Edge {
                            source: entity,
                            target: *rhs,
                            label: Label::BiRhs,
                        });
                    }
                    element::Element::Variable(element::Variable {
                        name: _,
                        initializer,
                    }) => edges.push(Edge {
                        source: entity,
                        target: *initializer,
                        label: Label::VariableInitializer,
                    }),
                    element::Element::Select(element::Select { record, field }) => {
                        edges.push(Edge {
                            source: entity,
                            target: *record,
                            label: Label::SelectField(field),
                        });
                    }
                    element::Element::Apply(element::Apply {
                        function,
                        parameters,
                    }) => {
                        edges.push(Edge {
                            source: entity,
                            target: *function,
                            label: Label::AppliedFunction,
                        });
                        for (idx, parameter) in parameters.iter().enumerate() {
                            edges.push(Edge {
                                source: entity,
                                target: *parameter,
                                label: Label::AppliedParameter(idx),
                            });
                        }
                    }
                    element::Element::Parameter(element::Parameter { name: _, signature }) => {
                        if let Some(signature) = signature {
                            edges.push(Edge {
                                source: entity,
                                target: *signature,
                                label: Label::ParameterSignature,
                            });
                        }
                    }
                    element::Element::Capture(element::Capture { ref name, captured }) => edges
                        .push(Edge {
                            source: entity,
                            target: *captured,
                            label: Label::ClosureCaptureDefinition(name),
                        }),
                    element::Element::Closure(element::Closure {
                        captures,
                        parameters,
                        statements,
                        signature,
                        result,
                    }) => {
                        for (idx, capture) in captures.iter().enumerate() {
                            edges.push(Edge {
                                source: entity,
                                target: *capture,
                                label: Label::ClosureCaptureUsage(idx),
                            });
                        }
                        for (idx, parameter) in parameters.iter().enumerate() {
                            edges.push(Edge {
                                source: entity,
                                target: *parameter,
                                label: Label::ClosureParameter(idx),
                            });
                        }
                        for (idx, statement) in statements.iter().enumerate() {
                            edges.push(Edge {
                                source: entity,
                                target: *statement,
                                label: Label::ClosureStatement(idx),
                            });
                        }
                        if let Some(signature) = signature {
                            edges.push(Edge {
                                source: entity,
                                target: *signature,
                                label: Label::ClosureSignature,
                            });
                        }
                        edges.push(Edge {
                            source: entity,
                            target: *result,
                            label: Label::ClosureResult,
                        });
                    }
                    element::Element::Module(element::Module { variables }) => {
                        for (name, variable) in variables {
                            edges.push(Edge {
                                source: entity,
                                target: *variable,
                                label: Label::ModuleDefinition(name),
                            });
                        }
                    }
                }
            }
        }

        borrow::Cow::Owned(edges)
    }

    fn source(&'a self, edge: &Edge) -> Node {
        edge.source
    }

    fn target(&'a self, edge: &Edge) -> Node {
        edge.target
    }
}

impl<'a> dot::Labeller<'a, Node, Edge<'a>> for Graph<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("ir").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("g{}_i{}", n.gen().id(), n.id())).unwrap()
    }

    fn node_shape(&'a self, _n: &Node) -> Option<dot::LabelText<'a>> {
        Some(dot::LabelText::LabelStr("record".into()))
    }

    fn node_label(&'a self, n: &Node) -> dot::LabelText<'a> {
        use std::fmt::Write;

        let mut result = format!("({}) ", n.id());

        if let Some(element) = self.elements.get(*n) {
            match element {
                element::Element::NumberValue(n) => write!(result, "num <b>{:?}</b>", n).unwrap(),
                element::Element::StringValue(element::StringValue(s)) => {
                    write!(result, "str <b>{:?}</b>", s).unwrap()
                }
                element::Element::Tuple(element::Tuple { fields }) => {
                    write!(result, "tuple <br/> <b>{:?}</b> fields", fields.len()).unwrap()
                }
                element::Element::Record(element::Record { fields }) => {
                    write!(result, "record <br/> <b>{:?}</b> fields", fields.len()).unwrap()
                }
                element::Element::UnOp(element::UnOp {
                    operator,
                    operand: _,
                }) => write!(result, "un op <b>{}</b>", operator).unwrap(),
                element::Element::BiOp(element::BiOp {
                    lhs: _,
                    operator,
                    rhs: _,
                }) => write!(result, "bi op <b>{}</b>", operator).unwrap(),
                element::Element::Variable(element::Variable {
                    name,
                    initializer: _,
                }) => write!(result, "variable <b>{:?}</b>", name).unwrap(),
                element::Element::Select(element::Select {
                    record: _,
                    field: _,
                }) => write!(result, "select").unwrap(),
                element::Element::Apply(element::Apply {
                    function: _,
                    parameters,
                }) => write!(result, "apply <br/> <b>{:?}</b> params", parameters.len()).unwrap(),
                element::Element::Parameter(element::Parameter { name, signature: _ }) => {
                    write!(result, "param <b>{:?}</b>", name).unwrap()
                }
                element::Element::Capture(element::Capture { name, captured: _ }) => {
                    write!(result, "capture <b>{:?}</b>", name).unwrap()
                }
                element::Element::Closure(element::Closure {
                    captures,
                    parameters,
                    statements: _,
                    signature: _,
                    result: _,
                }) => write!(
                    result,
                    "closure <br/> <b>{:?}</b> parameters <br/> <b>{:?}</b> captures",
                    parameters.len(),
                    captures.len()
                )
                .unwrap(),

                element::Element::Module(element::Module { variables }) => write!(
                    result,
                    "module <br/> <b>{:?}</b> variables",
                    variables.len()
                )
                .unwrap(),
            }
        } else {
            write!(result, "(unknown)").unwrap();
        };

        if let Some(ty) = self.types.get(*n) {
            write!(result, "<br/> <font color=\"blue\">{}</font>", PrettyTy(ty)).unwrap();
        }

        if let Some(layout) = self.layouts.get(*n) {
            write!(result, "<br/> <font color=\"brown\">{}</font>", layout).unwrap();
        }

        if let Some(symbol) = self.symbols.get(*n) {
            if symbol.is_empty() {
                write!(result, "<br/> <font color=\"purple\">(root)</font>").unwrap();
            } else {
                write!(result, "<br/> <font color=\"purple\">{}</font>", symbol).unwrap();
            }
        }

        dot::LabelText::HtmlStr(result.into())
    }

    fn edge_label(&'a self, e: &Edge<'a>) -> dot::LabelText<'a> {
        match e.label {
            Label::RecordField(ref name) => {
                dot::LabelText::HtmlStr(format!("field <b>{}</b>", name).into())
            }
            Label::TupleField(idx) => {
                dot::LabelText::HtmlStr(format!("field <b>{}</b>", idx).into())
            }
            Label::VariableInitializer => dot::LabelText::LabelStr("initializer".into()),
            Label::SelectField(ref name) => {
                dot::LabelText::HtmlStr(format!("select <b>{}</b>", name).into())
            }
            Label::AppliedFunction => dot::LabelText::LabelStr("func".into()),
            Label::AppliedParameter(idx) => {
                dot::LabelText::HtmlStr(format!("param <b>{}</b>", idx).into())
            }
            Label::ParameterSignature => dot::LabelText::LabelStr("sig".into()),
            Label::ClosureCaptureDefinition(ref name) => {
                dot::LabelText::HtmlStr(format!("capture definition <b>{}</b>", name).into())
            }
            Label::ClosureCaptureUsage(idx) => {
                dot::LabelText::HtmlStr(format!("capture usage <b>{}</b>", idx).into())
            }
            Label::ClosureParameter(idx) => {
                dot::LabelText::HtmlStr(format!("param <b>{}</b>", idx).into())
            }
            Label::ClosureStatement(idx) => {
                dot::LabelText::HtmlStr(format!("stmt <b>{}</b>", idx).into())
            }
            Label::ClosureResult => dot::LabelText::HtmlStr("result".into()),
            Label::ClosureSignature => dot::LabelText::LabelStr("sig".into()),
            Label::ModuleDefinition(ref name) => {
                dot::LabelText::HtmlStr(format!("def <b>{}</b>", name).into())
            }
            Label::UnOperand => dot::LabelText::LabelStr("operand".into()),
            Label::BiLhs => dot::LabelText::LabelStr("lhs".into()),
            Label::BiRhs => dot::LabelText::LabelStr("rhs".into()),
        }
    }

    fn edge_style(&'a self, e: &Edge<'a>) -> dot::Style {
        match e.label {
            Label::RecordField(_) => dot::Style::None,
            Label::TupleField(_) => dot::Style::None,
            Label::VariableInitializer => dot::Style::None,
            Label::SelectField(_) => dot::Style::None,
            Label::AppliedFunction => dot::Style::None,
            Label::AppliedParameter(_) => dot::Style::None,
            Label::ParameterSignature => dot::Style::Dotted,
            Label::ClosureCaptureDefinition(_) => dot::Style::Dashed,
            Label::ClosureCaptureUsage(_) => dot::Style::None,
            Label::ClosureParameter(_) => dot::Style::None,
            Label::ClosureStatement(_) => dot::Style::Dashed,
            Label::ClosureResult => dot::Style::None,
            Label::ClosureSignature => dot::Style::Dotted,
            Label::ModuleDefinition(_) => dot::Style::None,
            Label::UnOperand => dot::Style::None,
            Label::BiLhs => dot::Style::None,
            Label::BiRhs => dot::Style::None,
        }
    }
}

impl<'a> fmt::Debug for Graph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Graph").finish()
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Type> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            ty::Type::Boolean => write!(f, "bool"),
            ty::Type::Number(ref number) => PrettyTy(number).fmt(f),
            ty::Type::String => write!(f, "str"),
            ty::Type::Tuple(ref tuple) => PrettyTy(tuple).fmt(f),
            ty::Type::Record(ref record) => PrettyTy(record).fmt(f),
            ty::Type::Function(ref function) => PrettyTy(function).fmt(f),
            ty::Type::Conflict(ref conflict) => PrettyTy(conflict).fmt(f),
            ty::Type::Any => write!(f, "any"),
        }
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Number> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            ty::Number::U8 => write!(f, "u8"),
            ty::Number::U16 => write!(f, "u16"),
            ty::Number::U32 => write!(f, "u32"),
            ty::Number::U64 => write!(f, "u64"),
            ty::Number::I8 => write!(f, "i8"),
            ty::Number::I16 => write!(f, "i16"),
            ty::Number::I32 => write!(f, "i32"),
            ty::Number::I64 => write!(f, "i64"),
            ty::Number::F32 => write!(f, "f32"),
            ty::Number::F64 => write!(f, "f64"),
        }
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Tuple> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut needs_sep = false;
        for ty in &self.0.fields {
            if needs_sep {
                write!(f, ",")?;
            }
            PrettyTy(ty).fmt(f)?;
            needs_sep = true;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Record> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\{{")?;
        let mut needs_sep = false;
        for (id, ty) in &self.0.fields {
            if needs_sep {
                write!(f, ",")?;
            }
            write!(f, "{}:", id)?;
            PrettyTy(ty).fmt(f)?;
            needs_sep = true;
        }
        write!(f, "\\}}")?;
        Ok(())
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Function> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\|")?;
        let mut needs_sep = false;
        for ty in &self.0.parameters {
            if needs_sep {
                write!(f, ",")?;
            }
            PrettyTy(ty).fmt(f)?;
            needs_sep = true;
        }
        write!(f, "\\|:")?;
        PrettyTy(&*self.0.result).fmt(f)?;
        Ok(())
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::Conflict> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        PrettyTy(&self.0.expected).fmt(f)?;
        write!(f, "!=")?;
        PrettyTy(&*self.0.actual).fmt(f)?;
        Ok(())
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::ExpectedType> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ty::ExpectedType::Specific(ref ty) => PrettyTy(&**ty).fmt(f),
            ty::ExpectedType::ScalarClass(ref class) => PrettyTy(class).fmt(f),
        }
    }
}

impl<'a> fmt::Display for PrettyTy<&'a ty::ScalarClass> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ty::ScalarClass::Boolean => f.write_str("(any bool type)"),
            ty::ScalarClass::Integral(ty::IntegralScalarClass::Unsigned) => {
                f.write_str("(any unsigned integer type)")
            }
            ty::ScalarClass::Integral(ty::IntegralScalarClass::Signed) => {
                f.write_str("(any signed integer type)")
            }
            ty::ScalarClass::Integral(ty::IntegralScalarClass::Any) => {
                f.write_str("(any integer type)")
            }
            ty::ScalarClass::Fractional => f.write_str("(any floating point type)"),
            ty::ScalarClass::Complex => f.write_str("(any complex type)"),
            ty::ScalarClass::Undefined => f.write_str("(any non-scalar type)"),
        }
    }
}
