//! Graph manipulation and rendering tools for the intermediate representation.
use std::borrow;
use std::fmt;

use dot;
use specs;

use ir;
use ir::component::element;
use ir::component::ty;

/// A graph representation of IR.
pub struct Graph<'a> {
    entities: specs::Entities<'a>,
    elements: specs::ReadStorage<'a, element::Element>,
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
    SelectField(&'a str),
    AppliedFunction,
    AppliedParameter(usize),
    ParameterSignature,
    ClosureCapture(&'a str),
    ClosureParameter(usize),
    ClosureStatement(usize),
    ClosureSignature,
    ModuleDefinition(&'a str),
}

struct PrettyTy<T>(T);

impl<'a> Graph<'a> {
    /// Creates a new IR graph based on the supplied intermediate representation.
    pub fn new(ir: &'a ir::Ir) -> Graph<'a> {
        let world = &ir.world;
        let entities = world.entities();
        let elements = world.read_storage::<element::Element>();
        let types = world.read_storage::<ty::Type>();

        Graph {
            entities,
            elements,
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

        let mut result = Vec::new();
        let elements = &self.elements;

        for entity in self.entities.join() {
            if let Some(element) = elements.get(entity) {
                match element {
                    element::Element::NumberValue(_) => {}
                    element::Element::StringValue(_) => {}
                    element::Element::Tuple(element::Tuple { fields }) => {
                        for (idx, field) in fields.iter().enumerate() {
                            result.push(Edge {
                                source: entity,
                                target: *field,
                                label: Label::TupleField(idx),
                            });
                        }
                    }
                    element::Element::Record(element::Record { fields }) => {
                        for (name, field) in fields {
                            result.push(Edge {
                                source: entity,
                                target: *field,
                                label: Label::RecordField(name),
                            });
                        }
                    }
                    element::Element::Reference(_) => {}
                    element::Element::Select(element::Select { record, field }) => {
                        result.push(Edge {
                            source: entity,
                            target: *record,
                            label: Label::SelectField(field),
                        });
                    }
                    element::Element::Apply(element::Apply {
                        function,
                        parameters,
                    }) => {
                        result.push(Edge {
                            source: entity,
                            target: *function,
                            label: Label::AppliedFunction,
                        });
                        for (idx, parameter) in parameters.iter().enumerate() {
                            result.push(Edge {
                                source: entity,
                                target: *parameter,
                                label: Label::AppliedParameter(idx),
                            });
                        }
                    }
                    element::Element::Parameter(element::Parameter { name: _, signature }) => {
                        if let Some(signature) = signature {
                            result.push(Edge {
                                source: entity,
                                target: *signature,
                                label: Label::ParameterSignature,
                            });
                        }
                    }
                    element::Element::Capture(element::Capture { ref name, captured }) => result
                        .push(Edge {
                            source: entity,
                            target: *captured,
                            label: Label::ClosureCapture(name),
                        }),
                    element::Element::Closure(element::Closure {
                        captures,
                        parameters,
                        statements,
                        signature,
                    }) => {
                        for (name, capture) in captures {
                            result.push(Edge {
                                source: entity,
                                target: *capture,
                                label: Label::ClosureCapture(name),
                            });
                        }
                        for (idx, parameter) in parameters.iter().enumerate() {
                            result.push(Edge {
                                source: entity,
                                target: *parameter,
                                label: Label::ClosureParameter(idx),
                            });
                        }
                        for (idx, statement) in statements.iter().enumerate() {
                            result.push(Edge {
                                source: entity,
                                target: *statement,
                                label: Label::ClosureStatement(idx),
                            });
                        }
                        if let Some(signature) = signature {
                            result.push(Edge {
                                source: entity,
                                target: *signature,
                                label: Label::ClosureSignature,
                            });
                        }
                    }
                    element::Element::Module(element::Module { definitions }) => {
                        for (name, definition) in definitions {
                            result.push(Edge {
                                source: entity,
                                target: *definition,
                                label: Label::ModuleDefinition(name),
                            });
                        }
                    }
                }
            }
        }

        borrow::Cow::Owned(result)
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
                element::Element::Reference(element::Reference(v)) => {
                    write!(result, "reference <br/> to <b>{:?}</b>", v).unwrap()
                }
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
                }) => write!(
                    result,
                    "closure <br/> <b>{:?}</b> parameters <br/> <b>{:?}</b> captures",
                    parameters.len(),
                    captures.len()
                ).unwrap(),

                element::Element::Module(element::Module { definitions }) => write!(
                    result,
                    "module <br/> <b>{:?}</b> definitions",
                    definitions.len()
                ).unwrap(),
            }
        } else {
            write!(result, "(unknown)").unwrap();
        };

        if let Some(ty) = self.types.get(*n) {
            write!(result, "<br/> <font color=\"blue\">{}</font>", PrettyTy(ty)).unwrap();
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
            Label::SelectField(ref name) => {
                dot::LabelText::HtmlStr(format!("select <b>{}</b>", name).into())
            }
            Label::AppliedFunction => dot::LabelText::LabelStr("func".into()),
            Label::AppliedParameter(idx) => {
                dot::LabelText::HtmlStr(format!("param <b>{}</b>", idx).into())
            }
            Label::ParameterSignature => dot::LabelText::LabelStr("sig".into()),
            Label::ClosureCapture(ref name) => {
                dot::LabelText::HtmlStr(format!("capture <b>{}</b>", name).into())
            }
            Label::ClosureParameter(idx) => {
                dot::LabelText::HtmlStr(format!("param <b>{}</b>", idx).into())
            }
            Label::ClosureStatement(idx) => {
                dot::LabelText::HtmlStr(format!("stmt <b>{}</b>", idx).into())
            }
            Label::ClosureSignature => dot::LabelText::LabelStr("sig".into()),
            Label::ModuleDefinition(ref name) => {
                dot::LabelText::HtmlStr(format!("def <b>{}</b>", name).into())
            }
        }
    }

    fn edge_style(&'a self, e: &Edge<'a>) -> dot::Style {
        match e.label {
            Label::RecordField(_) => dot::Style::None,
            Label::TupleField(_) => dot::Style::None,
            Label::SelectField(_) => dot::Style::None,
            Label::AppliedFunction => dot::Style::None,
            Label::AppliedParameter(_) => dot::Style::None,
            Label::ParameterSignature => dot::Style::Dotted,
            Label::ClosureCapture(_) => dot::Style::Dashed,
            Label::ClosureParameter(_) => dot::Style::None,
            Label::ClosureStatement(_) => dot::Style::Dashed,
            Label::ClosureSignature => dot::Style::Dotted,
            Label::ModuleDefinition(_) => dot::Style::None,
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
        PrettyTy(&*self.0.expected).fmt(f)?;
        write!(f, "!=")?;
        PrettyTy(&*self.0.actual).fmt(f)?;
        Ok(())
    }
}
