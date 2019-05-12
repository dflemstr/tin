//! Graph rendering tools for the internal representation of Tin code.
//!
//! Use this module to diagnose errors encountered in a piece of code.  The graph representation
//! aims to provide all of the information available to the Tin compiler.
use std::borrow;
use std::fmt;

use dot;

use crate::db;
use crate::ir;
use crate::ir::element;

/// A graph representation of IR.
pub struct Graph<'a> {
    db: &'a db::Db,
}

/// A node in the IR graph.
#[derive(Clone, Copy, Debug)]
pub struct Node(ir::Entity);

/// An edge in the IR graph.
#[derive(Clone, Copy, Debug)]
pub struct Edge {
    source: Node,
    target: Node,
    role: ir::EntityRole,
}

impl<'a> Graph<'a> {
    /// Creates a new IR graph based on the supplied intermediate representation.
    pub(crate) fn new(db: &'a db::Db) -> Graph<'a> {
        Graph { db }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge> for Graph<'a> {
    fn nodes(&'a self) -> borrow::Cow<'a, [Node]> {
        use crate::ir::db::IrDb;

        let entities = self.db.entities().unwrap();
        borrow::Cow::Owned(entities.infos.keys().cloned().map(Node).collect())
    }

    fn edges(&'a self) -> borrow::Cow<'a, [Edge]> {
        use crate::ir::db::IrDb;

        let mut edges = Vec::new();

        let entities = self.db.entities().unwrap();
        for entity in entities.infos.keys() {
            let (parent, role) = self.db.lookup_entity(*entity);
            if let Some(parent) = parent {
                let source = Node(parent);
                let target = Node(*entity);
                edges.push(Edge {
                    source,
                    target,
                    role,
                });
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

impl<'a> dot::Labeller<'a, Node, Edge> for Graph<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("ir").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("n{}", n.0.id())).unwrap()
    }

    fn node_shape(&'a self, _n: &Node) -> Option<dot::LabelText<'a>> {
        Some(dot::LabelText::LabelStr("record".into()))
    }

    fn node_label(&'a self, n: &Node) -> dot::LabelText<'a> {
        use crate::ir::db::IrDb;
        use crate::layout::db::LayoutDb;
        use crate::ty::db::TyDb;
        use std::fmt::Write;

        let mut result = format!("({}) ", n.0.id());

        if let Ok(element) = self.db.entity_element(n.0) {
            match &*element {
                element::Element::Reference(e) => write!(result, "ref <b>{:?}</b>", e).unwrap(),
                element::Element::Number(n) => write!(result, "num <b>{:?}</b>", n).unwrap(),
                element::Element::String(s) => write!(result, "str <b>{:?}</b>", s).unwrap(),
                element::Element::Symbol(element::Symbol { label }) => {
                    write!(result, "sym <b>{:?}</b>", self.db.lookup_ident(*label)).unwrap()
                }
                element::Element::Tuple(element::Tuple { fields }) => {
                    write!(result, "tuple <br/> <b>{:?}</b> fields", fields.len()).unwrap()
                }
                element::Element::Record(element::Record { fields }) => {
                    write!(result, "record <br/> <b>{:?}</b> fields", fields.len()).unwrap()
                }
                element::Element::UnOp(element::UnOp { operator, .. }) => {
                    write!(result, "un op <b>{}</b>", operator).unwrap()
                }
                element::Element::BiOp(element::BiOp { operator, .. }) => {
                    write!(result, "bi op <b>{}</b>", operator).unwrap()
                }
                element::Element::Variable(element::Variable { name, .. }) => {
                    write!(result, "variable <b>{:?}</b>", self.db.lookup_ident(*name)).unwrap()
                }
                element::Element::Select(element::Select { .. }) => {
                    write!(result, "select").unwrap()
                }
                element::Element::Apply(element::Apply { parameters, .. }) => {
                    write!(result, "apply <br/> <b>{:?}</b> params", parameters.len()).unwrap()
                }
                element::Element::Parameter(element::Parameter { name, .. }) => {
                    write!(result, "param <b>{:?}</b>", self.db.lookup_ident(*name)).unwrap()
                }
                element::Element::Capture(element::Capture { name, .. }) => {
                    write!(result, "capture <b>{:?}</b>", self.db.lookup_ident(*name)).unwrap()
                }
                element::Element::Closure(element::Closure {
                    captures,
                    parameters,
                    ..
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

        if let Ok(ty) = self.db.entity_type(n.0) {
            write!(result, "<br/> <font color=\"blue\">{}</font>", ty).unwrap();
        }

        if let Ok(layout) = self.db.entity_layout(n.0) {
            write!(result, "<br/> <font color=\"brown\">{}</font>", layout).unwrap();
        }

        dot::LabelText::HtmlStr(result.into())
    }

    fn edge_label(&'a self, e: &Edge) -> dot::LabelText<'a> {
        use crate::ir::db::IrDb;
        use crate::source::db::SourceDb;

        match e.role {
            ir::EntityRole::File(file_id) => dot::LabelText::HtmlStr(
                format!("file <b>{:?}</b>", self.db.file_relative_path(file_id)).into(),
            ),
            ir::EntityRole::Reference(ident) => dot::LabelText::HtmlStr(
                format!("ref <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::RecordField(ident) => dot::LabelText::HtmlStr(
                format!("field <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::TupleField(idx) => {
                dot::LabelText::HtmlStr(format!("field <b>{:?}</b>", idx).into())
            }
            ir::EntityRole::VariableDefinition(ident) => dot::LabelText::HtmlStr(
                format!("def <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::VariableInitializer => dot::LabelText::HtmlStr("init".into()),
            ir::EntityRole::SelectField(ident) => dot::LabelText::HtmlStr(
                format!("select <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::AppliedFunction => dot::LabelText::HtmlStr("fun".into()),
            ir::EntityRole::AppliedParameter(idx) => {
                dot::LabelText::HtmlStr(format!("param <b>{:?}</b>", idx).into())
            }
            ir::EntityRole::ParameterSignature => dot::LabelText::HtmlStr("sig".into()),
            ir::EntityRole::ClosureCaptureDefinition(ident) => dot::LabelText::HtmlStr(
                format!("capture <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::ClosureParameter(ident) => dot::LabelText::HtmlStr(
                format!("param <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::ClosureStatement(idx) => {
                dot::LabelText::HtmlStr(format!("stmt <b>{:?}</b>", idx).into())
            }
            ir::EntityRole::ClosureSignature => dot::LabelText::HtmlStr("sig".into()),
            ir::EntityRole::ClosureResult => dot::LabelText::HtmlStr("result".into()),
            ir::EntityRole::ModuleDefinition(ident) => dot::LabelText::HtmlStr(
                format!("mod <b>{:?}</b>", self.db.lookup_ident(ident)).into(),
            ),
            ir::EntityRole::UnOperand => dot::LabelText::HtmlStr("op".into()),
            ir::EntityRole::BiLhs => dot::LabelText::HtmlStr("lhs".into()),
            ir::EntityRole::BiRhs => dot::LabelText::HtmlStr("rhs".into()),
        }
    }

    fn edge_style(&'a self, e: &Edge) -> dot::Style {
        match e.role {
            ir::EntityRole::ParameterSignature | ir::EntityRole::ClosureSignature => {
                dot::Style::Dotted
            }
            ir::EntityRole::ClosureCaptureDefinition(_) | ir::EntityRole::ClosureStatement(_) => {
                dot::Style::Dashed
            }
            _ => dot::Style::None,
        }
    }
}

impl<'a> fmt::Debug for Graph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Graph").finish()
    }
}
