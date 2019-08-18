use crate::kind;

pub trait Node: rowan::TransparentNewType<Repr=rowan::SyntaxNode> {
    fn from_rowan(node: &rowan::SyntaxNode) -> Option<&Self>;

    fn to_owned(&self) -> rowan::TreeArc<Self>;
}

macro_rules! ast_node {
    ($ast:ident) => { ast_node!($ast, $ast); };
    ($ast:ident, $kind:ident) => {
        #[derive(Debug, Eq, Hash, PartialEq)]
        #[repr(transparent)]
        pub struct $ast(rowan::SyntaxNode);

        unsafe impl rowan::TransparentNewType for $ast {
            type Repr = rowan::SyntaxNode;
        }

        impl Node for $ast {
            fn from_rowan(node: &rowan::SyntaxNode) -> Option<&Self> {
                use rowan::TransparentNewType;

                if node.kind() == kind::Kind::$kind.to_rowan_kind() {
                    Some(Self::from_repr(node))
                } else {
                    None
                }
            }

            fn to_owned(&self) -> rowan::TreeArc<Self> {
                rowan::TreeArc::cast(self.0.to_owned())
            }
        }

        impl kind::HasKind for $ast {
            fn kind(&self) -> kind::Kind {
                kind::Kind::$kind
            }
        }
    };
}

ast_node!(Reference);
ast_node!(Module);
ast_node!(Expression);
ast_node!(Statement);
ast_node!(Number);
ast_node!(String);
ast_node!(Symbol);
ast_node!(Tuple);
ast_node!(Record);
ast_node!(UnOp);
ast_node!(BiOp);
ast_node!(Lambda);
ast_node!(Parameter);
ast_node!(Apply);
