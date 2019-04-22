use crate::ast;
use crate::error;
use crate::kind;

pub struct Parser<I> {
    tokens: I,
    builder: rowan::GreenNodeBuilder,
    errors: Vec<error::Error>,
}

impl<I> Parser<I> where I: Iterator<Item=(kind::Kind, rowan::SmolStr)> {
    fn parse(mut self) -> rowan::TreeArc<ast::Module> {
        use crate::ast::Node;

        self.builder.start_node(kind::Kind::Module.to_rowan_kind());

        loop {

        }

        self.skip_ws();
        self.builder.finish_node();

        let green = self.builder.finish();
        let node = rowan::SyntaxNode::new(green, None);
        ast::Module::from_rowan(&node).unwrap().to_owned()
    }

    fn skip_ws(&mut self) {

    }
}
