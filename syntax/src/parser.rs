use std::iter;
use std::result;
use crate::ast;
use crate::error;
use crate::kind;
use crate::token;

pub struct Parser<I> where I: Iterator<Item=token::Token> {
    tokens: iter::Peekable<I>,
    builder: rowan::GreenNodeBuilder,
    errors: Vec<error::Error>,
}

pub enum Error {
    Expected(kind::Kind),
}

impl<I> Parser<I> where I: Iterator<Item=token::Token> {
    fn into_module(mut self) -> rowan::TreeArc<ast::Module> {
        use crate::ast::Node;

        self.module();

        let green = self.builder.finish();
        let node = rowan::SyntaxNode::new(green, None);
        ast::Module::from_rowan(&node).unwrap().to_owned()
    }

    fn module(&mut self) {
        self.builder.start_node(kind::Kind::Module.to_rowan_kind());

        while self.tokens.peek().is_some() {
            self.variable();
        }

        self.builder.finish_node();
    }

    fn variable(&mut self) {
        self.builder.start_node(kind::Kind::Variable.to_rowan_kind());
        self.identifier();
        self.expect_kind(kind::Kind::Is);
        self.skip_ws();

        self.builder.finish_node();
    }

    fn identifier(&mut self) {
        if self.expect_kind(kind::Kind::Identifier) {
            self.builder.start_node(kind::Kind::Identifier.to_rowan_kind());
            self.bump();
            self.builder.finish_node();
        }
    }

    fn expect_kind(&mut self, kind: kind::Kind) -> bool {
        if self.peek_kind() == Some(kind::Kind::Identifier) {
            true
        } else {
            self.error(Error::Expected(kind::Kind::Identifier));
            self.bump();
            false
        }
    }

    fn error(&mut self, error: Error) {}

    fn bump(&mut self) {
        let token = self.tokens.next().unwrap();
        self.builder.token(token.kind.to_rowan_kind(), token.contents);
    }

    fn peek(&mut self) -> Option<&token::Token> {
        self.tokens.peek()
    }

    fn peek_kind(&mut self) -> Option<kind::Kind> {
        self.peek().map(|t| t.kind)
    }

    fn skip_ws(&mut self) {
        while self.peek_kind() == Some(kind::Kind::Whitespace) {
            self.bump();
        }
    }
}
