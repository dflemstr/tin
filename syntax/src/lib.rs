mod ast;
mod error;
mod kind;
mod parser;
mod token;

fn parse_module(text: &str) -> rowan::TreeArc<ast::Module> {
    unimplemented!()
}
