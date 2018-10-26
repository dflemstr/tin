#![allow(warnings, unused)]
use std::collections;
use std::fmt;

use bimap;

use ast;

pub trait IrNode: Sized {}

#[derive(Clone, Copy, Debug)]
struct AstContext {
    id: Id,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Id(u64);

#[derive(Debug)]
pub struct Module {
    pub next_id: u64,
    pub symbols: bimap::BiMap<Id, Symbol>,
    pub types: collections::HashMap<Id, Type>,
    pub kinds: collections::HashMap<Id, Kind>,
    pub consts: collections::HashMap<Id, Const>,
    pub functions: collections::HashMap<Id, Function>,
}

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Symbol {
    parts: Vec<SymbolPart>,
}

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SymbolPart {
    Name(String),
    Scope(u64),
}

#[derive(Debug)]
pub struct Const {
    data: Data,
}

#[derive(Debug)]
pub struct Type {
    layout: Layout,
}

#[derive(Debug)]
pub enum Kind {}

#[derive(Debug)]
pub struct Function {}

#[derive(Debug)]
pub struct Layout {}

#[derive(Debug)]
pub struct Data {}

#[derive(Debug)]
struct SymbolVisitor<'a> {
    stack: Vec<SymbolPart>,
    symbol_id: u64,
    scope_id: u64,
    symbols: &'a mut bimap::BiMap<Id, Symbol>,
}

#[derive(Debug)]
struct KindVisitor<'a> {
    kinds: &'a mut collections::HashMap<Id, Kind>,
}

impl Module {
    fn new(next_id: u64) -> Module {
        let symbols = bimap::BiMap::new();
        let types = collections::HashMap::new();
        let kinds = collections::HashMap::new();
        let consts = collections::HashMap::new();
        let functions = collections::HashMap::new();
        Module { next_id, symbols, types, kinds, consts, functions }
    }

    pub fn build<C>(ast: ast::Module<C>) -> Module {
        use ast::AstNode;

        let mut next_id = 0u64;
        let ast = ast.map_context(&mut |c| {
            let result = AstContext { id: Id(next_id) };
            next_id += 1;
            result
        });

        let mut result = Module::new(next_id);

        ast.visit(&mut SymbolVisitor::new(&mut result.symbols));

        result
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, part) in self.parts.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            part.fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for SymbolPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SymbolPart::Name(ref name) => write!(f, "{}", name),
            SymbolPart::Scope(ref lambda) => write!(f, "#l{}", lambda),
        }
    }
}

impl<'a> SymbolVisitor<'a> {
    fn new(symbols: &'a mut bimap::BiMap<Id, Symbol>) -> SymbolVisitor<'a> {
        let stack = Vec::new();
        let symbol_id = 0;
        let lambda_id = 0;
        SymbolVisitor { stack, symbol_id, scope_id: lambda_id, symbols }
    }
}

impl<'a, C> ast::Visitor<C> for SymbolVisitor<'a> {
    fn define_ident(&mut self, ident: &ast::Identifier) {
        let mut parts = self.stack.clone();
        parts.push(SymbolPart::Name(ident.0.clone()));
        self.symbols.insert(Id(self.symbol_id), Symbol { parts });
        self.symbol_id += 1;
    }

    fn push_scope(&mut self) {
        self.stack.push(SymbolPart::Scope(self.scope_id));
        self.scope_id += 1;
    }

    fn push_definition(&mut self, ident: &ast::Identifier) {
        self.stack.push(SymbolPart::Name(ident.0.clone()));
    }

    fn pop_scope(&mut self) {
        let part = self.stack.pop().expect("mismatched number of push/pop");
    }

    fn pop_definition(&mut self) {
        self.stack.pop().expect("mismatched number of push/pop");
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use ast;

    #[test]
    fn e2e() {
        use parser::Parse;
        let ast_module = ast::Module::parse(
            r#"
/* A record describing a person */
Person = { name: String, age: Int };

/* Makes any person old */
makeOld = |person: Person| {
  getName = |person: Person| { person.name };
  getOldAge = |number| {|unused| {number}}(90);
  { name: getName(person), age: getOldAge(0) }
};

/* Application main entry point */
main = || {
  /* Print a debug representation of the old person */
  print(makeOld({ name: "David", age: 27 }))
};
"#,
        ).unwrap();
        let ir_module = Module::build(ast_module);
        println!("{:#?}", ir_module);
    }
}
