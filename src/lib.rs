//! Tin is a simple embeddable programming language.
//!
//! The aim is to have a very light-weight footprint and a simple API.
//!
//! # Examples
//!
//! ```
//! # extern crate failure;
//! # extern crate tin;
//! # fn main() -> Result<(), failure::Error> {
//! let source = r#"
//! pickFirst = |a: i32, b: i32| -> i32 {
//!   capture = |x: i32| -> i32 { a + x };
//!   capture(b)
//! };
//! main = || -> i32 { pickFirst(42i32, 62i32) };
//! "#;
//!
//! let mut tin = tin::Tin::new();
//! tin.load("main.tn", source)?;
//!
//! /*
//! let mut module = tin.compile()?;
//! let main = module.function::<tin::module::Function0<i32>>("main").unwrap();
//!
//! let result = main();
//! assert_eq!(42, result);
//! */
//! # Ok(())
//! # }
//! ```
#![deny(nonstandard_style, warnings, unused)]
#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    //unstable_features,
    unused_import_braces,
    unused_qualifications
)]
#![feature(const_vec_new)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy::all, clippy::pedantic))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::use_self))]

#[macro_use]
extern crate enum_primitive_derive;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate specs_derive;
#[macro_use]
extern crate specs_visitor_derive;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

use std::fmt;

mod ast;
mod best_iter;
mod codegen;
mod interpreter;
mod ir;
mod parser;
mod value;

#[cfg(test)]
mod test_util;

pub mod diagnostic;
pub mod error;
pub mod graph;
pub mod module;

pub use crate::error::Error;
pub use crate::error::Result;

/// An instance of the Tin runtime.
pub struct Tin {
    ir: ir::Ir,
    codemap: codespan::CodeMap,
    parser: <ast::Module<parser::Context> as parser::Parse>::Parser,
}

impl Tin {
    /// Creates a new instance of the Tin runtime.
    pub fn new() -> Tin {
        use crate::parser::Parse;

        let ir = ir::Ir::new();
        let codemap = codespan::CodeMap::new();
        let parser = ast::Module::new_parser();

        Tin {
            ir,
            codemap,
            parser,
        }
    }

    /// Loads the specified source code as a module.
    ///
    /// # Errors
    ///
    /// This function will return an error if the source code contains a syntax error or if the
    /// resulting logical structure has semantic errors or type errors.
    ///
    /// Calling this function several times will load source code into the same module scope, but
    /// references in code from earlier calls will not be able to refer to definitions from code
    /// from later calls.  Any references will eagerly be resolved and fail early.
    ///
    /// # Examples
    ///
    /// Loading a very basic module:
    ///
    /// ```
    /// # extern crate failure;
    /// # extern crate tin;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin::Tin::new();
    /// tin.load("main.tn", "main = || -> i32 { 42i32 };")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Unresolved references are not allowed:
    ///
    /// ```
    /// # extern crate failure;
    /// # extern crate tin;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin::Tin::new();
    /// let result = tin.load("main.tn", "main = || -> i32 { a };");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn load<F>(&mut self, file_name: F, source: &str) -> Result<()>
    where
        F: Into<codespan::FileName>,
    {
        let span = self
            .codemap
            .add_filemap(file_name.into(), source.to_owned())
            .span();
        let module = parser::Parser::parse(&mut self.parser, span, source)?;
        self.ir.load(&module)?;

        Ok(())
    }

    /// Creates a graph representation of the current IR of this Tin instance.
    ///
    /// This can be used to for example visualize the code using GraphViz or other tools.
    pub fn graph(&self) -> graph::Graph {
        graph::Graph::new(&self.ir)
    }

    /// Compiles the code loaded so far into a stand-alone module.
    ///
    /// This module is detached from the runtime and can be used even after the runtime has been
    /// dropped.  Once all required Tin code has been loaded, it is therefore recommended to drop
    /// this instance and only keep the compiled module around.
    ///
    /// # Examples
    ///
    /// Compiling a very basic module:
    ///
    /// ```
    /// # extern crate failure;
    /// # extern crate tin;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin::Tin::new();
    /// tin.load("main.tn", "main = || -> i32 { 42i32 };")?;
    ///
    /// let mut module = tin.compile()?;
    /// let main = module.function::<tin::module::Function0<i32>>("main").unwrap();
    ///
    /// let result = main.call()?;
    /// assert_eq!(42, result);
    /// # Ok(())
    /// # }
    /// ```
    pub fn compile(&mut self) -> Result<module::Module> {
        self.ir.check_types()?;
        let module = codegen::Codegen::new(&self.ir, &self.codemap).compile();
        Ok(module)
    }

    /// Returns a reference to the current code map, which contains location mapping for all source
    /// code loaded so far.
    pub fn codemap(&self) -> &codespan::CodeMap {
        &self.codemap
    }
}

impl Default for Tin {
    fn default() -> Self {
        Tin::new()
    }
}

impl fmt::Debug for Tin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Tin").finish()
    }
}
