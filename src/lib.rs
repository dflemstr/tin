//! Tin is a simple embeddable programming language.
//!
//! The aim is to have a very light-weight footprint and a simple API.
//!
//! # Examples
//!
//! ```
//! # extern crate failure;
//! # extern crate tin_lang;
//! # fn main() -> Result<(), failure::Error> {
//! let source = r#"
//! Int = 0u32;
//! pickFirst = |a: Int, b: Int| Int {
//!   capture = |x: Int| Int { a };
//!   capture(b)
//! };
//! main = || Int { pickFirst(42u32, 62u32) };
//! "#;
//!
//! let mut tin = tin_lang::Tin::new();
//! tin.load(source)?;
//!
//! /*
//! let mut module = tin.compile()?;
//! let main = module.function::<tin_lang::module::Function0<u32>>("main").unwrap();
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
    unstable_features,
    unused_import_braces,
    unused_qualifications
)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy::all, clippy::pedantic))]

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

use std::fmt;

mod ast;
mod codegen;
mod interpreter;
mod ir;
mod parser;
mod value;

#[cfg(test)]
mod test_util;

pub mod error;
pub mod graph;
pub mod module;

pub use crate::error::Error;
pub use crate::error::Result;

/// An instance of the Tin runtime.
pub struct Tin {
    ir: ir::Ir,
    parser: <ast::Module<parser::Context> as parser::Parse>::Parser,
}

impl Tin {
    /// Creates a new instance of the Tin runtime.
    pub fn new() -> Tin {
        use crate::parser::Parse;

        let ir = ir::Ir::new();
        let parser = ast::Module::new_parser();

        Tin { ir, parser }
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
    /// # extern crate tin_lang;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin_lang::Tin::new();
    /// tin.load("U32 = 0u32; main = || U32 { 42u32 };")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Unresolved references are not allowed:
    ///
    /// ```
    /// # extern crate failure;
    /// # extern crate tin_lang;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin_lang::Tin::new();
    /// let result = tin.load("U32 = 0u32; main = || U32 { a };");
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn load(&mut self, source: &str) -> Result<()> {
        let module = parser::Parser::parse(&mut self.parser, source)?;
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
    /// # extern crate tin_lang;
    /// # fn main() -> Result<(), failure::Error> {
    /// let mut tin = tin_lang::Tin::new();
    /// tin.load("U32 = 0u32; main = || U32 { 42u32 };")?;
    ///
    /// let mut module = tin.compile()?;
    /// let main = module.function::<tin_lang::module::Function0<u32>>("main").unwrap();
    ///
    /// let result = main();
    /// assert_eq!(42, result);
    /// # Ok(())
    /// # }
    /// ```
    pub fn compile(&mut self) -> Result<module::Module> {
        self.ir.check_types();
        let module = codegen::Codegen::new(&self.ir).compile();
        Ok(module)
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
