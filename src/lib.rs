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
//! use tin_lang::parser::Parse;
//!
//! let source = r#"
//! Int = 0u32;
//! pickFirst = |a: Int, b: Int| Int {
//!   capture = |x: Int| Int { a };
//!   capture(b)
//! };
//! main = || Int { pickFirst(42u32, 62u32) };
//! "#;
//!
//! let module = tin_lang::ast::Module::parse(source).unwrap();
//!
//! let mut ir = tin_lang::ir::Ir::new();
//! ir.module(&module)?;
//! ir.check_types();
//!
//! /*
//! let mut module = tin_lang::codegen::Codegen::new(&ir).compile();
//! let main = module.function::<tin_lang::codegen::module::Function0<u32>>("main").unwrap();
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
    unsafe_code,
    unstable_features,
    unused_import_braces,
    unused_qualifications
)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy::all, clippy::pedantic))]

extern crate cranelift;
extern crate cranelift_module;
extern crate cranelift_simplejit;
extern crate dot;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate log;
extern crate specs;
#[macro_use]
extern crate specs_derive;
extern crate specs_visitor;
#[macro_use]
extern crate specs_visitor_derive;
#[cfg(test)]
extern crate env_logger;

pub mod ast;
mod builtin;
pub mod codegen;
pub mod error;
pub mod ir;
pub mod parser;
#[cfg(test)]
mod test_util;

pub use error::Error;
pub use error::Result;

/// An instance of the Tin runtime.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct Tin;

impl Tin {
    /// Creates a new instance of the Tin runtime.
    pub fn new() -> Tin {
        Tin
    }
}

impl Default for Tin {
    fn default() -> Self {
        Tin::new()
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parser(err)
    }
}
