//! Norm is a simple embeddable programming language.
//!
//! The aim is to have a very light-weight footprint and a simple API.
//!
//! # Examples
//!
//! ```
//! extern crate norm;
//!
//! let mut norm = norm::Norm::new();
//!
//! let program = r#"
//!   /* You define types by specifying their default value. This defines the String type. */
//!   String = "";
//!   /* Here, we define the Int type. */
//!   Int = 0;
//!
//!   /*
//!    * Now, we have enough info to define a Person type.
//!    * Norm is structurally typed, so any object with a String name and Int age field is a Person.
//!    */
//!   Person = { name: String, age: Int };
//!
//!   /*
//!    * Everything in Norm is an expression.  There is no way to declare functions; you instead have
//!    * to create a variable with a lambda value.
//!    */
//!   getAge = |person: Person| {
//!     person.age
//!   };
//!
//!   getName = |person: Person| {
//!     person.name
//!   };
//!
//!   /* By convention, main() is the entry point, but it's the embedders choice, really. */
//!   main = || {
//!     getName({ name: "David", age: 27 })
//!   };
//! "#;
//! println!("{}", program);
//! norm.run(program).unwrap();
//!
//! let result = norm.eval("main()").unwrap();
//! assert_eq!(norm::value::Value::String("David".to_owned()), result);
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

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate failure;
extern crate itertools;

mod ast;
pub mod error;
mod interpreter;
mod parser;
pub mod value;

pub use error::Error;
pub use error::Result;
pub use value::Value;

/// An instance of the Norm runtime.
#[derive(Debug)]
pub struct Norm {
    interpreter: interpreter::Interpreter,
}

impl Norm {
    /// Creates a new instance of the Norm runtime.
    pub fn new() -> Norm {
        let interpreter = interpreter::Interpreter::new();
        Norm { interpreter }
    }

    /// Evaluates a string in expression context.
    ///
    /// This needs to be a single expression; it is not valid to define additional variables inline.
    /// To do that, it is advised to create an inline lambda that is called directly, like so:
    ///
    /// ```
    /// let mut norm = norm::Norm::new();
    /// let result = norm.eval("|| { a = { x: 2, y: 3 }; a.y }()").unwrap();
    /// assert_eq!(norm::Value::Number(3.0), result);
    /// ```
    pub fn eval(&mut self, expression: &str) -> Result<Value> {
        use parser::Parse;
        let expression = ast::Expression::parse(expression)?;
        let result = self.interpreter.eval(expression)?;
        Ok(result)
    }

    /// Runs a string in definition context.
    ///
    /// The string is expected to only contain definitions.  It is not possible to evaluate an
    /// expression and get its results.
    pub fn run(&mut self, program: &str) -> Result<()> {
        use parser::Parse;
        let program = ast::Norm::parse(program)?;
        self.interpreter.run(program)?;
        Ok(())
    }
}

impl Default for Norm {
    fn default() -> Self {
        Norm::new()
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parser(err)
    }
}

impl From<interpreter::Error> for Error {
    fn from(err: interpreter::Error) -> Self {
        Error::Interpreter(err)
    }
}
