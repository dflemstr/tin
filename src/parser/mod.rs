//! A parser for Tin code.
use std::fmt;

use lalrpop_util;

use ast;

mod util;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(clippy::pedantic)]
    #[allow(missing_debug_implementations)]
    #[allow(unused)]
    tin,
    "/parser/tin.rs"
);

/// The context of a parsed AST node.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Context {
    /// The source code span that this AST was parsed from.
    pub span: Span,
    /// The kind of AST node that the parser identified.
    pub kind: ast::Kind,
}

/// A source code span.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    /// The starting (byte) position of the span.
    pub start: usize,
    /// The ending (byte) position of the span.
    pub end: usize,
}

/// An error that occurs while parsing Tin.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// There was an invalid token in the code.
    #[fail(display = "invalid token at {}", _0)]
    Invalid {
        /// The location (byte index) of the invalid token.
        location: usize,
    },
    /// There was an unexpected token in the code.
    #[fail(display = "unrecognized token {:?}, expected {:?}", _0, _1)]
    Unrecognized {
        /// The start (byte index), seen token, and end (byte index), or `None` if we are at the end
        /// of the file.
        token: Option<(usize, String, usize)>,
        /// Tokens that would have been valid at this point.
        expected: Vec<String>,
    },
    /// There was an extra token at the end of the file.
    #[fail(display = "got extra token {:?}", _0)]
    Trailing {
        /// The start (byte index), seen token, and end (byte index).
        token: (usize, String, usize),
    },
}

/// Something that can be parsed.
pub trait Parse: Sized {
    /// The type of parser created by `new_parser`.
    type Parser: Parser<Self>;
    /// Creates a re-usable parser that can be used for bulk parse operations.
    fn new_parser() -> Self::Parser;

    /// Parses the supplied string into a value.
    fn parse(source: &str) -> Result<Self, Error> {
        Self::new_parser().parse(source)
    }
}

/// A re-usable parser for a specific type.
pub trait Parser<A> {
    /// Parses the supplied string into a value.
    fn parse(&mut self, source: &str) -> Result<A, Error>;
}

impl Context {
    /// Creates a new context from span start and end points.
    pub fn new(kind: ast::Kind, start: usize, end: usize) -> Context {
        let span = Span { start, end };
        Context { span, kind }
    }
}

macro_rules! parser_impl {
    ($parser:ident, $result:ty) => {
        impl Parse for $result {
            type Parser = ::parser::tin::$parser;

            fn new_parser() -> Self::Parser {
                ::parser::tin::$parser::new()
            }
        }

        impl Parser<$result> for ::parser::tin::$parser {
            fn parse(&mut self, source: &str) -> Result<$result, Error> {
                ::parser::tin::$parser::parse(self, source).map_err(Into::into)
            }
        }
    };
}

parser_impl!(ModuleParser, ast::Module<Context>);
parser_impl!(ExpressionParser, ast::Expression<Context>);

impl<T> From<lalrpop_util::ParseError<usize, T, Error>> for Error
where
    T: fmt::Display,
{
    fn from(error: lalrpop_util::ParseError<usize, T, Error>) -> Self {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => Error::Invalid { location },
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let token = token.map(|(s, t, e)| (s, format!("{}", t), e));
                Error::Unrecognized { token, expected }
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let token = (token.0, format!("{}", token.1), token.2);
                Error::Trailing { token }
            }
            lalrpop_util::ParseError::User { error } => error,
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger;

    use super::tin;
    use ast;
    use ast::map_context::MapContext;

    #[test]
    fn e2e() {
        let _ = env_logger::try_init();

        let actual = tin::ModuleParser::new().parse(
            r#"
/* A record describing a person */
Person = { name: String, age: U32 };

/* Makes any person old */
makeOld = |person: Person| {
  { name: person.name, age: 70u32 + person.age }
};

/* Application main entry point */
main = || {
  /* Print a debug representation of the old person */
  print(makeOld({ name: "David", age: 27u32 }))
};
"#,
        );
        assert!(actual.is_ok());
    }

    #[test]
    fn identifier() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Identifier(ast::Identifier {
            context: (),
            value: "whatever".to_owned(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"whatever"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn identifier_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Identifier(ast::Identifier {
            context: (),
            value: "なんでも".to_owned(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"なんでも"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"-1f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-0.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"-0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.5),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"-1.5f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"-1.5000e3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000E3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"-1.5000E3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_small() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0015),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e-3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_padded_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e0003f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e+3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"1.5000e+0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "abc".to_owned(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#""abc""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "なんでも".to_owned(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#""なんでも""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_escape() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "\"\\/\u{0008}\u{000C}\n\r\t\u{1234}".to_owned(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#""\"\\/\b\f\n\r\t\u{1234}""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Tuple(ast::Tuple {
            context: (),
            fields: vec![
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(2.0),
                }),
            ],
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"(1f64, 2f64)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Tuple(ast::Tuple {
            context: (),
            fields: vec![
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(2.0),
                }),
            ],
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"(1f64, 2f64,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_single_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Tuple(ast::Tuple {
            context: (),
            fields: vec![ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::F64(1.0),
            })],
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"(1f64,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Tuple(ast::Tuple {
            context: (),
            fields: vec![],
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"()"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty_no_comma() {
        let _ = env_logger::try_init();

        let actual = tin::ExpressionParser::new()
            .parse(r#"(,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert!(actual.is_err());
    }

    #[test]
    fn record() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![
                (
                    ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    ast::Expression::NumberLiteral(ast::NumberLiteral {
                        context: (),
                        value: ast::NumberValue::F64(1.0),
                    }),
                ),
                (
                    ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    ast::Expression::StringLiteral(ast::StringLiteral {
                        context: (),
                        value: "c".to_owned(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"{a: 1f64, b: "c"}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![
                (
                    ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    ast::Expression::NumberLiteral(ast::NumberLiteral {
                        context: (),
                        value: ast::NumberValue::F64(1.0),
                    }),
                ),
                (
                    ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    ast::Expression::StringLiteral(ast::StringLiteral {
                        context: (),
                        value: "c".to_owned(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"{a: 1f64, b: "c",}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![(
                ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                },
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
            )]
            .into_iter()
            .collect(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"{a: 1f64}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![(
                ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                },
                ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
            )]
            .into_iter()
            .collect(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"{a: 1f64,}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![].into_iter().collect(),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"{}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty_no_trailing_comma() {
        let _ = env_logger::try_init();

        let actual = tin::ExpressionParser::new()
            .parse(r#"{,}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert!(actual.is_err());
    }

    #[test]
    fn un_op_not() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Not,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"!0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_not_not() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Not,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Not,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"!!0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_bnot() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::BNot,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"~!0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_bnot_bnot() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::BNot,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::BNot,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"~!~!0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cl0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cl0,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cl0_cl0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cl0,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Cl0,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^0#^0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cl1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cl1,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cl1_cl1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cl1,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Cl1,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^1#^1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cls() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cls,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^- 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_cls_cls() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Cls,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Cls,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#^-#^- 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_ct0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Ct0,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#$0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_ct0_ct0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Ct0,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Ct0,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#$0#$0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_ct1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Ct1,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#$1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_ct1_ct1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Ct1,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Ct1,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#$1#$1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_c0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::C0,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_c0_c0() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::C0,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::C0,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#0#0 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_c1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::C1,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_c1_c1() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::C1,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::C1,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"#1#1 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_sqrt() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Sqrt,
            operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::U32(0),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"^/ 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn un_op_sqrt_sqrt() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::UnOp(ast::UnOp {
            context: (),
            operator: ast::UnOperator::Sqrt,
            operand: Box::new(ast::Expression::UnOp(ast::UnOp {
                context: (),
                operator: ast::UnOperator::Sqrt,
                operand: Box::new(ast::Expression::NumberLiteral(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::U32(0),
                })),
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"^/^/ 0u32"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Lambda(ast::Lambda {
            context: (),
            parameters: vec![
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    signature: None,
                },
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    signature: None,
                },
            ],
            signature: None,
            statements: vec![],
            result: Box::new(ast::Expression::Apply(ast::Apply {
                context: (),
                function: Box::new(ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                })),
                parameters: vec![ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "b".to_owned(),
                })],
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"|a, b| { a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_with_definition() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Lambda(ast::Lambda {
            context: (),
            parameters: vec![
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    signature: None,
                },
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    signature: None,
                },
            ],
            signature: None,
            statements: vec![ast::Statement::Variable(ast::Variable {
                context: (),
                name: ast::Identifier {
                    context: (),
                    value: "c".to_owned(),
                },
                initializer: ast::Expression::Lambda(ast::Lambda {
                    context: (),
                    parameters: vec![ast::Parameter {
                        context: (),
                        name: ast::Identifier {
                            context: (),
                            value: "b".to_owned(),
                        },
                        signature: None,
                    }],
                    signature: None,
                    statements: vec![],
                    result: Box::new(ast::Expression::Apply(ast::Apply {
                        context: (),
                        function: Box::new(ast::Expression::Identifier(ast::Identifier {
                            context: (),
                            value: "a".to_owned(),
                        })),
                        parameters: vec![ast::Expression::Identifier(ast::Identifier {
                            context: (),
                            value: "b".to_owned(),
                        })],
                    })),
                }),
            })],
            result: Box::new(ast::Expression::Apply(ast::Apply {
                context: (),
                function: Box::new(ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "c".to_owned(),
                })),
                parameters: vec![ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "b".to_owned(),
                })],
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"|a, b| { c = |b| { a(b) }; c(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_with_definition_comment() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Lambda(ast::Lambda {
            context: (),
            parameters: vec![
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    signature: None,
                },
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    signature: None,
                },
            ],
            signature: None,
            statements: vec![ast::Statement::Variable(ast::Variable {
                context: (),
                name: ast::Identifier {
                    context: (),
                    value: "c".to_owned(),
                },
                initializer: ast::Expression::Lambda(ast::Lambda {
                    context: (),
                    parameters: vec![ast::Parameter {
                        context: (),
                        name: ast::Identifier {
                            context: (),
                            value: "b".to_owned(),
                        },
                        signature: None,
                    }],
                    signature: None,
                    statements: vec![],
                    result: Box::new(ast::Expression::Apply(ast::Apply {
                        context: (),
                        function: Box::new(ast::Expression::Identifier(ast::Identifier {
                            context: (),

                            value: "a".to_owned(),
                        })),
                        parameters: vec![ast::Expression::Identifier(ast::Identifier {
                            context: (),
                            value: "b".to_owned(),
                        })],
                    })),
                }),
            })],
            result: Box::new(ast::Expression::Apply(ast::Apply {
                context: (),
                function: Box::new(ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "c".to_owned(),
                })),
                parameters: vec![ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "b".to_owned(),
                })],
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"|a, b| { /* define c */ c = |b| { a(b) }; /* call c */ c(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_signature() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Lambda(ast::Lambda {
            context: (),
            parameters: vec![
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    signature: Some(ast::Expression::Identifier(ast::Identifier {
                        context: (),
                        value: "Int".to_owned(),
                    })),
                },
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    signature: Some(ast::Expression::Identifier(ast::Identifier {
                        context: (),
                        value: "Int".to_owned(),
                    })),
                },
            ],
            signature: None,
            statements: vec![],
            result: Box::new(ast::Expression::Apply(ast::Apply {
                context: (),
                function: Box::new(ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                })),
                parameters: vec![ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "b".to_owned(),
                })],
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"|a: Int, b: Int| { a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_many_statements() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Lambda(ast::Lambda {
            context: (),
            parameters: vec![
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    signature: None,
                },
                ast::Parameter {
                    context: (),
                    name: ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    signature: None,
                },
            ],
            signature: None,
            statements: vec![ast::Statement::Expression(ast::Expression::Apply(
                ast::Apply {
                    context: (),
                    function: Box::new(ast::Expression::Identifier(ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    })),
                    parameters: vec![ast::Expression::Identifier(ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    })],
                },
            ))],
            result: Box::new(ast::Expression::Apply(ast::Apply {
                context: (),
                function: Box::new(ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                })),
                parameters: vec![ast::Expression::Identifier(ast::Identifier {
                    context: (),
                    value: "b".to_owned(),
                })],
            })),
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"|a, b| { a(b); a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn apply() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Apply(ast::Apply {
            context: (),
            function: Box::new(ast::Expression::Identifier(ast::Identifier {
                context: (),
                value: "a".to_owned(),
            })),
            parameters: vec![ast::Expression::Identifier(ast::Identifier {
                context: (),
                value: "b".to_owned(),
            })],
        }));
        let actual = tin::ExpressionParser::new()
            .parse(r#"a(b)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }
}
