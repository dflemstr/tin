//! A parser for Norm code.
use std::fmt;

use nom;

use ast;

mod rules;

/// The context of a parsed AST node.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Context {
    /// The source code span that this AST was parsed from.
    pub span: Span,
    /// The kind of AST node that the parser identified.
    pub kind: ast::Kind,
}

/// A specific point in the input code.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point {
    /// The byte offset from the start of the code.
    pub offset: usize,
    /// The line number, 1-based.
    pub line: usize,
    /// The column number, 1-based.
    pub column: usize,
}

/// A source code span.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    /// The point at which this span starts.
    pub start: Point,
    /// The point at which this span ends.
    pub end: Point,
}

/// An error that occurs while parsing Norm.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// More input is needed to finish the parse.
    #[fail(display = "input is incomplete; need {}", needed)]
    Incomplete {
        /// The amount of needed input.
        needed: Needed,
    },
    /// There was more input than needed.
    #[fail(display = "there is superfluous code: {}", extra)]
    Superfluous {
        /// Extra, unconsumed input.
        extra: String,
    },
    /// Unrecognized input was encountered.
    #[fail(display = "{}: unrecognized input: {}", at, message)]
    Unrecognized {
        /// The point at which the error occurred.
        at: Point,
        /// A human-readable message describing the error.
        message: String,
    }
}

/// An indicator for how much more input the parser needs.
#[derive(Debug, PartialEq)]
#[allow(missing_copy_implementations)]
pub enum Needed {
    /// The needed additional input is unknown.
    Unknown,
    /// The needed additional input is at least the specified number of bytes.
    Size(usize),
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
    fn parse(&self, source: &str) -> Result<A, Error>;
}

impl Point {
    fn from_located_span<A>(span: nom_locate::LocatedSpan<A>) -> Point
        where
            A: nom::AsBytes, {
        Point {
            offset: span.offset,
            line: span.line as usize,
            column: span.get_utf8_column(),
        }
    }
}

impl Context {
    /// Creates a new context from span start and end points.
    pub fn new(kind: ast::Kind, start: usize, end: usize) -> Context {
        // TODO: remove this
        let start = Point {
            offset: start,
            line: 1,
            column: 1,
        };
        let end = Point {
            offset: end,
            line: 1,
            column: 1,
        };
        let span = Span { start, end };

        Context { span, kind }
    }

    fn from_located_span<A>(
        kind: ast::Kind,
        start: nom_locate::LocatedSpan<A>,
        end: nom_locate::LocatedSpan<A>,
    ) -> Context
    where
        A: nom::AsBytes,
    {
        let start = Point::from_located_span(start);
        let end = Point::from_located_span(end);
        let span = Span { start, end };

        Context { span, kind }
    }
}

macro_rules! parser_impl {
    ($parser:ident, $result:ty, $function:expr) => {
        #[allow(missing_copy_implementations, missing_docs)]
        #[derive(Debug)]
        pub struct $parser;

        impl Parse for $result {
            type Parser = $parser;

            fn new_parser() -> Self::Parser {
                $parser
            }
        }

        impl Parser<$result> for $parser {
            fn parse(&self, source: &str) -> Result<$result, Error> {
                $function(rules::Span::new(nom::types::CompleteStr(source)))
                    .map_err(Into::into)
                    .and_then(|(rest, result)| {
                        if rest.fragment.is_empty() {
                            Ok(result)
                        } else {
                            Err(Error::Superfluous { extra: (*rest.fragment).to_owned() })
                        }
                    })
            }
        }
    };
}

parser_impl!(
    ModuleParser,
    ast::Module<Context>,
    rules::parse_module
);
parser_impl!(
    IdentifierParser,
    ast::Identifier<Context>,
    rules::parse_identifier
);
parser_impl!(
    ExpressionParser,
    ast::Expression<Context>,
    rules::parse_expression
);
parser_impl!(
    NumberLiteralParser,
    ast::NumberLiteral<Context>,
    rules::parse_number_literal
);
parser_impl!(
    StringLiteralParser,
    ast::StringLiteral<Context>,
    rules::parse_string_literal
);
parser_impl!(TupleParser, ast::Tuple<Context>, rules::parse_tuple);
parser_impl!(
    RecordParser,
    ast::Record<Context>,
    rules::parse_record
);
parser_impl!(
    LambdaParser,
    ast::Lambda<Context>,
    rules::parse_lambda
);
parser_impl!(
    StatementParser,
    ast::Statement<Context>,
    rules::parse_statement
);
parser_impl!(
    VariableParser,
    ast::Variable<Context>,
    rules::parse_variable
);
parser_impl!(
    SelectParser,
    ast::Select<Context>,
    rules::parse_select
);
parser_impl!(ApplyParser, ast::Apply<Context>, rules::parse_apply);
parser_impl!(
    ParameterParser,
    ast::Parameter<Context>,
    rules::parse_parameter
);

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl fmt::Display for Needed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Needed::Unknown => f.write_str("an unknown amount of additional input"),
            Needed::Size(n) => write!(f, "at least {} more bytes", n),
        }
    }
}

impl<'a> From<nom::Err<rules::Span<'a>>> for Error {
    fn from(err: nom::Err<rules::Span<'a>>) -> Self {
        match err {
            nom::Err::Incomplete(nom::Needed::Unknown) =>
            Error::Incomplete { needed: Needed::Unknown },
            nom::Err::Incomplete(nom::Needed::Size(n)) =>
            Error::Incomplete { needed: Needed::Size(n) },
            nom::Err::Error(nom::Context::Code(input, kind)) => {
                Error::Unrecognized { at: Point::from_located_span(input), message: kind.description().to_owned() }
            },
            nom::Err::Failure(nom::Context::Code(input, kind)) => {
                Error::Unrecognized { at: Point::from_located_span(input), message: kind.description().to_owned() }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger;

    use super::*;
    use ast;
    use ast::map_context::MapContext;

    #[test]
    fn e2e() {
        let _ = env_logger::try_init();

        let actual = ModuleParser.parse(
            r#"
/* A record describing a person */
Person = { name: String, age: Int };

/* Makes any person old */
makeOld = |person: Person| {
  { name: person.name, age: 90f64 }
};

/* Application main entry point */
main = || {
  /* Print a debug representation of the old person */
  print(makeOld({ name: "David", age: 27f64 }))
};
"#,
        );
        assert!(actual.is_ok());
    }

    #[test]
    fn identifier() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Identifier {
            context: (),
            value: "whatever".to_owned(),
        });
        let actual = IdentifierParser.parse(r#"whatever"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn identifier_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Identifier {
            context: (),
            value: "なんでも".to_owned(),
        });
        let actual = IdentifierParser
            .parse(r#"なんでも"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"1f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"-1f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-0.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"-0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.5),
        });
        let actual = NumberLiteralParser
            .parse(r#"-1.5f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"-1.5000e3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000E3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"-1.5000E3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_small() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0015),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e-3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_padded_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e0003f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e+3f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        });
        let actual = NumberLiteralParser
            .parse(r#"1.5000e+0f64"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::StringLiteral {
            context: (),
            value: "abc".to_owned(),
        });
        let actual = StringLiteralParser
            .parse(r#""abc""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::StringLiteral {
            context: (),
            value: "なんでも".to_owned(),
        });
        let actual = StringLiteralParser
            .parse(r#""なんでも""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_escape() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::StringLiteral {
            context: (),
            value: "\"\\/\u{0008}\u{000C}\n\r\t\u{1234}".to_owned(),
        });
        let actual = StringLiteralParser
            .parse(r#""\"\\/\b\f\n\r\t\u{1234}""#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Tuple {
            context: (),
            fields: vec![
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(2.0),
                }),
            ],
        });
        let actual = TupleParser
            .parse(r#"(1f64, 2f64)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Tuple {
            context: (),
            fields: vec![
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(2.0),
                }),
            ],
        });
        let actual = TupleParser
            .parse(r#"(1f64, 2f64,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_single() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Tuple {
            context: (),
            fields: vec![ast::Expression::Number(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::F64(1.0),
            })],
        });
        let actual = TupleParser
            .parse(r#"(1f64)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_single_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Tuple {
            context: (),
            fields: vec![ast::Expression::Number(ast::NumberLiteral {
                context: (),
                value: ast::NumberValue::F64(1.0),
            })],
        });
        let actual = TupleParser
            .parse(r#"(1f64,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Tuple {
            context: (),
            fields: vec![],
        });
        let actual = TupleParser
            .parse(r#"()"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty_no_comma() {
        let _ = env_logger::try_init();

        let actual = TupleParser
            .parse(r#"(,)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert!(actual.is_err());
    }

    #[test]
    fn record() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Record {
            context: (),
            fields: vec![
                (
                    ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    ast::Expression::Number(ast::NumberLiteral {
                        context: (),
                        value: ast::NumberValue::F64(1.0),
                    }),
                ),
                (
                    ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    ast::Expression::String(ast::StringLiteral {
                        context: (),
                        value: "c".to_owned(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        });
        let actual = RecordParser
            .parse(r#"{a: 1f64, b: "c"}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Record {
            context: (),
            fields: vec![
                (
                    ast::Identifier {
                        context: (),
                        value: "a".to_owned(),
                    },
                    ast::Expression::Number(ast::NumberLiteral {
                        context: (),
                        value: ast::NumberValue::F64(1.0),
                    }),
                ),
                (
                    ast::Identifier {
                        context: (),
                        value: "b".to_owned(),
                    },
                    ast::Expression::String(ast::StringLiteral {
                        context: (),
                        value: "c".to_owned(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        });
        let actual = RecordParser
            .parse(r#"{a: 1f64, b: "c",}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Record {
            context: (),
            fields: vec![(
                ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                },
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
            )]
            .into_iter()
            .collect(),
        });
        let actual = RecordParser
            .parse(r#"{a: 1f64}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single_trailing_comma() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Record {
            context: (),
            fields: vec![(
                ast::Identifier {
                    context: (),
                    value: "a".to_owned(),
                },
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: ast::NumberValue::F64(1.0),
                }),
            )]
            .into_iter()
            .collect(),
        });
        let actual = RecordParser
            .parse(r#"{a: 1f64,}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Record {
            context: (),
            fields: vec![].into_iter().collect(),
        });
        let actual = RecordParser
            .parse(r#"{}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty_no_trailing_comma() {
        let _ = env_logger::try_init();

        let actual = RecordParser
            .parse(r#"{,}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert!(actual.is_err());
    }

    #[test]
    fn lambda() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Lambda {
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
        });
        let actual = LambdaParser
            .parse(r#"|a, b| { a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_with_definition() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Lambda {
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
        });
        let actual = LambdaParser
            .parse(r#"|a, b| { c = |b| { a(b) }; c(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_with_definition_comment() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Lambda {
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
        });
        let actual = LambdaParser
            .parse(r#"|a, b| { /* define c */ c = |b| { a(b) }; /* call c */ c(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_signature() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Lambda {
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
        });
        let actual = LambdaParser
            .parse(r#"|a: Int, b: Int| { a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_many_statements() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Lambda {
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
        });
        let actual = LambdaParser
            .parse(r#"|a, b| { a(b); a(b) }"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn apply() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Apply {
            context: (),
            function: Box::new(ast::Expression::Identifier(ast::Identifier {
                context: (),
                value: "a".to_owned(),
            })),
            parameters: vec![ast::Expression::Identifier(ast::Identifier {
                context: (),
                value: "b".to_owned(),
            })],
        });
        let actual = ApplyParser
            .parse(r#"a(b)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }
}
