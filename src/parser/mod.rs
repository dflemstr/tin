//! A parser for Norm code.
use std::borrow;
use std::char;
use std::fmt;

use lalrpop_util;

use ast;

lalrpop_mod!(
    #[allow(clippy)]
    #[allow(missing_debug_implementations)]
    #[allow(unused)]
    norm,
    "/parser/norm.rs"
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

/// An error that occurs while parsing Norm.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// There was an invalid token in the code.
    #[fail(display = "invalid token at {}", _0)]
    InvalidToken {
        /// The location (byte index) of the invalid token.
        location: usize,
    },
    /// There was an unexpected token in the code.
    #[fail(display = "unrecognized token {:?}, expected {:?}", _0, _1)]
    UnrecognizedToken {
        /// The start (byte index), seen token, and end (byte index), or `None` if we are at the end
        /// of the file.
        token: Option<(usize, String, usize)>,
        /// Tokens that would have been valid at this point.
        expected: Vec<String>,
    },
    /// There was an extra token at the end of the file.
    #[fail(display = "got extra token {:?}", _0)]
    ExtraToken {
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
            type Parser = ::parser::norm::$parser;

            fn new_parser() -> Self::Parser {
                ::parser::norm::$parser::new()
            }
        }

        impl Parser<$result> for ::parser::norm::$parser {
            fn parse(&mut self, source: &str) -> Result<$result, Error> {
                ::parser::norm::$parser::parse(self, source).map_err(Into::into)
            }
        }
    };
}

parser_impl!(ModuleParser, ast::Module<Context>);
parser_impl!(IdentifierParser, ast::Identifier<Context>);
parser_impl!(ExpressionParser, ast::Expression<Context>);
parser_impl!(TupleParser, ast::Tuple<Context>);
parser_impl!(RecordParser, ast::Record<Context>);
parser_impl!(LambdaParser, ast::Lambda<Context>);
parser_impl!(StatementParser, ast::Statement<Context>);
parser_impl!(SelectParser, ast::Select<Context>);
parser_impl!(ApplyParser, ast::Apply<Context>);
parser_impl!(ParameterParser, ast::Parameter<Context>);

fn parse_escaped_string(input: &str) -> borrow::Cow<str> {
    let input = &input[1..input.len() - 1];

    if !input.contains('\\') {
        input.into()
    } else {
        let mut result = String::with_capacity(input.len());
        enum State {
            Normal,
            Escape,
            Unicode,
        };
        let mut state = State::Normal;
        let mut unicode: u32 = 0;

        for c in input.chars() {
            match state {
                State::Normal => {
                    if c == '\\' {
                        state = State::Escape;
                    } else {
                        result.push(c);
                    }
                }
                State::Escape => match c {
                    '"' => {
                        result.push('"');
                        state = State::Normal
                    }
                    '\\' => {
                        result.push('\\');
                        state = State::Normal
                    }
                    '/' => {
                        result.push('/');
                        state = State::Normal
                    }
                    'b' => {
                        result.push('\u{0008}');
                        state = State::Normal
                    }
                    'f' => {
                        result.push('\u{000C}');
                        state = State::Normal
                    }
                    'n' => {
                        result.push('\n');
                        state = State::Normal
                    }
                    'r' => {
                        result.push('\r');
                        state = State::Normal
                    }
                    't' => {
                        result.push('\t');
                        state = State::Normal
                    }
                    'u' => state = State::Unicode,
                    _ => unreachable!(),
                },
                State::Unicode => match c {
                    '{' => {
                        unicode = 0;
                    }
                    'a'...'f' => {
                        unicode = (c as u32 - 'a' as u32 + 10) + 16 * unicode;
                    }
                    'A'...'F' => {
                        unicode = (c as u32 - 'A' as u32 + 10) + 16 * unicode;
                    }
                    '0'...'9' => {
                        unicode = (c as u32 - '0' as u32) + 16 * unicode;
                    }
                    '}' => {
                        result.push(char::from_u32(unicode).unwrap_or('\u{FFFD}'));
                        state = State::Normal;
                    }
                    _ => unreachable!(),
                },
            }
        }

        result.into()
    }
}

impl<T> From<lalrpop_util::ParseError<usize, T, Error>> for Error
where
    T: fmt::Display,
{
    fn from(error: lalrpop_util::ParseError<usize, T, Error>) -> Self {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => Error::InvalidToken { location },
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let token = token.map(|(s, t, e)| (s, format!("{}", t), e));
                Error::UnrecognizedToken { token, expected }
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let token = (token.0, format!("{}", token.1), token.2);
                Error::ExtraToken { token }
            }
            lalrpop_util::ParseError::User { error } => error,
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger;

    use super::norm;
    use ast;

    #[test]
    fn e2e() {
        let _ = env_logger::try_init();

        let actual = norm::ModuleParser::new().parse(
            r#"
/* A record describing a person */
Person = { name: String, age: Int };

/* Makes any person old */
makeOld = |person: Person| {
  { name: person.name, age: 90 }
};

/* Application main entry point */
main = || {
  /* Print a debug representation of the old person */
  print(makeOld({ name: "David", age: 27 }))
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
        let actual = norm::IdentifierParser::new()
            .parse(r#"whatever"#)
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
        let actual = norm::IdentifierParser::new()
            .parse(r#"なんでも"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: -1.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"-1"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 0.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"0"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: -0.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"-0"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1.5,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: -1.5,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"-1.5"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: -1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"-1.5000e3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000E3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: -1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"-1.5000E3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_small() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 0.0015,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e-3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_padded_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e0003"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1500.0,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e+3"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1.5,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e0"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Number(ast::NumberLiteral {
            context: (),
            value: 1.5,
        }));
        let actual = norm::ExpressionParser::new()
            .parse(r#"1.5000e+0"#)
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
        let actual = norm::StringParser::new()
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
        let actual = norm::StringParser::new()
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
        let actual = norm::StringParser::new()
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
                    value: 1.0,
                }),
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: 2.0,
                }),
            ],
        });
        let actual = norm::TupleParser::new()
            .parse(r#"(1, 2)"#)
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
                    value: 1.0,
                }),
                ast::Expression::Number(ast::NumberLiteral {
                    context: (),
                    value: 2.0,
                }),
            ],
        });
        let actual = norm::TupleParser::new()
            .parse(r#"(1, 2,)"#)
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
                value: 1.0,
            })],
        });
        let actual = norm::TupleParser::new()
            .parse(r#"(1)"#)
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
                value: 1.0,
            })],
        });
        let actual = norm::TupleParser::new()
            .parse(r#"(1,)"#)
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
        let actual = norm::TupleParser::new()
            .parse(r#"()"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty_no_comma() {
        let _ = env_logger::try_init();

        let actual = norm::TupleParser::new()
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
                        value: 1.0,
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
            ].into_iter()
            .collect(),
        });
        let actual = norm::RecordParser::new()
            .parse(r#"{a: 1, b: "c"}"#)
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
                        value: 1.0,
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
            ].into_iter()
            .collect(),
        });
        let actual = norm::RecordParser::new()
            .parse(r#"{a: 1, b: "c",}"#)
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
                    value: 1.0,
                }),
            )].into_iter()
            .collect(),
        });
        let actual = norm::RecordParser::new()
            .parse(r#"{a: 1}"#)
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
                    value: 1.0,
                }),
            )].into_iter()
            .collect(),
        });
        let actual = norm::RecordParser::new()
            .parse(r#"{a: 1,}"#)
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
        let actual = norm::RecordParser::new()
            .parse(r#"{}"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty_no_trailing_comma() {
        let _ = env_logger::try_init();

        let actual = norm::RecordParser::new()
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
        });
        let actual = norm::LambdaParser::new()
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
            statements: vec![
                ast::Statement::Definition(
                    ast::Identifier {
                        context: (),
                        value: "c".to_owned(),
                    },
                    ast::Expression::Lambda(ast::Lambda {
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
                    }),
                ),
                ast::Statement::Expression(ast::Expression::Apply(ast::Apply {
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
            ],
        });
        let actual = norm::LambdaParser::new()
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
            statements: vec![
                ast::Statement::Definition(
                    ast::Identifier {
                        context: (),
                        value: "c".to_owned(),
                    },
                    ast::Expression::Lambda(ast::Lambda {
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
                    }),
                ),
                ast::Statement::Expression(ast::Expression::Apply(ast::Apply {
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
            ],
        });
        let actual = norm::LambdaParser::new()
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
        });
        let actual = norm::LambdaParser::new()
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
            statements: vec![
                ast::Statement::Expression(ast::Expression::Apply(ast::Apply {
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
                ast::Statement::Expression(ast::Expression::Apply(ast::Apply {
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
            ],
        });
        let actual = norm::LambdaParser::new()
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
        let actual = norm::ApplyParser::new()
            .parse(r#"a(b)"#)
            .map(|r| r.map_context(&mut |_| ()));
        assert_eq!(expected, actual);
    }
}
