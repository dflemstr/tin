//! A parser for Tin code.
use lalrpop_util;

use crate::ast;
use crate::diagnostic;

mod util;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(clippy::pedantic)]
    #[allow(missing_debug_implementations)]
    #[allow(trivial_numeric_casts)]
    #[allow(unused)]
    tin,
    "/parser/tin.rs"
);

/// The context of a parsed AST node.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Context {
    /// The source code span that this AST was parsed from.
    pub span: codespan::ByteSpan,
    /// The kind of AST node that the parser identified.
    pub kind: ast::Kind,
}

/// An error that occurs while parsing Tin.
#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    /// There was an invalid token in the code.
    #[fail(display = "invalid token")]
    Invalid {
        /// The location of the invalid token.
        location: codespan::ByteSpan,
    },
    /// There was an unexpected token in the code.
    #[fail(display = "unexpected token")]
    Unexpected {
        /// The seen token's span.
        token: codespan::ByteSpan,
        /// Tokens that would have been valid at this point.
        expected: Vec<String>,
    },
    /// There was an extra token at the end of the file.
    #[fail(display = "extra token")]
    Extra {
        /// The seen token's span.
        token: codespan::ByteSpan,
    },
    /// There were multiple parse errors.
    #[fail(display = "multiple parse errors")]
    Multiple {
        /// The errors, in the order they were encountered in the source code.
        errors: Vec<Error>,
    },
}

/// Something that can be parsed.
pub trait Parse: Sized {
    /// The type of parser created by `new_parser`.
    type Parser: Parser<Self>;
    /// Creates a re-usable parser that can be used for bulk parse operations.
    fn new_parser() -> Self::Parser;

    /// Parses the supplied string into a value.
    fn parse(span: codespan::ByteSpan, source: &str) -> Result<Self, Error> {
        Self::new_parser().parse(span, source)
    }
}

/// A re-usable parser for a specific type.
pub trait Parser<A> {
    /// Parses the supplied string into a value.
    fn parse(&mut self, span: codespan::ByteSpan, source: &str) -> Result<A, Error>;
}

impl Context {
    /// Creates a new context from span start and end points.
    pub fn new(kind: ast::Kind, span: codespan::ByteSpan, lo: usize, hi: usize) -> Context {
        let span = span.subspan(
            codespan::ByteOffset(lo as codespan::RawOffset),
            codespan::ByteOffset(hi as codespan::RawOffset),
        );
        Context { span, kind }
    }
}

fn handle_parse_result<A, T1, T2>(
    span: codespan::ByteSpan,
    result: Result<A, lalrpop_util::ParseError<usize, T1, Error>>,
    errors: Vec<lalrpop_util::ParseError<usize, T2, Error>>,
) -> Result<A, Error> {
    let mut errors = errors
        .into_iter()
        .map(|e| Error::from_lalrpop(span, e))
        .collect::<Vec<_>>();

    let result = match result {
        Ok(r) => Some(r),
        Err(e) => {
            errors.push(Error::from_lalrpop(span, e));
            None
        }
    };

    let len = errors.len();
    if errors.is_empty() {
        Ok(result.unwrap())
    } else {
        if len == 1 {
            Err(errors.pop().unwrap())
        } else {
            Err(Error::Multiple { errors })
        }
    }
}

macro_rules! parser_impl {
    ($parser:ident, $result:ty) => {
        impl Parse for $result {
            type Parser = crate::parser::tin::$parser;

            fn new_parser() -> Self::Parser {
                crate::parser::tin::$parser::new()
            }
        }

        impl Parser<$result> for crate::parser::tin::$parser {
            fn parse(&mut self, span: codespan::ByteSpan, source: &str) -> Result<$result, Error> {
                let mut errors = Vec::new();
                let result = crate::parser::tin::$parser::parse(self, span, &mut errors, source);
                handle_parse_result(span, result, errors)
            }
        }
    };
}

parser_impl!(ModuleParser, ast::Module<Context>);
parser_impl!(ExpressionParser, ast::Expression<Context>);

impl Error {
    fn from_lalrpop<T>(
        span: codespan::ByteSpan,
        error: lalrpop_util::ParseError<usize, T, Error>,
    ) -> Error {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => Error::Invalid {
                location: span.subspan(
                    codespan::ByteOffset(location as codespan::RawOffset),
                    codespan::ByteOffset((location + 1) as codespan::RawOffset),
                ),
            },
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let token = token
                    .map(|(s, _, e)| {
                        span.subspan(
                            codespan::ByteOffset(s as codespan::RawOffset),
                            codespan::ByteOffset(e as codespan::RawOffset),
                        )
                    })
                    .unwrap_or(codespan::Span::new(
                        span.end() - codespan::ByteOffset(1),
                        span.end() - codespan::ByteOffset(1),
                    ));
                Error::Unexpected { token, expected }
            }
            lalrpop_util::ParseError::ExtraToken { token: (s, _, e) } => {
                let token = span.subspan(
                    codespan::ByteOffset(s as codespan::RawOffset),
                    codespan::ByteOffset(e as codespan::RawOffset),
                );
                Error::Extra { token }
            }
            lalrpop_util::ParseError::User { error } => error,
        }
    }
}

impl diagnostic::Diagnostic for Error {
    fn to_diagnostics(&self, result: &mut Vec<codespan_reporting::Diagnostic>) {
        let message = self.to_string();
        match self {
            Error::Invalid { location } => result.push(codespan_reporting::Diagnostic {
                severity: codespan_reporting::Severity::Error,
                message,
                code: None,
                labels: vec![codespan_reporting::Label {
                    span: *location,
                    message: None,
                    style: codespan_reporting::LabelStyle::Primary,
                }],
            }),
            Error::Unexpected { token, expected } => {
                result.push(codespan_reporting::Diagnostic {
                    severity: codespan_reporting::Severity::Error,
                    message,
                    code: None,
                    labels: vec![codespan_reporting::Label {
                        span: *token,
                        message: None,
                        style: codespan_reporting::LabelStyle::Primary,
                    }],
                });

                if !expected.is_empty() {
                    use itertools::Itertools;

                    result.push(codespan_reporting::Diagnostic {
                        severity: codespan_reporting::Severity::Help,
                        message: format!(
                            "valid tokens at this point: [{}]",
                            expected.iter().join(", ")
                        ),
                        code: None,
                        labels: vec![],
                    })
                }
            }
            Error::Extra { token } => result.push(codespan_reporting::Diagnostic {
                severity: codespan_reporting::Severity::Error,
                message,
                code: None,
                labels: vec![codespan_reporting::Label {
                    span: *token,
                    message: None,
                    style: codespan_reporting::LabelStyle::Primary,
                }],
            }),
            Error::Multiple { errors } => {
                for error in errors {
                    error.to_diagnostics(result);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use env_logger;

    use crate::ast;
    use crate::ast::MapContext;

    #[test]
    fn e2e() {
        let _ = env_logger::try_init();

        let actual = parse_module(
            "test",
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
    fn error_invalid_token() {
        let _ = env_logger::try_init();

        let expected = Err(r#"error: invalid token
- <test>:1:1
1 | #+-
  | ^
"#
        .to_owned());
        let actual = parse_module("test", r#"#+-"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn error_misplaced_token() {
        let _ = env_logger::try_init();

        let expected = Err(r#"error: unexpected token
- <test>:1:21
1 | main = || { 0u32 }; <-<
  |                     ^^^
help: valid tokens at this point: [Comment, IdentifierName]
"#
        .to_owned());
        let actual = parse_module("test", r#"main = || { 0u32 }; <-<"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn identifier() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Identifier(ast::Identifier {
            context: (),
            value: "whatever".to_owned(),
        }));
        let actual = parse_expression("test", r#"whatever"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn identifier_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Identifier(ast::Identifier {
            context: (),
            value: "なんでも".to_owned(),
        }));
        let actual = parse_expression("test", r#"なんでも"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.0),
        }));
        let actual = parse_expression("test", r#"1f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.0),
        }));
        let actual = parse_expression("test", r#"-1f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0),
        }));
        let actual = parse_expression("test", r#"0f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-0.0),
        }));
        let actual = parse_expression("test", r#"-0f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = parse_expression("test", r#"1.5f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1.5),
        }));
        let actual = parse_expression("test", r#"-1.5f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = parse_expression("test", r#"1.5000e3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        }));
        let actual = parse_expression("test", r#"-1.5000e3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = parse_expression("test", r#"1.5000E3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase_negative() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(-1500.0),
        }));
        let actual = parse_expression("test", r#"-1.5000E3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_small() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(0.0015),
        }));
        let actual = parse_expression("test", r#"1.5000e-3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_padded_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = parse_expression("test", r#"1.5000e0003f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1500.0),
        }));
        let actual = parse_expression("test", r#"1.5000e+3f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = parse_expression("test", r#"1.5000e0f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit_zero_exponent() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::NumberLiteral(ast::NumberLiteral {
            context: (),
            value: ast::NumberValue::F64(1.5),
        }));
        let actual = parse_expression("test", r#"1.5000e+0f64"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "abc".to_owned(),
        }));
        let actual = parse_expression("test", r#""abc""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_unicode() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "なんでも".to_owned(),
        }));
        let actual = parse_expression("test", r#""なんでも""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_escape() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::StringLiteral(ast::StringLiteral {
            context: (),
            value: "\"\\/\u{0008}\u{000C}\n\r\t\u{1234}".to_owned(),
        }));
        let actual = parse_expression("test", r#""\"\\/\b\f\n\r\t\u{1234}""#);
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
        let actual = parse_expression("test", r#"(1f64, 2f64)"#);
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
        let actual = parse_expression("test", r#"(1f64, 2f64,)"#);
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
        let actual = parse_expression("test", r#"(1f64,)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Tuple(ast::Tuple {
            context: (),
            fields: vec![],
        }));
        let actual = parse_expression("test", r#"()"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty_no_comma() {
        let _ = env_logger::try_init();

        let expected = Err(r##"error: unexpected token
- <test>:1:2
1 | (,)
  |  ^
help: valid tokens at this point: ["!", "#$0", "#$1", "#0", "#1", "#^-", "#^0", "#^1", "(", ")", "^/", "{", "|", "~!", IdentifierName, NumberValue, StringValue, SymbolLabel]
"##.to_owned());
        let actual = parse_expression("test", r#"(,)"#);
        assert_eq!(expected, actual);
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
        let actual = parse_expression("test", r#"{a: 1f64, b: "c"}"#);
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
        let actual = parse_expression("test", r#"{a: 1f64, b: "c",}"#);
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
        let actual = parse_expression("test", r#"{a: 1f64}"#);
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
        let actual = parse_expression("test", r#"{a: 1f64,}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty() {
        let _ = env_logger::try_init();

        let expected = Ok(ast::Expression::Record(ast::Record {
            context: (),
            fields: vec![].into_iter().collect(),
        }));
        let actual = parse_expression("test", r#"{}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty_no_trailing_comma() {
        let _ = env_logger::try_init();

        let actual = parse_expression("test", r#"{,}"#);
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
        let actual = parse_expression("test", r#"!0u32"#);
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
        let actual = parse_expression("test", r#"!!0u32"#);
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
        let actual = parse_expression("test", r#"~!0u32"#);
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
        let actual = parse_expression("test", r#"~!~!0u32"#);
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
        let actual = parse_expression("test", r#"#^0 0u32"#);
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
        let actual = parse_expression("test", r#"#^0#^0 0u32"#);
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
        let actual = parse_expression("test", r#"#^1 0u32"#);
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
        let actual = parse_expression("test", r#"#^1#^1 0u32"#);
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
        let actual = parse_expression("test", r#"#^- 0u32"#);
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
        let actual = parse_expression("test", r#"#^-#^- 0u32"#);
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
        let actual = parse_expression("test", r#"#$0 0u32"#);
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
        let actual = parse_expression("test", r#"#$0#$0 0u32"#);
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
        let actual = parse_expression("test", r#"#$1 0u32"#);
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
        let actual = parse_expression("test", r#"#$1#$1 0u32"#);
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
        let actual = parse_expression("test", r#"#0 0u32"#);
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
        let actual = parse_expression("test", r#"#0#0 0u32"#);
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
        let actual = parse_expression("test", r#"#1 0u32"#);
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
        let actual = parse_expression("test", r#"#1#1 0u32"#);
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
        let actual = parse_expression("test", r#"^/ 0u32"#);
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
        let actual = parse_expression("test", r#"^/^/ 0u32"#);
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
        let actual = parse_expression("test", r#"|a, b| { a(b) }"#);
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
        let actual = parse_expression("test", r#"|a, b| { c = |b| { a(b) }; c(b) }"#);
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
        let actual = parse_expression(
            "test",
            r#"|a, b| { /* define c */ c = |b| { a(b) }; /* call c */ c(b) }"#,
        );
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
        let actual = parse_expression("test", r#"|a: Int, b: Int| { a(b) }"#);
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
        let actual = parse_expression("test", r#"|a, b| { a(b); a(b) }"#);
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
        let actual = parse_expression("test", r#"a(b)"#);
        assert_eq!(expected, actual);
    }

    fn format_error(code_map: codespan::CodeMap, error: super::Error) -> String {
        use crate::diagnostic::Diagnostic;

        let mut diagnostics = Vec::new();
        error.to_diagnostics(&mut diagnostics);
        crate::test_util::format_diagnostics(&code_map, &diagnostics)
    }

    fn parse_module(name: &'static str, source: &str) -> Result<ast::Module<()>, String> {
        let mut code_map = codespan::CodeMap::new();
        let span = code_map
            .add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned())
            .span();

        let mut errors = Vec::new();
        let result = crate::parser::tin::ModuleParser::new().parse(span, &mut errors, source);
        super::handle_parse_result(span, result, errors)
            .map(|r| r.map_context(&mut |_| ()))
            .map_err(|e| format_error(code_map, e))
    }

    fn parse_expression(name: &'static str, source: &str) -> Result<ast::Expression<()>, String> {
        let mut code_map = codespan::CodeMap::new();
        let span = code_map
            .add_filemap(codespan::FileName::Virtual(name.into()), source.to_owned())
            .span();

        let mut errors = Vec::new();
        let result = crate::parser::tin::ExpressionParser::new().parse(span, &mut errors, source);
        super::handle_parse_result(span, result, errors)
            .map(|r| r.map_context(&mut |_| ()))
            .map_err(|e| format_error(code_map, e))
    }
}
