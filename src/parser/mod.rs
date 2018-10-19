lalrpop_mod!(#[allow(dead_code)] norm);

use std::borrow;
use std::char;

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

#[cfg(test)]
mod tests {
    use std::borrow;

    use super::norm;
    use ast;

    #[test]
    fn e2e() {
        let actual = norm::NormParser::new().parse(r#"
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
"#);
        assert!(actual.is_ok());
    }

    #[test]
    fn identifier() {
        let expected = Ok(ast::Identifier("whatever".into()));
        let actual = norm::IdentifierParser::new().parse(r#"whatever"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn identifier_unicode() {
        let expected = Ok(ast::Identifier("なんでも".into()));
        let actual = norm::IdentifierParser::new().parse(r#"なんでも"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number() {
        let expected = Ok(ast::Expression::Number(1.0));
        let actual = norm::ExpressionParser::new().parse(r#"1"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_negative() {
        let expected = Ok(ast::Expression::Number(-1.0));
        let actual = norm::ExpressionParser::new().parse(r#"-1"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero() {
        let expected = Ok(ast::Expression::Number(0.0));
        let actual = norm::ExpressionParser::new().parse(r#"0"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_zero_negative() {
        let expected = Ok(ast::Expression::Number(-0.0));
        let actual = norm::ExpressionParser::new().parse(r#"-0"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction() {
        let expected = Ok(ast::Expression::Number(1.5));
        let actual = norm::ExpressionParser::new().parse(r#"1.5"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_fraction_negative() {
        let expected = Ok(ast::Expression::Number(-1.5));
        let actual = norm::ExpressionParser::new().parse(r#"-1.5"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific() {
        let expected = Ok(ast::Expression::Number(1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_negative() {
        let expected = Ok(ast::Expression::Number(-1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"-1.5000e3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase() {
        let expected = Ok(ast::Expression::Number(1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000E3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_uppercase_negative() {
        let expected = Ok(ast::Expression::Number(-1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"-1.5000E3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_small() {
        let expected = Ok(ast::Expression::Number(0.0015));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e-3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_padded_exponent() {
        let expected = Ok(ast::Expression::Number(1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e0003"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit() {
        let expected = Ok(ast::Expression::Number(1500.0));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e+3"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_zero_exponent() {
        let expected = Ok(ast::Expression::Number(1.5));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e0"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn number_scientific_explicit_zero_exponent() {
        let expected = Ok(ast::Expression::Number(1.5));
        let actual = norm::ExpressionParser::new().parse(r#"1.5000e+0"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string() {
        let expected = Ok(borrow::Cow::Borrowed("abc"));
        let actual = norm::StringParser::new().parse(r#""abc""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_unicode() {
        let expected = Ok(borrow::Cow::Borrowed("なんでも"));
        let actual = norm::StringParser::new().parse(r#""なんでも""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn string_escape() {
        let expected = Ok(borrow::Cow::Owned(
            "\"\\/\u{0008}\u{000C}\n\r\t\u{1234}".to_owned(),
        ));
        let actual = norm::StringParser::new().parse(r#""\"\\/\b\f\n\r\t\u{1234}""#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple() {
        let expected = Ok(ast::Tuple {
            fields: vec![ast::Expression::Number(1.0), ast::Expression::Number(2.0)],
        });
        let actual = norm::TupleParser::new().parse(r#"(1, 2)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_trailing_comma() {
        let expected = Ok(ast::Tuple {
            fields: vec![ast::Expression::Number(1.0), ast::Expression::Number(2.0)],
        });
        let actual = norm::TupleParser::new().parse(r#"(1, 2,)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_single() {
        let expected = Ok(ast::Tuple {
            fields: vec![ast::Expression::Number(1.0)],
        });
        let actual = norm::TupleParser::new().parse(r#"(1)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_single_trailing_comma() {
        let expected = Ok(ast::Tuple {
            fields: vec![ast::Expression::Number(1.0)],
        });
        let actual = norm::TupleParser::new().parse(r#"(1,)"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty() {
        let expected = Ok(ast::Tuple { fields: vec![] });
        let actual = norm::TupleParser::new().parse(r#"()"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tuple_empty_no_comma() {
        let actual = norm::TupleParser::new().parse(r#"(,)"#);
        assert!(actual.is_err());
    }

    #[test]
    fn record() {
        let expected = Ok(ast::Record {
            fields: vec![
                (ast::Identifier("a".into()), ast::Expression::Number(1.0)),
                (
                    ast::Identifier("b".into()),
                    ast::Expression::String("c".into()),
                ),
            ].into_iter()
                .collect(),
        });
        let actual = norm::RecordParser::new().parse(r#"{a: 1, b: "c"}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_trailing_comma() {
        let expected = Ok(ast::Record {
            fields: vec![
                (ast::Identifier("a".into()), ast::Expression::Number(1.0)),
                (
                    ast::Identifier("b".into()),
                    ast::Expression::String("c".into()),
                ),
            ].into_iter()
                .collect(),
        });
        let actual = norm::RecordParser::new().parse(r#"{a: 1, b: "c",}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single() {
        let expected = Ok(ast::Record {
            fields: vec![(ast::Identifier("a".into()), ast::Expression::Number(1.0))]
                .into_iter()
                .collect(),
        });
        let actual = norm::RecordParser::new().parse(r#"{a: 1}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_single_trailing_comma() {
        let expected = Ok(ast::Record {
            fields: vec![(ast::Identifier("a".into()), ast::Expression::Number(1.0))]
                .into_iter()
                .collect(),
        });
        let actual = norm::RecordParser::new().parse(r#"{a: 1,}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty() {
        let expected = Ok(ast::Record {
            fields: vec![].into_iter().collect(),
        });
        let actual = norm::RecordParser::new().parse(r#"{}"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn record_empty_no_trailing_comma() {
        let actual = norm::RecordParser::new().parse(r#"{,}"#);
        assert!(actual.is_err());
    }

    #[test]
    fn lambda() {
        let expected = Ok(ast::Lambda {
            parameters: vec![
                ast::Parameter {
                    name: ast::Identifier("a".into()),
                    signature: None,
                },
                ast::Parameter {
                    name: ast::Identifier("b".into()),
                    signature: None,
                },
            ],
            statements: vec![ast::Statement::Expression(ast::Expression::Apply(
                ast::Apply {
                    function: Box::new(ast::Expression::Identifier(ast::Identifier("a".into()))),
                    parameters: vec![ast::Expression::Identifier(ast::Identifier("b".into()))],
                },
            ))],
        });
        let actual = norm::LambdaParser::new().parse(r#"|a, b| { a(b) }"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_signature() {
        let expected = Ok(ast::Lambda {
            parameters: vec![
                ast::Parameter {
                    name: ast::Identifier("a".into()),
                    signature: Some(ast::Expression::Identifier(ast::Identifier("Int".into()))),
                },
                ast::Parameter {
                    name: ast::Identifier("b".into()),
                    signature: Some(ast::Expression::Identifier(ast::Identifier("Int".into()))),
                },
            ],
            statements: vec![ast::Statement::Expression(ast::Expression::Apply(
                ast::Apply {
                    function: Box::new(ast::Expression::Identifier(ast::Identifier("a".into()))),
                    parameters: vec![ast::Expression::Identifier(ast::Identifier("b".into()))],
                },
            ))],
        });
        let actual = norm::LambdaParser::new().parse(r#"|a: Int, b: Int| { a(b) }"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn lambda_many_statements() {
        let expected = Ok(ast::Lambda {
            parameters: vec![
                ast::Parameter {
                    name: ast::Identifier("a".into()),
                    signature: None,
                },
                ast::Parameter {
                    name: ast::Identifier("b".into()),
                    signature: None,
                },
            ],
            statements: vec![
                ast::Statement::Expression(ast::Expression::Apply(
                    ast::Apply {
                        function: Box::new(ast::Expression::Identifier(ast::Identifier("a".into()))),
                        parameters: vec![ast::Expression::Identifier(ast::Identifier("b".into()))],
                    },
                )),
                ast::Statement::Expression(ast::Expression::Apply(
                    ast::Apply {
                        function: Box::new(ast::Expression::Identifier(ast::Identifier("a".into()))),
                        parameters: vec![ast::Expression::Identifier(ast::Identifier("b".into()))],
                    },
                )),
            ],
        });
        let actual = norm::LambdaParser::new().parse(r#"|a, b| { a(b); a(b) }"#);
        assert_eq!(expected, actual);
    }

    #[test]
    fn apply() {
        let expected = Ok(ast::Apply {
            function: Box::new(ast::Expression::Identifier(ast::Identifier("a".into()))),
            parameters: vec![ast::Expression::Identifier(ast::Identifier("b".into()))],
        });
        let actual = norm::ApplyParser::new().parse(r#"a(b)"#);
        assert_eq!(expected, actual);
    }

}
