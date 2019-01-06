use std::char;
use std::num;

use crate::ast;
use crate::parser;

fn parse_integer_literal<I, P, C, T>(
    span: codespan::ByteSpan,
    input: &str,
    errors: &mut Vec<lalrpop_util::ParseError<usize, T, parser::Error>>,
    suffix_len: usize,
    default: I,
    parser: P,
    ctor: C,
) -> ast::NumberValue
where
    P: FnOnce(&str) -> Result<I, num::ParseIntError>,
    C: FnOnce(I) -> ast::NumberValue,
{
    let input = &input[..input.len() - suffix_len];

    if input.is_empty() {
        ctor(default)
    } else {
        match parser(input) {
            Ok(value) => ctor(value),
            Err(cause) => {
                errors.push(lalrpop_util::ParseError::User {
                    error: parser::Error::IllegalIntLiteral { token: span, cause },
                });
                ast::NumberValue::Invalid
            }
        }
    }
}

fn parse_float_literal<I, P, C, T>(
    span: codespan::ByteSpan,
    input: &str,
    errors: &mut Vec<lalrpop_util::ParseError<usize, T, parser::Error>>,
    suffix_len: usize,
    default: I,
    parser: P,
    ctor: C,
) -> ast::NumberValue
where
    P: FnOnce(&str) -> Result<I, num::ParseFloatError>,
    C: FnOnce(I) -> ast::NumberValue,
{
    let input = &input[..input.len() - suffix_len];

    if input.is_empty() {
        ctor(default)
    } else {
        match parser(input) {
            Ok(value) => ctor(value),
            Err(cause) => {
                errors.push(lalrpop_util::ParseError::User {
                    error: parser::Error::IllegalFloatLiteral { token: span, cause },
                });
                ast::NumberValue::Invalid
            }
        }
    }
}

pub fn parse_number_literal<T>(
    span: codespan::ByteSpan,
    lo: usize,
    hi: usize,
    input: &str,
    errors: &mut Vec<lalrpop_util::ParseError<usize, T, parser::Error>>,
) -> ast::NumberValue {
    use std::str::FromStr;

    let span = span.subspan(
        codespan::ByteOffset(lo as codespan::RawOffset),
        codespan::ByteOffset(hi as codespan::RawOffset),
    );

    if input.ends_with("u8") {
        parse_integer_literal(
            span,
            input,
            errors,
            2,
            0u8,
            u8::from_str,
            ast::NumberValue::U8,
        )
    } else if input.ends_with("u16") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0u16,
            u16::from_str,
            ast::NumberValue::U16,
        )
    } else if input.ends_with("u32") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0u32,
            u32::from_str,
            ast::NumberValue::U32,
        )
    } else if input.ends_with("u64") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0u64,
            u64::from_str,
            ast::NumberValue::U64,
        )
    } else if input.ends_with("i8") {
        parse_integer_literal(
            span,
            input,
            errors,
            2,
            0i8,
            i8::from_str,
            ast::NumberValue::I8,
        )
    } else if input.ends_with("i16") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0i16,
            i16::from_str,
            ast::NumberValue::I16,
        )
    } else if input.ends_with("i32") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0i32,
            i32::from_str,
            ast::NumberValue::I32,
        )
    } else if input.ends_with("i64") {
        parse_integer_literal(
            span,
            input,
            errors,
            3,
            0i64,
            i64::from_str,
            ast::NumberValue::I64,
        )
    } else if input.ends_with("f32") {
        parse_float_literal(
            span,
            input,
            errors,
            3,
            0f32,
            f32::from_str,
            ast::NumberValue::F32,
        )
    } else if input.ends_with("f64") {
        parse_float_literal(
            span,
            input,
            errors,
            3,
            0f64,
            f64::from_str,
            ast::NumberValue::F64,
        )
    } else {
        unreachable!()
    }
}

pub fn parse_escaped_string<T>(
    span: codespan::ByteSpan,
    lo: usize,
    hi: usize,
    input: &str,
    errors: &mut Vec<lalrpop_util::ParseError<usize, T, parser::Error>>,
) -> ast::StringValue {
    let input = &input[1..input.len() - 1];

    let span = span.subspan(
        codespan::ByteOffset(lo as codespan::RawOffset),
        codespan::ByteOffset(hi as codespan::RawOffset),
    );

    if input.contains('\\') {
        enum State {
            Normal,
            Escape,
            Unicode,
        };

        let mut result = String::with_capacity(input.len());
        let mut state = State::Normal;
        let mut unicode_start = codespan::ByteOffset(0);
        let mut unicode: u32 = 0;
        let mut is_invalid = false;

        for (i, c) in input.char_indices() {
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
                    _ => {
                        // Omit +1 here to include the previous \ character
                        let escape_start = codespan::ByteOffset(i as codespan::RawOffset);
                        // The +1 is for the initial quote char
                        let escape_end = codespan::ByteOffset((i + 1) as codespan::RawOffset);
                        errors.push(lalrpop_util::ParseError::User {
                            error: parser::Error::IllegalEscapeSequence {
                                token: span,
                                escape: span.subspan(escape_start, escape_end),
                                bad_escape_char: c,
                            },
                        });
                        state = State::Normal;
                        is_invalid = true;
                    },
                },
                State::Unicode => match c {
                    '{' => {
                        unicode = 0;
                        // The +1 is for the initial quote char
                        unicode_start = codespan::ByteOffset((i + 1) as codespan::RawOffset);
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
                        // Adjust to not include leading {
                        let unicode_start = unicode_start + codespan::ByteOffset(1);
                        // The +1 is for the initial quote char
                        let unicode_end = codespan::ByteOffset((i + 1) as codespan::RawOffset);
                        match char::from_u32(unicode) {
                            Some(c) => {
                                result.push(c);
                                state = State::Normal;
                            }
                            None => {
                                errors.push(lalrpop_util::ParseError::User {
                                    error: parser::Error::IllegalUnicode {
                                        token: span,
                                        escape: span.subspan(unicode_start, unicode_end),
                                        bad_codepoint: unicode,
                                    },
                                });
                                state = State::Normal;
                                is_invalid = true;
                            }
                        }
                    }
                    _ => {
                        // Adjust to include leading \
                        let unicode_start = unicode_start - codespan::ByteOffset(2);
                        // The +1 is for the initial quote char
                        let unicode_end = codespan::ByteOffset((i + 1) as codespan::RawOffset);
                        errors.push(lalrpop_util::ParseError::User {
                            error: parser::Error::UnterminatedUnicodeEscapeSequence {
                                token: span,
                                escape: span.subspan(unicode_start, unicode_end),
                            },
                        });
                        state = State::Normal;
                        is_invalid = true;
                    },
                },
            }
        }
        match state {
            State::Normal => {},
            State::Escape => {
                // Omit +1 here to include the previous \ character
                let escape_start = codespan::ByteOffset(input.len() as codespan::RawOffset);
                // The +1 is for the initial quote char
                let escape_end = codespan::ByteOffset((input.len() + 1) as codespan::RawOffset);
                errors.push(lalrpop_util::ParseError::User {
                    error: parser::Error::IllegalEscapeSequence {
                        token: span,
                        escape: span.subspan(escape_start, escape_end),
                        bad_escape_char: '"',
                    },
                });
                is_invalid = true;
            }
            State::Unicode => {
                // Adjust to include leading \
                let unicode_start = unicode_start - codespan::ByteOffset(2);
                // The +1 is for the initial quote char
                let unicode_end = codespan::ByteOffset((input.len() + 1) as codespan::RawOffset);
                errors.push(lalrpop_util::ParseError::User {
                    error: parser::Error::UnterminatedUnicodeEscapeSequence {
                        token: span,
                        escape: span.subspan(unicode_start, unicode_end),
                    },
                });
                is_invalid = true;
            }
        }

        if is_invalid {
            ast::StringValue::Invalid
        } else {
            ast::StringValue::String(result.to_owned())
        }
    } else {
        ast::StringValue::String(input.to_owned())
    }
}
