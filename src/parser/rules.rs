use std::char;

use nom;
use nom_locate;

use ast;
use parser;

pub type Span<'a> = nom_locate::LocatedSpan<nom::types::CompleteStr<'a>>;

named!(pub parse_module(Span) -> ast::Module<parser::Context>,
    do_parse!(
        parse_blank >>
        start: position!() >>
        variables: separated_list!(delimited!(parse_blank, char!(';'), parse_blank), parse_variable) >>
        end: position!() >>
        parse_blank >>
        (ast::Module {
            context: parser::Context::from_located_span(ast::Kind::Module, start, end),
            variables,
        })
    )
);

named!(pub parse_variable(Span) -> ast::Variable<parser::Context>,
    do_parse!(
        start: position!() >>
        name: parse_identifier >>
        parse_blank >>
        char!('=') >>
        parse_blank >>
        initializer: parse_expression >>
        end: position!() >>
        (ast::Variable {
            context: parser::Context::from_located_span(ast::Kind::Variable, start, end),
            name,
            initializer,
        })
    )
);

named!(pub parse_expression(Span) -> ast::Expression<parser::Context>,
    alt!(
        parse_identifier => { ast::Expression::Identifier } |
        parse_number_literal => { ast::Expression::Number } |
        parse_string_literal => { ast::Expression::String } |
        parse_tuple => { ast::Expression::Tuple } |
        parse_record => { ast::Expression::Record } |
        parse_lambda => { ast::Expression::Lambda } |
        parse_select => { ast::Expression::Select } |
        parse_apply => { ast::Expression::Apply }
    )
);

named!(pub parse_identifier(Span) -> ast::Identifier<parser::Context>,
    do_parse!(
        start: position!() >>
        value: recognize!(tuple!(
            take_while_m_n!(1, 1, unicode_xid::UnicodeXID::is_xid_start),
            take_while!(unicode_xid::UnicodeXID::is_xid_continue)
        )) >>
        end: position!() >>
        (ast::Identifier {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            value: (*value.fragment).to_owned(),
        })
    )
);

named!(pub parse_tuple(Span) -> ast::Tuple<parser::Context>,
    do_parse!(
        start: position!() >>
        char!('(') >>
        parse_blank >>
        fields: separated_list!(delimited!(parse_blank, char!(','), parse_blank), parse_expression) >>
        parse_blank >>
        char!(')') >>
        end: position!() >>
        (ast::Tuple {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            fields,
        })
    )
);

named!(pub parse_record(Span) -> ast::Record<parser::Context>,
    do_parse!(
        start: position!() >>
        char!('{') >>
        parse_blank >>
        fields: separated_list!(delimited!(parse_blank, char!(','), parse_blank), parse_field) >>
        parse_blank >>
        char!('}') >>
        end: position!() >>
        (ast::Record {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            fields,
        })
    )
);

named!(pub parse_field(Span) -> (ast::Identifier<parser::Context>, ast::Expression<parser::Context>),
    do_parse!(
        name: parse_identifier >>
        parse_blank >>
        char!(':') >>
        parse_blank >>
        value: parse_expression >>
        ((name, value))
    )
);

named!(pub parse_number_literal(Span) -> ast::NumberLiteral<parser::Context>,
    do_parse!(
        start: position!() >>
        value: recognize!(tuple!(
            parse_sign,
            parse_digits_raw,
            opt!(tuple!(char!('.'), parse_digits_raw)),
            opt!(tuple!(char!('e'), tuple!(parse_sign, parse_digits_raw)))
        )) >>
        sigil: alt!(
            tag!("u8") | tag!("u16") | tag!("u32") | tag!("u64") |
            tag!("i8") | tag!("i16") | tag!("i32") | tag!("i64") |
                                       tag!("f32") | tag!("f64")
        ) >>
        end: position!() >>
        (ast::NumberLiteral {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            value: parse_number_value(value, sigil),
        })
    )
);

named!(parse_sign(Span) -> bool,
    map!(opt!(char!('-')), |o| o.is_some())
);

named!(pub parse_string_literal(Span) -> ast::StringLiteral<parser::Context>,
    do_parse!(
        start: position!() >>
        char!('"') >>
        value: many0!(alt!(
            none_of!("\"\\") |
            preceded!(char!('\\'), alt!(
                value!('"', char!('"')) |
                value!('\\', char!('\\')) |
                value!('/', char!('/')) |
                value!('\u{0008}', char!('b')) |
                value!('\u{000C}', char!('f')) |
                value!('\n', char!('n')) |
                value!('\r', char!('r')) |
                value!('\t', char!('t')) |
                map!(
                    delimited!(tag!("u{"), recognize!(many_m_n!(1, 6, parse_hex_digit)), char!(')')),
                    parse_unicode_char
                )
            ))
        )) >>
        char!('"') >>
        end: position!() >>
        (ast::StringLiteral {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            value: value.into_iter().collect(),
        })
    )
);

named!(pub parse_lambda(Span) -> ast::Lambda<parser::Context>,
    do_parse!(
        start: position!() >>
        char!('|') >>
        parse_blank >>
        parameters: separated_list!(delimited!(parse_blank, char!(','), parse_blank), parse_parameter) >>
        parse_blank >>
        char!('|') >>
        parse_blank >>
        signature: opt!(map!(parse_expression, Box::new)) >>
        parse_blank >>
        char!('{') >>
        parse_blank >>
        statements: many0!(terminated!(parse_statement, delimited!(parse_blank, char!(';'), parse_blank))) >>
        parse_blank >>
        result: map!(parse_expression, Box::new) >>
        parse_blank >>
        char!('}') >>
        end: position!() >>
        (ast::Lambda {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            parameters,
            signature,
            statements,
            result,
        })
    )
);

named!(pub parse_statement(Span) -> ast::Statement<parser::Context>,
    alt!(
        parse_variable => { ast::Statement::Variable } |
        parse_expression => { ast::Statement::Expression }
    )
);

named!(pub parse_select(Span) -> ast::Select<parser::Context>,
    do_parse!(
        start: position!() >>
        record: map!(parse_expression, Box::new) >>
        parse_blank >>
        char!('.') >>
        parse_blank >>
        field: parse_identifier >>
        end: position!() >>
        (ast::Select {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            record,
            field,
        })
    )
);

named!(pub parse_apply(Span) -> ast::Apply<parser::Context>,
    do_parse!(
        start: position!() >>
        function: map!(parse_expression, Box::new) >>
        char!('(') >>
        parse_blank >>
        parameters: separated_list!(delimited!(parse_blank, char!(','), parse_blank), parse_expression) >>
        parse_blank >>
        char!(')') >>
        end: position!() >>
        (ast::Apply {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            function,
            parameters,
        })
    )
);

named!(pub parse_parameter(Span) -> ast::Parameter<parser::Context>,
    do_parse!(
        start: position!() >>
        name: parse_identifier >>
        parse_blank >>
        char!(':') >>
        parse_blank >>
        signature: opt!(parse_expression) >>
        end: position!() >>
        (ast::Parameter {
            context: parser::Context::from_located_span(ast::Kind::Identifier, start, end),
            name,
            signature,
        })
    )
);

named!(parse_blank(Span) -> (),
    value!((), take_while!(|c| c == ' ' || c == '\n'))
);

fn parse_digits_raw(input: Span) -> nom::IResult<Span, Span, u32> {
    use nom::InputTakeAtPosition;
    input.split_at_position1(|c| !c.is_ascii_digit(), nom::ErrorKind::TakeWhile1)
}

fn parse_unicode_char(input: Span) -> char {
    char::from_u32(u32::from_str_radix(*input.fragment, 16).unwrap()).unwrap()
}

fn parse_hex_digit(input: Span) -> nom::IResult<Span, Span, u32> {
    use nom::InputLength;
    use nom::InputTake;

    if input.input_len() > 0 {
        let (c, rest) = input.take_split(1);
        match c.fragment.chars().next().unwrap() {
            '0'...'9' | 'a'...'f' => Ok((c, rest)),
            _ => Err(nom::Err::Error(error_position!(
                input,
                nom::ErrorKind::HexDigit
            ))),
        }
    } else {
        Err(nom::Err::Incomplete(nom::Needed::Size(1)))
    }
}

fn parse_number_value(value: Span, sigil: Span) -> ast::NumberValue {
    match *sigil.fragment {
        "u8" => ast::NumberValue::U8(value.fragment.parse().unwrap()),
        "u16" => ast::NumberValue::U16(value.fragment.parse().unwrap()),
        "u32" => ast::NumberValue::U32(value.fragment.parse().unwrap()),
        "u64" => ast::NumberValue::U64(value.fragment.parse().unwrap()),
        "i8" => ast::NumberValue::I8(value.fragment.parse().unwrap()),
        "i16" => ast::NumberValue::I16(value.fragment.parse().unwrap()),
        "i32" => ast::NumberValue::I32(value.fragment.parse().unwrap()),
        "i64" => ast::NumberValue::I64(value.fragment.parse().unwrap()),
        "f32" => ast::NumberValue::F32(value.fragment.parse().unwrap()),
        "f64" => ast::NumberValue::F64(value.fragment.parse().unwrap()),
        _ => unreachable!(),
    }
}
