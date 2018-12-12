use std::borrow;
use std::char;

pub fn parse_escaped_string(input: &str) -> borrow::Cow<str> {
    let input = &input[1..input.len() - 1];

    if !input.contains('\\') {
        input.into()
    } else {
        let mut result = String::with_capacity(input.len());
        enum State {
            Tinal,
            Escape,
            Unicode,
        };
        let mut state = State::Tinal;
        let mut unicode: u32 = 0;

        for c in input.chars() {
            match state {
                State::Tinal => {
                    if c == '\\' {
                        state = State::Escape;
                    } else {
                        result.push(c);
                    }
                }
                State::Escape => match c {
                    '"' => {
                        result.push('"');
                        state = State::Tinal
                    }
                    '\\' => {
                        result.push('\\');
                        state = State::Tinal
                    }
                    '/' => {
                        result.push('/');
                        state = State::Tinal
                    }
                    'b' => {
                        result.push('\u{0008}');
                        state = State::Tinal
                    }
                    'f' => {
                        result.push('\u{000C}');
                        state = State::Tinal
                    }
                    'n' => {
                        result.push('\n');
                        state = State::Tinal
                    }
                    'r' => {
                        result.push('\r');
                        state = State::Tinal
                    }
                    't' => {
                        result.push('\t');
                        state = State::Tinal
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
                        state = State::Tinal;
                    }
                    _ => unreachable!(),
                },
            }
        }

        result.into()
    }
}
