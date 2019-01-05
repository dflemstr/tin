use std::borrow;
use std::char;

pub fn parse_escaped_string(input: &str) -> borrow::Cow<str> {
    let input = &input[1..input.len() - 1];

    if input.contains('\\') {
        enum State {
            Normal,
            Escape,
            Unicode,
        };

        let mut result = String::with_capacity(input.len());
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
    } else {
        input.into()
    }
}
