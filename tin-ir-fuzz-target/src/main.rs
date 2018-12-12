#[macro_use]
extern crate afl;
extern crate tin_lang;

use std::panic;

fn main() {
    use tin_lang::parser::Parse;
    let parser = panic::AssertUnwindSafe(tin_lang::ast::Module::new_parser());

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            let _ = parser.parse(s);
        }
    });
}
