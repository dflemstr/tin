#[macro_use]
extern crate afl;
extern crate tin;

use std::panic;

fn main() {
    use tin::parser::Parse;
    let parser = panic::AssertUnwindSafe(tin::ast::Module::new_parser());

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            let _ = parser.parse(s);
        }
    });
}
