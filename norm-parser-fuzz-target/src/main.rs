#[macro_use]
extern crate afl;
extern crate norm;

use std::panic;

fn main() {
    use norm::parser::Parse;
    let parser = panic::AssertUnwindSafe(norm::ast::Module::new_parser());

    fuzz!(|data: &[u8]| if let Ok(s) = std::str::from_utf8(data) {
        let _ = parser.parse(s);
    });
}
