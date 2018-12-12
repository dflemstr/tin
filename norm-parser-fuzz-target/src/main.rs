#[macro_use]
extern crate afl;
extern crate norm;

use std::panic;

fn main() {
    use norm::parser::Parse;
    let parser = panic::AssertUnwindSafe(norm::ast::Module::new_parser());

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            if let Ok(module) = parser.parse(s) {
                let mut ir = norm::ir::Ir::new();
                ir.module(&module).unwrap();
                ir.check_types();
            }
        }
    });
}
