#[macro_use]
extern crate afl;
extern crate norm;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            use norm::parser::Parse;
            let _ = norm::ast::Module::parse(s);
        }
    });
}
