#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate norm;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        use norm::parser::Parse;
        let _ = norm::ast::Module::parse(s);
    }
});
