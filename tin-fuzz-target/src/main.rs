#[macro_use]
extern crate afl;

fn run(data: &[u8]) {
    if let Ok(s) = std::str::from_utf8(data) {
        let mut tin = tin::Tin::new();
        if tin.load("fuzz", s).is_ok() {
            if let Ok(mut module) = tin.compile() {
                if let Some(main) = module.function::<tin::module::Function0<i32>>("main") {
                    let _ = main.call();
                }
            }
        }
    }
}

fn main() {
    fuzz!(|data: &[u8]| {
        run(data);
        ()
    })
}
