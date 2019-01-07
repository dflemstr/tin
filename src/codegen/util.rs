use std::collections;

pub fn define_string(
    module: &mut cranelift_module::Module<cranelift_simplejit::SimpleJITBackend>,
    defined_strings: &mut collections::HashMap<String, cranelift_module::DataId>,
    name: &str,
    string: String,
) -> cranelift_module::DataId {
    *defined_strings.entry(name.to_owned()).or_insert_with(|| {
        let data_id = module
            .declare_data(&name, cranelift_module::Linkage::Local, false)
            .unwrap();

        let mut ctx = cranelift_module::DataContext::new();
        ctx.define(string.into_bytes().into_boxed_slice());
        module.define_data(data_id, &ctx).unwrap();

        data_id
    })
}
