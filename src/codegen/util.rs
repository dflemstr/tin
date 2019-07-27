use std::collections;

pub fn define_string<B>(
    module: &mut cranelift_module::Module<B>,
    defined_strings: &mut collections::HashMap<String, cranelift_module::DataId>,
    name: &str,
    string: String,
) -> cranelift_module::DataId
where
    B: cranelift_module::Backend,
{
    *defined_strings.entry(name.to_owned()).or_insert_with(|| {
        let data_id = module
            .declare_data(&name, cranelift_module::Linkage::Local, false, None)
            .unwrap();

        let mut ctx = cranelift_module::DataContext::new();
        ctx.define(string.into_bytes().into_boxed_slice());
        module.define_data(data_id, &ctx).unwrap();

        data_id
    })
}
