use marsc_query_system;

fn main() {
    let type_context = TypeContext::default();
    if let Some(provider) = GLOBAL_PROVIDERS.get_provider::<MyKey, String>("MyProvider") {
        let result = provider(type_context, MyKey);
        println!("{}", result);
    }
}

#[cfg(test)]
mod tests {}
