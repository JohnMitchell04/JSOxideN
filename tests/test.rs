#[cfg(test)]
mod tests {
    use JSOxideN::from_str;
    use std::fs;

    #[test]
    fn basic() {
        let data = fs::read_to_string("./tests/test_data/html.json").unwrap();
        let value = from_str(&data);

        assert!(value.is_ok());
        print!("{}", value.unwrap());
    }
}