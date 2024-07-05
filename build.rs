use std::{env, fs::{self, File}, io::Write, path::Path};

/// Generate tests cases for all of the JSON test cases provided in the [JSONTestSuite repositry](https://github.com/nst/JSONTestSuite.git)
fn main() {
    let out = env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out).join("generated_tests.rs");
    let mut f = File::create(&dest).unwrap();

    let test_files = fs::read_dir("./tests/test_data/").unwrap();

    for file in test_files {
        let path = file.unwrap().path();
        let test_name = path.file_stem().unwrap().to_str().unwrap();
        let test_name = test_name.chars().filter_map(|c| match c {
            '-' => Some('_'),
            '+' => Some('p'),
            '.' => Some('d'),
            '#' => None,
            _ => Some(c),
        }).collect::<String>();

        let succeed;
        match test_name.chars().nth(0) {
            Some('n') => succeed = false,
            Some('y') => succeed = true,
            Some('i') => continue,
            Some(_) => return,
            None => return,
        }

        let test_function = format!(
            r#"
#[test]
fn test_{test_name}() {{
    let data = std::fs::read_to_string("{path}").unwrap();
    let value = JSOxideN::from_str(&data);
    if {succeed} {{
        assert!(value.is_ok(), "Failed to parse {path}, expected success but got {{:?}}", value)
    }} else {{
        assert!(value.is_err(), "Failed to parse {path}, expected failure but got {{:?}}", value)
    }}
}}
            "#,
            test_name = test_name,
            path = path.display(),
            succeed = succeed,
        );
        f.write_all(test_function.as_bytes()).unwrap();
    }
}