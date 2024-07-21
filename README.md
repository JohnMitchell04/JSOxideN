# Educational JSON Deserialiser in Rust

**JSOxideN** is a JSON deserialiser written in Rust, created as a personal learning exercise to deepen my understanding of traits, macros, and procedural macros. The deserializer leverages the [`syn`](https://github.com/dtolnay/syn) and `proc_macro` crates to handle the deserialization of any Rust struct from JSON data.

## Learning Objectives

- **Traits**: Implemented the deserialise trait to indicate the ability to construct from JSON. Explored creating traits to work around Rust's Oprhan rules.
- **Macro usage**: Utilized Rust macros to generate code, thereby reducing boilerplate.
- **Procedural macros**: Created procedural macros to automate code generation for users.
- **Syn and proc_macro crates**: Leveraged the `syn` crate for parsing Rust code and the `proc_macro` crate for creating procedural macros.
- **Automatic test generation**: Used `build.rs` to automatically generate tests based on the files provided by the [JSONTestSuite](https://github.com/nst/JSONTestSuite).

## Usage

The library can be used with the inbuilt `Value` type that represents the JSON types, keys of the JSON object can be accessed using the standard index operator and a variety of helpful methods are provided. 

For easier usage the library also allows strong typing through structs, allowing the user to define a struct and automatically derive a deserialisation implementation.

## Example

The provided derive macro can be used to implement deserialisation for structs:

```rust
use jsoxiden::Deserialise;

#[derive(Deserialise)]
struct MyStruct {
    field1: String,
    field2: i32,
}
```

Deserialisation implementations are already provided for all signed and unsigned integers (excluding `usize` and `isize`), the floating point types, characters, strings and vectors.

JSON input can then be deserialised as follows:

```rust
let json_data = r#"
{
    "field1": "value",
    "field2": 42
}
"#;

let my_struct = MyStruct::from_str(json_data).unwrap();
```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

This project represents a personal educational journey into the depths of Rust, pushing the boundaries of my knowledge and skills with the language.
