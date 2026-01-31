#[test]
fn map() {
    #[derive(serde::Deserialize, Debug, PartialEq)]
    struct Config {
        name: String,
    }

    let config: Config = kisu::from_str(r#"{ name = "Joe"; }"#).unwrap();

    assert_eq!(
        config,
        Config {
            name: "Joe".to_string()
        }
    )
}

#[test]
fn number() {
    let config: f32 = kisu::from_str(r#" 10.0 "#).unwrap();

    assert_eq!(config, 10.0)
}

#[test]
fn string() {
    let config: String = kisu::from_str(r#" "Hello" + " world!" "#).unwrap();

    assert_eq!(config, "Hello world!".to_string())
}
