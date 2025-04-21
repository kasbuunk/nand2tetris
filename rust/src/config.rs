use std::error;

pub fn load_config(args: Vec<String>) -> Result<Config, Box<dyn error::Error>> {
    if args.len() != 1 {
        return Err(format!("Please provide the name of the source file.\n").into());
    }

    let source_file_name = args[0].clone();
    let mut source_iter = source_file_name.split(".");
    let program_name = source_iter.next().unwrap();
    let output_file_name = program_name.to_string();

    Ok(Config {
        source_file_name,
        output_file_name,
    })
}

#[derive(Debug, PartialEq)]
pub struct Config {
    pub source_file_name: String,
    pub output_file_name: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() -> Result<(), Box<dyn error::Error>> {
        struct TestCase {
            name: String,
            args: Vec<String>,
            expected_config: Config,
        }

        let test_cases = vec![TestCase {
            name: "assembly".to_string(),
            args: vec!["Program.asm".to_string()],
            expected_config: Config {
                source_file_name: "Program.asm".to_string(),
                output_file_name: "Program".to_string(),
            },
        }];

        for test_case in test_cases {
            let config = load_config(test_case.args)?;
            assert_eq!(
                test_case.expected_config, config,
                "{} failed: expected {:?}, actual {:?}",
                test_case.name, test_case.expected_config, config,
            );
        }

        Ok(())
    }
}
