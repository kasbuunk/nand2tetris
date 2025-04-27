use std::error;
use std::fmt;

pub fn load_config(args: Vec<String>) -> Result<Config, ConfigError> {
    if args.len() != 1 {
        return Err(ConfigError::MissingSource);
    }

    let source_file_name = args[0].clone();
    let mut source_iter = source_file_name.split(".");
    let program_name = match source_iter.next() {
        None => return Err(ConfigError::InvalidFileName),
        Some(program) if !program.is_ascii() => {
            return Err(ConfigError::InvalidFileName);
        }
        Some(program) => match program.chars().next() {
            None => return Err(ConfigError::InvalidFileName),
            Some(c) if !c.is_ascii_uppercase() => {
                return Err(ConfigError::StartUpperCase);
            }
            _ => program,
        },
    };

    let (file_type, target_extension) = match source_iter.next() {
        None => return Err(ConfigError::MissingExtension),
        Some(extension) => match extension {
            "asm" => (FileType::Assembly, ""),
            "vm" => (FileType::VMTranslate, ".asm"),
            _ => return Err(ConfigError::UnsupportedExtension),
        },
    };

    let output_file_name = format!("{}{}", program_name, target_extension);

    Ok(Config {
        file_type,
        source_file_name,
        output_file_name,
    })
}

#[derive(Debug, PartialEq)]
pub struct Config {
    pub file_type: FileType,
    pub source_file_name: String,
    pub output_file_name: String,
}

impl Config {
    pub fn program_name(&self) -> String {
        self.source_file_name
            .split(".")
            .nth(0)
            .expect("valid loaded programs must have an extension")
            .to_string()
    }
}

#[derive(Debug, PartialEq)]
pub enum FileType {
    Assembly,
    VMTranslate,
}

#[derive(PartialEq, Debug)]
pub enum ConfigError {
    MissingSource,
    InvalidFileName,
    StartUpperCase,
    MissingExtension,
    UnsupportedExtension,
}

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let err = match self {
            ConfigError::MissingSource => "must provide a source file name",
            ConfigError::InvalidFileName => "source file name is invalid",
            ConfigError::StartUpperCase => "source file name must start with upper case letter",
            ConfigError::MissingExtension => "missing file extension",
            ConfigError::UnsupportedExtension => "unsupported file extension",
        };
        write!(f, "config error: {}", err)?;
        Ok(())
    }
}

impl error::Error for ConfigError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() -> Result<(), Box<dyn error::Error>> {
        struct TestCase {
            name: String,
            args: Vec<String>,
            expected_config: Result<Config, ConfigError>,
        }

        let test_cases = vec![
            TestCase {
                name: "assembly".to_string(),
                args: vec!["Program.asm".to_string()],
                expected_config: Ok(Config {
                    file_type: FileType::Assembly,
                    source_file_name: "Program.asm".to_string(),
                    output_file_name: "Program".to_string(),
                }),
            },
            TestCase {
                name: "vm_translate".to_string(),
                args: vec!["Program.vm".to_string()],
                expected_config: Ok(Config {
                    file_type: FileType::VMTranslate,
                    source_file_name: "Program.vm".to_string(),
                    output_file_name: "Program.asm".to_string(),
                }),
            },
            TestCase {
                name: "missing_source".to_string(),
                args: vec![],
                expected_config: Err(ConfigError::MissingSource),
            },
            TestCase {
                name: "start_with_capital_err".to_string(),
                args: vec!["program.asm".to_string()],
                expected_config: Err(ConfigError::StartUpperCase),
            },
            TestCase {
                name: "unsupported_extension".to_string(),
                args: vec!["Program.rs".to_string()],
                expected_config: Err(ConfigError::UnsupportedExtension),
            },
            TestCase {
                name: "only_extension".to_string(),
                args: vec![".asm".to_string()],
                expected_config: Err(ConfigError::InvalidFileName),
            },
            TestCase {
                name: "missing extension".to_string(),
                args: vec!["Program".to_string()],
                expected_config: Err(ConfigError::MissingExtension),
            },
        ];

        for test_case in test_cases {
            let config = load_config(test_case.args);
            assert_eq!(
                test_case.expected_config, config,
                "{} failed: expected {:?}, actual {:?}",
                test_case.name, test_case.expected_config, config,
            );
        }

        Ok(())
    }

    #[test]
    fn test_program_name() -> Result<(), Box<dyn error::Error>> {
        struct TestCase {
            args: Vec<String>,
            expected_program_name: String,
        }
        let test_cases = vec![
            TestCase {
                args: vec!["Program.vm".to_string()],
                expected_program_name: "Program".to_string(),
            },
            TestCase {
                args: vec!["Another.vm".to_string()],
                expected_program_name: "Another".to_string(),
            },
        ];

        for test_case in test_cases {
            let config = load_config(test_case.args)?;
            let program_name = config.program_name();
            assert_eq!(
                test_case.expected_program_name, program_name,
                "failed: expected {:?}, actual {:?}",
                test_case.expected_program_name, program_name,
            );
        }

        Ok(())
    }
}
