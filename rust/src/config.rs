use std::env;
use std::error;

pub fn load_config() -> Result<Config, Box<dyn error::Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return Err(format!("Please provide the name of the source file.\n").into());
    }

    Ok(Config {
        source_file_name: args[1].clone(),
        output_file_name: String::from("output"),
    })
}

pub struct Config {
    pub source_file_name: String,
    pub output_file_name: String,
}
