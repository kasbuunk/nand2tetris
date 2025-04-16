use std::env;
use std::error;
use std::fs;
use std::io::Write;

mod parse;

fn main() -> Result<(), Box<dyn error::Error>> {
    let config = load_config()?;

    let input = read_source_file(&config.source_file_name)?;

    let output = parse::assemble(&input).unwrap();

    write_output_file(&config.output_file_name, output.as_bytes())?;

    Ok(())
}

fn read_source_file(name: &str) -> Result<String, Box<dyn error::Error>> {
    let bytes = fs::read(name)?;

    let contents = String::from_utf8(bytes)?;

    Ok(contents)
}

fn write_output_file(path: &str, content: &[u8]) -> Result<(), Box<dyn error::Error>> {
    let mut output_file = fs::File::create(path)?;

    output_file.write_all(content)?;

    Ok(())
}

fn load_config() -> Result<Config, Box<dyn error::Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return Err(format!("Please provide the name of the source file.\n").into());
    }

    Ok(Config {
        source_file_name: args[1].clone(),
        output_file_name: String::from("output"),
    })
}

struct Config {
    source_file_name: String,
    output_file_name: String,
}
