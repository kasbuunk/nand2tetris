use std::env;
use std::error;
use std::fs;
use std::io::Write;

mod assemble;
mod config;

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = env::args().skip(1).collect();

    let config = config::load_config(args)?;

    let input = read_source_file(&config.source_file_name)?;

    let output = assemble::assemble(&input)?;

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
