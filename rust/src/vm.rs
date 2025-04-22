use core::fmt;
use std::error;

use crate::assemble;

pub fn translate(input: &str) -> Result<String, TranslateError> {
    let parsed_vm_lines: Vec<Command> = input
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<Command>, TranslateError>>()?;

    let assembly_lines: Vec<assemble::AssemblyLine> = parsed_vm_lines
        .into_iter()
        .map(|x| to_assembly(x))
        .flatten()
        .collect();

    let assembly_code: Vec<String> = assembly_lines
        .into_iter()
        .map(|x| <assemble::AssemblyLine as Into<String>>::into(x))
        .collect();

    Ok(assembly_code.join("\n"))
}

#[derive(Debug)]
pub enum TranslateError {}

impl fmt::Display for TranslateError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl error::Error for TranslateError {}

enum Command {
    Push(MemorySegment),
}

enum MemorySegment {
    Constant(u16),
}

fn parse_line(line: &str) -> Result<Command, TranslateError> {
    let command = match line {
        command if command.starts_with("push") => Command::Push(MemorySegment::Constant(0)),
        _ => todo!(),
    };

    Ok(command)
}

fn to_assembly(command: Command) -> Vec<assemble::AssemblyLine> {
    let sp = "SP".to_string();
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(7),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(sp.clone()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::D,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(sp),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MPlusOne,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
    ]
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_push() -> Result<(), Box<dyn error::Error>> {
        struct TestCase {
            name: String,
            vm_commands: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                name: "push constant 7".to_string(),
                vm_commands: "push constant 7".to_string(),
                expected_assembly: "@7
D=A
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
            }, //
        ];

        for test_case in test_cases {
            let assembly = translate(&test_case.vm_commands)?;

            assert_eq!(
                test_case.expected_assembly, assembly,
                "{} failed: expected {}, got {}",
                test_case.name, test_case.expected_assembly, assembly,
            );
        }

        Ok(())
    }

    #[test]
    #[ignore]
    fn test_simple_add() -> Result<(), Box<dyn error::Error>> {
        let input = "
push constant 7
push constant 8
add";
        let expected_output = "
@7
D=A
@SP
A=M
M=D
@SP
M=M+1
@8
D=A
@SP
A=M
M=D
@SP
M=M+1
// TODO add
"; // TODO: implement add
        let output = translate(input.trim())?;

        assert_eq!(
            expected_output, output,
            "{} failed: expected {}, got {}",
            "simple_add", expected_output, output,
        );
        Ok(())
    }
}
