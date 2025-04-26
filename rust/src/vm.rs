use core::fmt;
use std::error;

use crate::assemble;

static SP: &str = "SP";
static LCL: &str = "LCL";
static ARG: &str = "ARG";

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
pub enum TranslateError {
    EmptyLine,
    Invalid,
}

impl fmt::Display for TranslateError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl error::Error for TranslateError {}

#[derive(Debug)]
enum Command {
    Push(MemorySegment),
    Pop(MemorySegment),
}

#[derive(Debug)]
enum MemorySegment {
    // TODO: should not contain constant, this is only for push command.
    Constant(u16),
    Local(u16),
    Arg(u16),
}

fn parse_line(line: &str) -> Result<Command, TranslateError> {
    let push = "push";
    let pop = "pop";
    let constant = "constant";
    let local = "local";
    let arg = "arg";

    let words: Vec<&str> = line.split(" ").collect();

    let command = match (
        words.iter().next(),
        words.iter().skip(1).next(),
        words.iter().skip(2).next(),
    ) {
        (None, _, _) => {
            return Err(TranslateError::Invalid);
        }
        (_, None, _) => {
            return Err(TranslateError::Invalid);
        }
        (_, _, None) => {
            return Err(TranslateError::Invalid);
        }
        (Some(cmd), Some(segment), Some(offset)) => {
            let n = offset.parse::<u16>().unwrap();
            let segment = match segment {
                segment if *segment == local => MemorySegment::Local(n),
                segment if *segment == arg => MemorySegment::Arg(n),
                segment if *segment == constant => MemorySegment::Constant(n),
                _ => {
                    return Err(TranslateError::Invalid);
                }
            };
            match cmd {
                cmd if *cmd == push => Command::Push(segment),
                cmd if *cmd == pop => Command::Pop(segment),
                _ => {
                    return Err(TranslateError::Invalid);
                }
            }
        }
    };

    Ok(command)
}

fn to_assembly(command: Command) -> Vec<assemble::AssemblyLine> {
    match command {
        Command::Push(memory_segment) => push(memory_segment),
        Command::Pop(memory_segment) => pop(memory_segment),
        s => panic!("{:?}", s),
    }
}

fn push(memory_segment: MemorySegment) -> Vec<assemble::AssemblyLine> {
    let x = match memory_segment {
        MemorySegment::Constant(n) => n,
        MemorySegment::Local(n) => n,
        MemorySegment::Arg(n) => n,
    };

    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(x),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
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
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MPlusOne,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn pop(memory_segment: MemorySegment) -> Vec<assemble::AssemblyLine> {
    let (symbol, offset) = match memory_segment {
        MemorySegment::Local(offset) => (LCL, offset),
        MemorySegment::Arg(offset) => (ARG, offset),
        MemorySegment::Constant(_) => panic!("unexpected constant pop"),
    };

    vec![
        // Store address symbol[offset] in @SP.
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(offset),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(symbol.to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DPlusM,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
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
        // Store popped value in D.
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusOne,
            destination: assemble::Destination::AM,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::APlusOne,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        // Store D in symbol[offset].
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
    ]
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_push() -> Result<(), Box<dyn error::Error>> {
        struct TestCase {
            command: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                command: "push constant 7".to_string(),
                expected_assembly: "@7
D=A
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
            },
            TestCase {
                command: "pop local 2".to_string(),
                expected_assembly: "@2
D=A
@LCL
D=D+M
@SP
A=M
M=D
@SP
AM=M-1
D=M
A=A+1
A=M
M=D"
                .to_string(),
            },
            TestCase {
                command: "pop arg 1".to_string(),
                expected_assembly: "@1
D=A
@ARG
D=D+M
@SP
A=M
M=D
@SP
AM=M-1
D=M
A=A+1
A=M
M=D"
                .to_string(),
            },
        ];

        for test_case in test_cases {
            let assembly =
                translate(&test_case.command).expect(&format!("failed: {}", &test_case.command));

            assert_eq!(
                test_case.expected_assembly, assembly,
                "{} failed: expected {}, got {}",
                test_case.command, test_case.expected_assembly, assembly,
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
