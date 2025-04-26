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
    Invalid,
}

impl fmt::Display for TranslateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl error::Error for TranslateError {}

#[derive(Debug)]
enum Command {
    Push(PushArg),
    Pop(MemorySegment),
}

#[derive(Debug)]
enum MemorySegment {
    Local(u16),
    Arg(u16),
}

#[derive(Debug)]
enum PushArg {
    Constant(u16),
    MemorySegment(MemorySegment),
}

fn parse_line(line: &str) -> Result<Command, TranslateError> {
    let push = "push";
    let pop = "pop";

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
        (Some(cmd), Some(segment), Some(offset)) => match cmd {
            cmd if *cmd == push => Command::Push(parse_push_operands(segment, offset)?),
            cmd if *cmd == pop => Command::Pop(parse_pop_operands(segment, offset)?),
            _ => {
                return Err(TranslateError::Invalid);
            }
        },
    };

    Ok(command)
}

fn parse_push_operands(segment: &str, offset: &str) -> Result<PushArg, TranslateError> {
    let constant = "constant";
    let local = "local";
    let arg = "arg";

    let n = offset.parse::<u16>().unwrap();
    let push_operand = match segment {
        segment if segment == local => PushArg::MemorySegment(MemorySegment::Local(n)),
        segment if segment == arg => PushArg::MemorySegment(MemorySegment::Arg(n)),
        segment if segment == constant => PushArg::Constant(n),
        _ => {
            return Err(TranslateError::Invalid);
        }
    };

    Ok(push_operand)
}

fn parse_pop_operands(segment: &str, offset: &str) -> Result<MemorySegment, TranslateError> {
    let local = "local";
    let arg = "arg";

    let n = offset.parse::<u16>().unwrap();
    let segment = match segment {
        segment if segment == local => MemorySegment::Local(n),
        segment if segment == arg => MemorySegment::Arg(n),
        _ => {
            return Err(TranslateError::Invalid);
        }
    };

    Ok(segment)
}

fn to_assembly(command: Command) -> Vec<assemble::AssemblyLine> {
    match command {
        Command::Push(memory_segment) => push(memory_segment),
        Command::Pop(memory_segment) => pop(memory_segment),
    }
}

fn push(push_arg: PushArg) -> Vec<assemble::AssemblyLine> {
    // Determine the instructions to load the data in the D-register.
    let load_instructions = match push_arg {
        PushArg::MemorySegment(segment) => {
            let (segment, offset) = match segment {
                MemorySegment::Local(offset) => (LCL, offset),
                MemorySegment::Arg(offset) => (ARG, offset),
            };

            vec![
                assemble::AssemblyLine::Instruction(assemble::Instruction::A(
                    assemble::AInstruction::Symbol(String::from(segment)),
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::C(
                    assemble::CInstruction {
                        computation: assemble::Computation::M,
                        destination: assemble::Destination::A,
                        jump: assemble::Jump::Null,
                    },
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::C(
                    assemble::CInstruction {
                        computation: assemble::Computation::A,
                        destination: assemble::Destination::D,
                        jump: assemble::Jump::Null,
                    },
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::A(
                    assemble::AInstruction::Address(offset),
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::C(
                    assemble::CInstruction {
                        computation: assemble::Computation::DPlusA,
                        destination: assemble::Destination::A,
                        jump: assemble::Jump::Null,
                    },
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::C(
                    assemble::CInstruction {
                        computation: assemble::Computation::M,
                        destination: assemble::Destination::D,
                        jump: assemble::Jump::Null,
                    },
                )),
            ]
        }
        PushArg::Constant(number) => {
            vec![
                assemble::AssemblyLine::Instruction(assemble::Instruction::A(
                    assemble::AInstruction::Address(number),
                )),
                assemble::AssemblyLine::Instruction(assemble::Instruction::C(
                    assemble::CInstruction {
                        computation: assemble::Computation::A,
                        destination: assemble::Destination::D,
                        jump: assemble::Jump::Null,
                    },
                )),
            ]
        }
    };

    // Determine the instructions to push the D-register's content onto the stack.
    let push_instructions = vec![
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
    ];

    load_instructions
        .into_iter()
        .chain(push_instructions.into_iter())
        .collect()
}

fn pop(memory_segment: MemorySegment) -> Vec<assemble::AssemblyLine> {
    let (symbol, offset) = match memory_segment {
        MemorySegment::Local(offset) => (LCL, offset),
        MemorySegment::Arg(offset) => (ARG, offset),
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
                command: "push arg 3".to_string(),
                expected_assembly: "@ARG
A=M
D=A
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
            },
            TestCase {
                command: "push local 8".to_string(),
                expected_assembly: "@LCL
A=M
D=A
@8
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
            },
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
