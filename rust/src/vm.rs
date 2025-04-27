use core::fmt;
use std::error;

use crate::assemble;

static SP: &str = "SP";
static ARG: &str = "ARG";
static LCL: &str = "LCL";
static THIS: &str = "THIS";
static THAT: &str = "THAT";
static R5: &str = "R5";
static R6: &str = "R6";
static R7: &str = "R7";
static R8: &str = "R8";
static R9: &str = "R9";
static R10: &str = "R10";
static R11: &str = "R11";
static R12: &str = "R12";

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
    Pop(PopArg),
}

#[derive(Debug)]
enum MemorySegment {
    Arg(u16),
    Local(u16),
    This(u16),
    That(u16),
}

#[derive(Debug)]
enum PushArg {
    Constant(u16),
    Static(u16),
    Pointer(u16),
    Temp(u16),
    MemorySegment(MemorySegment),
}

#[derive(Debug)]
enum PopArg {
    Static(u16),
    Pointer(u16),
    Temp(u16),
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
    let arg = "arg";
    let local = "local";
    let stat = "arg";
    let this = "this";
    let that = "that";
    let pointer = "pointer";
    let temp = "temp";

    let n = offset.parse::<u16>().unwrap();
    let push_operand = match segment {
        segment if segment == constant => PushArg::Constant(n),
        segment if segment == arg => PushArg::MemorySegment(MemorySegment::Arg(n)),
        segment if segment == local => PushArg::MemorySegment(MemorySegment::Local(n)),
        segment if segment == stat => PushArg::Static(n),
        segment if segment == this => PushArg::MemorySegment(MemorySegment::This(n)),
        segment if segment == that => PushArg::MemorySegment(MemorySegment::That(n)),
        segment if segment == pointer => PushArg::Pointer(n),
        segment if segment == temp => PushArg::Temp(n),
        _ => {
            return Err(TranslateError::Invalid);
        }
    };

    Ok(push_operand)
}

fn parse_pop_operands(segment: &str, offset: &str) -> Result<PopArg, TranslateError> {
    let local = "local";
    let arg = "arg";
    let this = "this";
    let that = "that";
    let pointer = "pointer";

    let n = offset.parse::<u16>().unwrap();
    let segment = match segment {
        segment if segment == arg => PopArg::MemorySegment(MemorySegment::Arg(n)),
        segment if segment == local => PopArg::MemorySegment(MemorySegment::Local(n)),
        segment if segment == this => PopArg::MemorySegment(MemorySegment::This(n)),
        segment if segment == that => PopArg::MemorySegment(MemorySegment::That(n)),
        segment if segment == pointer => PopArg::Pointer(n),
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
                MemorySegment::Arg(offset) => (ARG, offset),
                MemorySegment::Local(offset) => (LCL, offset),
                MemorySegment::This(offset) => (THIS, offset),
                MemorySegment::That(offset) => (THAT, offset),
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
        PushArg::Static(_) => todo!(),
        PushArg::Pointer(n) => match n {
            0 => return push(PushArg::MemorySegment(MemorySegment::This(0))),
            1 => return push(PushArg::MemorySegment(MemorySegment::That(0))),
            _ => todo!(), // TODO: handle error.
        },
        PushArg::Temp(offset) => {
            let symbol = match offset {
                0 => R5,
                1 => R6,
                2 => R7,
                3 => R8,
                4 => R9,
                5 => R10,
                6 => R11,
                7 => R12,
                _ => todo!(),
            };

            vec![
                assemble::AssemblyLine::Instruction(assemble::Instruction::A(
                    assemble::AInstruction::Symbol(String::from(symbol)),
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
                        computation: assemble::Computation::M,
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

fn pop(pop_arg: PopArg) -> Vec<assemble::AssemblyLine> {
    let (symbol, offset) = match pop_arg {
        PopArg::MemorySegment(MemorySegment::Arg(offset)) => (ARG, offset),
        PopArg::MemorySegment(MemorySegment::Local(offset)) => (LCL, offset),
        PopArg::MemorySegment(MemorySegment::This(offset)) => (THIS, offset),
        PopArg::MemorySegment(MemorySegment::That(offset)) => (THAT, offset),
        PopArg::Static(_) => todo!(),
        PopArg::Pointer(n) => match n {
            0 => return pop(PopArg::MemorySegment(MemorySegment::This(0))),
            1 => return pop(PopArg::MemorySegment(MemorySegment::That(0))),
            _ => todo!(), // TODO: handle error.
        },
        PopArg::Temp(_) => todo!(),
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
                expected_assembly: push_with_offset(ARG, 3),
            },
            TestCase {
                command: "push local 8".to_string(),
                expected_assembly: push_with_offset(LCL, 8),
            },
            TestCase {
                command: "push this 2".to_string(),
                expected_assembly: push_with_offset(THIS, 2),
            },
            TestCase {
                command: "push that 2".to_string(),
                expected_assembly: push_with_offset(THAT, 2),
            },
            TestCase {
                command: "push pointer 0".to_string(),
                expected_assembly: push_with_offset(THIS, 0),
            },
            TestCase {
                command: "push pointer 1".to_string(),
                expected_assembly: push_with_offset(THAT, 0),
            },
            TestCase {
                command: "push temp 0".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R5),
            },
            TestCase {
                command: "push temp 1".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R6),
            },
            TestCase {
                command: "push temp 2".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R7),
            },
            TestCase {
                command: "push temp 3".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R8),
            },
            TestCase {
                command: "push temp 4".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R9),
            },
            TestCase {
                command: "push temp 5".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R10),
            },
            TestCase {
                command: "push temp 6".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R11),
            },
            TestCase {
                command: "push temp 7".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(R12),
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
                expected_assembly: pop_with_offset(LCL, 2).to_string(),
            },
            TestCase {
                command: "pop arg 1".to_string(),
                expected_assembly: pop_with_offset(ARG, 1).to_string(),
            },
            TestCase {
                command: "pop this 10".to_string(),
                expected_assembly: pop_with_offset(THIS, 10),
            },
            TestCase {
                command: "pop that 8".to_string(),
                expected_assembly: pop_with_offset(THAT, 8),
            },
            TestCase {
                command: "pop pointer 0".to_string(),
                expected_assembly: pop_with_offset(THIS, 0),
            },
            TestCase {
                command: "pop pointer 1".to_string(),
                expected_assembly: pop_with_offset(THAT, 0),
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

    fn push_dereferenced_symbol_pointer(symbol: &str) -> String {
        format!(
            "@{}
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1",
            symbol
        )
    }

    fn push_with_offset(segment: &str, offset: u16) -> String {
        format!(
            "@{}
A=M
D=A
@{}
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1",
            segment, offset
        )
    }

    fn pop_with_offset(segment: &str, offset: u16) -> String {
        format!(
            "@{}
D=A
@{}
D=D+M
@SP
A=M
M=D
@SP
AM=M-1
D=M
A=A+1
A=M
M=D",
            offset, segment
        )
    }
}
