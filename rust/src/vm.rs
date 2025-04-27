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

pub fn translate_with_program_name(
    program_name: &str,
    input: &str,
) -> Result<String, TranslateError> {
    let parsed_vm_lines: Vec<Command> = input
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<Command>, TranslateError>>()?;

    let assembly_lines: Vec<assemble::AssemblyLine> = parsed_vm_lines
        .into_iter()
        .map(|x| to_assembly(x, program_name))
        .flatten()
        .collect();

    let assembly_code: Vec<String> = assembly_lines
        .into_iter()
        .map(|x| <assemble::AssemblyLine as Into<String>>::into(x))
        .collect();

    Ok(assembly_code.join("\n"))
}

pub fn translate(input: &str) -> Result<String, TranslateError> {
    translate_with_program_name("TODO", input)
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
    let stat = "static";
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
    let temp = "temp";

    let n = offset.parse::<u16>().unwrap();
    let segment = match segment {
        segment if segment == arg => PopArg::MemorySegment(MemorySegment::Arg(n)),
        segment if segment == local => PopArg::MemorySegment(MemorySegment::Local(n)),
        segment if segment == this => PopArg::MemorySegment(MemorySegment::This(n)),
        segment if segment == that => PopArg::MemorySegment(MemorySegment::That(n)),
        segment if segment == pointer => PopArg::Pointer(n),
        segment if segment == temp => PopArg::Temp(n),
        _ => {
            return Err(TranslateError::Invalid);
        }
    };

    Ok(segment)
}

fn to_assembly(command: Command, program_name: &str) -> Vec<assemble::AssemblyLine> {
    match command {
        Command::Push(memory_segment) => push(memory_segment, program_name),
        Command::Pop(memory_segment) => pop(memory_segment),
    }
}

fn push(push_arg: PushArg, program_name: &str) -> Vec<assemble::AssemblyLine> {
    // Determine the instructions to load the data in the D-register.
    let load_instructions = match push_arg {
        PushArg::MemorySegment(segment) => {
            let (symbol, offset) = match segment {
                MemorySegment::Arg(offset) => (ARG, offset),
                MemorySegment::Local(offset) => (LCL, offset),
                MemorySegment::This(offset) => (THIS, offset),
                MemorySegment::That(offset) => (THAT, offset),
            };

            let dereference_without_offset = offset == 0;

            if dereference_without_offset {
                dereference_symbol(symbol)
            } else {
                dereference_symbol_with_offset(symbol, offset)
            }
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
        PushArg::Static(n) => dereference_symbol(&format!("{}.{}", program_name, n)),
        PushArg::Pointer(n) => match n {
            0 => return push(PushArg::MemorySegment(MemorySegment::This(0)), program_name),
            1 => return push(PushArg::MemorySegment(MemorySegment::That(0)), program_name),
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

            dereference_symbol(symbol)
        }
    };

    // Determine the instructions to push the D-register's content onto the stack.
    let push_instructions = push_d_onto_stack();

    load_instructions
        .into_iter()
        .chain(push_instructions.into_iter())
        .collect()
}

fn dereference_symbol(symbol: &str) -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(String::from(symbol)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn dereference_symbol_with_offset(symbol: &str, offset: u16) -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(String::from(symbol)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(offset),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DPlusA,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn push_d_onto_stack() -> Vec<assemble::AssemblyLine> {
    vec![
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
        PopArg::Temp(n) => match n {
            0 => (R5, 0),
            1 => (R6, 0),
            2 => (R7, 0),
            3 => (R8, 0),
            4 => (R9, 0),
            5 => (R10, 0),
            6 => (R11, 0),
            7 => (R12, 0),
            n => panic!("unexpected temp: {}", n),
        },
    };

    let dereference_with_offset = offset != 0;

    let store_in_sp = if dereference_with_offset {
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
        ]
    } else {
        vec![
            // Store value in @symbol in @SP.
            assemble::AssemblyLine::Instruction(assemble::Instruction::A(
                assemble::AInstruction::Symbol(symbol.to_string()),
            )),
            assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
                computation: assemble::Computation::M,
                destination: assemble::Destination::A,
                jump: assemble::Jump::Null,
            })),
            assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
                computation: assemble::Computation::M,
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
        ]
    };

    let load_into_d = vec![
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
    ];

    store_in_sp.into_iter().chain(load_into_d).collect()
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
                expected_assembly: push_dereferenced_symbol_pointer(THIS),
            },
            TestCase {
                command: "push pointer 1".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer(THAT),
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
                expected_assembly: pop_dereferenced_symbol_pointer(THIS),
            },
            TestCase {
                command: "pop pointer 1".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(THAT),
            },
            TestCase {
                command: "pop temp 0".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R5),
            },
            TestCase {
                command: "pop temp 1".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R6),
            },
            TestCase {
                command: "pop temp 2".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R7),
            },
            TestCase {
                command: "pop temp 3".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R8),
            },
            TestCase {
                command: "pop temp 4".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R9),
            },
            TestCase {
                command: "pop temp 5".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R10),
            },
            TestCase {
                command: "pop temp 6".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R11),
            },
            TestCase {
                command: "pop temp 7".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer(R12),
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
    fn test_static() {
        struct TestCase {
            command: String,
            program_name: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            //
            TestCase {
                command: "push static 0".to_string(),
                program_name: "Test".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer("Test.0"),
            },
        ];

        for test_case in test_cases {
            let assembly = translate_with_program_name(&test_case.program_name, &test_case.command)
                .expect(&format!("failed: {}", &test_case.command));

            assert_eq!(
                test_case.expected_assembly, assembly,
                "{} failed: expected {}, got {}",
                test_case.command, test_case.expected_assembly, assembly,
            );
        }
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

    fn pop_dereferenced_symbol_pointer(symbol: &str) -> String {
        format!(
            "@{}
A=M
D=M
@SP
A=M
M=D
@SP
AM=M-1
D=M
A=A+1
A=M
M=D",
            symbol
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
