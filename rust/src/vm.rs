use core::fmt;
use std::error;

use crate::assemble::{self, AssemblyLine};

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

pub fn translate(program_name: String, input: &str) -> Result<String, TranslateError> {
    let parsed_vm_lines: Vec<Command> = input
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<Command>, TranslateError>>()?;

    let assembly_lines: Vec<assemble::AssemblyLine> = parsed_vm_lines
        .into_iter()
        .map(|x| to_assembly(x, &program_name))
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
    Add,
    Sub,
    Neg,
    And,
    Or,
    Not,
    Eq,
    Gt,
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

fn initial_assembly() -> Vec<AssemblyLine> {
    let label_set_true = assemble::Symbol(String::from("SET_TRUE"));
    let label_set_false = assemble::Symbol(String::from("SET_FALSE"));
    let label_set_sp = String::from("SET_SP");

    vec![
        assemble::AssemblyLine::LabelDeclaration(label_set_true),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MinusOne,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(label_set_sp.clone()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JMP,
        })),
        assemble::AssemblyLine::LabelDeclaration(label_set_false),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(label_set_sp.clone()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JMP,
        })),
        assemble::AssemblyLine::LabelDeclaration(assemble::Symbol(label_set_sp.clone())),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(String::from(SP)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusOne,
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

fn parse_line(line: &str) -> Result<Command, TranslateError> {
    let push = "push";
    let pop = "pop";
    let add = "add";
    let sub = "sub";
    let neg = "neg";
    let and = "and";
    let or = "or";
    let not = "not";
    let eq = "eq";
    let gt = "gt";

    let words: Vec<&str> = line.split(" ").collect();

    let command = match (
        words.iter().next(),
        words.iter().skip(1).next(),
        words.iter().skip(2).next(),
    ) {
        (None, _, _) => {
            return Err(TranslateError::Invalid);
        }
        (Some(cmd), None, None) => match cmd {
            cmd if *cmd == add => Command::Add,
            cmd if *cmd == sub => Command::Sub,
            cmd if *cmd == neg => Command::Neg,
            cmd if *cmd == and => Command::And,
            cmd if *cmd == or => Command::Or,
            cmd if *cmd == not => Command::Not,
            cmd if *cmd == eq => Command::Eq,
            cmd if *cmd == gt => Command::Gt,
            _ => {
                return Err(TranslateError::Invalid);
            }
        },
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
    let this = "this";
    let that = "that";
    let pointer = "pointer";
    let temp = "temp";
    let stat = "static";

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
    let stat = "static";

    let n = offset.parse::<u16>().unwrap();
    let segment = match segment {
        segment if segment == arg => PopArg::MemorySegment(MemorySegment::Arg(n)),
        segment if segment == local => PopArg::MemorySegment(MemorySegment::Local(n)),
        segment if segment == this => PopArg::MemorySegment(MemorySegment::This(n)),
        segment if segment == that => PopArg::MemorySegment(MemorySegment::That(n)),
        segment if segment == pointer => PopArg::Pointer(n),
        segment if segment == temp => PopArg::Temp(n),
        segment if segment == stat => PopArg::Static(n),
        _ => {
            return Err(TranslateError::Invalid);
        }
    };

    Ok(segment)
}

fn to_assembly(command: Command, program_name: &str) -> Vec<assemble::AssemblyLine> {
    match command {
        Command::Push(memory_segment) => push(memory_segment, program_name),
        Command::Pop(memory_segment) => pop(memory_segment, program_name),
        Command::Add => add(),
        Command::Sub => subtract(),
        Command::Neg => negate(),
        Command::And => and(),
        Command::Or => or(),
        Command::Not => not(),
        Command::Eq => eq(),
        Command::Gt => gt(),
    }
}

fn add() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::DPlusM;

    binary_operation(computation)
}

fn subtract() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::DMinusM;

    binary_operation(computation)
}

fn negate() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::MinusM;

    unary_operation(computation)
}

fn not() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::NotM;

    unary_operation(computation)
}

fn and() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::DAndM;

    binary_operation(computation)
}

fn or() -> Vec<assemble::AssemblyLine> {
    let computation = assemble::Computation::DOrM;

    binary_operation(computation)
}

fn eq() -> Vec<assemble::AssemblyLine> {
    let pop_and_load_instructions = pop_and_load();

    let equality = vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DMinusM,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DPlusOne,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::One,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DAndM,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
    ];

    pop_and_load_instructions
        .into_iter()
        .chain(equality)
        .collect()
}

fn gt() -> Vec<assemble::AssemblyLine> {
    let pop_and_load_instructions = pop_and_load();

    let greater_than = vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusD,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol("SET_TRUE".to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::D,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JGT,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol("SET_FALSE".to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JMP,
        })),
    ];

    pop_and_load_instructions
        .into_iter()
        .chain(greater_than)
        .collect()
}

fn binary_operation(computation: assemble::Computation) -> Vec<assemble::AssemblyLine> {
    let pop_and_load_instructions = pop_and_load();

    let operation = vec![assemble::AssemblyLine::Instruction(
        assemble::Instruction::C(assemble::CInstruction {
            computation,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        }),
    )];

    pop_and_load_instructions
        .into_iter()
        .chain(operation)
        .collect()
}

fn unary_operation(computation: assemble::Computation) -> Vec<assemble::AssemblyLine> {
    let to_stack_top = goto_stack_top();

    let operation = vec![assemble::AssemblyLine::Instruction(
        assemble::Instruction::C(assemble::CInstruction {
            computation,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        }),
    )];

    to_stack_top.into_iter().chain(operation).collect()
}

// Pops the top of the stack into D, decrements the stack pointer, and loads the address of
// the new stack top.
fn pop_and_load() -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusOne,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
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
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::AMinusOne,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn goto_stack_top() -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
    ]
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
                load_symbol(symbol)
            } else {
                load_symbol_with_offset(symbol, offset)
            }
        }
        PushArg::Constant(number) => load_constant(number),
        PushArg::Static(n) => load_symbol(&format!("{}.{}", program_name, n)),
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

            load_symbol(symbol)
        }
    };

    // Determine the instructions to push the D-register's content onto the stack.
    let push_instructions = push_to_stack();

    load_instructions
        .into_iter()
        .chain(push_instructions.into_iter())
        .collect()
}

fn pop(pop_arg: PopArg, program_name: &str) -> Vec<assemble::AssemblyLine> {
    let (symbol, offset) = match pop_arg {
        PopArg::MemorySegment(MemorySegment::Arg(offset)) => (ARG.to_string(), offset),
        PopArg::MemorySegment(MemorySegment::Local(offset)) => (LCL.to_string(), offset),
        PopArg::MemorySegment(MemorySegment::This(offset)) => (THIS.to_string(), offset),
        PopArg::MemorySegment(MemorySegment::That(offset)) => (THAT.to_string(), offset),
        PopArg::Static(n) => (format!("{}.{}", program_name, n), 0),
        PopArg::Pointer(n) => match n {
            0 => return pop(PopArg::MemorySegment(MemorySegment::This(0)), program_name),
            1 => return pop(PopArg::MemorySegment(MemorySegment::That(0)), program_name),
            _ => todo!(), // TODO: handle error.
        },
        PopArg::Temp(n) => match n {
            0 => (R5.to_string(), 0),
            1 => (R6.to_string(), 0),
            2 => (R7.to_string(), 0),
            3 => (R8.to_string(), 0),
            4 => (R9.to_string(), 0),
            5 => (R10.to_string(), 0),
            6 => (R11.to_string(), 0),
            7 => (R12.to_string(), 0),
            n => panic!("unexpected temp: {}", n),
        },
    };

    let dereference_with_offset = offset != 0;

    let store_in_sp = if dereference_with_offset {
        store_temporarily_to_stack_with_offset(&symbol, offset)
    } else {
        store_temporarily_to_stack(&symbol)
    };

    let load_into_d = pop_stack();

    store_in_sp.into_iter().chain(load_into_d).collect()
}

fn store_temporarily_to_stack_with_offset(
    symbol: &str,
    offset: u16,
) -> Vec<assemble::AssemblyLine> {
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
}

fn store_temporarily_to_stack(symbol: &str) -> Vec<assemble::AssemblyLine> {
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
}

fn pop_stack() -> Vec<assemble::AssemblyLine> {
    vec![
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

fn load_symbol(symbol: &str) -> Vec<assemble::AssemblyLine> {
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

fn load_symbol_with_offset(symbol: &str, offset: u16) -> Vec<assemble::AssemblyLine> {
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

fn load_constant(n: u16) -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(n),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn push_to_stack() -> Vec<assemble::AssemblyLine> {
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
            let assembly = translate("Test".to_string(), &test_case.command)
                .expect(&format!("failed: {}", &test_case.command));

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
            TestCase {
                command: "push static 0".to_string(),
                program_name: "Test".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer("Test.0"),
            },
            TestCase {
                command: "push static 1".to_string(),
                program_name: "MyProgram".to_string(),
                expected_assembly: push_dereferenced_symbol_pointer("MyProgram.1"),
            },
            TestCase {
                command: "pop static 2".to_string(),
                program_name: "Test".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer("Test.2"),
            },
            TestCase {
                command: "pop static 3".to_string(),
                program_name: "MyProgram".to_string(),
                expected_assembly: pop_dereferenced_symbol_pointer("MyProgram.3"),
            },
        ];

        for test_case in test_cases {
            let assembly = translate(test_case.program_name, &test_case.command)
                .expect(&format!("failed: {}", &test_case.command));

            assert_eq!(
                test_case.expected_assembly, assembly,
                "{} failed: expected {}, got {}",
                test_case.command, test_case.expected_assembly, assembly,
            );
        }
    }

    #[test]
    fn test_arithmetic_logic() {
        struct TestCase {
            command: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                command: "add".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
M=D+M"
                    .to_string(),
            },
            TestCase {
                command: "sub".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
M=D-M"
                    .to_string(),
            },
            TestCase {
                command: "neg".to_string(),
                expected_assembly: "@SP
A=M
M=-M"
                    .to_string(),
            },
            TestCase {
                command: "eq".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
D=D-M
D=D+1
M=1
M=D&M"
                    .to_string(),
            },
            TestCase {
                command: "gt".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
D=M-D
@SET_TRUE
D;JGT
@SET_FALSE
0;JMP"
                    .to_string(),
            },
            TestCase {
                command: "and".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
M=D&M"
                    .to_string(),
            },
            TestCase {
                command: "or".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
M=D|M"
                    .to_string(),
            },
            TestCase {
                command: "not".to_string(),
                expected_assembly: "@SP
A=M
M=!M"
                    .to_string(),
            },
        ];

        for test_case in test_cases {
            let assembly = translate("Test".to_string(), &test_case.command)
                .expect(&format!("failed: {}", &test_case.command));

            assert_eq!(
                test_case.expected_assembly, assembly,
                "{} failed: expected {}, got {}",
                test_case.command, test_case.expected_assembly, assembly,
            );
        }
    }

    #[test]
    fn test_initial_assembly() {
        let expected_assembly = "(SET_TRUE)
D=-1
@SET_SP
0;JMP
(SET_FALSE)
D=0
@SET_SP
0;JMP
(SET_SP)
@SP
A=M-1
M=D";

        let assembly = initial_assembly()
            .into_iter()
            .map(|x| x.into())
            .collect::<Vec<String>>()
            .join("\n");

        assert_eq!(
            expected_assembly, assembly,
            "expected {}, got {}",
            expected_assembly, assembly,
        );
    }

    #[test]
    fn test_simple_add() -> Result<(), Box<dyn error::Error>> {
        let input = "
push constant 7
push constant 8
add";
        let expected_output = "@7
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
@SP
M=M-1
A=M
D=M
A=A-1
M=D+M";
        let output = translate("SimpleAdd".to_string(), input.trim())?;

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
