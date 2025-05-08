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

static SET_TRUE: &str = "SET_TRUE";
static SET_SP: &str = "SET_SP";

pub fn translate(program_name: String, input: &str) -> Result<String, TranslateError> {
    let parsed_vm_lines: Vec<Command> = input
        .lines()
        .map(parse_line)
        .collect::<Result<Vec<Command>, TranslateError>>()?;

    let initial_function = None;
    let nth_call_offset = 0;

    let assembly_lines: Vec<assemble::AssemblyLine> = parsed_vm_lines
        .into_iter()
        .scan(initial_function, |function_name, command| {
            match command {
                Command::Function {
                    ref fn_name,
                    num_arguments: _,
                } => {
                    *function_name = Some(fn_name.clone());
                }
                // Other commands don't change the calling function context.
                _ => (),
            };

            let line = to_assembly(command, &program_name, function_name, nth_call_offset);

            Some(line)
        })
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
    Lt,
    Label(assemble::Symbol),
    Goto(assemble::Symbol),
    IfGoto(assemble::Symbol),
    Function { fn_name: String, num_arguments: u16 },
    Call { callee: String, num_arguments: u16 },
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
    let words: Vec<&str> = line.split(" ").collect();

    let command = match (words.get(0), words.get(1), words.get(2)) {
        (None, _, _) => {
            return Err(TranslateError::Invalid);
        }
        (Some(cmd), None, None) => match *cmd {
            "add" => Command::Add,
            "sub" => Command::Sub,
            "neg" => Command::Neg,
            "and" => Command::And,
            "or" => Command::Or,
            "not" => Command::Not,
            "eq" => Command::Eq,
            "gt" => Command::Gt,
            "lt" => Command::Lt,
            _ => {
                return Err(TranslateError::Invalid);
            }
        },
        (_, None, _) => {
            return Err(TranslateError::Invalid);
        }
        (Some(&"label"), Some(label), None) => {
            Command::Label(assemble::Symbol(String::from(*label)))
        }
        (Some(&"goto"), Some(label), None) => Command::Goto(assemble::Symbol(String::from(*label))),
        (Some(&"if-goto"), Some(label), None) => {
            Command::IfGoto(assemble::Symbol(String::from(*label)))
        }
        (_, _, None) => {
            return Err(TranslateError::Invalid);
        }
        (Some(cmd), Some(arg1), Some(arg2)) => match *cmd {
            "push" => Command::Push(parse_push_operands(arg1, arg2)?),
            "pop" => Command::Pop(parse_pop_operands(arg1, arg2)?),
            "function" => Command::Function {
                fn_name: arg1.to_string(),
                num_arguments: arg2.parse::<u16>().map_err(|_| TranslateError::Invalid)?,
            },
            "call" => Command::Call {
                callee: arg1.to_string(),
                num_arguments: arg2.parse::<u16>().map_err(|_| TranslateError::Invalid)?,
            },
            _ => {
                return Err(TranslateError::Invalid);
            }
        },
    };

    Ok(command)
}

fn parse_push_operands(segment: &str, offset: &str) -> Result<PushArg, TranslateError> {
    let n = offset.parse::<u16>().unwrap();
    let push_operand = match segment {
        "constant" => PushArg::Constant(n),
        "arg" => PushArg::MemorySegment(MemorySegment::Arg(n)),
        "local" => PushArg::MemorySegment(MemorySegment::Local(n)),
        "static" => PushArg::Static(n),
        "this" => PushArg::MemorySegment(MemorySegment::This(n)),
        "that" => PushArg::MemorySegment(MemorySegment::That(n)),
        "pointer" => PushArg::Pointer(n),
        "temp" => PushArg::Temp(n),
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

fn to_assembly(
    command: Command,
    program_name: &str,
    caller: &Option<String>,
    nth_call_offset: u16,
) -> Vec<assemble::AssemblyLine> {
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
        Command::Lt => lt(),
        Command::Label(symbol) => label(symbol, program_name),
        Command::Goto(symbol) => goto(symbol, program_name),
        Command::IfGoto(symbol) => if_goto(symbol, program_name),
        Command::Function {
            fn_name: name,
            num_arguments,
        } => function(program_name, name, num_arguments),
        Command::Call {
            callee,
            num_arguments,
        } => call(
            program_name,
            caller
                .clone()
                .expect("function call should reside in a function scope"),
            callee,
            nth_call_offset,
            num_arguments,
        ),
    }
}

fn function(
    program_name: &str,
    function_name: String,
    num_arguments: u16,
) -> Vec<assemble::AssemblyLine> {
    let label = assemble::AssemblyLine::LabelDeclaration(assemble::Symbol(format!(
        "{}.{}",
        program_name, function_name
    )));

    let push0 = push(PushArg::Constant(0), program_name);

    let push_instructions = std::iter::repeat_n(push0, num_arguments.into())
        .into_iter()
        .flatten();

    std::iter::once(label).chain(push_instructions).collect()
}

fn call(
    program_name: &str,
    caller: String,
    callee: String,
    nth_call_offset: u16,
    num_arguments: u16,
) -> Vec<assemble::AssemblyLine> {
    let return_address = format!("{}.{}$ret{}", program_name, caller, nth_call_offset);

    // Save return address on stack.
    let save_return_address = push_address_to_stack(return_address.clone());

    // Save LCL on stack.
    let save_lcl = push_address_to_stack(String::from(LCL));
    // Save ARG on stack.
    let save_arg = push_address_to_stack(String::from(ARG));
    // Save THIS on stack.
    let save_this = push_address_to_stack(String::from(THIS));
    // Save THAT on stack.
    let save_that = push_address_to_stack(String::from(THAT));

    // ARG = SP - 5 - nArgs
    let set_arg = set_arg_for_call(num_arguments);

    // LCL = SP
    let set_lcl = set_lcl_for_call();

    // goto f
    let goto_fn = goto(assemble::Symbol(callee), program_name);

    // (return address)
    let label_return_address = vec![assemble::AssemblyLine::LabelDeclaration(assemble::Symbol(
        return_address,
    ))];

    save_return_address
        .into_iter()
        .chain(save_lcl.into_iter())
        .chain(save_arg.into_iter())
        .chain(save_this.into_iter())
        .chain(save_that.into_iter())
        .chain(set_arg.into_iter())
        .chain(set_lcl.into_iter())
        .chain(goto_fn.into_iter())
        .chain(label_return_address.into_iter())
        .collect()
}

fn set_arg_for_call(num_arguments: u16) -> Vec<assemble::AssemblyLine> {
    let fixed_sp_decrement = 5;
    let sp_decrement = fixed_sp_decrement + num_arguments;

    vec![
        // @SP
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        // D=A
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        // @5 + number of arguments.
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Address(sp_decrement),
        )),
        // D=D-A
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::DMinusA,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        // @ARG
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(ARG.to_string()),
        )),
        // A=M
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        // M=D
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::D,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn set_lcl_for_call() -> Vec<assemble::AssemblyLine> {
    vec![
        // @SP
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(SP.to_string()),
        )),
        // D=M
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        // @LCL
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(LCL.to_string()),
        )),
        // M=D
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::D,
            destination: assemble::Destination::M,
            jump: assemble::Jump::Null,
        })),
    ]
}

fn push_address_to_stack(symbol: String) -> Vec<assemble::AssemblyLine> {
    let load_address = vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(symbol),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::A,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
    ];

    load_address
        .into_iter()
        .chain(push_to_stack().into_iter())
        .collect()
}

fn label(symbol: assemble::Symbol, program_name: &str) -> Vec<assemble::AssemblyLine> {
    vec![assemble::AssemblyLine::LabelDeclaration(assemble::Symbol(
        format!("{}.{}", program_name, symbol.0),
    ))]
}

fn goto(symbol: assemble::Symbol, program_name: &str) -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(format!("{}.{}", program_name, symbol.0)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JMP,
        })),
    ]
}

fn if_goto(symbol: assemble::Symbol, program_name: &str) -> Vec<assemble::AssemblyLine> {
    vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(String::from(SP)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusOne,
            destination: assemble::Destination::A,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::M,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(format!("{}.{}", program_name, symbol.0)),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::Zero,
            destination: assemble::Destination::Null,
            jump: assemble::Jump::JNE,
        })),
    ]
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
            computation: assemble::Computation::MinusOne,
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
    compare(assemble::Jump::JGT)
}

fn lt() -> Vec<assemble::AssemblyLine> {
    compare(assemble::Jump::JLT)
}

fn compare(jump: assemble::Jump) -> Vec<assemble::AssemblyLine> {
    let pop_and_load_instructions = pop_and_load();
    let label_set_true = String::from(SET_TRUE);
    let label_set_sp = String::from(SET_SP);

    let greater_than = vec![
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::MMinusD,
            destination: assemble::Destination::D,
            jump: assemble::Jump::Null,
        })),
        assemble::AssemblyLine::Instruction(assemble::Instruction::A(
            assemble::AInstruction::Symbol(label_set_true.clone()),
        )),
        assemble::AssemblyLine::Instruction(assemble::Instruction::C(assemble::CInstruction {
            computation: assemble::Computation::D,
            destination: assemble::Destination::Null,
            jump,
        })),
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
        assemble::AssemblyLine::LabelDeclaration(assemble::Symbol(label_set_true)),
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
M=-1
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
D=0
@SET_SP
0;JMP
(SET_TRUE)
D=-1
@SET_SP
0;JMP
(SET_SP)
@SP
A=M-1
M=D"
                .to_string(),
            },
            TestCase {
                command: "lt".to_string(),
                expected_assembly: "@SP
M=M-1
A=M
D=M
A=A-1
D=M-D
@SET_TRUE
D;JLT
D=0
@SET_SP
0;JMP
(SET_TRUE)
D=-1
@SET_SP
0;JMP
(SET_SP)
@SP
A=M-1
M=D"
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

    #[test]
    fn test_branch() {
        struct TestCase {
            command: String,
            program_name: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                command: "label MY_LABEL".to_string(),
                program_name: "MyTest".to_string(),
                expected_assembly: "(MyTest.MY_LABEL)".to_string(),
            },
            TestCase {
                command: "goto ANOTHER_LABEL".to_string(),
                program_name: "AnotherTest".to_string(),
                expected_assembly: "@AnotherTest.ANOTHER_LABEL
0;JMP"
                    .to_string(),
            },
            TestCase {
                command: "if-goto MY_LABEL".to_string(),
                program_name: "Test".to_string(),
                expected_assembly: "@SP
A=M-1
D=M
@Test.MY_LABEL
0;JNE"
                    .to_string(),
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
    fn test_function_definition() {
        struct TestCase {
            command: String,
            program_name: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                command: "function myfn 0".to_string(),
                program_name: "Test".to_string(),
                expected_assembly: "(Test.myfn)".to_string(),
            },
            TestCase {
                command: "function anotherfn 1".to_string(),
                program_name: "MyTest".to_string(),
                expected_assembly: "(MyTest.anotherfn)
@0
D=A
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
            },
            TestCase {
                command: "function anotherfn 2".to_string(),
                program_name: "MyTest".to_string(),
                expected_assembly: "(MyTest.anotherfn)
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
@0
D=A
@SP
A=M
M=D
@SP
M=M+1"
                    .to_string(),
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
    fn test_function_call() {
        struct TestCase {
            command: String,
            program_name: String,
            expected_assembly: String,
        }

        let test_cases = vec![
            TestCase {
                // Include a function definition to specify the return address label.
                command: "function currentfn 0
call myfn 0"
                    .to_string(),
                program_name: "Test".to_string(),
                expected_assembly: format!(
                    "{}
{}",
                    fn_declaration("Test.currentfn"),
                    fn_call("Test.currentfn", "Test.myfn", 0, 0)
                ),
            },
            TestCase {
                // Include a function definition to specify the return address label.
                command: "function my_currentfn 0
call anotherfn 1"
                    .to_string(),
                program_name: "MyTest".to_string(),
                expected_assembly: format!(
                    "{}
{}",
                    fn_declaration("MyTest.my_currentfn"),
                    fn_call("MyTest.my_currentfn", "MyTest.anotherfn", 0, 1)
                ),
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

    fn fn_declaration(name: &str) -> String {
        format!("({})", name)
    }

    fn fn_call(caller: &str, callee: &str, nth_call_in_scope: u16, num_args: u16) -> String {
        let fixed_sp_decrement = 5;
        let decrement = fixed_sp_decrement + num_args;

        format!(
            // save return address on stack.
            // Save LCL on stack.
            // Save ARG on stack.
            // Save THIS on stack.
            // Save THAT on stack.
            // ARG = SP - 5 - nArgs
            // LCL = SP
            // goto f
            // (return address)
            "@{}$ret{}
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=A
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=A
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=A
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
D=A
@{}
D=D-A
@ARG
A=M
M=D
@SP
D=M
@LCL
M=D
@{}
0;JMP
({}$ret{})",
            caller, nth_call_in_scope, decrement, callee, caller, nth_call_in_scope
        )
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
