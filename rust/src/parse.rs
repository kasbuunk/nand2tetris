use std::fmt::{self, Display};

pub fn assemble(assembly_code: &str) -> Result<String, ParseError> {
    let assembly_lines = parse(assembly_code)?;

    // Initialise symbol table.
    let predefined_symbols = initialise_symbol_table();

    // First pass: gather all used symbols and labels.
    let label_symbols = find_label_symbols(&assembly_lines);

    // Build symbol_table.
    let symbol_table = build_symbol_table(predefined_symbols, label_symbols);

    // Second pass: Generate machine code.
    let machine_code = generate_machine_code(assembly_lines, symbol_table);

    Ok(machine_code.to_string())
}

fn initialise_symbol_table() -> SymbolTable {
    SymbolTable(vec![])
}

fn find_label_symbols(_: &Vec<AssemblyLine>) -> SymbolTable {
    SymbolTable(vec![])
}

fn build_symbol_table(x: SymbolTable, _: SymbolTable) -> SymbolTable {
    x
}

fn generate_machine_code(assembly_lines: Vec<AssemblyLine>, _: SymbolTable) -> String {
    assembly_lines
        .into_iter()
        .map(generate_machine_line)
        .collect::<Vec<String>>()
        .into_iter()
        .filter(|s| *s != "")
        .collect::<Vec<String>>()
        .join("\n")
}

fn generate_machine_line(assembly_line: AssemblyLine) -> String {
    assembly_line.into()
}

enum Assembler {
    Initialise,
    FirstPass,
    SecondPass,
}

fn parse(assembly_lines: &str) -> Result<Vec<AssemblyLine>, ParseError> {
    assembly_lines.lines().map(parse_line).collect()
}

fn parse_line(assembly_line: &str) -> Result<AssemblyLine, ParseError> {
    let trimmed_line = assembly_line.trim();

    let parsed_line = match trimmed_line {
        comment if comment.starts_with("//") => AssemblyLine::Comment(String::from(comment)),
        a_instruction if a_instruction.starts_with("@") => {
            let symbol = a_instruction.chars().skip(1).collect::<String>();

            let a_instruction_content = match symbol.parse::<u16>() {
                Ok(address) => AInstruction::Address(address),
                Err(_) => AInstruction::Symbol(symbol),
            };
            // TODO: parse type of symbol, decimal value or symbol bound to such value.
            AssemblyLine::Instruction(Instruction::A(a_instruction_content))
        }
        d_eq_a if d_eq_a.starts_with("D=A") => {
            AssemblyLine::Instruction(Instruction::C(CInstruction {
                destination: Destination::D,
                computation: Computation::A,
                jump: Jump::Null,
            }))
        }
        _ => return Err(ParseError::InvalidInput),
    };

    Ok(parsed_line)
}

struct SymbolTable(Vec<(Symbol, u32)>);

#[derive(Debug, Clone)]
pub enum ParseError {
    InvalidInput,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "no comment")
    }
}

#[derive(PartialEq, Debug)]
enum Bit {
    Zero,
    One,
}

#[derive(PartialEq, Debug)]
struct MemoryAddress(u16);

#[derive(PartialEq, Debug)]
enum AssemblyLine {
    Instruction(Instruction),
    LabelDeclaration(LabelSymbol),
    Comment(String),
}

fn u16_to_binary(n: u16) -> String {
    format!("{:#018b}", n).chars().skip(2).collect::<String>()
}

impl Into<String> for AssemblyLine {
    fn into(self) -> String {
        match self {
            AssemblyLine::Comment(_) => "".into(),
            AssemblyLine::Instruction(Instruction::A(AInstruction::Symbol(decimal))) => {
                // TODO: validate before that this is u16 and type it.
                let number = decimal.parse::<u16>().unwrap();
                u16_to_binary(number)
            }
            AssemblyLine::Instruction(Instruction::A(AInstruction::Address(address))) => {
                u16_to_binary(address)
            }
            AssemblyLine::Instruction(Instruction::C(c_instruction)) => {
                let computation: String = c_instruction.computation.into();
                let destination: String = c_instruction.destination.into();
                let jump: String = c_instruction.jump.into();

                format!("111{}{}{}", computation, destination, jump,)
            }
            _ => todo!(),
        }
    }
}

#[derive(PartialEq, Debug)]
enum Instruction {
    A(AInstruction),
    C(CInstruction),
}

enum Symbol {
    Predefined(PredefinedSymbol),
    Label(LabelSymbol),
    Variable(VariableSymbol),
}

#[derive(PartialEq, Debug)]
enum AInstruction {
    Symbol(String),
    Address(u16),
}

#[derive(PartialEq, Debug)]
struct CInstruction {
    computation: Computation,
    destination: Destination,
    jump: Jump,
}

impl Into<String> for Computation {
    fn into(self) -> String {
        match self {
            Computation::A => "0110000".into(),
            _ => todo!(),
        }
    }
}

impl Into<String> for Destination {
    fn into(self) -> String {
        match self {
            Destination::D => "010".into(),
            _ => todo!(),
        }
    }
}
impl Into<String> for Jump {
    fn into(self) -> String {
        match self {
            Jump::Null => "000".into(),
            _ => todo!(),
        }
    }
}

#[derive(PartialEq, Debug)]
enum Computation {
    Zero,
    One,
    MinusOne,
    D,
    A,
    NotD,
    NotA,
    MinusD,
    MinusA,
    DPlusOne,
    APlusOne,
    DMinusOne,
    AMinusOne,
    DPlusA,
    DMinusA,
    AMinusD,
    DAndA,
    DOrA,
    M,
    NotM,
    MinusM,
    MPlusOne,
    MMinusOne,
    DPlusM,
    DMinusM,
    MMinusD,
    DAndM,
    DOrM,
}

#[derive(PartialEq, Debug)]
enum Destination {
    Null,
    M,
    D,
    DM,
    A,
    AM,
    AD,
    ADM,
}

#[derive(PartialEq, Debug)]
enum Jump {
    Null,
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
}

enum PredefinedSymbol {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SP,
    LCL,
    Arg,
    This,
    That,
    Screen,
    KBD,
}

#[derive(PartialEq, Debug)]
struct LabelSymbol {
    name: String,
    value: MemoryAddress,
}

struct VariableSymbol(String);

// TODO: Syntax: Symbols, Constants and White space (p. 106).

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_line() {
        struct TestCase {
            name: String,
            assembly_line: String,
            expected_parsed_assembly_line: AssemblyLine,
        }

        let test_cases = vec![
            TestCase {
                name: "comment".into(),
                assembly_line: "// I am a comment.".into(),
                expected_parsed_assembly_line: AssemblyLine::Comment("// I am a comment.".into()),
            },
            TestCase {
                name: "variable_declaration".into(),
                assembly_line: "@i".into(),
                expected_parsed_assembly_line: AssemblyLine::Instruction(Instruction::A(
                    AInstruction::Symbol("i".into()),
                )),
            },
            TestCase {
                name: "address".into(),
                assembly_line: "@18".into(),
                expected_parsed_assembly_line: AssemblyLine::Instruction(Instruction::A(
                    AInstruction::Address(18),
                )),
            },
            TestCase {
                name: "set_d".into(),
                assembly_line: "D=A".into(),
                expected_parsed_assembly_line: AssemblyLine::Instruction(Instruction::C(
                    CInstruction {
                        computation: Computation::A,
                        destination: Destination::D,
                        jump: Jump::Null,
                    },
                )),
            },
        ];

        for test_case in test_cases {
            let output = parse_line(&test_case.assembly_line).expect(&format!(
                "could not parse line '{}'",
                &test_case.assembly_line
            ));
            assert_eq!(
                test_case.expected_parsed_assembly_line, output,
                "failed {}: expected: {:?}; got {:?}",
                test_case.name, test_case.expected_parsed_assembly_line, output,
            );
        }
    }

    #[test]
    fn test_generate_machine_code() {
        struct TestCase {
            name: String,
            assembly_line: AssemblyLine,
            expected_machine_code: String,
        }

        let test_cases = vec![
            TestCase {
                name: "comment".into(),
                assembly_line: AssemblyLine::Comment("// I am a comment".into()),
                expected_machine_code: "".into(),
            },
            TestCase {
                name: "@17".into(),
                assembly_line: AssemblyLine::Instruction(Instruction::A(AInstruction::Address(17))),
                expected_machine_code: "0000000000010001".into(),
            },
            TestCase {
                name: "D=A".into(),
                assembly_line: AssemblyLine::Instruction(Instruction::C(CInstruction {
                    computation: Computation::A,
                    destination: Destination::D,
                    jump: Jump::Null,
                })),
                expected_machine_code: "1110110000010000".into(),
            },
        ];

        for test_case in test_cases {
            let output = generate_machine_line(test_case.assembly_line);

            assert_eq!(
                test_case.expected_machine_code, output,
                "{} failed: {}, {}",
                &test_case.name, &test_case.expected_machine_code, &output
            );
        }
    }

    static sum_1_to_n_asm: &str = "// i = 1
@i
M=1
// sum = 0
@sum
M=0
(LOOP)
// things
@i
D=M
@R0
D=D-M
@STOP
D;JGT
// sum = sum + 1
@sum
D=M
@i
D=D+M
@sum
M=D
// i = i + 1
@i
M=M+1
// goto Loop
@LOOP
0;JMP
(STOP)
// R1 = sum
@sum
D=M
@R1
M=D
(END)
@END
0;JMP";
    static sum_1_to_n_bin: &str = "0000000000010000
1110111111001000
0000000000010001
1110101010001000
0000000000010000
1111110000010000
0000000000000000
1111010011010000
0000000000010100
1110001100000001
0000000000010001
1111110000010000
0000000000010000
1111000010010000
0000000000010001
1110001100001000
0000000000010000
1111110111001000
0000000000000100
1110101010000111
0000000000010001
1111110000010000
0000000000000001
1110001100001000
0000000000011000
1110101010000111";

    static d_eq_17_asm: &str = "// d = 17
@17
D=A";
    static d_eq_17_bin: &str = "0000000000010001
1110110000010000";

    static ram_set_asm: &str = "// RAM[100] = 17
@17
D=A
@100
M=D";
    static ram_set_bin: &str = "0000000000010001
1110110000010000
0000000001100100
1110001100001000";

    static cp_ram_asm: &str = "// RAM[100] = RAM[200]
@200
D=M
@100
M=D";
    static cp_ram_bin: &str = "0000000011001000
1111110000010000
0000000001100100
1110001100001000";

    static goto_29_asm: &str = "// goto 29
@29
0;JMP";
    static goto_29_bin: &str = "0000000000011101
1110101010000111";

    static goto_conditional_asm: &str = "// if D>0 goto 63
@63
D;JGT";
    static goto_conditional_bin: &str = "0000000000111111
1110001100000001";

    static set_x_asm: &str = "// x = -1
@x
M=-1";
    static set_x_bin: &str = "0000000000010000
1110111010001000";

    static decrement_asm: &str = "// count = count - 1
@count
M=M-1";
    static decrement_bin: &str = "0000000000010000
1111110010001000";

    static increase_by_x_asm: &str = "// sum = sum + x
@sum
D=M
@x
D=D+M
@sum
M=D";
    static increase_by_x_bin: &str = "0000000000010000
1111110000010000
0000000000010001
1111000010010000
0000000000010000
1110001100001000";

    #[test]
    fn test_parse() {
        let test_cases = vec![
            ("empty", "", ""),
            ("comment", "// only some comment", ""),
            ("set_d", d_eq_17_asm, d_eq_17_bin),
            // ("set_ram", ram_set_asm, ram_set_bin),
            // ("copy_ram", cp_ram_asm, cp_ram_bin),
            // ("goto", goto_29_asm, goto_29_bin),
            // ("cond_goto", goto_conditional_asm, goto_conditional_bin),
            // ("set_x", set_x_asm, set_x_bin),
            // ("decrement", decrement_asm, decrement_bin),
            // ("increase_by", increase_by_x_asm, increase_by_x_bin),
            // ("sum_1_to_n", sum_1_to_n_asm, sum_1_to_n_bin),
        ];

        for test_case in test_cases {
            let output = assemble(&test_case.1).expect("assemble failed");
            assert_eq!(test_case.2, output, "test case failed: {}", test_case.0);
        }
    }
}
