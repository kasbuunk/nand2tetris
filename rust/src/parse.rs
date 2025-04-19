use std::fmt;

pub fn assemble(assembly_code: &str) -> Result<String, ParseError> {
    let assembly_lines = parse(assembly_code)?;

    // Initialise symbol table.
    let predefined_symbols = initialise_symbol_table();

    // First pass: gather all used symbols and labels.
    let label_symbols = find_label_symbols(&assembly_lines);

    // Build symbol_table.
    let symbol_table = build_symbol_table(predefined_symbols, label_symbols);

    // Second pass: Generate machine code.
    let machine_instructions = generate_machine_code(assembly_lines, symbol_table);

    // Format instructions as binary code of zeroes and ones in ASCII.
    let machine_code = machine_instructions_to_string(machine_instructions);

    Ok(machine_code)
}

fn parse(assembly_lines: &str) -> Result<Vec<AssemblyLine>, ParseError> {
    assembly_lines.lines().map(parse_line).collect()
}

fn initialise_symbol_table() -> SymbolTable {
    SymbolTable(vec![
        (Symbol(String::from("R0")), Address(0)),
        (Symbol(String::from("R1")), Address(1)),
        (Symbol(String::from("R2")), Address(2)),
        (Symbol(String::from("R3")), Address(3)),
        (Symbol(String::from("R4")), Address(4)),
        (Symbol(String::from("R5")), Address(5)),
        (Symbol(String::from("R6")), Address(6)),
        (Symbol(String::from("R7")), Address(7)),
        (Symbol(String::from("R8")), Address(8)),
        (Symbol(String::from("R9")), Address(9)),
        (Symbol(String::from("R10")), Address(10)),
        (Symbol(String::from("R11")), Address(11)),
        (Symbol(String::from("R12")), Address(12)),
        (Symbol(String::from("R13")), Address(13)),
        (Symbol(String::from("R14")), Address(14)),
        (Symbol(String::from("R15")), Address(15)),
        (Symbol(String::from("SP")), Address(0)),
        (Symbol(String::from("LCL")), Address(1)),
        (Symbol(String::from("ARG")), Address(2)),
        (Symbol(String::from("THIS")), Address(3)),
        (Symbol(String::from("THAT")), Address(4)),
        (Symbol(String::from("SCREEN")), Address(16384)),
        (Symbol(String::from("KBD")), Address(24576)),
    ])
}

fn find_label_symbols(lines: &Vec<AssemblyLine>) -> SymbolTable {
    let mut index: u16 = 0;
    let mut symbol_table: Vec<(Symbol, Address)> = Vec::new();

    for line in lines {
        match line {
            AssemblyLine::LabelDeclaration(Symbol(symbol)) => {
                symbol_table.push((Symbol(symbol.to_owned()), Address(index)));
            }
            AssemblyLine::Instruction(_) => {
                index += 1;
            }
            _ => {}
        }
    }

    SymbolTable(symbol_table)
}

fn build_symbol_table(x: SymbolTable, y: SymbolTable) -> SymbolTable {
    let entries =
        x.0.into_iter()
            .chain(y.0.into_iter())
            .collect::<Vec<(Symbol, Address)>>();
    SymbolTable(entries)
}

fn generate_machine_code(
    assembly_lines: Vec<AssemblyLine>,
    mut symbol_table: SymbolTable,
) -> Vec<MachineInstruction> {
    let mut instructions = Vec::new();
    let mut next_symbol_address = 16;

    for line in assembly_lines {
        match line {
            AssemblyLine::Instruction(Instruction::A(AInstruction::Symbol(s))) => {
                let symbol = Symbol(s);
                let address = match symbol_table.get(&symbol) {
                    None => {
                        let address = Address(next_symbol_address);
                        symbol_table.insert(symbol, address.clone());
                        next_symbol_address += 1;

                        address
                    }
                    Some(address) => address,
                };

                let instruction = MachineInstruction::A(address);

                instructions.push(instruction);
            }
            AssemblyLine::Instruction(Instruction::A(AInstruction::Address(address))) => {
                let instruction = MachineInstruction::A(Address(address));

                instructions.push(instruction);
            }
            AssemblyLine::Instruction(Instruction::C(c_instruction)) => {
                let instruction = MachineInstruction::C(c_instruction);

                instructions.push(instruction);
            }
            _ => {}
        }
    }

    instructions
}

fn machine_instructions_to_string(instructions: Vec<MachineInstruction>) -> String {
    instructions
        .into_iter()
        .map(|x| x.into())
        .collect::<Vec<String>>()
        .join("\n")
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

            AssemblyLine::Instruction(Instruction::A(a_instruction_content))
        }
        label_with_brackets if label_with_brackets.starts_with("(") => {
            let label_with_ending_bracket = label_with_brackets.chars().skip(1).collect::<String>();
            let label = match label_with_ending_bracket.split(")").next() {
                None => {
                    return Err(ParseError::InvalidInput("missing closing bracket".into()));
                }
                Some(label) => label.to_string(),
            };

            AssemblyLine::LabelDeclaration(Symbol(String::from(label)))
        }
        c_instruction => {
            let (destination_str, unconsumed_instruction) = match c_instruction.split_once("=") {
                None => ("null", c_instruction),
                Some((destination, unconsumed_instruction)) => {
                    (destination, unconsumed_instruction)
                }
            };

            let (computation_str, jump_str) = match unconsumed_instruction.split_once(";") {
                None => (unconsumed_instruction, "null"),
                Some((computation, jump)) => (computation, jump),
            };

            let computation = match computation_str {
                "0" => Computation::Zero,
                "1" => Computation::One,
                "-1" => Computation::MinusOne,
                "D" => Computation::D,
                "A" => Computation::A,
                "!D" => Computation::NotD,
                "!A" => Computation::NotA,
                "-D" => Computation::MinusD,
                "-A" => Computation::MinusA,
                "D+1" => Computation::DPlusOne,
                "A+1" => Computation::APlusOne,
                "D-1" => Computation::DMinusOne,
                "A-1" => Computation::AMinusOne,
                "D+A" => Computation::DPlusA,
                "D-A" => Computation::DMinusA,
                "A-D" => Computation::AMinusD,
                "D&A" => Computation::DAndA,
                "D|A" => Computation::DOrA,
                "M" => Computation::M,
                "!M" => Computation::NotM,
                "-M" => Computation::MinusM,
                "M+1" => Computation::MPlusOne,
                "M-1" => Computation::MMinusOne,
                "D+M" => Computation::DPlusM,
                "D-M" => Computation::DMinusM,
                "M-D" => Computation::MMinusD,
                "D&M" => Computation::DAndM,
                "D|M" => Computation::DOrM,
                input => {
                    return Err(ParseError::InvalidInput(format!(
                        "expected computation, got {}",
                        input
                    )));
                }
            };

            let destination = match destination_str {
                "null" => Destination::Null,
                "M" => Destination::M,
                "D" => Destination::D,
                "DM" => Destination::DM,
                "A" => Destination::A,
                "AM" => Destination::AM,
                "AD" => Destination::AD,
                "ADM" => Destination::ADM,
                input => {
                    return Err(ParseError::InvalidInput(format!(
                        "expected destination, got {}",
                        input
                    )));
                }
            };

            let jump = match jump_str {
                "null" => Jump::Null,
                "JGT" => Jump::JGT,
                "JEQ" => Jump::JEQ,
                "JGE" => Jump::JGE,
                "JLT" => Jump::JLT,
                "JNE" => Jump::JNE,
                "JLE" => Jump::JLE,
                "JMP" => Jump::JMP,
                input => {
                    return Err(ParseError::InvalidInput(format!(
                        "expected jump, got {}",
                        input
                    )));
                }
            };

            AssemblyLine::Instruction(Instruction::C(CInstruction {
                destination,
                computation,
                jump,
            }))
        }
    };

    Ok(parsed_line)
}

struct SymbolTable(Vec<(Symbol, Address)>);

impl SymbolTable {
    fn get(&mut self, symbol: &Symbol) -> Option<Address> {
        match self.0.iter().find(|(key, _)| key == symbol) {
            None => None,
            Some((_, value)) => Some((*value).clone()),
        }
    }

    fn insert(&mut self, symbol: Symbol, address: Address) {
        let symbol_entry = (symbol, address);
        self.0.push(symbol_entry);
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    InvalidInput(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidInput(err) => write!(f, "invalid input: {}", err),
        }
    }
}

#[derive(PartialEq, Debug)]
enum AssemblyLine {
    Instruction(Instruction),
    LabelDeclaration(Symbol),
    Comment(String),
}

#[derive(PartialEq, Debug)]
enum MachineInstruction {
    A(Address),
    C(CInstruction),
}

impl Into<String> for MachineInstruction {
    fn into(self) -> String {
        match self {
            MachineInstruction::A(address) => u16_to_binary(address.0),
            MachineInstruction::C(c_instruction) => {
                let computation: String = c_instruction.computation.into();
                let destination: String = c_instruction.destination.into();
                let jump: String = c_instruction.jump.into();

                format!("111{}{}{}", computation, destination, jump,)
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Address(u16);

fn u16_to_binary(n: u16) -> String {
    format!("{:#018b}", n).chars().skip(2).collect::<String>()
}

#[derive(PartialEq, Debug)]
enum Instruction {
    A(AInstruction),
    C(CInstruction),
}

#[derive(PartialEq, Debug)]
struct Symbol(String);

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
            Computation::Zero => "0101010".into(),
            Computation::One => "0111111".into(),
            Computation::MinusOne => "0111010".into(),
            Computation::D => "0001100".into(),
            Computation::A => "0110000".into(),
            Computation::NotD => "0001101".into(),
            Computation::NotA => "0110001".into(),
            Computation::MinusD => "0001111".into(),
            Computation::MinusA => "0110011".into(),
            Computation::DPlusOne => "0011111".into(),
            Computation::APlusOne => "0110111".into(),
            Computation::DMinusOne => "0001110".into(),
            Computation::AMinusOne => "0110010".into(),
            Computation::DPlusA => "0000010".into(),
            Computation::DMinusA => "0010011".into(),
            Computation::AMinusD => "0000111".into(),
            Computation::DAndA => "0000000".into(),
            Computation::DOrA => "0010101".into(),
            Computation::M => "1110000".into(),
            Computation::NotM => "1110001".into(),
            Computation::MinusM => "1110011".into(),
            Computation::MPlusOne => "1110111".into(),
            Computation::MMinusOne => "1110010".into(),
            Computation::DPlusM => "1000010".into(),
            Computation::DMinusM => "1010011".into(),
            Computation::MMinusD => "1000111".into(),
            Computation::DAndM => "1000000".into(),
            Computation::DOrM => "1010101".into(),
        }
    }
}

impl Into<String> for Destination {
    fn into(self) -> String {
        match self {
            Destination::Null => "000".into(),
            Destination::M => "001".into(),
            Destination::D => "010".into(),
            Destination::DM => "011".into(),
            Destination::A => "100".into(),
            Destination::AM => "101".into(),
            Destination::AD => "110".into(),
            Destination::ADM => "111".into(),
        }
    }
}
impl Into<String> for Jump {
    fn into(self) -> String {
        match self {
            Jump::Null => "000".into(),
            Jump::JGT => "001".into(),
            Jump::JEQ => "010".into(),
            Jump::JGE => "011".into(),
            Jump::JLT => "100".into(),
            Jump::JNE => "101".into(),
            Jump::JLE => "110".into(),
            Jump::JMP => "111".into(),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_instruction() {
        struct TestCase {
            assembly_line: String,
            expected: String,
        }

        let test_cases = vec![
            TestCase {
                assembly_line: "0".into(),
                expected: "1110101010000000".into(),
            },
            TestCase {
                assembly_line: "M=1;JGT".into(),
                expected: "1110111111001001".into(),
            },
            TestCase {
                assembly_line: "D=-1;JEQ".into(),
                expected: "1110111010010010".into(),
            },
            TestCase {
                assembly_line: "DM=D;JGE".into(),
                expected: "1110001100011011".into(),
            },
            TestCase {
                assembly_line: "A=A;JLT".into(),
                expected: "1110110000100100".into(),
            },
            TestCase {
                assembly_line: "AM=!D;JNE".into(),
                expected: "1110001101101101".into(),
            },
            TestCase {
                assembly_line: "AD=!A;JLE".into(),
                expected: "1110110001110110".into(),
            },
            TestCase {
                assembly_line: "ADM=-D;JMP".into(),
                expected: "1110001111111111".into(),
            },
            TestCase {
                assembly_line: "-A".into(),
                expected: "1110110011000000".into(),
            },
            TestCase {
                assembly_line: "D+1".into(),
                expected: "1110011111000000".into(),
            },
            TestCase {
                assembly_line: "A+1".into(),
                expected: "1110110111000000".into(),
            },
            TestCase {
                assembly_line: "D-1".into(),
                expected: "1110001110000000".into(),
            },
            TestCase {
                assembly_line: "A-1".into(),
                expected: "1110110010000000".into(),
            },
            TestCase {
                assembly_line: "D+A".into(),
                expected: "1110000010000000".into(),
            },
            TestCase {
                assembly_line: "D-A".into(),
                expected: "1110010011000000".into(),
            },
            TestCase {
                assembly_line: "A-D".into(),
                expected: "1110000111000000".into(),
            },
            TestCase {
                assembly_line: "D&A".into(),
                expected: "1110000000000000".into(),
            },
            TestCase {
                assembly_line: "D|A".into(),
                expected: "1110010101000000".into(),
            },
            TestCase {
                assembly_line: "M".into(),
                expected: "1111110000000000".into(),
            },
            TestCase {
                assembly_line: "!M".into(),
                expected: "1111110001000000".into(),
            },
            TestCase {
                assembly_line: "-M".into(),
                expected: "1111110011000000".into(),
            },
            TestCase {
                assembly_line: "M+1".into(),
                expected: "1111110111000000".into(),
            },
            TestCase {
                assembly_line: "M-1".into(),
                expected: "1111110010000000".into(),
            },
            TestCase {
                assembly_line: "D+M".into(),
                expected: "1111000010000000".into(),
            },
            TestCase {
                assembly_line: "D-M".into(),
                expected: "1111010011000000".into(),
            },
            TestCase {
                assembly_line: "M-D".into(),
                expected: "1111000111000000".into(),
            },
            TestCase {
                assembly_line: "D&M".into(),
                expected: "1111000000000000".into(),
            },
            TestCase {
                assembly_line: "D|M".into(),
                expected: "1111010101000000".into(),
            },
        ];

        for test_case in test_cases {
            let output: String = assemble(&test_case.assembly_line).unwrap();
            assert_eq!(
                test_case.expected, output,
                "failed {}: expected: {:?}; got {:?}",
                test_case.assembly_line, test_case.expected, output,
            );
        }
    }

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
            let output =
                generate_machine_code(vec![test_case.assembly_line], initialise_symbol_table());
            let code = machine_instructions_to_string(output);

            assert_eq!(
                test_case.expected_machine_code,
                code.clone(),
                "{} failed: expected {}; actual {}",
                &test_case.name,
                &test_case.expected_machine_code,
                &code
            );
        }
    }

    #[test]
    fn test_parse() {
        let sum_1_to_n_asm: &str = "// i = 1
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
        let sum_1_to_n_bin: &str = "0000000000010000
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

        let d_eq_17_asm: &str = "// d = 17
@17
D=A";
        let d_eq_17_bin: &str = "0000000000010001
1110110000010000";

        let ram_set_asm: &str = "// RAM[100] = 17
@17
D=A
@100
M=D";
        let ram_set_bin: &str = "0000000000010001
1110110000010000
0000000001100100
1110001100001000";

        let cp_ram_asm: &str = "// RAM[100] = RAM[200]
@200
D=M
@100
M=D";
        let cp_ram_bin: &str = "0000000011001000
1111110000010000
0000000001100100
1110001100001000";

        let goto_29_asm: &str = "// goto 29
@29
0;JMP";
        let goto_29_bin: &str = "0000000000011101
1110101010000111";

        let goto_conditional_asm: &str = "// if D>0 goto 63
@63
D;JGT";
        let goto_conditional_bin: &str = "0000000000111111
1110001100000001";

        let set_x_asm: &str = "// x = -1
@x
M=-1";
        let set_x_bin: &str = "0000000000010000
1110111010001000";

        let decrement_asm: &str = "// count = count - 1
@count
M=M-1";
        let decrement_bin: &str = "0000000000010000
1111110010001000";

        let increase_by_x_asm: &str = "// sum = sum + x
@sum
D=M
@x
D=D+M
@sum
M=D";
        let increase_by_x_bin: &str = "0000000000010000
1111110000010000
0000000000010001
1111000010010000
0000000000010000
1110001100001000";

        let test_cases = vec![
            ("empty", "", ""),
            ("comment", "// only some comment", ""),
            ("set_d", d_eq_17_asm, d_eq_17_bin),
            ("set_ram", ram_set_asm, ram_set_bin),
            ("copy_ram", cp_ram_asm, cp_ram_bin),
            ("goto", goto_29_asm, goto_29_bin),
            ("cond_goto", goto_conditional_asm, goto_conditional_bin),
            ("set_x", set_x_asm, set_x_bin),
            ("decrement", decrement_asm, decrement_bin),
            ("increase_by", increase_by_x_asm, increase_by_x_bin),
            ("sum_1_to_n", sum_1_to_n_asm, sum_1_to_n_bin),
        ];

        for test_case in test_cases {
            let output = assemble(&test_case.1).expect("assemble failed");
            assert_eq!(
                test_case.2, output,
                "failed {}: expected {}, actual {}",
                test_case.0, test_case.2, output
            );
        }
    }
}
