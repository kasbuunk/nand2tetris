// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// The algorithm is based on repetitive addition.

(LOOP)
	// D = RAM[0]
	@R0
	D=M
	// Stop if R0 is 0.
	@END
	D;JEQ
	// Decrement R0
	@R0
	M=M-1
	// RAM[2] = RAM[2] + RAM[1].
	@R1
	D=M
	@R3
	M=D+M
	// Back to start of loop.
	@LOOP
	0;JMP
(END)
	@END
	0;JMP
