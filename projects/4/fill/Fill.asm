// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.

(CLEARED)
	// Fill if key is pressed.
	@KBD 
	D=M
	@FILL
	D;JNE
	@CLEARED
	0;JMP
(CLEAR)
	// Make all pixels white.
	@SCREEN
	D=A
	@PIXEL
	M=D
	@CLEARING
	0;JMP
(CLEARING)
	// Done if PIXEL reaches KBD, i.e. end of screen.
	@PIXEL
	D=M
	@KBD
	D=A-D
	@CLEARED
	D;JEQ
	// Whiten current PIXEL.
	@PIXEL
	A=M
	M=0
	// Increment PIXEL.
	D=A+1
	@PIXEL
	M=D
	// Loop.
	@CLEARING
	0;JMP
(FILL)
	// Make all pixels black.
	@SCREEN
	D=A
	@PIXEL
	M=D
	@FILLING
	0;JMP
(FILLING)
	// Done if PIXEL reaches KBD, i.e. end of screen.
	@PIXEL
	D=M
	@KBD
	D=A-D
	@FILLED
	D;JEQ
	// Blacken current PIXEL.
	@PIXEL
	A=M
	M=-1
	// Increment PIXEL.
	D=A+1
	@PIXEL
	M=D
	// Loop.
	@FILLING
	0;JMP
(FILLED)
	// Clear if no key is pressed.
	@KBD 
	D=M
	@CLEAR
	D;JEQ
	@FILLED
	0;JMP
