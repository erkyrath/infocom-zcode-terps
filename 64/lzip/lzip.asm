

	TITLE "LOWER CASE EZIP/6502 INFOCOM, INC. --- EQUATES"

	; --------------------------
	; LOWER CASE EZIP/6502 VERSION A
	; Z-CODE INTERPRETER PROGRAM
	; FOR COMMODORE 64
	; --------------------------

	; INFOCOM, INC.
	; 125 CAMBRIDGEPARK DRIVE
	; CAMBRIDGE, MA 02140

	; COMPANY PRIVATE -- NOT FOR DISTRIBUTION

MSTART	EQU	$800		; START OF FREE PROGRAM RAM
ZEROPG	EQU	$03		; START OF FREE Z-PAGE RAM
ZPGTOP	EQU	$8F		; END OF FREE Z-PAGE RAM

DEBUG	EQU	0		; ASSEMBLY FLAG FOR DEBUGGER (1=YES,0=NO)
TESTING	EQU	1		; ASS. FLG FOR TESTING MSG


	; CHANGES FOR LOWER CASE
	; 
	; - SCREEN CONTROLS ADAPTED FROM C64 ZIP
	; - GETBYT, NEXTPC, GETDSK SET TO USE MEMORY ARRANGEMENT OF
	;   $800 = BUFFERS, FOLLOWED BY LCEZIP, THEN GAME PRELOAD
	;   UP TO $CFFF (GIVING A PRELOAD SIZE LIMIT OF 38K)
	;   AND SWITCHING OUT KERNAL FROM $E000-$FFFF TO USE THE
	;   RAM UNDERNEATH FOR PAGING, SO ALL MEMORY USE IN THE 3
	;   MENTIONED RTNS CHECKS FOR START OF PAGING, ALL OTHER MEMORY
	;   ACCESS (AS BEFORE) ASSUMES DIRECT ACCESS ALLOWED.
	; - "PLOT" GIVES Y = WIDTH, X = LENGTH   C-64 & LC-A USE
	;   X = WIDTH, Y = LENGTH


	; -----------
	; ERROR CODES
	; -----------

	; 00 -- INSUFFICIENT RAM
	; 01 -- ILLEGAL X-OP
	; 02 -- ILLEGAL 0-OP
	; 03 -- ILLEGAL 1-OP
	; 04 -- ILLEGAL 2-OP
	; 05 -- Z-STACK UNDERFLOW
	; 06 -- Z-STACK OVERFLOW
	; 07 -- ILLEGAL PROPERTY LENGTH (GETP)
	; 08 -- DIVISION BY ZERO
	; 09 -- ILLEGAL ARGUMENT COUNT (EQUAL?)
	; 10 -- ILLEGAL PROPERTY ID (PUTP)
	; 11 -- ILLEGAL PROPERTY LENGTH (PUTP)
	; 12 -- DISK ADDRESS OUT OF RANGE
	; 13 -- PRELOAD/IMPURE TOO LARGE
	; 14 -- DRIVE ACCESS
	; 15 -- Z-STACK DESTROYED
	; 16 -- NOT AN EZIP GAME

	; NOTE: DISK ACCESS IS SET SO THAT SIDE 1 STARTS ON TRACK 3
	; SIDE 2 ON TRACK 1 AND SAVES STARTING ON TRACK 1. THIS IS IN 
	; THE CODE IN ZDOS.ASM (APPEARS NOWHERE ELSE 3/18/86 LE)

	INCLUD SHAKE.ASM
	INCLUD EQ.ASM

	TITLE "LC EZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT"
	INCLUD HARDEQ.ASM
	INCLUD COLD.ASM

	TITLE "LC EZIP/6502 INFOCOM, INC. --- INIT & MAINLINE"
	INCLUD WARM.ASM
	INCLUD MAIN.ASM
	INCLUD SUBS.ASM
	INCLUD DISPATCH.ASM

	TITLE "LC EZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS"
	INCLUD OPS012.ASM
	INCLUD OPSX.ASM
	INCLUD READ.ASM

	TITLE "LC EZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT"
	INCLUD XPAGING.ASM
	INCLUD ZSTRING.ASM
	INCLUD OBJECTS.ASM

	TITLE "LC EZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O"

	INCLUD IO.ASM
	INCLUD MACHINE.ASM
	INCLUD ZDOS.ASM
	INCLUD DISK.ASM
	INCLUD FAST.ASM

	IF	DEBUG
	INCLUD BUGGER.ASM
	ENDIF

	END

